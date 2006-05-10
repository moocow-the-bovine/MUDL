##-*- Mode: CPerl -*-

## File: MUDL::Morph::Goldsmith.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Goldsmith (2001,2004) morphology induction
##======================================================================

package MUDL::Morph::Goldsmith;
use MUDL::Trie::FreqPair;
use MUDL::Corpus::Profile;
use MUDL::Enum;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile);

##======================================================================
## Globals

##-- verbosity levels
our %VLEVELS = (
		none=>0,
		error=>1,
		warn=>2,
		info=>3,
		debug=>10,
		full=>255,

		default=>'debug',
	       );


##======================================================================
## $prof = $class_or_obj->new(%args)
##   + %args:
##
##     ##-- profiling arguments
##     #bos    => $bos_str,     ##-- bos string
##     #eos    => $eos_str,     ##-- eos string
##     tsplit  => $re_or_undef, ##-- split regex for input tokens (undef to profile whole words as symbols)
##     #tjoin   => $str,         ##-- join string for debug
##     reversed=>$bool,         ##-- if true, search for suffxies
##
##     ##-- induction arguments
##     minStemLen1 => $len,              ##-- minimum stem length for succFreqs1() (default=3)
##     maxAffixLen => $len,              ##-- maximum suffix length (default=5)
##     maxNeighborSuccCount1 => $count,  ##-- maximum neighbor successor count for succFreqs1() (default=1)
##     minStemsPerAffix=>$count,         ##-- min(card(stems(affix))) for succFreqs1()
##     minStemsPerLargeSig=>$count,      ##-- min(card(stems(sig))) for large sigs in succFreqs1() (default=25)
##     minLongAffixLen=>$len,            ##-- min(length(affix)) for small sigs in succFreqs1() (default=2)
##     minLongAffixCount=>$count,        ##-- min(count(long-affixes)) for small sigs (default=2)
##
##     ##-- profiling data
##     abet     => $abet,     ##-- a MUDL::Gfsm::Alphabet
##     pta      => $pta,      ##-- a MUDL::Gfsm::FreqTrie (using $prof->{abet}, reversed=>0)
##     wenum    => $enum      ##-- enumerates whole-word label-vectors (packed)
##
##     ##-- model data
##     cuts    => \%cuts,     ##-- suffix-cuts: maps word-id ($morph->{wenum}) to suffix cuts (sorted vec)
##                            ##     id($w) => $cutvec = pack('S*', @cuts)
##                            ##   such that:
##                            ##     stem($w)      = $w[0             ..  $cuts[0   ] ]
##                            ##     suffix($w,$i) = $w[($cuts[$i]+1) .. ($cuts[$i+1])]
##                            ##     ##-- ($i)-th suffix, where ($i==$#cuts) implies $cuts[$i+1]==$#w
##                            ##   ...i.e. $word is split right *after* $cuts[$i] (counting from 0)
##
##     f2t     => \%f2t,      ##-- maps affixes to pseudo-set (hash) of co-occurrent stems (vectors)
##                            ##     $suffix_vector => { $stemvec1=>undef, ..., $stemvecN=>undef }
##
##     t2sig   => \%t2sig,    ##-- maps stems to their sigs (packed sorted list of suffix vectors)
##                            ##     $stemvec => $sig = pack('(S/a*)*', sort @suffix_vectors);
##                            ##       where:
##                            ##     @suffix_vectors = map { pack('S*',@$_) } @suffix_labarys;
##                            ##       alternatively:
##                            ##     @suffix_vectors = unpack('(S/a*)*', $morph->{t2sig}{$stemvec});
##                            ##     @suffix_labarys = map { [unpack('S*', $_)] } @suffix_vectors;
##
##     sig2t   => \%sig2t,    ##-- maps sigs  to pseudo-hashes of the stems they contain
##                            ##     ($sig=$sufflist=pack('(S/a*)*', sort @suffix_vectors))
##                            ##      => { $stemvec1=>undef, ..., $stemvecN=>undef }
##
##     sig2robust => \%sig2r, ##-- "robustness" value for each signature, defined as:
##                            ##   log2($abet->size) * (  $avg_stem_len*($nsuffs($sig)-1)
##                            ##                        + $avg_suff_len*($nstems($sig)-1) )
##
##     ##-- useful flags
##     #leftMargin => $int,    ##-- length (in symbols) to allow for BOS
##     #rightMargin => $int,   ##-- length (in symbols) to allow for EOS
sub new {
  my ($that,%args) = @_;

  my $self = $that->SUPER::new
    (
     ##-- profiling arguments
     #bos=>'__$',
     #eos=>'__$',
     tsplit=>undef,
     #tjoin=>'',
     nosta=>0,

     ##-- induction arguments
     minStemLen1 => 3,
     maxNeighborSuccCount1 => 1,
     maxAffixLen=>5,
     minStemsPerAffix=>3,
     minStemsPerLargeSig=>25,
     minLongAffixLen=>2,
     minLongAffixCount=>2,

     ##-- profiling data
     #abet=>MUDL::Gfsm::Alphabet->new(),
     pta=>MUDL::Gfsm::FreqTrie->new(reversed=>0),
     wenum=>MUDL::Enum->new(),

     ##-- model data
     ##...

     ##-- debug
     verbose => 'default',

     ##-- user args
     %args,
    );

  ##-- undefine tsplit if it is the string 'undef'
  $self->{tsplit} = undef if (defined($self->{tsplit}) && $self->{tsplit} eq 'undef');

  ##-- share alphabet
  if (defined($self->{abet})) {
    #$self->{pta}{abet} = $self->{sta}{abet} = $self->{abet};
    $self->{pta}{abet} = $self->{abet};
    $self->{pta}->ensureEpsilon();
  } else {
    $self->{abet} = $self->{pta}{abet};
  }

  ##-- reverse pta?
  #$morph->{pta}{reversed} = $morph->{reversed};

  ##-- compute margins
  #$self->{leftMargin}  = (defined($self->{bos}) ? 1 : 0);
  #$self->{rightMargin} = (defined($self->{eos}) ? 1 : 0);

  return $self;
}

##======================================================================
## Utilities: Messages
##======================================================================

## $mp->vmsg($level, @msg)
sub vmsg {
  my ($morph,$level,@msg) = @_;
  $morph->{verbose} = 'default' if (!defined($morph->{verbose}));
  my $mlevel = $morph->{verbose};
  $mlevel = $VLEVELS{$mlevel} while (defined($VLEVELS{$mlevel}));
  $level  = $VLEVELS{$level}  while (defined($VLEVELS{$level}));
  return if ($mlevel < $level);

  print STDERR ref($morph), ": ", @msg;
}


##======================================================================
## Induction: step 1: successor frequencies (#1)

## undef = $morph->successorFrequencies1(%args)
##  + populates initial $morph->{model}
##  + %args:
##    mode=>$mode,     ##-- one of '1', ...

*succFreqs = *successorFreqs = \&successorFrequencies;
sub successorFrequencies {
  my ($morph,%args) = @_;

  my $trie = $morph->{pta};
  my $maxAffixLen = $morph->{maxAffixLen};

  my $fsm  = $trie->{fsm};
  my $mode = defined($args{mode}) ? $args{mode} : '1';
  my $cuts = $morph->{cuts};
  $cuts = $morph->{cuts} = {} if (!$cuts);

  $morph->vmsg('info', "successorFrequencies(mode=>$mode)\n");

  my ($vec,@labs,$qid,$i,@sf,$wcutsv,@wcuts);
  my $vid = -1;
  foreach $vec (@{$morph->{wenum}{id2sym}}) {
    ++$vid;
    @labs = unpack('S*',$vec);
    @sf   = qw();  ##-- maps position index $i to card(alternatives(@labs[0..$i]))
    $qid  = $fsm->root;
    $wcutsv = $cuts->{$vid};
    @wcuts  = defined($wcutsv) ? unpack('S*',$wcutsv) : qw();

    ##-- get successor frequencies for this word
    foreach $i (0..$#labs) {
      $qid = $fsm->find_arc_lower($qid, $labs[$i]);
      croak(ref($morph)."::successorFrequencies(): inconsitent word-enum -- bailing out!")
	if ($qid==$Gfsm::noState);
      push(@sf, $fsm->out_degree($qid));
    }

    ##-- attempt to find an acceptable cut (longest stem first)
    for ($i=$#labs -1;
	 (
	  $#labs-$i   <= $maxAffixLen
	  && $i+1     >= $morph->{minStemLen1}
	  && $i       >  0
	 );
	 $i--)
      {
	##-- ignore cuts we've already made
	#last if (@wcuts && $i==$wcuts[$#wcuts] || $i==$wcuts[0]);

	if ($mode eq '1') {
	  if (   $sf[$i-1] <= $morph->{maxNeighborSuccCount1}
	      && $sf[$i]   >  1
	      && $sf[$i+1] <= $morph->{maxNeighborSuccCount1})
	    {
	      #cutRightBeforeHere($vec,$i)
	      ##-- Linguistica
	      ##    + "use only the smallest suffix: this is wrong in some cases
	      ##      (e.g. assist-an-t, assist-an-ce)"
	      push(@wcuts,$i);
	      last;
	    }
	}
	else {
	  croak(ref($morph)."::successorFrequencies(): unknown mode '$mode' -- bailing out!\n");
	}
      }

    ##-- update word cuts
    $cuts->{$vid} = pack('S*', sort { $a <=> $b } @wcuts) if (@wcuts);
  }

  ##-- get first-stab signatures
  #$morph->generateModelFromCuts(%args);

  ##-- filter 'em
  #$morph->filterFirstModel(%args);

  ##-- compute robustness of remaining signatures
  #$morph->computeRobustness(%args);

  return $morph;
}

##======================================================================
## $morph = $morph->generateModelFromCuts()
##  + generates initial model
##  + populates @$moph{'stems','suffixes','sigs'}

##-- CONTINUE HERE: get *all* stems (incl. unanalyzed-words) for each sig
##                  and NOT just cuts made by succFreqs(mode=>1)
##    + maybe do something tricky with fsms?
sub generateModelFromCuts {
  my $morph = shift;
  my $cuts  = $morph->{cuts};

  $morph->vmsg('info', "generateModelFromCuts()\n");

  ##-- generate model enums
  my $f2t   = $morph->{f2t}   = {};
  my $t2sig = $morph->{t2sig} = {};
  my $sig2t = $morph->{sig2t} = {};

  ##-- get all stems & tentative signatures
  my ($vid,$wvec, $cutvec,$cut, $stem, $suff, $sig, $i);
  $vid = -1;
  foreach $wvec (@{$morph->{wenum}{id2sym}}) {
    ++$vid;
    $cutvec = $cuts->{$vid};
    if (!defined($cutvec) || $cutvec eq '') {
      ##-- no cuts for this word: use NULL suffix
      $stem = $wvec;
      $suff = '';
    } else {
      $cut  = unpack('S',$cutvec);
      $stem = substr($wvec, 0, 2*($cut+1));
      $suff = substr($wvec, 2*($cut+1));
    }
    $sig = $t2sig->{$stem};
    $sig = '' if (!defined($sig)); ##-- '' => NO signature (NULL sig => pack('S',0)==pack('(S/a*)*', '')
    $sig = pack('(S/a*)*', sort($suff, unpack('(S/a*)*', $sig)));
    ##-- no need to build a hash here: suffs will always be unique for each stem (?)
    $t2sig->{$stem} = $sig;
  }

  ##-- get pseudo-sets of all sigs & all suffs
  my $sigs  = {};
  my $suffs = {};
  foreach $sig (values(%$t2sig)) {
    $sigs->{$sig} = undef;
    @$suffs{unpack('(S/a*)*', $sig)} = undef;
  }

  ##-- foreach sig, get list of all matching stems
  my (@suffs,$stems);
  foreach $sig (sort { length($a) <=> length($b) } keys(%$sigs)) {
    @suffs = unpack('(S/a*)*', $sig);
    #$stems = $morph->{pta}->
  }

  return $morph;
}

sub generateModelFromCuts0 {
  my $morph = shift;
  my $cuts  = $morph->{cuts};

  $morph->vmsg('info', "generateModelFromCuts()\n");

  ##-- generate model enums
  my $f2t   = $morph->{f2t}   = {};
  my $t2sig = $morph->{t2sig} = {};
  my $sig2t = $morph->{sig2t} = {};

  ##-- get all stems & their suffixes
  my ($vid,$wvec, $cutvec,$cut, $stem, $suff, $sig, $i);
  $vid = -1;
  foreach $wvec (@{$morph->{wenum}{id2sym}}) {
    ++$vid;
    $cutvec = $cuts->{$vid};
    if (!defined($cutvec) || $cutvec eq '') {
      ##-- no cuts for this word: use NULL suffix
      $stem = $wvec;
      $suff = '';
    } else {
      $cut  = unpack('S',$cutvec);
      $stem = substr($wvec, 0, 2*($cut+1));
      $suff = substr($wvec, 2*($cut+1));
    }
    $sig = $t2sig->{$stem};
    $sig = '' if (!defined($sig)); ##-- '' => NO signature (NULL sig => pack('S',0)==pack('(S/a*)*', '')
    $sig = pack('(S/a*)*', sort($suff, unpack('(S/a*)*', $sig)));
    ##-- no need to build a hash here: suffs will always be unique for each stem (?)
    $t2sig->{$stem} = $sig;
    $f2t->{$suff}{$stem} = undef;
  }

  ##-- now, create sigs
  while (($stem,$sig)=each(%$t2sig)) {
    $sig2t->{$sig}{$stem} = undef;
  }

  return $morph;
}


##======================================================================
## $morph = $morph->filterFirstModel(%args);
##  + filters first model
sub filterFirstModel {
  my $morph = shift;

  ##-- filter 1: eliminate all suffixes associated with too few stems
  $morph->{minStemsPerAffix} = 3 if (!defined($morph->{minStemsPerAffix}));
  $morph->vmsg('info',
	       "filterFirstModel(): ensure many stems:\n",
	       "  + minStemsPerAffix    = $morph->{minStemsPerAffix}\n");

  my $f2t  = $morph->{f2t};
  my @fbad = grep { scalar(keys(%{$f2t->{$_}})) < $morph->{minStemsPerAffix} } keys(%$f2t);
  $morph->removeSuffixes(\@fbad);
  @fbad=qw();

  ##-- filter 2: doubt single-letter suffixes
  $morph->{minStemsPerLargeSig} = 25 if (!defined($morph->{minStemsPerLargeSig}));
  $morph->{minLongAffixLen}     = 2  if (!defined($morph->{minLongAffixLen}));
  $morph->{minLongAffixCount}   = 2  if (!defined($morph->{minLongAffixCount}));
  $morph->vmsg('info',
	       "filterFirstModel(): doubt single-letter suffixes:\n",
	       "  + minStemsPerLargeSig = $morph->{minStemsPerLargeSig}\n",
	       "  + minLongAffixLen     = $morph->{minLongAffixLen}\n",
	       "  + minLongAffixCount   = $morph->{minLongAffixCount}\n");

  my ($sig,$sigth, @suffs, $nlongf, $f);
  my @badsigs = qw();
 SIG:
  while (($sig,$sigth)=each(%{$morph->{sig2t}})) {
    next if (keys(%$sigth) >= $morph->{minStemsPerLargeSig}); ##-- large sig: ok

    @suffs = unpack('(S/a*)*', $sig);
    $nlongf = 0;
    foreach $f (@suffs) {
      ++$nlongf if ($f eq '' || length($f)/2 >= $morph->{minLongAffixLen});
      next SIG if ($nlongf >= $morph->{minLongAffixCount});
    }
    push(@badsigs, $sig); ##-- if we get here, it's a bad sig
  }
  $morph->removeSignatures(\@badsigs);


  ##-- filtering complete

  return $morph;
}

##======================================================================
## undef = $morph->removeSuffixes(\@bad_suffix_vectors)
##  + remove some suffixes from model
##  + alters: @$morph{qw(cuts t2sig sig2t f2t)}
sub removeSuffixes {
  my ($morph,$badsuffs) = @_;

  #$morph->vmsg('info', "removeSuffixes()\n");

  my ($f,$fth, $t, $sig, $cutvec, $w,$wid, %fh);
  foreach $f (@$badsuffs) {
    $fth = $morph->{f2t}{$f};

    ##-- remove {f2t} entry
    delete($morph->{f2t}{$f});

    foreach $t (keys(%$fth)) {
      ##-- remove {cuts} entry
      $w   = $t.$f;
      $wid = $morph->{wenum}{sym2id}{$w};
      $cutvec = $morph->{cuts}{$wid};
      $cutvec = pack('S*', grep { $_ != (length($t)/2)-1 } unpack('S*',$cutvec));
      if ($cutvec ne '') {
	$morph->{cuts}{$wid} = $cutvec;
      } else {
	delete($morph->{cuts}{$wid});
      }

      ##-- delete old {sig2t} entry, and check for empty (no stems) signatures
      $sig = $morph->{t2sig}{$t};
      delete($morph->{sig2t}{$sig}{$t});
      delete($morph->{sig2t}{$sig}) if (!%{$morph->{sig2t}{$sig}});

      ##-- revise signature, {sig2t}, {t2sig}
      $sig = pack('(S/a*)*', grep { $_ ne $f } unpack('(S/a*)*', $sig));
      if ($sig ne '') {
	$morph->{t2sig}{$t} = $sig;
	$morph->{sig2t}{$sig}{$t} = undef;
      }

      ##-- add monomorphemic entry for stem $w=$t.$f
      $sig = $morph->{t2sig}{$w};
      $sig = '' if (!defined($sig));
      %fh  = map { $_=>undef } ('', unpack('(S/a*)*', $sig));
      $sig = pack('(S/a*)*', sort(keys(%fh)));
      $morph->{sig2t}{$sig}{$w} = undef;
      $morph->{t2sig}{$w}       = $sig;
      $morph->{f2t}{''}{$w}     = undef;
    }
  }

  return $morph;
}

##======================================================================
## undef = $morph->removeSignatures(\@badsigs)
##  + remove some bad signatures from model
##  + alters: @$morph{qw(cuts t2sig sig2t f2t)}
sub removeSignatures {
  my ($morph,$badsigs) = @_;

  #$morph->vmsg('info', "removeSignatures()\n");

  my ($sig,$newsig,$sigth,@sigfs, $t,$f,$w,$wid,$cutvec,%fh);
  foreach $sig (@$badsigs) {
    $sigth = $morph->{sig2t}{$sig};
    @sigfs = unpack('(S/a*)*', $sig);

    ##-- remove {sig2t} entry
    delete($morph->{sig2t}{$sig});

    ##-- remove {t2sig} entries
    delete(@{$morph->{t2sig}}{keys(%$sigth)});

    foreach $t (keys(%$sigth)) {
      foreach $f (@sigfs) {
	##-- remove {cuts} entry
	$w   = $t.$f;
	$wid = $morph->{wenum}{sym2id}{$w};
	$cutvec = $morph->{cuts}{$wid};
	$cutvec = pack('S*', grep { $_ != (length($t)/2)-1 } unpack('S*',defined($cutvec) ? $cutvec : ''));
	if ($cutvec ne '') {
	  $morph->{cuts}{$wid} = $cutvec;
	} else {
	  delete($morph->{cuts}{$wid});
	}

	##-- remove {f2t} entry, check for empty stems(suffix)
	delete($morph->{f2t}{$f}{$t});
	delete($morph->{f2t}{$f}) if (!%{$morph->{f2t}{$f}});

	##-- add monomorphemic entry for stem $w=$t.$f
	$newsig = $morph->{t2sig}{$w};
	$newsig = '' if (!defined($newsig));
	%fh  = map { $_=>undef } ('', unpack('(S/a*)*', $newsig));
	$newsig = pack('(S/a*)*', sort(keys(%fh)));
	$morph->{sig2t}{$newsig}{$w} = undef;
	$morph->{t2sig}{$w}          = $newsig;
	$morph->{f2t}{''}{$w}        = undef;
      }
    }
  }

  return $morph;
}

##======================================================================
## $morph = $morph->computeRobustness(%args)
##  + %args:
##     avg=>'global' or 'local' : default='local'
##  + computes "robustness" of signatures, defined as:
##     log2($morph->{abet}->size) * (  $avg_stem_len*($nsuffs($sig)-1)
##                                   + $avg_suff_len*($nstems($sig)-1) )
sub computeRobustness {
  my ($morph,%args) = @_;
  my $avg_mode = $args{avg};
  $avg_mode = 'local' if (!defined($avg_mode) || $avg_mode ne 'global');

  $morph->vmsg('info', "computeRobustness(avg=$avg_mode)\n");

  my ($nt,$nf,$avg_len_t,$avg_len_f);

  ##-- get global averages
  if ($avg_mode eq 'global') {
    ##-- get global average stem length
    $avg_len_t = 0;
    $avg_len_t += length($_)/2 foreach (keys %{$morph->{t2sig}});
    $avg_len_t /= scalar(keys(%{$morph->{t2sig}}));

    ##-- get global average suffix length
    $avg_len_f = 0;
    $avg_len_f += length($_)/2 foreach (keys %{$morph->{f2t}});
    $avg_len_f /= scalar(keys(%{$morph->{f2t}}));
  }

  ##-- get log alphabet size
  my $log_abet_size = log($morph->{abet}->size-1)/log(2);

  ##-- get output hash
  my $sig2r = $morph->{sig2robust} = {};

  my ($sig,$sigth,@sigfs);
  while (($sig,$sigth)=each(%{$morph->{sig2t}})) {

    $nt    = scalar(keys(%$sigth));
    @sigfs = unpack('(S/a*)*', $sig);
    $nf    = scalar(@sigfs);

    if ($avg_mode ne 'global') {
      ##-- get local average stem length
      $avg_len_t  = 0;
      $avg_len_t += length($_)/2 foreach (keys(%$sigth));
      $avg_len_t /= (1.0*$nt);

      ##-- get local average suffix length
      $avg_len_f  = 0;
      $avg_len_f += length($_)/2 foreach (@sigfs);
      $avg_len_f /= $nf;
    }

    ##-- compute robustness
    $sig2r->{$sig} = $log_abet_size * ( $avg_len_t*($nf-1) + $avg_len_f*($nt-1) );
  }

  return $morph;
}



##======================================================================
## DEBUG
##======================================================================

##--------------------------------------------------------------
## $sig_string = $morph->signature2string($sig)
*sig2str = *sig2s = *sig2string = \&signature2string;
sub signature2string {
  my ($morph,$sig) = @_;
  my @fstrs = map { $morph->f2str($_) } unpack('(S/a*)*', $sig);
  #@fstrs = ('NULL') if (!@fstrs);
  return join('|', sort(@fstrs));
}

*str2sig = *s2sig = *string2sig = \&string2signature;
sub string2signature {
  my ($morph,$str) = @_;
  return pack('(S/a*)*', map { $morph->str2vec($_) } split(/\|/,$str));
}


##--------------------------------------------------------------
## $vector_string = $morph->vector2string($vec)
*v2s = *vec2str = *t2s = *f2s = *t2str = *f2str = \&vector2string;
sub vector2string {
  my ($morph,$vec) = @_;
  return ($vec ne ''
	  ? $morph->{pta}->labels2chars([$morph->{reversed}
					 ? reverse(unpack('S*',$vec))
					 : unpack('S*',$vec)])
	  : 'NULL');
}


*s2v = *str2vec = *s2t = *s2f = *str2t = *str2f = \&string2vector;
sub string2vector {
  my ($morph,$str) = @_;
  return ($str eq 'NULL'
	  ? ''
	  : pack('S*', ($morph->{reversed}
			? reverse(@{$morph->{pta}->chars2labels($str)})
			: @{$morph->{pta}->chars2labels($str)})));
}


##--------------------------------------------------------------
## \%cuts_debug = $morph->debug_cuts()
sub debug_cuts {
  my $morph = shift;
  my $cuts = $morph->{cuts};
  my $wcuts = {};

  my ($vid,$wcutsv,@wcuts, $labs,$word,$bword, $i);
  while (($vid,$wcutsv)=each(%$cuts)) {
    @wcuts = unpack('S*',$wcutsv);

    $labs  = $morph->{pta}->vector2labels($morph->{wenum}{id2sym}[$vid]);
    $word  = $morph->{pta}->labels2chars($labs);
    $bword = $word;
    foreach $i (0..$#wcuts) {
      substr($bword, $wcuts[$i]+$i+1, 0) = '.';
    }
    if ($morph->{reversed}) {
      $word  = join('',reverse(split(//,$word )));
      $bword = join('',reverse(split(//,$bword)));
    }
    $wcuts->{$word} = $bword;
  }

  return $wcuts;
}

##--------------------------------------------------------------
## \%t2sig_debug = $morph->debug_t2sig()
*debug_t2f = \&debug_t2sig;
sub debug_t2sig {
  my $morph = shift;
  my $t2sig = $morph->{t2sig};
  my $wt2sig = {};
  my ($t,$sig);
  while (($t,$sig)=each(%$t2sig)) {
    $wt2sig->{$morph->t2str($t)} = $morph->sig2str($sig);
  }
  return $wt2sig;
}

##--------------------------------------------------------------
## \%f2t_debug = $morph->debug_f2t()
sub debug_f2t {
  my $morph = shift;
  my $f2t = $morph->{f2t};
  my $wf2t = {};
  my ($f,$fth);
  while (($f,$fth)=each(%$f2t)) {
    $wf2t->{$morph->f2str($f)} = join(' ', sort(map {$morph->t2str($_)} keys(%$fth)));
  }
  return $wf2t;
}

##--------------------------------------------------------------
## \%sig2t_debug = $morph->debug_sig2t()
sub debug_sig2t {
  my $morph  = shift;
  my $sig2t  = $morph->{sig2t};
  my $wsig2t = {};
  my ($sig,$sigth);
  while (($sig,$sigth)=each(%$sig2t)) {
    $wsig2t->{$morph->sig2str($sig)} = join(' ', sort(map {$morph->t2str($_)} keys(%$sigth)));
  }
  return $wsig2t;
}

##--------------------------------------------------------------
## \%sig2robust_debug = $morph->debug_sig2robust()
sub debug_sig2robust {
  my $morph  = shift;
  my $sig2r  = $morph->{sig2robust};
  my $wsig2r = {};
  my ($sig,$r);
  while (($sig,$r)=each(%$sig2r)) {
    $wsig2r->{$morph->sig2str($sig)} = $r;
  }
  return $wsig2r;
}



##======================================================================
## I/O: Debug: by signature (later!)



##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($prof,$sent) = @_;
  my (@labs);
  if (defined($prof->{tsplit})) {
    my ($tok);
    ##-- symbols are characters
    foreach $tok (@$sent) {
      @labs = (map { $prof->{abet}->get_label($_) }
	       (
		#(defined($prof->{bos}) ? $prof->{bos} : qw()),
		split($prof->{tsplit}, ref($tok) ? $tok->text : $tok),
		#(defined($prof->{eos}) ? $prof->{eos} : qw()),
	       ));
      @labs = reverse(@labs) if ($prof->{reversed});
      $prof->{pta}->addPathLabels(\@labs,1);
      $prof->{wenum}->addSymbol(pack('S*',@labs));
    }
  } else {
    ##-- symbols are words
    @labs = (
	     #(defined($prof->{bos}) ? $prof->{bos} : qw()),
	     (map { $prof->{abet}->get_label(ref($_) ? $_->text : $_) } @$sent),
	     #(defined($prof->{eos}) ? $prof->{eos} : qw()),
	    );
    @labs = reverse(@labs) if ($prof->{reversed});
    $prof->{pta}->addPathLabels(\@labs,1);
    $prof->{wenum}->addSymbol(pack('S*',@labs));
  }
}

##======================================================================
## Profiling: Finish

## undef = $prof->finish()
#sub finish { my $prof = shift; }


##======================================================================
## Profiling: Help String

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Class for corpus profiles and morphology induction a la Goldsmith (2001,2004).\n)
     .qq( +Profiling Options\n)
     #.qq(  eos=EOS_STRING     [default='__\$']\n)
     #.qq(  bos=BOS_STRING     [default='__\$']\n)
     .qq(  tsplit=RE_OR_UNDEF [default=undef]\n)
     .qq( +Induction Options\n)
     .qq(  (not yet documented)\n)
    );
}

1;

##======================================================================
## Docs
=pod

=head1 NAME

MUDL - MUDL Unsupervised Dependency Learner

=head1 SYNOPSIS

 use MUDL;

=cut

##======================================================================
## Description
=pod

=head1 DESCRIPTION

...

=cut

##======================================================================
## Footer
=pod

=head1 ACKNOWLEDGEMENTS

perl by Larry Wall.

=head1 AUTHOR

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
