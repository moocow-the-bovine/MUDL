##-*- Mode: CPerl -*-

## File: MUDL::Morph::Goldsmith.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Goldsmith (2001,2004) morphology induction
##======================================================================

package MUDL::Morph::Goldsmith;
use MUDL::Gfsm::FreqTrie;
use MUDL::Corpus::Profile;
use MUDL::Enum;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile);

##======================================================================
## $prof = $class_or_obj->new(%args)
##   + %args:
##
##     ##-- profiling arguments
##     #bos    => $bos_str,     ##-- bos string
##     #eos    => $eos_str,     ##-- eos string
##     tsplit => $re_or_undef, ##-- split regex for input tokens (undef to profile whole words as symbols)
##     reversed=>$bool,         ##-- if true, search for suffxies
##
##     ##-- induction arguments
##     minStemLen1 => $len,              ##-- minimum stem length for succFreqs1() (default=3)
##     maxAffixLen => $len,              ##-- maximum suffix length (default=5)
##     maxNeighborSuccCount1 => $count,  ##-- maximum neighbor successor count for succFreqs1() (default=1)
##     minStemsPerAffix=>$count,         ##-- min(card(stems(suffix))) for succFreqs1()
##
##     ##-- profiling data
##     abet     => $abet,     ##-- a MUDL::Gfsm::Alphabet
##     pta      => $pta,      ##-- a MUDL::Gfsm::FreqTrie (using $prof->{abet}, reversed=>0)
##     wenum    => $enum      ##-- enumerates whole-word label-vectors (packed)
##
##     ##-- model data
##     cuts    => \%cuts,     ##-- suffix-cuts: maps word-id ($morph->{wenum}) to suffix cuts (sorted vec)
##                            ##     id($w) => pack('S*', @scuts)
##                            ##   such that:
##                            ##     stem($w)      = $w[0              ..  $wcuts[0   ] ]
##                            ##     suffix($w,$i) = $w[($wcuts[$i]+1) .. ($wcuts[$i+1])] ## ($i)-th suffix
##                            ##     ##-- ($i)-th suffix, where ($i==$#wcuts) implies $wcuts[$i+]==$#w
##                            ##   ...i.e. $word is split right *after* $scuts[$i] (counting from 0)
##
##     #stems   => \%stemset,  ##-- pseudo-set of stems (vectors) for current model
##     #suffs   => \%suffset,  ##-- pseudo-set of affixes (vectors) for current model, incl. NULL ('')
##     #sigs    => \%sigset,   ##-- enum of sigs, encoded as:
##                             ##     pack('S*', (sort { $a <=> $b } @suff_ids))
##
##     t2f     => \%t2f,      ##-- maps stems to sorted-vectors of affixes, joined with pack('S',0)
##     f2t     => \%f2t,      ##-- maps affixes to pseudo-hashes stems
##
##     t2sig   => \%t2sig,    ##-- maps stems to their sigs
##     sig2t   => \%sig2t,    ##-- maps sigs  to pseudo-hashes of the stems they contain
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
     nosta=>0,
     ##-- induction arguments
     minStemLen1 => 3,
     maxNeighborSuccCount1 => 1,
     maxAffixLen=>5,
     minStemsPerAffix=>3, ##-- min(card(stems(affix))) on succFreqs1
     ##-- profiling data
     #abet=>MUDL::Gfsm::Alphabet->new(),
     pta=>MUDL::Gfsm::FreqTrie->new(reversed=>0),
     wenum=>MUDL::Enum->new(),
     ##-- model data
     model=>{},
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

  return $morph;
}

##======================================================================
## $morph = $morph->generateModelFromCuts()
##  + generates initial model
##  + populates @$moph{'stems','suffixes','sigs'}

sub generateModelFromCuts {
  my $morph = shift;
  my $cuts  = $morph->{cuts};

  ##-- generate model enums
  my $t2f   = $morph->{t2f}   = {};
  my $f2t   = $morph->{f2t}   = {};
  my $t2sig = $morph->{t2sig} = {};
  my $sig2t = $morph->{sig2t} = {};

  ##-- get all stems & their suffixes
  my $suffsep = pack('S',0);
  my ($vid,$wvec, $cutvec,$cut, $stem, $suff, $suffsv, $i);
  $vid = -1;
  foreach $wvec (@{$morph->{wenum}{id2sym}}) {
    ++$vid;
    $cutvec = $cuts->{$vid};
    if (!defined($cutvec) || $cutvec eq '') {
      ##-- no cuts for this word
      $t2f->{$wvec} = pack('S', 0);
      $f2t->{''}{$wvec} = undef;
      next;
    }
    $cut    = unpack('S',$cutvec);
    $stem   = substr($wvec, 0, 2*($cut+1));
    $suff   = substr($wvec, 2*($cut+1));
    $suffsv = $t2f->{$stem};
    $suffsv = '' if (!defined($suffsv));
    $t2f->{$stem} = pack('(S/a*)*', sort(unpack('(S/a*)*', $suffsv), $suff));
    $f2t->{$suff}{$stem} = undef;
  }

  ##-- now, create sigs
  while (($stem,$suffsv)=each(%$t2f)) {
    $t2sig->{$stem} = $suffsv;
    $sig2t->{$suffsv}{$stem} = undef;
  }

  return $morph;
}

sub generateModelFromCuts_withids {
  my $morph = shift;
  my $cuts  = $morph->{cuts};

  ##-- generate model enums
  my $stems = $morph->{stems} = MUDL::Enum->new();
  my $suffs = $morph->{suffs} = MUDL::Enum->new();
  my $sigs  = $morph->{sigs}  = MUDL::Enum->new();
  my $t2f   = $morph->{t2f}   = {};
  my $f2t   = $morph->{f2t}   = {};
  my $t2sig = $morph->{t2sig} = {};
  my $sig2t = $morph->{sig2t} = {};

  ##-- ensure the NULL affix (id=0) is defined
  $suffs->addIndexedSymbol('',0);

  ##-- get all stems & their suffixes
  my $stemsh = {}; ##-- all stems
  my $suffsh = {}; ##-- all suffixes
  my ($vid,$wvec, $cutvec,$cut, $stem,$stemid,$suff,$suffid,$suffidsv,@suffids, $i);
  $vid = -1;
  foreach $wvec (@{$morph->{wenum}{id2sym}}) {
    ++$vid;
    $cutvec = $cuts->{$vid};
    if (!defined($cutvec) || $cutvec eq '') {
      ##-- no cuts for this word
      $stemid = $stems->addSymbol($wvec);  ##-- ... add it as a stem
      $t2f->{$stemid} = pack('S',0);       ##-- ... with only the null suffix
      $f2t->{0}{$stemid} = undef;
      ##--
      #$stems->addSymbol($wvec);  ##-- ... add it as a stem
      ##--
      next;
    }
    $cut    = unpack('S',$cutvec);
    $stem   = substr($wvec, 0, 2*($cut+1));
    $suff   = substr($wvec, 2*($cut+1));
    $stemid = $stems->addSymbol($stem);
    $suffid = $suffs->addSymbol($suff);
    $suffidsv = $t2f->{$stemid};
    $suffidsv = '' if (!defined($suffidsv));
    $t2f->{$stemid} = pack('S*', sort {$a<=>$b} unpack('S*',$suffidsv), $suffid);
    $f2t->{$suffid}{$stemid} = undef;
  }

  ##-- now, create sigs
  my ($sigid);
  while (($stemid,$suffidsv)=each(%$t2f)) {
    $sigid = $sigs->addSymbol($suffidsv);
    $t2sig->{$stemid} = $sigid;
    $sig2t->{$sigid}{$stemid} = undef;
  }

  return $morph;
}


##======================================================================
## $morph = $morph->filterFirstModel(%args);
##  + filters first model
sub filterFirstModel {
  my $morph = shift;

  ##-- filter 1: eliminate all suffixes associated with too few stems
  my $f2t  = $morph->{f2t};
  my @fbad = grep { scalar(keys(%{$f2t->{$_}})) < $morph->{minStemsPerAffix} } keys(%$f2t);

  ##-- filter 2: doubt single-letter suffixes
  

  ##-- filters done: re-assign everything (ack ack ack: can we avoid all of this id-assignment?)
}


##======================================================================
## DEBUG
##======================================================================

##--------------------------------------------------------------
## \%broken_words = $morph->debug_cuts()
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
## \%t2f_debug = $morph->debug_t2f()
*debug_t2sig = \&debug_t2f;
sub debug_t2f {
  my $morph = shift;
  my $t2f = $morph->{t2f};
  my $wt2f = {};
  my ($tvec,$fvecs, $tstr, @fstrs);
  while (($tvec,$fvecs)=each(%$t2f)) {
    $tstr  = $morph->{pta}->labels2chars([$morph->{reversed}
					  ? reverse(unpack('S*',$tvec))
					  : unpack('S*',$tvec)]);
    @fstrs = map {
      ($_ ne ''
       ? $morph->{pta}->labels2chars([$morph->{reversed}
				      ? reverse(unpack('S*',$_))
				      : unpack('S*',$_)])
       : 'NULL')
    } unpack('(S/a*)*', $fvecs);

    $wt2f->{$tstr} = join('|',sort(@fstrs));
  }
  return $wt2f;
}

##--------------------------------------------------------------
## \%f2t_debug = $morph->debug_f2t()
sub debug_f2t {
  my $morph = shift;
  my $f2t = $morph->{f2t};
  my $wf2t = {};
  my ($fvec,$fts, $fstr,@tstrs);
  while (($fvec,$fts)=each(%$f2t)) {
    $fstr = ($fvec ne ''
	     ? $morph->{pta}->labels2chars([$morph->{reversed}
					    ? reverse(unpack('S*',$fvec))
					    : unpack('S*',$fvec)])
	     : 'NULL');

    @tstrs = map {
      $morph->{pta}->labels2chars([$morph->{reversed}
				   ? reverse(unpack('S*',$_))
				   : unpack('S*',$_)])
    } keys(%$fts);

    $wf2t->{$fstr} = join(' ', sort(@tstrs));
  }
  return $wf2t;
}

##--------------------------------------------------------------
## \%sig2t_debug = $morph->debug_sig2t()
sub debug_sig2t {
  my $morph  = shift;
  my $sig2t  = $morph->{sig2t};
  my $wsig2t = {};
  my ($sigvecs,$sigts, @fstrs,@tstrs);
  while (($sigvecs,$sigts)=each(%$sig2t)) {
    @fstrs = map {
      ($_ ne ''
       ? $morph->{pta}->labels2chars([$morph->{reversed}
				      ? reverse(unpack('S*',$_))
				      : unpack('S*',$_)])
       : 'NULL')
    } unpack('(S/a*)*',$sigvecs);

    @tstrs = map {
      $morph->{pta}->labels2chars([$morph->{reversed}
				   ? reverse(unpack('S*',$_))
				   : unpack('S*',$_)])
    } keys(%$sigts);

    $wsig2t->{join('|', sort(@fstrs))} = join(' ', sort(@tstrs));
  }

  return $wsig2t;
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
