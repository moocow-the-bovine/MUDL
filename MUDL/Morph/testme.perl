#!/usr/bin/perl -wd

use lib '../..';
use Gfsm;
use MUDL::Morph::Goldsmith;
use MUDL::Dist;
use MUDL::CorpusIO;
use MUDL::CmdUtils;
use PDL;

use Encode qw(encode decode);
use utf8;
#use encoding qw(iso-8859-1);

##--------------------------------------------------
## initialization
BEGIN {
  our (@words);
  @words = (
	    #qw(a as aa aas ab abs abc abcs),
	    ##--
	    #(map { 'xxx' . $_ } ('', qw(ed s ion ing))),
	    #(map { $_ . 'yyy' } qw(post un)),
	    ##--
	    #(map { 'zz' . $_ } qw(es s ion ing)),
	    #(map { $_ . 'zz' } qw(post un)),
	    ##--
	    #(map { 'xxx' . $_ } ('', qw(ed s ion ing))),
	    #(map { 'yyy' . $_ }  ('', qw(ed))),
	    ##--
	    (map { 'xxx' . $_ } ('', qw(e ed))),
	    (map { 'yyy' . $_ }  ('', qw(ed))),
	   );
  ##--
  our ($bos,$eos);
  #($bos,$eos) = ('__$','__$');
  #($bos,$eos) = ('#','#');
  #($bos,$eos) = ('','');
  ($bos,$eos) = (undef,undef);

  ##--
  our $maxtoks = 10000;
}

##--------------------------------------------------
## object creation: dummy
sub newmorph {
  $morph = MUDL::Morph::Goldsmith->new(bos=>$bos,eos=>$eos, tsplit=>'', @_);
  $morph->addSentence(\@words);
  $morph->finish;
}

sub newmorph_w {
  $morph = MUDL::Morph::Goldsmith->new(bos=>$bos,eos=>$eos, tsplit=>undef);
  $morph->addSentence(\@words);
  $morph->finish;
}
#newmorph_w;

##--------------------------------------------------
## object creation: profile

sub profmorph {
  $morph = MUDL::Morph::Goldsmith->new(bos=>$bos,eos=>$eos,tsplit=>'',@_);
  $cr = MUDL::CorpusIO->fileReader('utest-nl.t');
  $sent = [];
  for ($i=0; defined($sent=$cr->getSentence) && $i < $maxtoks; $i += @$sent) {
    @$sent = splice(@$sent,0,($maxtoks-$i)) if ($i+@$sent > $maxtoks);
    $morph->addSentence($sent);
  }
  return $morph;
}

sub morphsave {
  my $file = shift;
  $file = 'morph-test.bin' if (!$file);
  $morph->saveFile($file);
}
sub loadmorph { load_morph(@_); }
sub load_morph {
  my $file=shift;
  $file = 'morph-test.bin' if (!$file);
  $morph = MUDL::Morph::Goldsmith->loadFile($file);
}


##--------------------------------------------------
## test: successor frequencies

sub testsf1_0 {
  my %args = @_;
  newmorph(reversed=>$args{reversed}) if (!$morph);
  #newmorph(minStemLen1=>0);
  $morph->succFreqs(mode=>1,%args);
  our $wcuts = $morph->debug_cuts();

  ##-- generate initial model
  $morph->generateModelFromCuts(%args);
  #our $model0 = debugModel();

  ##-- filter initial model
  #$morph->filterFirstModel();

  our $model = debugModel();
}
#loadmorph;
#profmorph;
#testsf1_0(reversed=>0);
#testsf1_0(reversed=>1);

sub debugModel {
  $morph->vmsg(0, "debugModel()\n");

  my $f2t   = $morph->debug_f2t;
  my $t2sig = $morph->debug_t2sig;
  my $sig2t = $morph->debug_sig2t;
  my $sig2r = $morph->debug_sig2robust;

  my $f2t_nn   = { %$f2t };
  my $t2sig_nn = { %$t2sig };
  my $sig2t_nn = { %$sig2t };
  my $sig2r_nn = { %$sig2r };

  delete($f2t_nn->{'NULL'});
  delete(@$t2sig_nn{grep { $t2sig->{$_} eq 'NULL' } keys(%$t2sig)});
  delete($sig2t_nn->{'NULL'});
  delete($sig2r_nn->{'NULL'});

  return {
	  f2t=>$f2t,
	  t2sig=>$t2sig,
	  sig2t=>$sig2t,
	  sig2robust=>$sig2r,
	  ##--
	  f2t_nn=>$f2t_nn,
	  t2sig_nn=>$t2sig_nn,
	  sig2t_nn=>$sig2t_nn,
	  sig2robust_nn=>$sig2r_nn,
	 };
}

##--------------------------------------------------
## debug: signature matches

## $stems = sigStems($sig);
sub sigStems0 {
  our ($sigstr) = shift;

  print STDERR "DEBUG: sigStems()\n";

  $sigstr = 'NULL|en' if (!$sigstr);
  our $sig   = $morph->str2sig($sigstr);
  our @suffs = unpack('(S/a*)*', $sig);

  our $pta      = $morph->{pta};
  our $pta_fsm  = $pta->{fsm};
  our $pta_root = $pta_fsm->root;

  ##-- create a recognizer for this suffix-set
  our $suff_fsm = $pta_fsm->shadow;
  $suff_fsm->add_paths([unpack('S*',$_)],[], 0.0, 0,1,1) foreach (@suffs);
  our @suffs_prefixes = map { pack('S*', @{$_->{lo}}) } @{$suff_fsm->paths};

  our $stems={};
  our ($prefix,$qid);
  $pta->traverse(sub {
		   ($prefix,$qid)=@_;

		   $stems->{$pta->labels2chars($prefix)} = undef
		     #if (qMatchesSuffs($pta,$qid,\@suffs_prefixes,\@suffs));
		     if (0);

		   return $MUDL::Gfsm::FreqTrie::TRAVERSE_CONTINUE;
		 });

  $pta_fsm->root($pta_root);
}

sub makeSta {
  print STDERR "$0: makeSta()\n";
  our $pta = $morph->{pta};

  ##-- reverse Pta
  our $rpta = $pta->{fsm}->reverse;

  ##-- real suffix trie
  our $sta = $morph->{pta}->clone;
  $sta->{fsm}->clear;
  $sta->{reversed} = 1;
  foreach $path (@{$pta->{fsm}->paths}) {
    $sta->addPathLabels($path->{lo});
  }
}
#loadmorph();
#makeSta();
#testsf1_0;

sub sigStems {
  our ($sigstr) = shift;
  print STDERR "DEBUG: sigStems()\n";

  $sigstr = 'NULL|en' if (!$sigstr);
  our $sig   = $morph->str2sig($sigstr);
  our @suffs = unpack('(S/a*)*', $sig);

  our $pta      = $morph->{pta};
  our $pta_fsm  = $pta->{fsm};
  our $pta_root = $pta_fsm->root;

  ##-- create a recognizer for this suffix-set
  our $suff_fsm = $pta_fsm->shadow;
  $suff_fsm->add_paths([unpack('S*',$_)],[], 0.0, 0,1,1) foreach (@suffs);
  our @suffs_prefixes = map { pack('S*', @{$_->{lo}}) } @{$suff_fsm->paths};

  our $stems={};
  our ($prefix,$qid);
  $pta->traverse(sub {
		   ($prefix,$qid)=@_;

		   $stems->{$pta->labels2chars($prefix)} = undef
		     #if (qMatchesSuffs($pta,$qid,\@suffs_prefixes,\@suffs));
		     if (0);

		   return $MUDL::Gfsm::FreqTrie::TRAVERSE_CONTINUE;
		 });

  $pta_fsm->root($pta_root);
}
#newmorph;
#sigStems('NULL|ed');
##--
#loadmorph;
#sigStems('NULL|en');

##-- CONTINUE HERE: test for sub-prefixes (i.e. "e" is a sub-prefix of "ed")
##    + final-state check?
##    + use real suffs FSM / FreqTrie?
sub qMatchesSuffs {
  my ($pta,$qid_test,$sprefixes,$suffs) = @_;
  my %sph = map { $_=>undef } @$sprefixes;
  my %sh  = map { $_=>undef } @$suffs;
  my ($prefix,$pv,$qid);
  my $rc=1;
  $pta->traverse(sub {
		   ($prefix,$qid) = @_;
		   $pv = pack('S*',@$prefix);
		   if (exists($sph{$pv})) {
		     ##-- found a match: check it off
		     delete($sph{$pv});

		     ##-- is this prefix-match a full suffix?
		     if (exists($sh{$pv})) {
		       delete($sh{$pv});
		       if (!$pta->{fsm}->is_final($qid)) {
			 $rc = 0;
			 return $MUDL::Gfsm::FreqTrie::TRAVERSE_STOP;
		       }
		     }
		     ##-- is this prefix-match a final state
		     elsif ($pta->{fsm}->is_final($qid)) {
		       $rc=0;
		       return $MUDL::Gfsm::FreqTrie::TRAVERSE_STOP;
		     }

		     ##-- keep going
		     return $MUDL::Gfsm::FreqTrie::TRAVERSE_CONTINUE;
		   }
		   ##-- non-match: we can stop here
		   $rc=0;
		   return $MUDL::Gfsm::FreqTrie::TRAVERSE_STOP;
		 }, $qid_test);
  return $rc && !%sph && !%sh;
}

##--------------------------------------------------
## debug: suffix2prefix
## + requires: $morph, $morph->{pta}, $sta, $model
## + provides: %suffix2prefix
sub debug_suffix2prefix {
  print STDERR "$0: debug_suffix2prefix()\n";

  our $suffix2prefix = {};
  @$suffix2prefix{map { split(/\|/,$_) } values(%{$model->{t2sig_nn}})} = undef;
  $suffix2prefix->{$_} = [prefixes($_ eq 'NULL' ? '' : $_)] foreach (keys(%$suffix2prefix));
  ##@$_ = sort(@$_) foreach (values(%$suffix2prefix));
}
sub test_suffix2prefix {
  loadmorph;
  testsf1_0;
  makeSta;
  debug_suffix2prefix;
}
#test_suffix2prefix;

##--------------------------------------------------
## debug: sig2stems

## \@stem_strs = sig2stems($sig_str)
## + requires: $morph, $suffix2prefix, $model
sub sig2stems {
  my $sigstr = shift;

  ##-- sort suffixes by the number of prefixes they contain
  my @suffs  = sort { $#{$suffix2prefix->{$a}} <=> $#{$suffix2prefix->{$b}} } split(/\|/,$sigstr);
  return [] if (!@suffs);

  ##-- get all stems which have all of the given suffixes
  my %stems  = qw();
  @stems{ @{$suffix2prefix->{$suffs[0]}} } = undef;
  my ($f,%stems_f);
  foreach $f (@suffs[1..$#suffs]) {
    %stems_f = map { $_=>undef } (@{$suffix2prefix->{$f}});
    delete(@stems{grep { !exists($stems_f{$_}) } keys(%stems)});
  }

  ##-- eliminate stems which also have other suffixes
  return [sort grep { suffixes(decode('utf8',$_))==@suffs } keys %stems];
}
#$stems = sig2stems('NULL|en');

##--------------------------------------------------
## debug: sig2stems

## + requires: $morph, $suffix2prefix, $model
## + provides: $sig2stems
sub debug_sig2stems {
  print STDERR "$0: debug_sig2stems\n";

  our %sigs = qw();
  @sigs{('NULL', values(%{$model->{t2sig_nn}}))} = undef;
  our $sig2stems = {};
  $sig2stems->{$_} = sig2stems($_) foreach (keys %sigs);
}

sub test_sig2stems {
  loadmorph;
  testsf1_0;
  makeSta;
  debug_suffix2prefix;
  debug_sig2stems();
}
test_sig2stems;

##--------------------------------------------------
## debug: prefixes, suffixes
sub prefixes { return stringsFrom($sta, shift); }
sub suffixes { return stringsFrom($morph->{pta}, shift); }

##--------------------------------------------------
## debug: stringsFrom
## @strings = stringsFrom($trie,$string)
sub stringsFrom {
  my ($trie,$chars) = @_;
  my $labs = $trie->chars2labels($chars);
  return qw() if (grep { $_ == $Gfsm::noLabel } @$labs);
  my $qid  = $trie->{fsm}->find_prefixes(
					 ($trie->{reversed} ? [reverse @$labs] : $labs),
					 []
					);
  return
    map {
      $trie->labels2chars($trie->{reversed} ? [reverse @{$_->{lo}}] : $_->{lo});
    } @{pathsFrom($trie->{fsm}, $qid)};
}


##--------------------------------------------------
## debug: pathsFrom
## \@paths = pathsFrom($fsm,$qid)
sub pathsFrom {
  my ($fsm,$qid) = @_;
  return [] if (!defined($qid) || $qid == $Gfsm::noState);
  my $root_tmp = $fsm->root;
  $fsm->root($qid);
  my $paths = $fsm->paths;
  $fsm->root($root_tmp);
  return $paths;
}


##--------------------------------------------------
## debug: signature matches: data

BEGIN {
  our $src_encoding = 'utf-8';
  our @null_en_ling =
    #map { decode($src_encoding, $_) }
      (qw(öffentlich einfach fest gemeinsam hektisch million präsident student wohnung),
       qw(amtsgericht einrichtung fraktion genannt krankheit ortschaft preiswert versicherung),
       qw(angebot entschied fremd gespräch mannschaft partei punkt vorstellung),
       qw(archiv erfolg gefahr gewinn meist parteilos region würdigt),
       qw(begabt fand gefeiert höher menschlich präsentiert schrift wanderung),
      );

  our $ls = 'gespräch';
  #our $lsu = decode($src_encoding,$ls);

  our @null_en_ling_only =
    #map { decode($src_encoding, $_) }
      (qw(schrift million fraktion fremd partei fest wohnung mannschaft meist erfolg öffentlich));

  our @null_en_me_only =
    #map { decode($src_encoding, $_) }
      (qw(arbeitet früher ihn uhr));
}

sub get_ling_me_diffs {
  #our @null_en_me = sort(map {Encode::decode('utf-8',$_)} keys(%$stems));
  our @null_en_me = sort(map {Encode::decode('utf-8',$_)} @{$sig2stems->{'NULL|en'}});

  our %null_en_ling_only = map {$_=>undef} @null_en_ling;
  delete(@null_en_ling_only{@null_en_me});
  @null_en_ling_only = sort(keys(%null_en_ling_only));
  $null_en_ling_only{$_} = join(' | ',stem2suffs($_)) foreach (keys(%null_en_ling_only));

  our %null_en_me_only   = map {$_=>undef} @null_en_me;
  delete(@null_en_me_only{@null_en_ling});
  @null_en_me_only = sort(keys(%null_en_me_only));
  $null_en_me_only{$_} = join(' | ',stem2suffs($_)) foreach (keys(%null_en_me_only));
}
#get_ling_me_diffs;

sub stem2suffs {
  my ($stem,$encoding) = @_;
  $stem = Encode::decode($encoding,$stem) if (defined($encoding)
					      && ($encoding ne 'utf-8'
						  ||
						  !utf8::is_utf8($stem)));
  my $root_tmp = $morph->{pta}{fsm}->root;
  my $qid = $morph->{pta}->getStateChars($stem);
  if (!defined($qid)) {
    warn("stem2suffs($stem): unknown stem '$stem'");
    return qw();
  }
  $morph->{pta}{fsm}->root($qid);
  my @tsuffs = map { $morph->{pta}->labels2chars($_->{lo}) } @{$morph->{pta}{fsm}->paths};
  $morph->{pta}{fsm}->root($root_tmp);
  return sort(map { $_ eq '' ? 'NULL' : $_ } @tsuffs);
}

##--------------------------------------------------
## dummy
package main;
foreach $i (0..10) {
  print "--dummy[$i]--\n";
}
