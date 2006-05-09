#!/usr/bin/perl -w

#use lib qw(. ./MUDL ... ../MUDL ../.. ../../MUDL);
use lib qw(../..);
use Gfsm;
use MUDL::Gfsm::FreqTrie;
use MUDL::Unigrams;
use MUDL::CorpusIO;
use MUDL::Dist;
use MUDL::CmdUtils;

use Encode qw(encode decode);

use Benchmark qw(timethese cmpthese);

##========================================================================
## Globals

BEGIN {
  select(STDERR);
  $|=1;
  select(STDOUT);
  $|=1;

  our $progname = 'trietest.perl';

  ##-- hack: use ASCII tokens only?
  our $ASCII_ONLY_HACK = 0;

  ##-- benchmarking options
  our %bench =
    (
     populate=>1,
     filesize=>1,
     string2id=>1,
     id2string=>1,
     stem2suffs=>1,
     sig2stems=>1,
     sig2stems_len=>1,
    );
  ##-- DEBUG: no benchmarking
  %bench = qw();
  #%bench = (sig2stems=>1,sig2stems_len=>1);

  ##-- %tries: $label=>$info, ...
  ##  + options:
  ##      bench_populate=>$bool,
  ##      bench_string2id=>$bool,
  ##      bench_id2string=>$bool,
  ##      bench_stem2suffs=>$bool,
  ##      bench_sig2stems=>$bool,
  ##      bench_sig2stems_len=>$bool,
  ##  + info:
  ##      trie     => $trie,           ##-- trie data, should be filled in by 'populate'
  ##      populate => $generate_code,  ##-- should populate from global unigram dist $main::ugp
  ##      stringi2d => $code,          ##-- s.t. $id     = $code->($trie,$string)
  ##      id2string => $code,          ##-- s.t. $string = $code->($trie,$id)
  ##      nstates   => $code,          ##-- s.t. $nstates = $code->($trie);
  ##      stem2suffs=> $code,          ##-- \@suffix_identifiers = $code->($trie,$qid)
  ##      sig2stems => $code,          ##-- \@stem_identifiers = $code->($trie,\@suffix_strings)
  ##      sig2stems_len => $code,      ##-- \@stem_identifiers = $code->($trie,\@suffix_strings,$max_suffix_len)
  ##      ...
  our %tries = qw();
}

##========================================================================
## Utilities: messages


## undef = vmsg(@msg)
sub vmsg {
  print STDERR $progname, ": ", @_;
  #print $progname, ": ", @_;
}

##========================================================================
## Preliminaries

##--------------------------------------------------------------
## Preliminaries: get unigram distribution in $ugp
sub getUnigrams {
  vmsg("getUnigrams()...\n");
  my $nwords = shift;
  $nwords = 10000 if (!defined($nwords));
  my $nread  = 0;

  my $cr = MUDL::CorpusIO->formatReader('utest-nl.t');
  our $ugp = MUDL::Unigrams->new();

  my ($s);
  while (defined($s = $cr->getSentence) && $nread < $nwords) {
    ##-- HACK: ignore non-alphabetics
    @$s = grep { $_->text =~ /^[a-zA-Z]+$/ } @$s if ($ASCII_ONLY_HACK);

    ##-- respect requested number of words
    if ($nread + @$s > $nwords) {
      @$s = @$s[0..($nwords-$nread-1)];
    }
    $ugp->addSentence($s);
    $nread += @$s;
  }
  undef $cr;

  ##-- save it
  $ugp->saveFile('trietest.ug.bin');
}
#getUnigrams();

##--------------------------------------------------------------
## Preliminaries: load unigram distribution from 'trietest.ug.bin'
sub loadUnigrams {
  vmsg("loadUnigrams()...\n");
  our $ugp = load('trietest.ug.bin');
}
#loadUnigrams();

##--------------------------------------------------------------
## Preliminaries: get or load unigrams
sub getOrLoadUnigrams {
  if (-r 'trietest.ug.bin') { loadUnigrams(); }
  else { getUnigrams(); }
}
getOrLoadUnigrams();

##-- #/unigrams (types): 3744


##========================================================================
## Trie Population


##--------------------------------------------------------------
## Trie Population: MUDL::Gfsm::FreqTrie

sub populateTrieGfsm {
  our $trie_gfsm = $tries{gfsm}{trie} = MUDL::Gfsm::FreqTrie->new();
  my ($w,$f);
  while (($w,$f)=each(%$ugp)) {
    $trie_gfsm->addPathChars($w,$f);
  }
  $trie_gfsm->reverseIndex();
}
BEGIN {
  $tries{gfsm} = {
		  populate => \&populateTrieGfsm,
		  string2id => \&MUDL::Gfsm::FreqTrie::getStateChars,
		  id2string => \&MUDL::Gfsm::FreqTrie::stateChars,
		  nstates   => sub { return $_[0]{fsm}->n_states(); },
		 };
}


##--------------------------------------------------------------
## Trie Population: MUDL::Trie::Freq

use MUDL::Trie::Freq;
sub populateTrie1 {
  our $trie1 = $tries{test1}{trie} = MUDL::Trie::Freq->new();
  my ($w,$f);
  while (($w,$f)=each(%$ugp)) {
    $trie1->addString($w,$f);
  }
  $trie1->finish();
}
BEGIN {
  $tries{test1} = {
		   populate => \&populateTrie1,
		   string2id => \&MUDL::Trie::Base::string2id,
		   id2string => \&MUDL::Trie::Base::id2string,
		   nstates   => sub { return $_[0]{nq}; },
		  };
}

##--------------------------------------------------------------
## Trie Population: MUDL::Trie::Freq (indexed)

sub populateTrie1i {
  our $trie1i = $tries{test1i}{trie} = MUDL::Trie::Freq->new();
  my ($w,$f);
  while (($w,$f)=each(%$ugp)) {
    $trie1i->addString($w,$f);
  }
  $trie1i->finish(
		  indexDepth=>1,
		  indexWidth=>1,
		  indexStrings=>1,
		 );
}
BEGIN {
  $tries{test1i} = {
		    %{$tries{test1}},
		    populate => \&populateTrie1i,
		   };
}


##--------------------------------------------------------------
## Trie Population: MUDL::Trie::FreqPair

use MUDL::Trie::FreqPair;
sub populateTriePair1 {
  our $tp1 = $tries{pair1}{trie} = MUDL::Trie::FreqPair->new(indexSort=>1);
  my ($w,$f);
  while (($w,$f)=each(%$ugp)) {
    $tp1->addString($w,$f);
  }
  $tp1->finish();
}
BEGIN {
  $tries{pair1} = {
		   populate => \&populateTriePair1,
		   string2id => sub { return $_[0]{pta}->string2id($_[1]); },
		   id2string => sub { return $_[0]{pta}->id2string($_[1]); },
		   nstates   => sub { return $_[0]{pta}{nq}; },

		   bench_string2id=>0,
		   bench_id2string=>0,
		  };
}

##--------------------------------------------------------------
## Trie Population: MUDL::Trie::FreqPair (+indexed)

sub populateTriePair1i {
  our $tp1i = $tries{pair1i}{trie} = MUDL::Trie::FreqPair->new(
							      indexDepth=>1,
							      indexWidth=>1,
							      indexStrings=>1,
							      indexSort=>1,
							     );
  my ($w,$f);
  while (($w,$f)=each(%$ugp)) {
    $tp1i->addString($w,$f);
  }
  $tp1i->finish();
}
BEGIN {
  $tries{pair1i} = {
		    %{$tries{pair1}},
		    populate=>\&populateTriePair1i,
		    bench_string2id=>0,
		    bench_id2string=>0,
		   };
}


##--------------------------------------------------------------
## Trie Population: all

sub populateAllTries {
  vmsg("populateAllTries()...\n");

  my ($label);
  foreach $label (sort(keys(%tries))) {
    vmsg("populateAllTries(): $label...\n");
    $tries{$label}{populate}->();
  }
}
populateAllTries();

##--------------------------------------------------------------
## Trie Population: ensure

sub ensureAllTries {
  vmsg("ensureAllTries()...\n");

  my ($label);
  foreach $label (sort(keys(%tries))) {
    vmsg("ensureAllTries(): $label...\n");
    $tries{$label}{populate}->() if (!defined($tries{$label}{trie}));
  }
}


##========================================================================
## Test: Tries: file size

##--------------------------------------------------------------
## Test: Tries: file size: save all

sub saveAllTries {
  vmsg("saveAllTries()...\n");

  my ($label,$trie);
  foreach $label (sort(keys(%tries))) {
    $trie = $tries{$label}{trie};
    $trie->saveFile("trietest.trie-${label}.bin");
  }
}

##--------------------------------------------------------------
## Test: Tries: file size: test

sub testFileSizes {
  vmsg("testFileSizes()...\n");
  return if (!$bench{filesize});
  ensureAllTries();
  saveAllTries();

  print
    (
     "+ FILE SIZES:\n",
     (map {
       sprintf("   filesize(%8s) = %8.2f KB\n", $_, (-s "trietest.trie-$_.bin")/1024)
     } sort(keys(%tries))),
     "\n",
    );
}
testFileSizes();
#exit 0;


##========================================================================
## Bench: populate

sub bench_populate {
  vmsg("bench_populate()...\n");
  return if (!$bench{populate});

  my $n = shift;
  $n = -3 if (!defined($n));

  my $cmpus = {};
  my ($label,$info);
  while (($label,$info) = each(%tries)) {
    next if (defined($info->{bench_populate} && !$info->{bench_populate}));
    $cmpus->{$label} = $info->{populate};
  }

  print "+  BENCH: populate()\n";
  timethese($n,$cmpus);
  #print STDERR (('.' x 80), "\n");
  #cmpthese($n,$cmpus);
  print "\n";
}
bench_populate();
#exit 0;

##========================================================================
## Bench: lookup: string -> id

sub bench_string2id {
  vmsg("bench_string2id()...\n");
  return if (!$bench{string2id});

  my $n = shift;
  $n = -3 if (!defined($n));

  my $cmpus = {};
  my ($label,$info);
  my @ugkeys = keys(%$ugp);
  while (($label,$info) = each(%tries)) {
    next if (defined($info->{bench_string2id} && !$info->{bench_string2id}));

    my $trie   = $info->{trie};
    my $str2id = $info->{string2id};

    $cmpus->{$label} =
      sub {
	my ($str,$id);
	foreach $str (@ugkeys) {
	  $id = $str2id->($trie,$str);
	}
      };
  }

  print "+  BENCH: string2id()\n";
  timethese($n,$cmpus);
  #print STDERR (('.' x 80), "\n");
  #cmpthese($n,$cmpus);
  print "\n";
}
bench_string2id();
#exit 0;


##========================================================================
## Bench: lookup: id -> string

sub bench_id2string {
  vmsg("bench_id2string()...\n");
  return if (!$bench{id2string});

  my $n = shift;
  $n = -3 if (!defined($n));

  my $cmpus = {};
  my ($label,$info);
  while (($label,$info) = each(%tries)) {
    next if (defined($info->{bench_id2string} && !$info->{bench_id2string}));

    my $trie    = $info->{trie};
    my $nstates = $info->{nstates};
    my $id2str  = $info->{id2string};

    $cmpus->{$label} =
      sub {
	my @ids = (0..($nstates->($trie)));
	my ($str,$id);
	foreach $id (@ids) {
	  $str = $id2str->($trie,$id);
	}
      };
  }

  print "+ BENCH: id2string()\n";
  timethese($n,$cmpus);
  #print STDERR (('.' x 80), "\n");
  #cmpthese($n,$cmpus);
  print "\n";
}
bench_id2string();


##========================================================================
## Stems -> Suffixes
##========================================================================

##--------------------------------------------------------------
## Stems -> Suffixes: Gfsm

BEGIN {
  $tries{gfsm}{stem2suffs} = \&MUDL::Gfsm::FreqTrie::suffixesState;
}

##--------------------------------------------------------------
## Stems -> Suffixes: FreqTrie

sub stem2suffs_freqtrie {
  my ($trie,$qid) = @_;
  my ($final,$goto) = @$trie{qw(final goto)};
  my @suffids = qw();
  my @fifo = ('',$qid);
  my ($path);
  while (@fifo) {
    ($path,$qid) = splice(@fifo,0,2);
    push(@suffids,$path) if ($final->{$qid});
    push(@fifo, map { (($path.$_),$goto->[$qid]{$_}) } keys(%{$goto->[$qid]}));
  }
  return \@suffids;
}

BEGIN {
  $tries{test1}{stem2suffs} =
    $tries{test1i}{stem2suffs} = \&stem2suffs_freqtrie;
}

##--------------------------------------------------------------
## Stems -> Suffixes: FreqTriePair

BEGIN {
  $tries{pair1}{stem2suffs} =
    $tries{pair1i}{stem2suffs} = sub { return $_[0]{p2s}[$_[1]]; };
}

##========================================================================
## Test: Stem->Suffixes

sub test_stem2suffs {
  vmsg("test_stem2suffs()...\n");
  my ($label,$info);
  while (($label,$info) = each(%tries)) {
    vmsg("test_stem2suffs(): $label...\n");
    my $trie    = $info->{trie};
    my $nstates = $info->{nstates}->($trie);
    my $stem2suffs  = $info->{stem2suffs};
    my $qid = int(rand($nstates));

    my $suffs = $stem2suffs->($trie,$qid);
  };
}
#test_stem2suffs();

##========================================================================
## Bench: Stem->Suffixes

sub bench_stem2suffs {
  vmsg("bench_stem2suffs()...\n");
  return if (!$bench{stem2suffs});

  my $n = shift;
  $n = -3 if (!defined($n));

  my $cmpus = {};
  my ($label,$info);
  my (@qids);
  while (($label,$info) = each(%tries)) {
    next if (defined($info->{bench_stem2suffs} && !$info->{bench_stem2suffs}));
    my $trie    = $info->{trie};
    my $nstates = $info->{nstates}->($trie);
    my $stem2suffs  = $info->{stem2suffs};
    @qids = map { int(rand($nstates)) } (0..1024) if (!@qids);

    $cmpus->{$label} =
      sub {
	my @suffs = map { $stem2suffs->($trie,$_) } @qids;
      };
  }

  print "+ BENCH: stem2suffs()\n";
  timethese($n,$cmpus);
  #print STDERR (('.' x 80), "\n");
  #cmpthese($n,$cmpus);
  print "\n";
}
bench_stem2suffs();

##========================================================================
## Sig->Stems
##========================================================================

##--------------------------------------------------------------
## Sig->Stems: FreqTriePair

##-- TODO: revise: all prefixes of most *specific* suffix are all we need to go from...
##   TODO: write length-restriction test, benchmark!
sub sig2stems_FreqTriePair {
  my ($tp,$suffs) = @_;

  ##-- get suffix-ids, sorted in ascending order by number of equivalent prefixes
  my @suffids =
    sort {
      length($tp->{s2p}[$a]) <=> length($tp->{s2p}[$b])
    } map {
      $tp->{sta}->string2id($_)
    } @$suffs;

  ##-- get identifier for this suffix-set (packed sorted list of ids)
  my $suffids_packed = pack('L*', sort {$a<=>$b} @suffids);

  ##-- get list of all prefix-ids associated with EXACTLY this suffix-set
  ##   + @prefix_ids = { $qid_pta | suffixes($qid_pta) == @suffids }
  ##   + trick: filter prefix-ids of most specific suffix
  ##   + this makes a *HUGE* difference
  ##     - "trick" version is ca. 200x faster than untricked version
  my @prefids =
    grep {
      $tp->{p2s}[$_] eq $suffids_packed
    } unpack('L*', $tp->{s2p}[$suffids[0]]);

  return [sort {$a<=>$b} @prefids];
}


sub sig2stems_FreqTriePair0 {
  my ($tp,$suffs) = @_;

  ##-- get suffix-ids (unsorted)
  my @suffids        = map { $tp->{sta}->string2id($_) } @$suffs;

  ##-- get identifier for this suffix-set (packed sorted list of ids)
  my $suffids_packed = pack('L*', sort {$a<=>$b} @suffids);

  ##-- get hash of all prefix-ids associated with EXACTLY this suffix-set
  ##   + redundant ("no tricks") version: check all prefix-ids associated with
  ##     any suffix-id in the set
  ##   + SLOW!
  my %prefids =
    map {
      ($_=>undef)
    } grep {
      $tp->{p2s}[$_] eq $suffids_packed
    } map {
      unpack('L*', $_)
    } @{$tp->{s2p}}[@suffids];
  return [sort {$a<=>$b} keys(%prefids)];
}

BEGIN {
  $tries{pair1}{sig2stems} =
    $tries{pair1i}{sig2stems} = \&sig2stems_FreqTriePair;
}

##========================================================================
## Test: sig->stems

sub test_sig2stems {
  vmsg("test_sig2stems()...\n");

  my ($label,$info);
  my $sig = shift || ['','en'];

  our %lab2stems = qw();
  while (($label,$info) = each(%tries)) {
    next if (!$info->{sig2stems});
    vmsg("test_sig2stems(): $label...\n");

    my $trie       = $info->{trie};
    my $sig2stems  = $info->{sig2stems};

    my $stems = $lab2stems{$label} = $sig2stems->($trie,$sig);
  }

  vmsg("test_sig2stems(): done.\n");
}
test_sig2stems();


##========================================================================
## Bench: sig->stems

sub bench_sig2stems {
  vmsg("bench_sig2stems()...\n");
  return if (!$bench{sig2stems});

  my $n = shift;
  $n = -3 if (!defined($n));

  my $cmpus = {};
  my ($label,$info);
  my $sig = ['','en'];
  while (($label,$info) = each(%tries)) {
    next if (defined($info->{bench_sig2stems} && !$info->{bench_sig2stems}));
    next if (!$info->{sig2stems});
    my $trie       = $info->{trie};
    my $sig2stems  = $info->{sig2stems};

    $cmpus->{$label} =
      sub {
	my $stems = $sig2stems->($trie,$sig);
      };
  }

  print "+ BENCH: sig2stems()\n";
  timethese($n,$cmpus);
  #print STDERR (('.' x 80), "\n");
  #cmpthese($n,$cmpus);
  print "\n";
}
bench_sig2stems();


##========================================================================
## Sig->Stems (+length)
##========================================================================

##--------------------------------------------------------------
## Sig->Stems (+length): FreqTriePair

sub sig2stems_len_FreqTriePair {
  my ($tp,$suffs,$maxlen) = @_;

  ##-- get suffix-ids, sorted in ascending order by number of equivalent prefixes
  my @suffids =
    sort {
      length($tp->{s2p}[$a]) <=> length($tp->{s2p}[$b])
    } map {
      $tp->{sta}->string2id($_)
    } @$suffs;

  ##-- index suffix-ids as a hash, for quick access
  my %suffids = map { ($_=>undef) } @suffids;

  ##-- get identifier for this suffix-set (packed sorted list of ids)
  my $suffids_packed = pack('L*', sort {$a<=>$b} @suffids);

  ##-- get list of all prefix-ids associated with ALMOST EXACTLY these suffixes
  ##   + @prefix_ids = { $qid_pta | suffixes_maxlen($qid_pta,$maxlen) == @suffids }
  ##        where:
  ##     suffixes_maxlen($qid_pta,$maxlen) = @suffids u
  ##        (
  ##           { $qid_sta \in suffixes($qid_pta) | length($sta->id2string($qid_sta)) <= $maxlen }
  ##         = { $qid_sta \in suffixes($qid_pta) | length($sta->id2path($qid_sta))   <= $maxlen }
  ##         = { $qid_sta \in suffixes($qid_pta) | $sta->depth($qid_sta)             <= $maxlen }
  ##        )
  ##   + trick: filter prefix-ids of most specific suffix
  my (@psuffids);
  my @prefids =
    grep {
      $suffids_packed eq pack('L*',
			      grep {
				exists($suffids{$_}) || $tp->{sta}->depth($_) <= $maxlen
			      } unpack('L*',$tp->{p2s}[$_]))
    } unpack('L*', $tp->{s2p}[$suffids[0]]);

  return [sort {$a<=>$b} @prefids];
}

BEGIN {
  $tries{pair1}{sig2stems_len} =
    $tries{pair1i}{sig2stems_len} = \&sig2stems_len_FreqTriePair;
}

##========================================================================
## Test: sig->stems(+length)

sub test_sig2stems_len {
  vmsg("test_sig2stems_len()...\n");

  my ($label,$info);
  my $sig = shift || ['','en'];
  my $maxlen = 5;

  our %lab2stems_len = qw();
  while (($label,$info) = each(%tries)) {
    next if (!$info->{sig2stems});
    vmsg("test_sig2stems_len(): $label...\n");

    my $trie       = $info->{trie};
    my $sig2stems_len  = $info->{sig2stems_len};

    my $stems = $lab2stems_len{$label} = $sig2stems_len->($trie,$sig,$maxlen);
  }

  vmsg("test_sig2stems_len(): done.\n");
}
test_sig2stems_len();


##========================================================================
## Bench: sig->stems (+length)

sub bench_sig2stems_len {
  vmsg("bench_sig2stems_len()...\n");
  return if (!$bench{sig2stems_len});

  my $n = shift;
  $n = -3 if (!defined($n));

  my $cmpus = {};
  my ($label,$info);
  my $sig = ['','en'];
  my $maxlen = 5;
  while (($label,$info) = each(%tries)) {
    next if (defined($info->{bench_sig2stems_len} && !$info->{bench_sig2stems_len}));
    next if (!$info->{sig2stems_len});
    my $trie       = $info->{trie};
    my $sig2stems  = $info->{sig2stems_len};

    $cmpus->{$label} =
      sub {
	my $stems = $sig2stems->($trie,$sig,$maxlen);
      };
  }

  print "+ BENCH: sig2stems_len()\n";
  timethese($n,$cmpus);
  #print STDERR (('.' x 80), "\n");
  #cmpthese($n,$cmpus);
  print "\n";
}
bench_sig2stems_len();


##========================================================================
## more...


##========================================================================
## Dummy
exit 0;
print STDERR ("-" x 8), ' END ';
foreach my $i (1..8) {
  print STDERR "-";
}
print STDERR "\n";

