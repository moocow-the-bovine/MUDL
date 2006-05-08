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
  our $progname = 'trietest.perl';

  ##-- %tries: $label=>$info, ...
  ##  + info:
  ##      trie     => $trie,           ##-- trie data, should be filled in by 'populate'
  ##      populate => $generate_code,  ##-- should populate from global unigram dist $main::ugp
  ##      stringi2d => $code,          ##-- s.t. $id     = $code->($trie,$string)
  ##      id2string => $code,          ##-- s.t. $string = $code->($trie,$id)
  ##      nstates   => $code,          ##-- s.t. $nstates = $code->($trie);
  ##      ...
  our %tries = qw();
}

##========================================================================
## Utilities: messages


## undef = vmsg(@msg)
sub vmsg {
  print STDERR $progname, ": ", @_;
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
    @$s = grep { $_->text =~ /^[a-zA-Z]+$/ } @$s;

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
  $tries{gfsm}{populate} = \&populateTrieGfsm;
  $tries{gfsm}{string2id} = \&MUDL::Gfsm::FreqTrie::getStateChars;
  $tries{gfsm}{id2string} = \&MUDL::Gfsm::FreqTrie::stateChars;
  $tries{gfsm}{nstates}   = sub { return $_[0]{fsm}->n_states(); };
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
  $tries{test1}{populate} = \&populateTrie1;
  $tries{test1}{string2id} = \&MUDL::Trie::Base::string2id;
  $tries{test1}{id2string} = \&MUDL::Trie::Base::id2string;
  $tries{test1}{nstates}   = sub { return $_[0]{nq}; };
}

##--------------------------------------------------------------
## Trie Population: MUDL::Trie::FreqPair

use MUDL::Trie::FreqPair;
sub populateTriePair1 {
  our $tp1 = $tries{pair1}{trie} = MUDL::Trie::FreqPair->new();
  my ($w,$f);
  while (($w,$f)=each(%$ugp)) {
    $tp1->addString($w,$f);
  }
  $tp1->finish();
}
BEGIN {
  $tries{pair1}{populate} = \&populateTriePair1;
  $tries{pair1}{string2id} = sub { return $_[0]{pta}->string2id($_[1]); },
  $tries{pair1}{id2string} = sub { return $_[0]{pta}->id2string($_[1]); },
  $tries{pair1}{nstates}   = sub { return $_[0]{pta}{nq}; };
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
  ensureAllTries();
  saveAllTries();

  print map {
    sprintf("filesize(%8s) = %8.2f KB\n", $_, (-s "trietest.trie-$_.bin")/1024)
  } sort(keys(%tries));
}
#testFileSizes();
#exit 0;

##--------------------------------------------------------------
## Test: Tries: file size: RESULTS

#filesize(    gfsm) =   725.67 KB
#filesize(   pair1) =  1777.24 KB
#filesize(   test1) =   489.27 KB


##========================================================================
## Bench: populate

sub bench_populate {
  vmsg("bench_populate()...\n");

  my $n = shift;
  $n = -3 if (!defined($n));

  my $cmpus = {};
  my ($label,$info);
  while (($label,$info) = each(%tries)) {
    $cmpus->{$label} = $info->{populate};
  }

  timethese($n,$cmpus);
  #print STDERR (('.' x 80), "\n");
  #cmpthese($n,$cmpus);
}
bench_populate();
exit 0;

##--------------------------------------------------------------
## Bench: Populate: RESULTS

#Benchmark: running gfsm, pair1, test1 for at least 3 CPU seconds...
#      gfsm:  4 wallclock secs ( 2.99 usr +  0.01 sys =  3.00 CPU) @  2.67/s (n=8)
#     pair1:  6 wallclock secs ( 5.20 usr +  0.00 sys =  5.20 CPU) @  0.38/s (n=2)
#            (warning: too few iterations for a reliable count)
#     test1:  4 wallclock secs ( 3.69 usr +  0.01 sys =  3.70 CPU) @  1.35/s (n=5)


##========================================================================
## Bench: lookup: string -> id

sub bench_string2id {
  vmsg("bench_string2id()...\n");

  my $n = shift;
  $n = -3 if (!defined($n));

  my $cmpus = {};
  my ($label,$info);
  my @ugkeys = keys(%$ugp);
  while (($label,$info) = each(%tries)) {
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

  timethese($n,$cmpus);
  #print STDERR (('.' x 80), "\n");
  #cmpthese($n,$cmpus);
}
#bench_string2id();
#exit 0;

##--------------------------------------------------------------
## Bench: lookup: string->id: RESULTS

#Benchmark: running gfsm, test1 for at least 3 CPU seconds...
#      gfsm:  3 wallclock secs ( 3.20 usr +  0.00 sys =  3.20 CPU) @  3.75/s (n=12)
#     test1:  3 wallclock secs ( 3.10 usr +  0.01 sys =  3.11 CPU) @  8.36/s (n=26)


##========================================================================
## Bench: lookup: id -> string

sub bench_id2string {
  vmsg("bench_id2string()...\n");

  my $n = shift;
  $n = -3 if (!defined($n));

  my $cmpus = {};
  my ($label,$info);
  while (($label,$info) = each(%tries)) {
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

  timethese($n,$cmpus);
  #print STDERR (('.' x 80), "\n");
  #cmpthese($n,$cmpus);
}
bench_id2string();
exit 0;

##--------------------------------------------------------------
## Bench: lookup: string->id: RESULTS

#Benchmark: running gfsm, test1 for at least 3 CPU seconds...
#      gfsm:  4 wallclock secs ( 3.18 usr +  0.00 sys =  3.18 CPU) @  0.63/s (n=2)
#            (warning: too few iterations for a reliable count)
#     test1:  3 wallclock secs ( 3.56 usr +  0.00 sys =  3.56 CPU) @  1.12/s (n=4)


##========================================================================
## Dummy
foreach $i (0..10) {
  print STDERR "--dummy[$i]--\n";
}
