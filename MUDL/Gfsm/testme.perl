#!/usr/bin/perl -wd

use lib qw(../..);
use Gfsm;
use MUDL::Gfsm::FreqTrie;
use MUDL::Dist;
use MUDL::CmdUtils;

##--------------------------------------------------
## create a freq trie
BEGIN {
  our @words = qw(a as aa aas ab abs abc abcs);
  our ($bos,$eos);
  #($bos,$eos) = ('#','#');
  ($bos,$eos) = ('','');
}
sub newtrie {
  $trie = MUDL::Gfsm::FreqTrie->new();
  foreach $w (@words) {
    $trie->addPathChars((defined($bos) ? $bos : '')
			.$w
			.(defined($eos) ? $eos : ''),
			1);
  }
}

sub newcorpus {
  my ($filename,$n) = @_;
  $filename = 'testme.txt' if (!$filename);
  $n = 100 if (!defined($n));
  open(CORPUS,">$filename") or die("$0: open failed for '$filename': $!");
  foreach $w (@words) {
    print CORPUS ($w."\n") x $n;
  }
  close CORPUS;
}

##--------------------------------------------------
## load german freq trie
sub loadtrie {
  $trie = load('utest-nl.t.cpta.bin');
  loadModule($trie);
}

##--------------------------------------------------
## REVERSE LOOKUP

## get reverse lookup trie
sub rtrie {
  $trie->{rfsm} = $trie->{fsm}->reverse;
  return $trie;
}


## $labs = id2labels($qid)
##  + gets label-address of $qid in $trie
sub id2labels {
  my $qid = shift;
  return undef if ($qid >= $trie->{fsm}->n_states);
  my $rfsm = $trie->{rfsm};
  my $root_tmp = $rfsm->root;
  $rfsm->root($qid);
  my $paths = $rfsm->paths;
  $rfsm->root($root_tmp);
  return [reverse(@{$paths->[0]{lo}})];
}

## $chars = id2chars($qid)
sub id2chars { return $trie->labels2chars(id2labels(shift)); }

##--------------------------------------------------
## get all trie prefix-ngrams of length <= $n
##  --> DODGY : don't know precisely what Goldsmith wants here...

sub prefixes {
  my $n = shift;
  $n = 3 if (!defined($n));

  our $ngs = {};
  my $fsm = $trie->{fsm};
  my $eoslab = $trie->{abet}->find_label('#');
  my ($prefix,$qid);
  my $ai = Gfsm::ArcIter->new();
  $trie->traverse(sub {
		    ($prefix,$qid) = @_;

		    ##-- DEBUG
		    #print STDERR "traverse(): prefix=[", $trie->labels2chars($prefix), "], qid=$qid\n";

		    ##-- ignore the empty prefix (root state)
		    return $MUDL::Gfsm::FreqTrie::TRAVERSE_CONTINUE if (!@$prefix);

		    ##-- ignore $k > $n-grams
		    return $MUDL::Gfsm::FreqTrie::TRAVERSE_IGNORE if ($#$prefix >= $n);

		    ##-- ignore non-proper prefixes
		    $ai->open($fsm,$qid);
		    return $MUDL::Gfsm::FreqTrie::TRAVERSE_IGNORE if (!$ai->ok);

		    ##-- index this prefix
		    $ngs->{$trie->labels2chars($prefix)} = $fsm->final_weight($qid);

		    ##-- ... and its successors
		    return $MUDL::Gfsm::FreqTrie::TRAVERSE_CONTINUE;
		  });

  return bless $ngs, 'MUDL::Dist';
}

##--------------------------------------------------
## get all ngrams of length <= $n in the trie

sub ngrams {
  my $n = shift;
  $n = 3 if (!defined($n));

  our $ngs = {};
  my $fsm = $trie->{fsm};
  my ($prefix,$qid,$k);
  my $ai = Gfsm::ArcIter->new();
  $trie->traverse(sub {
		    ($prefix,$qid) = @_;

		    ##-- ignore empty string prefix (root state)
		    return $MUDL::Gfsm::FreqTrie::TRAVERSE_CONTINUE if (!@$prefix);

		    ##-- add any new $k <= $n-grams
		    foreach $k (1..($#$prefix < $n ? ($#$prefix+1) : $n)) {
		      $ngs->{$trie->labels2chars([@$prefix[($#$prefix-$k+1)..$#$prefix]])}
			+= $fsm->final_weight($qid);
		    }

		    return $MUDL::Gfsm::FreqTrie::TRAVERSE_CONTINUE;
		  });

  return bless $ngs, 'MUDL::Dist';
}

##--------------------------------------------------
## succfreqs1
##  + get Harris-style "successor frequencies"
##    (better: successor cardinalities)
use PDL;
sub succfreqs1 {
  my $fsm = $trie->{fsm};
  $sfpdl = zeroes(long,$fsm->n_states);
  $sfpdl->set($_,$fsm->out_degree($_)) foreach (0..($fsm->n_states-1));
  $sfpdl->where($sfpdl==0) .= 1; ##-- accomodate end-of-string
  return $sfpdl;
}

sub idxtrie {
  our $n2p = [];
  our $p2n = {};
  our $lenpdl = zeroes(long,$trie->{fsm}->n_states);
  my ($prefix,$qid,$pchars);
  $trie->traverse(sub {
		    ($prefix,$qid) = @_;
		    $pchars = $trie->labels2chars($prefix);
		    $n2p->[$qid] = $pchars;
		    $p2n->{$pchars} = $qid;
		    $lenpdl->set($qid,$#$prefix+1);
		    return $MUDL::Gfsm::FreqTrie::TRAVERSE_CONTINUE;
		  });
  return $n2p;
}




##--------------------------------------------------
## dummy
package main;
sub mydummy {
  foreach $i (0..10) {
    print "--dummy[$i]--\n";
  }
}
mydummy;
