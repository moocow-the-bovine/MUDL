#!/usr/bin/perl -wd

use lib qw(../..);
use Gfsm;
use MUDL::Gfsm::FreqTrie;
use MUDL::Corpus::Profile::PTA;
use MUDL::Corpus::Profile::STA;
use MUDL::Corpus::Profile::CPTA;
use MUDL::Corpus::Profile::CSTA;
use MUDL::Unigrams;
use MUDL::CorpusIO;
use MUDL::Dist;
use MUDL::CmdUtils;
use PDL;

use Encode qw(encode decode);

##--------------------------------------------------
## create a freq trie
BEGIN {
  #our @words = qw(a as aa aas ab abs abc abcs);
  our @words = qw(aha ab abs abc abcs);
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
## generate freq tries & unigram word profile
sub gentries {
  $nwords = shift;
  $nwords = 10000 if (!defined($nwords));
  $nread  = 0;

  $cr = MUDL::CorpusIO->formatReader('utest-nl.t');
  $pta = MUDL::Corpus::Profile::CPTA->new(bos=>$bos,eos=>$eos);
  $sta = MUDL::Corpus::Profile::CSTA->new(abet=>$pta->{abet},bos=>$bos,eos=>$eos);
  $ugp = MUDL::Unigrams->new();

  while (defined($s = $cr->getSentence) && $nread < $nwords) {
    if ($nread + @$s > $nwords) {
      @$s = @$s[0..($nwords-$nread-1)];
    }
    $pta->addSentence($s);
    $sta->addSentence($s);
    $ugp->addSentence($s);

    $nread += @$s;
  }
  $trie = $pta;
  undef $cr;

  ##-- save 'em
  $pta->saveFile('utest-nl.t.cpta.bin');
  $sta->saveFile('utest-nl.t.csta.bin');
  $ugp->saveFile('utest-nl.t.ug.bin');
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
## StateIdRelation
use MUDL::Gfsm::StateIdRelation;
sub genidr {
  $idr = MUDL::Gfsm::StateIdRelation->new;
  @related = ([0,1],[0,2],[0,3], [1,1],[1,2]);
  #push(@related, [100000,$Gfsm::noState]);
  $idr->insert(@$_) foreach (@related);
}
#genidr;

##--------------------------------------------------
## FreqTriePair
use MUDL::Gfsm::FreqTriePair;
sub gentpair {
  $tp = MUDL::Gfsm::FreqTriePair->new();
  foreach $w (@words) {
    $tp->addPathChars($w,1);
  }
}
#gentpair;

sub proftpair {
  my $maxtoks = shift;
  $maxtoks = 10000 if (!defined($maxtoks));

  our ($ugs,$tp);

  ##-- first, read in unigrams
  $ugs = {};
  my $cr = MUDL::CorpusIO->fileReader('utest-nl.t');
  my $sent = [];
  for ($i=0; defined($sent=$cr->getSentence) && (!$maxtoks || $i < $maxtoks); $i += @$sent) {
    @$sent = splice(@$sent,0,($maxtoks-$i)) if ($i+@$sent > $maxtoks);
    $ugs->{$_->text}++ foreach (@$sent);
  }

  ##-- now construct trie-pair
  $tp = MUDL::Gfsm::FreqTriePair->new();
  my ($text,$freq);
  while (($text,$freq)=each(%$ugs)) {
    $tp->addPathChars($text,$freq);
  }
}

##-- ca. 60kb larger stored files for 10k tokens, using shorts
##   + dummy is harder to construct than gfsm version:
## timethese(3, {
##               'prof+index_dummy'=>sub { proftpair_dummy(); indextpair_dummy; },
##               'prof+index_gfsm '=>sub { proftpair; $tp->index; $tp->reverseIndex; },
##           })
##
## > Benchmark: timing 3 iterations of prof+index_dummy, prof+index_gfsm...
## > prof+index_dummy: 20 wallclock secs (19.36 usr +  0.01 sys = 19.37 CPU) @  0.15/s (n=3)
## > prof+index_gfsm : 16 wallclock secs (15.76 usr +  0.02 sys = 15.78 CPU) @  0.19/s (n=3)
use MUDL::Enum;
sub proftpair_dummy {
  my $maxtoks = shift;
  $maxtoks = 10000 if (!defined($maxtoks));

  our ($ugs,$tp0);

  ##-- first, read in unigrams
  $ugs = {};
  my $cr = MUDL::CorpusIO->fileReader('utest-nl.t');
  my $sent = [];
  for ($i=0; defined($sent=$cr->getSentence) && (!$maxtoks || $i < $maxtoks); $i += @$sent) {
    @$sent = splice(@$sent,0,($maxtoks-$i)) if ($i+@$sent > $maxtoks);
    $ugs->{$_->text}++ foreach (@$sent);
  }

  ##-- now construct dummy trie-pair
  my ($penum,$senum,$pfreq,$sfreq,$pwidth,$swidth);
  $tp0 = bless {
		cenum => ($cenum=MUDL::Enum->new()),
		penum => ($penum=MUDL::Enum->new()),
		senum => ($senum=MUDL::Enum->new()),
		pfreq => ($pfreq=[]),
		sfreq => ($sfreq=[]),
		pwidth => ($pwidth=[]),
		swidth => ($swidth=[]),

		pairs=>($pairs={}),
		s2p=>([]),
		p2s=>([]),
	       }, 'MUDL::Object';
  my ($tvec,$text,$freq,$i, $pid,$sid, $lastpid,$lastsid);
  foreach $text (sort(keys(%$ugs))) {
    $freq = $ugs->{$text};
    $tvec = pack('S*', map { $cenum->addSymbol($_) } split(//,$text));
    ($lastpid,$lastsid) = (undef,undef);
    ##-- add ids, freqs
    foreach $i (0..(length($text))) {
      if (@{$penum->{id2sym}}==($pid=$penum->addSymbol(substr($tvec,0,($i*2))))) {
	$pwidth->[$lastpid]++ if (defined($lastpid));
      }
      if (@{$senum->{id2sym}}==($sid=$senum->addSymbol(substr($tvec,length($tvec)-($i*2),($i*2))))) {
	$swidth->[$lastsid]++ if (defined($lastsid));
      }
      $pfreq->[$pid] += $freq;
      $sfreq->[$sid] += $freq;
      ($lastpid,$lastsid) = ($pid,$sid);
    }
    ##-- add pairs
    foreach $i (0..(length($text))) {
      $pairs->{pack('LL',
		    $penum->{sym2id}{substr($tvec,0,($i*2))},
		    $senum->{sym2id}{substr($tvec,($i*2),(length($tvec)-($i*2)))})
	      } = undef;
    }
  }
}
sub indextpair_dummy {
  our $tp0;
  my $pairs = $tp0->{pairs};
  my $p2s = $tp0->{p2s} = [];
  my $s2p = $tp0->{s2p} = [];
  ##-- generate equivalence maps
  my ($pair,$qpta,$qsta);
  foreach $pair (sort(keys(%$pairs))) {
    ($qpta,$qsta) = unpack('LL',$pair);
    $p2s->[$qpta] .= pack('L',$qsta);
    $s2p->[$qsta] .= pack('L',$qpta);
  }
}

sub testtpair_dummy {
  proftpair_dummy;
  indextpair_dummy;
  $tp0->saveBinFile('triepair-test-dummy.bin');
}
#testtpair_dummy;

sub test_both {
  loadTp();
  $tp0 = MUDL::Object->loadBinFile('triepair-test-dummy.bin');

  our @prefix_ids = map { int(rand($tp->{pta}{fsm}->n_states)) } (0..1024);
  our @prefix_words = map { $tp->{pta}->stateChars($_) } @prefix_ids;
  our @prefix_vecs_gfsm = map { $tp->{pta}->stateVector($_) } @prefix_ids;
  our @prefix_vecs_dummy = map { pack('S*', @{$tp0->{cenum}{sym2id}}{split(//,$_)}) } @prefix_words;

#  timethese(-3,
#	    {
#	     'suffixes_gfsm'=>sub {
#	       $suffs_gfsm[$_]=$tp->suffixesVector($prefix_vecs_gfsm[$_])
#                foreach (0..$#prefix_vecs_gfsm);
#	     },
#	     'suffixes_dummy'=>sub {
#	       $suffs_dummy[$_] = [
#				   map { $tp0->{senum}{id2sym}[$_] }
#				   unpack('L*',
#					  $tp0->{p2s}[
#						      $tp0->{penum}{sym2id}{
#							$prefix_vecs_dummy[$_]
#						      }])
#				  ] foreach (0..$#prefix_vecs_dummy); },
#	    });
##--
# > Benchmark: running suffixes_dummy, suffixes_gfsm for at least 3 CPU seconds...
# > suffixes_dummy:  3 wallclock secs ( 3.22 usr +  0.00 sys =  3.22 CPU) @ 44.10/s (n=142)
# > suffixes_gfsm :  4 wallclock secs ( 3.56 usr +  0.00 sys =  3.56 CPU) @  1.69/s (n=6)
## --> WHOA.. is this just the embedding thing?


  our (@suffs_gfsm,@suffs_dummy);
  $suffs_gfsm[$_]=$tp->suffixesVector($prefix_vecs_gfsm[$_])
    foreach (0..$#prefix_vecs_gfsm);

  $suffs_dummy[$_] = [ map { $tp0->{senum}{id2sym}[$_] }
		       unpack('L*', $tp0->{p2s}[ $tp0->{penum}{sym2id}{$prefix_vecs_dummy[$_]} ]) ]
    foreach (0..$#prefix_vecs_dummy);

  our @suffs_words_gfsm =
    map { [map { $tp->{pta}->labels2chars([unpack('S*',$_)]) } @$_] } @suffs_gfsm;

  our @suffs_words_dummy =
    map { [ map { join('',@{$tp0->{cenum}{id2sym}}[unpack('S*',$_)]) } @$_ ] } @suffs_dummy;
}
test_both;


sub test_tp {
  proftpair;
  $tp->index;
  $tp->reverseIndex();

  $suff = 'rechtlichen';
  our $qsta = $tp->{sta}->getStateChars($suff);
  our @qpta = unpack('L*', $tp->{s2p}[$qsta]);

  $tp->saveBinFile('triepair-test.bin');
}
#test_tp;

sub loadTp {
  $tp = MUDL::Gfsm::FreqTriePair->loadFile('triepair-test.bin');
}
#loadTp;

sub sig2stems {
  my $sig = shift;
  our @suffs   = map { $_ eq 'NULL' ? '' : $_ } split(/\|/,$sig);
  our @suffids = map { $tp->{sta}->getStateChars($_) } @suffs;
  @suffids     = sort { length($tp->{s2p}[$a]) <=> length($tp->{s2p}[$b]) } @suffids;
  our %prefids = map { $_=>undef } $tp->sta2pta($suffids[0]);
  our $suffids_packed = pack('L*', sort { $a <=> $b } @suffids);
  our @prefids = grep {$tp->{p2s}[$_] eq $suffids_packed} $tp->sta2pta($suffids[0]);
  return @prefids;
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
