#!/usr/bin/perl -wd

use lib qw(../..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use MUDL::PdlDist;
use MUDL::Corpus::MetaProfile;
use MUDL::Corpus::MetaProfile::Full;
use MUDL::Corpus::MetaProfile::Deep;

BEGIN {
  $, = ' ';
  select STDERR; $|=1; select STDOUT;
}

##------------------------------------------------------------------------
## Dummy tests
##------------------------------------------------------------------------
sub dtest1 {
  ##--------------------------------------
  ## Stage-1

  ##--------------------------------
  ## Stage-1: Enums
  ## + "w$i"($i) for 1<=$i<=$n ; and '__$'(0)
  $Nt = 4;
  $eos = '__$';
  $tgs1 = MUDL::Enum->new();
  $tgs1->addSymbol("w$_") foreach (0..($Nt-1));
  $bds1 = $tgs1->copy;
  $eosi = $bds1->addSymbol($eos);

  ##--------------------------------
  ## Stage-1: Profile
  $pclass = MUDL::Corpus::Profile::LRBigrams; loadModule($pclass);
  $prf = $pclass->new(targets=>$tgs1, bounds=>$bds1);

  ##--------------------------------
  ## Stage-1: Profile: Probabilities
  $Nneighbors    = 2;
  $neighborCoeff = 10;
  foreach $i (0..($Nt-1)) {
    foreach $neighborNum (1..$Nneighbors) {
      $jLeft  = $i-$neighborNum;
      $jLeft  = $eosi if ($jLeft == -1);

      $jRight = $i+$neighborNum;
      $jRight = $eosi if ($jRight == $bds1->size);

      $f = $neighborCoeff/$neighborNum;

      $prf->{left}{nz}{"${i}\t${jLeft}"}   = $f if (0 <= $jLeft && $jLeft < $bds1->size);
      $prf->{right}{nz}{"${i}\t${jRight}"} = $f if (0 <= $jRight && $jRight < $bds1->size);
    }
  }
  $prf1 = $prf;

  ##--------------------------------
  ## Stage-1: Profile: Tree
  require MUDL::Cluster::Tree;
  $tcd = 's';
  $tcm = 'm';
  $tck = 3;
  use vars qw($data $Dists);
  $tc = MUDL::Cluster::Tree->new(
				 data=>($data=$prf->toPDL),
				 dist=>$tcd,
				 method=>$tcm,
				 enum=>$prf->{targets},
				 nclusters=>$tck,
				);
  $tc->cluster();
  $tc->cut($tck);
  $tc1 = $tc;

  $Dists = $tc->leafdistances();


  ##--------------------------------
  ## Stage-1: Bootstrap
  use MUDL::Corpus::MetaProfile;
  $mp = MUDL::Corpus::MetaProfile->new(d2p=>{method=>'nbest_inverse',n=>2,b=>2});
  $mp->bootstrap($prf, $tc);
  $mp1 = $mp;

  use vars qw($phat $pprof $pp3d $Mprev);

  ##-- phat? --> looks good
  $phat = $mp->{phat};

  ##-- pprof? --> seems ok
  $pprof = $mp->{pprof};

  ##-- Mprev --> ? (assumed working, cursory inspection -> seems sane)
  $Mp3d = $mp->{pprof}->toPDL3d;
  $Mprev = $Mp = $mp->{Mprev};
}

sub dtest2 {
  ##--------------------------------------
  ## Stage-2

  use vars (qw($Nt2 $prf2raw $pclass2));

  ##--------------------------------
  ## Stage-2: Enums
  ## + "w$i"($i) for 1<=$i<=$n ; and '__$'(0)
  $Nt2add = 2;
  $Nt2 = $Nt + $Nt2add;
  $bds2 = $tgs1->copy;
  $tgs2 = MUDL::Enum->new();
  $tgs2->addSymbol("w$_") foreach (($Nt+1)..($Nt+$Nt2add));

  ##--------------------------------
  ## Stage-2: Profile
  $pclass2 = ref($prf1);
  $prf2    = $pclass2->new(targets=>$tgs2, bounds=>$bds2);

  ##--------------------------------
  ## Stage-2: Profile: Probabilities
  $Nneighbors    = 1;
  $neighborCoeff = 10;
  foreach $i (0..($Nt2add-1)) {
    foreach $neighborNum (1..$Nneighbors) {
      $jLeft  = $i-$neighborNum;
      $jRight = $i+$neighborNum; $jRight = 0 if ($jRight==$Nt+1); ##-- EOS-hack
      $f = $neighborCoeff/$neighborNum;

      $prf2->{left}{nz}{"${i}\t${jLeft}"}   = $f if (0 <= $jLeft  && $jLeft  <= $Nt);
      $prf2->{right}{nz}{"${i}\t${jRight}"} = $f if (0 <= $jRight && $jRight <= $Nt);
    }
  }
  $prf2raw = $prf2->copy;

  ##--------------------------------
  ## Stage-2: Attach
  $mp->{tree}{dist} = 'b'; ##-- hack
  $mp->attach($prf2);

  use vars qw($Mhat $Mhat3d $Dists $tk2tlek);

  ##-- fhat? (tweaked $prf2) --> looks ok

  ##-- Mhat? (tweaked $prf2 PDL) ## --> seems ok
  $Mhat3d = $prf2->toPDL3d; ##-- ok
  $Mhat   = $mp->{Mhat};    ##-- ok
  $Mprev  = $mp->{Mprev};   ##-- matches, pre-rowdistances()

  ##-- Dists? --> looks ok (divided by weight total!)
  $Dists = $mp->{Dists};

  ##-- target-id translator: ok
  $tk2tlek = $mp->{tk2tlek};

  ##-- phat? --> looks ok
  $phat = $mp->{phat};

  ##-- pprof? --> seems kosher
  $pprof = $mp->{pprof};

  ##-- Mprev --> assume kosher
  $Mp3d  = $mp->{pprof}->toPDL3d;
  $Mp = $Mprev = $mp->{Mprev};

  ##-- tree?
  $tree = $mp->toTree;
  $dg   = $tree->toDendogram;
}


##-- profile viewing
sub showprf {
  my $pr = shift;
  $pr = $main::prf if (!defined($pr));
  foreach $dir (qw(left right)) {
    print
      '',
	(map {
	  my ($t,$b) = $pr->{$dir}->split($_);
	  my $ts = $pr->{targets}->symbol($t);
	  my $bs = $pr->{bounds}->symbol($b);
	  #sprintf(" f_{ %5s }( T=%2u, B=%2u ) = %8.2f\n", $dir, $t, $b, $pr->{$dir}{nz}{$_})
	  sprintf(" f_{ %5s }( T=%4s, B=%4s ) = %8.2f\n", $dir, $ts, $bs, $pr->{$dir}{nz}{$_})
	}
	 sort {
	   my ($at,$ab) = $pr->{$dir}->split($a);
	   my ($bt,$bb) = $pr->{$dir}->split($b);
	   $at <=> $bt || $ab <=> $bb
	 }
	 keys(%{$pr->{$dir}{nz}})
	);
  }
}

##-- phat viewing
BEGIN { $showphat_dozero = 0; }
sub showphat {
  my $ph = shift;
  $ph = $main::phat if (!defined($ph));
  foreach $cid (0..($ph->dim(0)-1)) {
    foreach $wid (0..($ph->dim(1)-1)) {
      next if (!$showphat_dozero && $ph->at($cid,$wid) == 0);
      print
	#sprintf(" ^p( C=%d | W=%d ) = %8.2f\n", $cid, $wid, $ph->at($cid,$wid));
	sprintf(" ^p( C=%5s | W=%5s ) = %8.2f\n", $mp->{cbtenum}->symbols($cid,$wid), $ph->at($cid,$wid));
    }
  }
}


##------------------------------------------------------------------------
## Deep tests
##------------------------------------------------------------------------
sub dptest3 {
  #require MUDL::Corpus::MetaProfile::Deep;
  use vars qw($bds2 $tgs2 $prf2 $mp2);
  use vars qw($bds $tgs $prf $prof $tree $tc $mp);

  $mp2  = load('stage2.mpd.bin');  loadModule(ref($mp2));
  $prf2 = $mp2->{prof};            loadModule(ref($prf2));
  $bds2 = $prf2->{bounds};
  $tgs2 = $prf2->{targets};
  $tc2  = $mp2->{tree};            loadModule(ref($tc2));

  $bds = $bds2;
  $tgs = $tgs2;
  $prf = $prof = $prf2;
  $tc = $tree = $tc2;
  $mp  = $mp2;
}

sub dptest2 {
  #require MUDL::Corpus::MetaProfile::Deep;
  use vars qw($bds2 $tgs2 $prf2);

  $prf2 = load('stage2.prf.bin');  loadModule(ref($prf2));
  $bds2 = $prf1->{bounds};
  $tgs2 = $prf1->{targets};

  $bds = $bds2;
  $tgs = $tgs2;
  $prf = $prof = $prf2;

  $mp->update($prf2);

  ##-- properties
  use vars qw($tenum $tenum_ltk $tenum_k $pprof);
  $tenum = $mp->{tenum};
  $tenum_ltk = $mp->{tenum_ltk};
  $tenum_k = $mp->{tenum_k};
  $pprof = $mp->{pprof};
}

sub dptest1 {
  use vars qw($bds1 $tgs1 $prf1 $tc1 $mp1);
  use vars qw($bds $tgs $prf $prof $tc $mp);
  #require MUDL::Corpus::MetaProfile::Deep;

  $mp1  = load('stage1.mpd.bin');  loadModule(ref($mp1));
  $prf1 = $mp1->{prof};            loadModule(ref($prf1));
  $bds1 = $prf1->{bounds};
  $tgs1 = $prf1->{targets};
  $tc1  = $mp1->{tree};            loadModule(ref($tc1));

  $bds = $bds1;
  $tgs = $tgs1;
  $prf = $prof = $prf1;
  $tc = $tree = $tc1;
  $mp  = $mp1;
}

sub dptest1bs {
  use vars qw($bds1 $tgs1 $prf1 $tc1 $mp1);
  use vars qw($bds $tgs $prf $prof $tc $mp);
  require MUDL::Corpus::MetaProfile::Deep;

  $prf1 = load('stage1.prf.bin');  loadModule(ref($prf1));
  $bds1 = $prf1->{bounds};
  $tgs1 = $prf1->{targets};
  $tc1  = load('stage1.tc.bin');   loadModule(ref($tc1));

  $bds = $bds1;
  $tgs = $tgs1;
  $prf = $prof = $prf1;
  $tc = $tree = $tc1;

  $mp1 = MUDL::Corpus::MetaProfile::Deep->new();
  $mp1->{d2p} = $d2p if (defined($d2p));
  $mp  = $mp1;

  $mp1->bootstrap($prf1,$tc1);
}


##------------------------------------------------------------------------
## Real tests (newer / recluster)
##------------------------------------------------------------------------
sub rctest {
  $phati=$mp->{phat};
  $mp->recluster();
  $phatip1=$mp->{phat};

  #rceval();
}

BEGIN { *rceval = \&rceval2; }
sub rceval2 {
  $nequal = 0;
  $ntotal = 0;

  $n = $mp->{d2p}{n};
  $phatii   = zeroes(long,$n,$phati->dim(1));   $phati->maximum_n_ind($phatii);

  $phatip1i = zeroes(long,$n,$phatip1->dim(1)); $phatip1->maximum_n_ind($phatip1i);

  foreach $i (0..($phatii->dim(1)-1)) {
    foreach $j (0..($i-1)) {
      foreach $pos (0..($n-1)) {
	++$ntotal;
	if ($phatii->at($pos,$i)==$phatii->at($pos,$j)) {
	  if ($phatip1i->at($pos,$i)==$phatip1i->at($pos,$j)) {
	    ++$nequal;
	  }
	} elsif ($phatip1i->at($pos,$i)!=$phatip1i->at($pos,$j)) {
	  ++$nequal;
	}
      }
    }
  }

  print sprintf("%8u equal / %8u total = %8.2f %%\n", $nequal, $ntotal, $nequal/(.01*$ntotal));
}

#*rceval = \&rceval1;
sub rceval1 {
  $nequal = (qsorti($phati) == qsorti($phatip1))->which->nelem;
  $ntotal = $phati->nelem;
  print sprintf("%8u equal / %8u total = %8.2f %%\n", $nequal, $ntotal, 100*$nequal/$ntotal);
}

BEGIN { our $d2p = {method=>'nbest_inverse',n=>2}; }
sub rtest1 {
  use vars qw($bds1 $tgs1 $prf1 $tc1 $mp1);
  use vars qw($bds $tgs $prf $tc $mp);
  require MUDL::Corpus::MetaProfile;

  $prf1 = load('stage1.prf.bin');  loadModule(ref($prf1));
  $bds1 = $prf1->{bounds};
  $tgs1 = $prf1->{targets};
  $tc1  = load('stage1.tc.bin');   loadModule(ref($tc1));
  $mp1  = load('stage1.mp.bin');   loadModule(ref($mp1));

  $mp1->{d2p} = $d2p if (defined($d2p));

  $bds = $bds1;
  $tgs = $tgs1;
  $prf = $prf1;
  $tc = $tree = $tc1;
  $mp  = $mp1;

  ##-- properties
  use vars qw($phat1 $ld1 $pprof1);
  use vars qw($phat $ld $pprof);
  $phat1 = $mp->{phat};
  $ld1   = $mp->{tree}->leafdistances;
  $pprof1 = $mp->{pprof};

  $phat = $phat1;
  $ld = $ld1;
  $pprof = $pprof1;
}

sub rtest1bs {
  use vars qw($bds1 $tgs1 $prf1 $tc1 $mp1);
  use vars qw($bds $tgs $prf $tc $mp);
  require MUDL::Corpus::MetaProfile;

  $prf1 = load('stage1.prf.bin');  loadModule(ref($prf1));
  $bds1 = $prf1->{bounds};
  $tgs1 = $prf1->{targets};
  $tc1  = load('stage1.tc.bin');   loadModule(ref($tc1));
  $mp1  = MUDL::Corpus::MetaProfile->new(d2p=>$d2p);
  $mp1->bootstrap($prf1,$tc1);
  $mp1->saveFile("stage1.mp.bin");

  $bds = $bds1;
  $tgs = $tgs1;
  $prf = $prf1;
  $tc = $tree = $tc1;
  $mp  = $mp1;

  ##-- properties
  use vars qw($phat1 $ld1 $pprof1);
  use vars qw($phat $ld $pprof);
  $phat1 = $mp->{phat};
  $ld1   = $mp->{tree}->leafdistances;
  $pprof1 = $mp->{pprof};

  $phat = $phat1;
  $ld = $ld1;
  $pprof = $pprof1;
}

##------------------------------------------------------------------------
## Real tests (old)
##------------------------------------------------------------------------

##-- test1()
sub test1 {

  use vars qw($bds1 $tgs1 $prf1 $tc1 $mp1);
  $prf1 = load('stage1.prf.bin');  loadModule(ref($prf1));
  $bds1 = $prf1->{bounds};
  $tgs1 = $prf1->{targets};
  $tc1  = load('stage1.tc.bin');   loadModule(ref($tc1));
  $mp1  = load('stage1.mp.bin');   loadModule(ref($mp1));

  use vars qw($bds2 $tgs2 $prf2 $tc2 $mp2);
  $prf2 = load('stage2.prf.bin'); loadModule(ref($prf2));
  $bds2 = $prf2->{bounds};
  $tgs2 = $prf2->{targets};

  use vars qw($mp $prof);
  $mp   = $mp1;
  $prof = $prf2;
}

sub bs {
  $mp1 = MUDL::Corpus::MetaProfile->new();
  $mp1->bootstrap($prf1,$tc1);
}

sub test2 {
  $mp->attach($prof);

  ##-- get data
  our ($Mhat, $Mprev, $Nt, $Nc, $cenum, $tenum, %tk2tlek, $Dists);
  ($Mhat,$Mprev) = @$mp{qw(Mhat Mprev)};
  $Nt = $tgs2->size;
  $Nc = $mp->{cenum}->size;
  $cenum = $mp->{cenum};
  $tenum = $mp->{tenum};
  %tk2tlek = %{$mp->{tk2tlek}};
  $Dists = $mp->{Dists};
}

sub test2b {
  use vars qw($vt $dg);
  $mp = load('stage2.mp.bin');
  $vt = $mp->toTree;
  $dg = $vt->toDendogram;
}


##-- dummy
foreach $i (0..100) {
  print "-- dummy ($i) --\n";
}
