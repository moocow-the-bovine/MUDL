#!/usr/bin/perl -wd

use lib qw(../..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use MUDL::PdlDist;
use MUDL::Corpus::MetaProfile;

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
  ## Stage-1: Words
  ## + "w$i"($i) for 1<=$i<=$n ; and '__$'(0)
  $Nt = 4;
  $e1 = MUDL::Enum->new();
  $e1->addIndexedSymbol('__$', 0);
  $e1->addIndexedSymbol("w$_",$_) foreach (1..$Nt);

  ##--------------------------------
  ## Stage-1: Targets, Bounds
  $tgs1 = $bds1 = $e1;

  ##--------------------------------
  ## Stage-1: Profile
  $pclass = MUDL::Corpus::Profile::LRBigrams; loadModule($pclass);
  $prf = $pclass->new(targets=>$tgs1, bounds=>$bds1);

  ##--------------------------------
  ## Stage-1: Profile: Probabilities
  $Nneighbors    = 2;
  $neighborCoeff = 10;
  foreach $i (0..$Nt) {
    foreach $neighborNum (1..$Nneighbors) {
      $jLeft  = $i-$neighborNum;
      $jRight = $i+$neighborNum; $jRight = 0 if ($jRight==$Nt+1);
      $f = $neighborCoeff/$neighborNum;

      $prf->{left}{nz}{"${i}\t${jLeft}"}   = $f if ($jLeft >= 0);
      $prf->{right}{nz}{"${i}\t${jRight}"} = $f if ($jRight <= $Nt);
    }
  }
  $prf1 = $prf;

  ##--------------------------------
  ## Stage-1: Profile: Tree
  require MUDL::Cluster::Tree;
  $tcd = 's';
  $tcm = 'm';
  $tck = 3;
  use vars qw($data);
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

  ##--------------------------------
  ## Stage-1: Bootstrap
  use MUDL::Corpus::MetaProfile;
  $mp = MUDL::Corpus::MetaProfile->new(d2p=>{method=>'nbest_base',n=>2,b=>2});
  $mp->bootstrap($prf, $tc);
  $mp1 = $mp;

  use vars qw($phat $pprof $pp3d $Mprev);

  ##-- phat? --> looks good
  $phat = $mp->{phat};

  ##-- pprof? --> ?
  $pprof = $mp->{pprof};
  $pp3d  = $mp->{pprof}->toPDL3d;

  ##-- Mprev --> ?


  ##--------------------------------------
  ## Stage-2

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
	  sprintf(" f_{ %5s }( T=%2u, B=%2u ) = %4.2g\n", $dir, $t, $b, $pr->{$dir}{nz}{$_})
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
sub showphat {
  my $ph = shift;
  $ph = $main::phat if (!defined($ph));
  foreach $cid (0..($ph->dim(0)-1)) {
    foreach $wid (0..($ph->dim(1)-1)) {
      print sprintf(" ^p( C=%d | W=%d ) = %4.2g\n", $cid, $wid, $ph->at($cid,$wid));
    }
  }
}

##------------------------------------------------------------------------
## Real tests
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
