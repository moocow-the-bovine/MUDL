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
