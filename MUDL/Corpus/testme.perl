#!/usr/bin/perl -wd

use lib qw(../..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use MUDL::PdlDist;
use MUDL::Corpus::MetaProfile;

BEGIN {
  $, = ' ';
}

##-- test1()
sub test1 {
  use vars qw($mp1 $bds2 $tgs2 $prf2);
  $mp1  = load('stage1.mp.bin');  loadModule(ref($mp1));
  $bds2 = load('stage2.bds.bin'); loadModule(ref($bds2));
  $tgs2 = load('stage2.tgs.bin'); loadModule(ref($tgs2));
  $prf2 = load('stage2.prf.bin'); loadModule(ref($prf2));

  use vars qw($mp $prof);
  $mp   = $mp1;
  $prof = $prf2;
}


##-- dummy
foreach $i (0..100) {
  print "-- dummy ($i) --\n";
}
