#!/usr/bin/perl -wd

use lib qw(../../..);
use MUDL;
use MUDL::CmdUtils;
use MUDL::Corpus::Profile::LR;
use MUDL::Corpus::Profile::LRMI;
use PDL;
use PDL::Cluster;
use PDL::CCS;
use PDL::Graphics::PGPLOT;

BEGIN { $,=' '; }


##-- test LR normalization
use vars qw($apdl $ppdl);
sub test1 {
  $pprof = load('utrain.t.lrmi-b200-t0+200.bin');
  $ppdl  = $pprof->toPDL;

  $aprof = load('utrain.t.lrmiAL-b200-t0+200.bin');
  $aprof->{norm_nan_p} = 0;
  $apdl = $aprof->toPDL;
}
test1;


##--- dummy
foreach $dummy (0..100) {
  print "--dummy ($dummy)--\n";
}
