#!/usr/bin/perl -wd

use lib qw(../..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use MUDL::PDL::Stats;
use MUDL::PDL::Ranks;
use MUDL::PDL::Compress qw(:all);
use Benchmark qw(cmpthese timethese);

use MUDL::Corpus::MetaProfile::Attach;

BEGIN { $, = ' '; }

##----------------------------------------------------------------------
## Test
##----------------------------------------------------------------------
sub test_zratio {
  #my $a = sequence(7,4);
  #my $b = $a->yvals->double;
  ##
  my $a = (random(100,100)*100)->rint->long;
  my $b = $a->pdl;
  $b->where($b<80) .= 0;
  ##
  #my $a = sequence(100,100)->rint->long;
  #my $b = $a->ones;

  my ($al,$av) = $a->flat->rlenz;
  my ($bl,$bv) = $b->flat->rlenz;

  my $azr = guessCompressionRatio($a);
  my $bzr = guessCompressionRatio($b);

  print STDERR "a_z/a_raw=$azr ; b_z/b_raw=$bzr\n";

  print STDERR "$0: test_zratio done: what now?\n";
}
test_zratio();

##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#ltest1;
foreach $i (0..10) {
  print "--dummy[$i]--\n";
}
