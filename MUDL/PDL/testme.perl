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

use PDL::Graphics::PGPLOT;

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

  my $azp = MUDL::PDL::Compress->new(pdl=>$a);
  my $bzp = MUDL::PDL::Compress->new(pdl=>$b);

  $azp->compress();
  $bzp->compress();
  #my $bz2 = $bzp->decode;

  ##-- test I/O: string
  my $azstr = $azp->saveBinString;
  my $bzstr = $bzp->saveBinString;
  my $azp2 = ref($azp)->loadBinString($azstr);
  my $bzp2 = ref($bzp)->loadBinString($bzstr);

  ##-- test I/O: file
  $azp->saveBinFile('azp.bin');
  $bzp->saveBinFile('bzp.bin');
  $azp2 = ref($azp)->loadBinFile('azp.bin');
  $bzp2 = ref($bzp)->loadBinFile('bzp.bin');

  ##-- test I/O: zfile
  $azp->saveFile('azp.zbin');
  $bzp->saveFile('bzp.zbin');
  $azp2 = ref($azp)->loadFile('azp.zbin');
  $bzp2 = ref($bzp)->loadFile('bzp.zbin');

  print STDERR "$0: test_zratio done: what now?\n";
}
#test_zratio();

##----------------------------------------------------------------------
## test: gaussian fitting

##-- random gaussian
sub ggrandom {
  my ($mu,$sigma,@dims) = @_;
  return (grandom(@dims)*$sigma)+$mu;
}

use PDL::Fit::Gaussian;
sub test_gfit {
  my ($mu,$sigma,$n) = (0,1,1000);
  my $rawdata = ggrandom($mu,$sigma,$n);
  my ($xvals,$ydata) = hist($rawdata);

  my ($yfit,$ypeak,$ymu,$ysigma) = $ydata->smoothGaussian($xvals);

  ##-- re-compute $yfit from parameters
  ## pdf($xvals, $mu,$sigma) =  1/($sigma*sqrt(2*$pi)) * exp( -($xvals*$ymu)**2 / (2*$ysigma**2) )
  #my $ypdf  = 1/($ysigma*sqrt(2*3.14195)) * exp( -($xvals*$ymu)**2 / (2*$ysigma**2) );
  my $yfit2 = $ypeak * exp( -($xvals-$ymu)**2 / (2*$ysigma**2) );

  ##-- plot
  bin($xvals,$ydata,{color=>'grey'});
  hold; points($xvals,$yfit,{color=>'red'});
  hold; line($xvals,$yfit2,{color=>'green'});
  release;

  ##-- ... looks good for histogram data; not so hot for raw data ...

  ##-- what now?
  print STDERR "$0: test_gfit done: what now?\n";
}
test_gfit();

##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#ltest1;
foreach $i (0..10) {
  print "--dummy[$i]--\n";
}
