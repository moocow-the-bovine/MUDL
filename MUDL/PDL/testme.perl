#!/usr/bin/perl -wd

use lib qw(../..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use PDL::CCS::Nd qw(:all);;
use MUDL::PdlDist;
use MUDL::PDL::Smooth;
use MUDL::PDL::Stats;
use MUDL::PDL::Ranks;
use MUDL::PDL::Compress qw(:all);
use Benchmark qw(cmpthese timethese);

use MUDL::Corpus::MetaProfile::Attach;
use MUDL::PDL::Plot;
use PDL::Graphics::PGPLOT;

BEGIN { $, = ' '; }

BEGIN {
  $PDL::CCS::Nd::CCSND_FLAGS_DEFAULT |= $PDL::CCS::Nd::CCSND_BAD_IS_MISSING | $PDL::CCS::Nd::CCSND_NAN_IS_MISSING;
}

##----------------------------------------------------------------------
## pgplot
sub usepgplot {
  dev('/xw');
  autolog(1);
}

##----------------------------------------------------------------------
## Test: compression ratio
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
## test: compressed I/O (gzip)
use IO::Compress::Gzip     qw(:all);
use IO::Uncompress::Gunzip qw(:all);
sub test_gzio {
  my $ugd = load("utrain-nl.t.ug.pdist.bin"); loadModule($ugd);

  ##-- test I/O
  $ugd->saveFile('ugd.bin');
  $ugd->saveFile('ugd.bin.gz');
  #$ugd->saveFile('ugd.zbin');
  #$ugd->saveFile('ugd_rle.bin.gz', gzargs=>{-Strategy=>Z_RLE});
  #$ugd->saveFile('ugd_l9.bin.gz', gzargs=>{-Level=>9});
  #$ugd->saveFile('ugd_l3.bin.gz', gzargs=>{-Level=>3});
  #$ugd->saveFile('ugd_l2.bin.gz', gzargs=>{-Level=>2});
  #$ugd->saveFile('ugd_l1.bin.gz', gzargs=>{-Level=>1});

  my ($ugd2);
  $ugd2 = ref($ugd)->loadFile('ugd.bin');
  $ugd2 = ref($ugd)->loadFile('ugd.bin.gz');

  my $fhfile = "ugd.fh.bin.gz";
  my $gzfh = IO::File->new(">$fhfile");
  $ugd->saveGZBinFh($gzfh);
  $gzfh->close();
  $gzfh = IO::File->new("<$fhfile");
  $ugd2 = ref($ugd)->loadGZBinFh($gzfh);
  $gzfh->close();

  print STDERR "$0: test_gzio() done: what now?\n";
}
#test_gzio();

##----------------------------------------------------------------------
## test: gaussian fitting

##-- random gaussian
sub ggrandom {
  my ($mu,$sigma,@dims) = @_;
  return (grandom(@dims)*$sigma)+$mu;
}

use PDL::Fit::Gaussian;
sub test_gfit {
  my ($mu,$sigma,$n) = (0.5,2,100);
  my $raw = ggrandom($mu,$sigma,$n);
  my ($xvals,$ydata) = hist($raw);
  my $xrange = [$raw->min-1, $raw->max+1];
  my $yrange = [0,1.1];

  ##-- test: NL data
  $PDL::CCS::Nd::CCSND_FLAGS_DEFAULT |= $PDL::CCS::Nd::CCSND_BAD_IS_MISSING | $PDL::CCS::Nd::CCSND_NAN_IS_MISSING;
  my $ugd = load("utrain-nl.t.ug.pdist.bin"); loadModule($ugd);

  my $ugf = $ugd->{pdl}->double;
  my $N   = $ugf->sumover;
  my $ugp = $ugf / $N;
  my $ugh = -log($ugp) / log(2);

  my $bgd = load("utrain-nl.t.bg.pdist.bin"); loadModule($bgd);
  my $bgf    = $bgd->{pdl}->double;
  my $A      = $bgf->dim(0);
  my $bguf   = $bgf->sumover->decode;
  my $bgnnz0 = $bgf->nnz->decode;
  my $bgp = $bgf / $bgf->sum;
  my $p1g2 = $bgp / $bgp->sumover->dummy(0,1);
  #my $p2g1 = ($bgp->xchg(0,1) / $bgp->xchg(0,1)->sumover->dummy(0,1))->xchg(0,1);

  ##-- get value-counts
  ($v,$vc) = $ugf->valcounts;
  qqplot($v->log);                ##-- looks pretty darned linear to me: unique frequency values (but all q>0)
  qqplot($ugf->qsort->uniq->log); ##-- ... same thing
  qqplot((-$ugh)->qsort->uniq);   ##-- ... same shape, but all qvals < 0
  qqplot($ugh->qsort->uniq);      ##-- ... same thing, (reversed because h=-log(...))
  qqplot(($ugp*$ugh)->qsort);     ##-- long tails, looks (inverse-?) exponential
  qqplot(($ugp*$ugh)->qsort->uniq); ##-- long tails, looks (inverse-?) exponential
  qqplot(($ugp*$ugh)->qsort->uniq->log); ##-- looks ok
  qqplot($vc);     ##-- long pre-tail
  qqplot($v);      ##-- long pre-tail, bad fit
  qqplot($vc->log); ##-- many 1s get in the way, bad fit
  qqplot($vc->qsort->uniq->log); ##-- good fit, but have we gotten anywhere?

  ##-- test: Q-Q plot
  qqplot($raw);
  my $eraw = $raw->exp;
  qqplot($eraw);

  ##-- try various fitting routines
  my ($x4,$y4) = hist($raw,undef,undef,($raw->max-$raw->min)/4);
  my ($x8,$y8) = hist($raw,undef,undef,($raw->max-$raw->min)/8);
  my ($x16,$y16) = hist($raw,undef,undef,($raw->max-$raw->min)/16);
  my ($x32,$y32) = hist($raw,undef,undef,($raw->max-$raw->min)/32);

  ##-- fits: @fitI = ($yfitI,$ypeakI,$ymuI,$ysigmaI);
  my @fit4  = $y4->smoothGaussian($x4);
  my @fit8  = $y8->smoothGaussian($x8);
  my @fit16 = $y16->smoothGaussian($x16);
  my @fit32 = $y32->smoothGaussian($x32);

  ##-- plot 'em
  errbin($x4,$y4/$y4->max,{color=>'red',xr=>$xrange,yr=>$yrange});
  hold; line(gausspoints(1,@fit4[2,3], @$xrange,100), {color=>'red'});
  hold; errbin($x8 +.1,  $y8/$y8->max,{color=>'yellow'});
  hold; line(gausspoints(1,@fit8[2,3], @$xrange,100), {color=>'yellow'});
  hold; errbin($x16+.2, $y16/$y16->max,{color=>'green'});
  hold; line(gausspoints(1,@fit16[2,3], @$xrange,100), {color=>'green'});
  hold; errbin($x32+.3, $y32/$y32->max,{color=>'blue'});
  hold; line(gausspoints(1,@fit32[2,3], @$xrange,100), {color=>'blue'});
  ##-- grand fit
  hold; points(gausspoints(1,$raw->mean,$raw->stddev, @$xrange,100), {color=>'cyan',symbol=>0});
  ##-- requested curve
  hold; line(gausspoints(1,$mu,$sigma, @$xrange,1000), {color=>'black'});
  release;

  ##~~~~~~~~~~

  ##-- fit gaussian (default)
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
