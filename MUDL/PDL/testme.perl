#!/usr/bin/perl -wd

use lib qw(../..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use PDL::CCS::Nd qw(:all);;
use MUDL::PdlDist;
use MUDL::PDL::Smooth;
use MUDL::PDL::Stats qw(:all);
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
  my ($wnumbers,$wnames) = window_list();
  if (!@$wnumbers) {
    dev('/xw');
  }
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
## test: iterated logarithm

## $iterlog = iterlog($pdl)
## $iterlog = iterlog($pdl,$base)
sub iterlog {
  my ($p,$base) = @_;
  my $pp       = $p->pdl;
  my $which    = ($pp>=1)->which;
  my $iterlog0 = $pp->zeroes;
  my $iterlog  = $iterlog0;
  my $ilogbase = defined($base) ? pdl(double,$base)->log**-1 : pdl(double,1);
  while (!$which->isempty) {
    $pp      = $pp->index($which);
    $iterlog = $iterlog->index($which);
    $iterlog += 1;
    $pp->inplace->log->inplace->mult($ilogbase,$pp);
    $which = ($pp>=1)->which;
  }
  return $iterlog0;
}


##----------------------------------------------------------------------
## test: gaussian fitting (and more)

##-- random gaussian
sub ggrandom {
  my ($mu,$sigma,@dims) = @_;
  return (grandom(@dims)*$sigma)+$mu;
}

## $zetap = zetap($ranks)
## $zetap = zetap($ranks,$zeta_constant_s)
sub zetap {
  my ($ranks,$s) = @_;
  $s=1.01 if (!defined($s) || $s == 1);
  my ($zs,$err)=gsl_sf_zeta($s)->abs;
  return ($ranks**-$s)/$zs;
}

## $zetaf = zetaf($freqs,$zeta_constant_s)
sub zetaf {
  my ($freq,$s) = @_;
  my $zetap = zetap($freq->ranks(order=>'desc')+1, $s);
  return $zetap * $freq->sumover / $zetap->sumover;
}

use PDL::Fit::Gaussian;
use PDL::GSLSF::ZETA;
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

  ##-- zipf / zeta
  my $ugrank = $ugf->ranks(order=>'desc')+1;
  my $zetas   = 1.05; ##-- zeta 's' value: stay close to 1 for zipf-safety
                      ##-- after "zipf again" fitting, we see that log-lin exponent fit value is: -1.0533697
  our $DOPLOT = 0;
  if ($DOPLOT) {
    usepgplot;
    points($ugrank, $ugf, {axis=>'logxy',color=>'cyan'}); hold;
    line($ugrank, zetaf($ugf,$zetas), {color=>'red',linewidth=>5} ); hold;
    legend(["Rank:Freq", "Rank:Zeta*N/Sum(Zeta)"], log10(128),log10(10000), {color=>['cyan','red']}); release;
  }

  ##-- zipf again
  my $ug_rank  = $ugrank;
  my $ug_arank = $ugf->avgranks(order=>'desc')+1;
  my $ug_uarank = $ug_arank->qsort->uniq;
  my $ug_uvals  = $ugf->qsort->uniq->slice("-1:0");
  my ($ug_fit,$ug_coeffs) = $ug_uvals->loglinfit($ug_uarank);

  if ($DOPLOT) {
    ##-- plot 'em
    usepgplot();
    points($ug_rank, $ugf,    {axis=>'logxy',color=>'cyan'}); hold;
    line($ug_uarank, $ug_fit, {axis=>'logxy',color=>'red',linewidth=>5}); release;
  }
  ##
  ##-- re-set $zetas & replot
  $zetas = -$ug_coeffs->slice("1");
  if ($DOPLOT) {
    points($ugrank, $ugf, {axis=>'logxy',color=>'cyan'}); hold;
    line($ugrank, zetaf($ugf,$zetas), {color=>'red'} ); hold;
    legend(["Rank:Freq", "Rank:Zeta*N/Sum(Zeta)"], log10(128),log10(10000), {color=>['cyan','red']}); release;
    ##
    ##-- using our qqplotx()
    autolog(0);
    qqplotx(zetap($ug_uarank,$zetas)->log10, $ugf->qsort->uniq->slice("-1:0")->log10, {noline=>0,axis=>'logxy'}, {linewidth=>5,color=>'red'});
  }

  my $bgd = load("utrain-nl.t.bg.pdist.bin"); loadModule($bgd);
  my $bgf    = $bgd->{pdl}->double;
  my $A      = $bgf->dim(0);
  my $bguf   = $bgf->sumover->decode;
  my $bgup   = $bguf / $bguf->sumover;
  my $bguh   = -($bgup->log2);
  my $bgufr  = $bguf->ranks(order=>'desc')+1;

  ##-- zipf/zeta: bigrams
  my $bgnz = $bgf->_nzvals;
  my $bgnz_rank  = $bgnz->ranks(order=>'desc')+1;
  my $bgnz_arank = $bgnz->avgranks(order=>'desc')+1;
  ##
  ##-- get unique values for log-linear fitting
  my $bgnz_uarank = $bgnz_arank->qsort->uniq;
  my $bgnz_uvals  = $bgnz->qsort->uniq->slice("-1:0");
  my ($bgnz_fit,$bgnz_coeffs) = $bgnz_uvals->loglinfit($bgnz_uarank);

  if ($DOPLOT) {
    ##-- plot 'em
    usepgplot();
    points($bgnz_rank, $bgnz, {axis=>'logxy',color=>'cyan'}); hold;
    line($bgnz_uarank, $bgnz_fit, {axis=>'logxy',color=>'red',linewidth=>5}); release;
  }

  my $bgf_nnz0     = $bgf->nnz->decode;
  my $bgf_nnz0n    = $bgf_nnz0->sumover;
  my $bgf_nnz0r    = $bgf_nnz0->ranks(order=>'desc')+1;
  #points( $bgf_nnz0r, $bgf_nnz0, {axis=>'logxy'} );

  my $bgf_pnew  = $bgf_nnz0 / $bguf;
  my $bgf_pnewr = $bgf_pnew->ranks(order=>'asc')+1;
  my $bgf_hnew  = -log2($bgf_pnew);
  #points( $bgf_pnewr, $bgf_pnew, {axis=>'logxy',xtitle=>'rank_asc(nnz0/f0)',ytitle=>'nnz0/f0'} );
  points( $bgf_pnewr, $bgf_hnew, {axis=>'logx',xtitle=>'rank_asc(nnz0/f0)',ytitle=>'h(nnz0/f0)'} );

  ##--
  my $bgf_pcons  = $bgf_nnz0 / $bgf_nnz0n;
  my $bgf_pconsr = $bgf_parc->ranks(order=>'desc')+1;
  my $bgf_hcons  = -log2($bgf_parc);
  points( $bgf_pconsr, $bgf_hcons, {axis=>'logx',xtitle=>'rank_desc(nnz0/NNZ)',ytitle=>'h(nnz0/NNZ)'} ); 

  ##--
  #points( $bguf, $bgf_hcons+$bguh, {axis=>'logx',xtitle=>'f0',ytitle=>'h(nnz0/NNZ)+h(f0/N)'} );
  #points( $bgufr, $bgf_hcons+$bguh, {axis=>'logx',xtitle=>'rank_desc(f0)',ytitle=>'h(nnz0/NNZ)+h(f0/N)'} ); 
  my $yr = [0, ($bgf_hcons+$bguh)->max];
  points( $bgufr, $bgf_hcons, {yrange=>$yr,axis=>'logx',xtitle=>'rank_desc(f0)',ytitle=>'h(nnz0/NNZ)',color=>'red'} ); hold;
  points( $bgufr, $bguh,      {yrange=>$yr,axis=>'logx',xtitle=>'rank_desc(f0)',ytitle=>'h(f0/N)',color=>'blue'} ); hold;
  points( $bgufr, $bgf_hcons+$bguh, {yrange=>$yr,axis=>'logx',xtitle=>'rank_desc(f0)',ytitle=>'h(nnz0/NNZ)+h(f0/N)'} ); release;

  ##--
  #my $bgnz = $bgf->_nzvals();
  my ($bgf_v,$bgf_vc) = $bgnz->valcounts;
  my $bgf_vp = $bgf_vc->double / $bgf_vc->sumover->double;
  my $bgf_vh = -log2($bgf_vp);
  my $bgnz_vh = $bgnz->interpol($bgf_v,$bgf_vh);
  my $bgvh    = $bgf->shadow(which=>$bgf->_whichND->pdl, vals=>$bgnz_vh->append(0));

  ##--
  my $bgvh_u0  = $bgvh->sumover;
  my $bgvh_u0r = $bgvh_u0->ranks(order=>'desc')+1;
  points( $bgvh_u0r, $bgvh_u0, {axis=>'logx',xtitle=>'rank_desc(sum(ff/NNZ))',ytitle=>'sum(ff/NNZ)',color=>'red'} ); hold;

  ##~~~~
  my $bgf_nzavg0   = $bgf->average_nz->decode;
  my $bgf_nzsigma0 = ($bgf**2)->average_nz->decode - ($bgf->average_nz->decode**2);

  my $bgfx = $bgf->xchg(0,1)->to_physically_indexed;
  my $bgf_nnz1     = $bgfx->nnz->decode;
  my $bgf_nzavg1   = $bgfx->average_nz->decode;
  my $bgf_nzsigma1 = ($bgfx**2)->average_nz->decode - ($bgfx->average_nz->decode**2);


  my $bgp  = $bgf / $bgf->sum;
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
#test_gfit();

##----------------------------------------------------------------------
## Test: bigram model+corpus size

sub test_totalsize {
  $PDL::CCS::Nd::CCSND_FLAGS_DEFAULT |= $PDL::CCS::Nd::CCSND_BAD_IS_MISSING | $PDL::CCS::Nd::CCSND_NAN_IS_MISSING;

  my $bgd = load("utrain-nl.t.bg.pdist.bin");
  loadModule($bgd);

  ##----------------------------
  ## corpus frequencies

  ##-- corpus freqs: bigrams
  my $fab = $bgd->{pdl}->double;                    ##-- [a,b] -> f(a,b)
  #$fab->[$PDIMS]->set(1,50000);                     ##-- DEBUG
  my $fba = $fab->xchg(0,1)->to_physically_indexed; ##-- [b,a] -> f(a,b)
  ##
  ##-- corpus freqs: unigrams
  my $fa  = $fba->sumover->decode;        ##-- [a] -> f(a,*)
  my $fb  = $fab->sumover->decode;        ##-- [b] -> f(*,b)
  my $far = $fa->ranks(order=>'desc')+1;  ##-- [a] -> rank(f(a,*))
  #my $fbr = $far;
  ##
  ##-- corpus freqs: totals
  my $N   = $fa->sumover;
  my $A   = $fa->nelem;

  ##----------------------------
  ## corpus probabilities

  ##-- corpus probabilities: unigrams
  my $pa = $fa / $N;    ##-- [a] -> p(a)
  my $pb = $fb / $N;    ##-- [b] -> p(b)
  ##
  ##-- corpus probabilities: bigrams
  my $pab  = $fab / $N;                 ##-- [a,b] -> p(a,b)
  #my $pba  = $fba / $N;                 ##-- [b,a] -> p(a,b)
  my $pbga = $fab / $fa->slice(",*1");  ##-- [a,b] -> p(b|a)
  my $pagb = $fab / $fb->slice("*1,");  ##-- [a,b] -> p(a|b)

  ##----------------------------
  ## corpus entropies

  ##-- corpus probabilities: unigrams
  my $ha = -log2z($pa);      ##-- [a] -> h(a) = -log2(p(a))
  my $hb = -log2z($pb);      ##-- [b] -> h(b) = -log2(p(b))
  ##
  ##-- corpus probabilities: bigrams
  my $hab  = -log2z($pab);   ##-- [a,b] -> h(a,b)
  my $hbga = -log2z($pbga);  ##-- [a,b] -> h(b|a)
  my $hagb = -log2z($pagb);  ##-- [a,b] -> h(a|b)

  ##----------------------------
  ## model stuff: promiscuity

  ##-- model counts: promiscuity (via number of non-zeroes "nnz")
  ##   + used to approximate p(sink_type) in model
  my $Nnz  = $fab->_nnz;        ##-- []  -> |{ (a,b) : f(a,b)>0 }| = |f(A,B)^{-1}(NatNum - {0})| ~ N_nz
  my $nnza = $fba->nnz->decode; ##-- [a] -> |{ b : f(a,b)>0 }| ~ f_nz(b)
  my $nnzb = $fab->nnz->decode; ##-- [b] -> |{ a : f(a,b)>0 }| ~ f_nz(a)
  ##
  ##-- model probabilities: promiscuity
  my $pnza = $nnza/$Nnz;        ##-- [a] -> p_nz(a)
  my $pnzb = $nnzb/$Nnz;        ##-- [b] -> p_nz(b)
  ##
  ##-- model entropies: promiscuity
  my $hnza = -log2z($pnza);     ##-- [a] -> h_nz(a)
  my $hnzb = -log2z($pnzb);     ##-- [b] -> h_nz(b)

  ##----------------------------
  ## model stuff: freq-freqs

  ##-- freq-freqs: CCS::Nd base data
  my $fab_nzvals  = $fab->_nzvals;             ##-- [nzi] -> f(a_nzi,b_nzi)
  my $fab_which   = $fab->_whichND;            ##-- [nzi] ->  [a_nzi,b_nzi]
  my $fab_which_a = $fab_which->slice("(0),"); ##-- [nzi] -> a_nzi
  my $fab_which_b = $fab_which->slice("(1),"); ##-- [nzi] -> b_nzi

  ##-- freq-freqs: value counts
  my ($fab_fv,$fab_fc) = $fab_nzvals->valcounts();
  my $fab_fcz = $fab_fc->smearvals($fab_fv);  ##-- smear GT-style?
  #$fab_fcz = $fab_fc;                        ##-- ... or don't
  my $pab_fcz = $fab_fcz / $fab_fcz->sumover;

  ##-- freq-freqs: back-fit
  my $fab_ff_nzvals = $fab_nzvals->interpol($fab_fv,$fab_fcz)->append(0); ##-- [nzi] -> f( f(a_nzi,b_nzi) )
  my $pab_ff_nzvals = $fab_nzvals->interpol($fab_fv,$pab_fcz)->append(0); ##-- [nzi] -> p( f(a_nzi,b_nzi) )
  my $fab_ff        = $fab->shadow(which=>$fab_which->pdl, vals=>$fab_ff_nzvals); ##-- [a,b] -> f( f(a,b) )
  my $pab_ff        = $fab->shadow(which=>$fab_which->pdl, vals=>$pab_ff_nzvals); ##-- [a,b] -> p( f(a,b) )
  my $hab_ff        = -log2z($pab_ff);                                            ##-- [a,b] -> h( f(a,b) )

  ##-- ???
 points( $fab_nzvals->xvals+1, ($hab_ff->_nzvals+$hnza->index($fab_which_a)/2+$hnzb->index($fab_which_b)/2+$hagb->indexND($fab_which)/2+$hbga->indexND($fab_which)/2)->qsort->slice("-1:0"), {axis=>'logx'});


  print "$0: test_totalsize() done: what now?\n";
}
test_totalsize();



##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#ltest1;
foreach $i (0..10) {
  print "--dummy[$i]--\n";
}
