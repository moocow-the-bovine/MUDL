##-*- Mode: CPerl -*-

## File: MUDL::PDL::Smooth.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL Smoothing utilities
##======================================================================

package MUDL::PDL::Smooth;
use MUDL::PDL::Ranks;
use MUDL::Limits;
use PDL;
use PDL::Math;
use PDL::CCS;
use PDL::VectorValued;

use strict;

our @ISA = qw(Exporter);
our %EXPORT_TAGS =
  (
   'vals'  => ['valcounts','smearvals','intervals'],
   'bins'  => ['logbins', 'makebins','makebins_exp','findbins',
	       'binavg', 'binsd', 'bin2d',
	      ],
   'fit'   => ['zipf_fit',
	       'zipf_fit_lm1', 'zipf_fit_lm2',
	       'mooLinfit', 'loglinfit',
	       'expfit',
	       'linfit',
	       'qqfit',
	      ],
   #'gt'    => ['smoothGTLogLin'],
   'gauss' => ['smoothGaussian', 'gausspoints', 'gaussyvals', 'probit',
	       'gausspdf', 'gausscdf', 'gausspeak',
	       'gaussquantiles', 'gaussqvals', 'gausscdfi', ##-- all aliases for one another
	       'gausswidth',
	       'uosm',
	      ],
   'di' => ['diLambdas2',
	   ],
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK   = @{$EXPORT_TAGS{all}};
our @EXPORT      = @EXPORT_OK;

##======================================================================
## Value Counts

## ($values,$valcounts) = $pdl->valcounts()  ##-- array context, suitable for PDL::Primitive::interpol(ate)
##  + flat only (?)
##  + returned PDLs are suitable for back-fitting into $pdl indices with:
##    $i2valcount = $pdl->interpol($values,$valcounts);
##  + see interpol() and interpolate() in PDL::Primitive
BEGIN { *PDL::valcounts = \&valcounts; }
sub valcounts {
  my ($pdl,%opts) = @_;
  my ($counts,$values) = $pdl->flat->qsort->rle();
  my $counts_mask      = ($counts>0);
  return ($values->where($counts_mask),$counts->where($counts_mask));
}

BEGIN { *PDL::CCS::Nd::valcounts = \&ccs_nd_valcounts; }
sub ccs_nd_valcounts {
  my $ccs = shift;
  my ($v,$vc)  = $ccs->_vals->valcounts;
  my $vmissing = $ccs->missing;
  my $nmissing = $ccs->nmissing;
  my $imissing = $vmissing->vsearch($v);
  ##
  ##-- maybe bash to double
  if ($nmissing > $INT_MAX) {
    $vc = $vc->float; ##-- bash to float in case of datatype overflow
  }
  $vc->index($imissing) .= $nmissing;
  return ($v,$vc);
}

## ($values,$valprobs) = $pdl->valprobs()  ##-- array context, suitable for PDL::Primitive::interpol(ate)
##  + as for valcounts(), but returns value-probabilities
BEGIN { *PDL::valprobs = *PDL::CCS::Nd::valprobs = \&valprobs; }
sub valprobs {
  my ($pdl,%opts) = @_;
  my ($v,$vc) = $pdl->valcounts(%opts);
  $vc = $vc->double;
  return ($v, $vc/$vc->sumover->dummy(0,1));
}

##======================================================================
## bin-sorting

## ($binids,$binub,$binfit) = logbins($data,$eps=1,$min=undef,$max=undef,$step=undef)   ##-- list context
## $binids                  = logbins($data,$eps=1,$min=undef,$max=undef,$step=undef)   ##-- scalar context
##   + wrapper for loghist() which maps input data to bin indices:
##      $binub = loghist($
##      all( $binids == $data->vsearch($binub) )
##      all( $binfit == $binub->index($binids) )
BEGIN { *PDL::logbins = \&logbins; }
sub logbins {
  my $data = shift;
  my ($binub,$binhist) = loghist($data,@_);
  my $binids = $data->vsearch($binub);
  return wantarray ? ($binids,$binub,$binub->index($binids)) : $binids;
}

## $binubs = makebins($data,%opts)
##  + %opts:
##     min => minimum bin ub (default=$data->min)
##     max => maximum bin ub (default=$data->max)
##     n   => n bins (default=100)
BEGIN { *PDL::makebins = \&makebins; }
sub makebins {
  my ($data,%opts) = @_;
  my $min = defined($opts{min}) ? $opts{min} : $data->minimum;
  my $max = defined($opts{max}) ? $opts{max} : $data->maximum;
  my $n   = defined($opts{n})   ? $opts{n}   : pdl(double,100.0);
  my $binsize = ($max-$min)/$n;
  return (ones($n)*$binsize)->cumusumover + $min;
}

## $binubs = makebins_exp($data,%opts)
##  + like makebins(), but produces exponentially sized bins
##  + %opts:
##     min => minimum bin ub (default=$data->min)
##     max => maximum bin ub (default=$data->max)
##     eps => small value added to ($max,$min) before computing bin sizes (default=0)
##     n   => n bins (default=100)
BEGIN { *PDL::makebins_exp = \&makebins_exp; }
sub makebins_exp {
  my ($data,%opts) = @_;
  $data = null if (!defined($data));
  my $eps = defined($opts{eps}) ? $opts{eps} : pdl(double,0);
  my $min = defined($opts{min}) ? $opts{min} : $data->min+$eps;
  my $max = defined($opts{max}) ? $opts{max} : $data->max+$eps;
  my $n   = defined($opts{n})   ? $opts{n}   : pdl(double,100.0);
  my $lbinsize = (log($max)-log($min))/$n;
  return ((ones($n)*$lbinsize)->cumusumover + log($min))->exp;
}

## $binids = findbins($data, $binubs)
##  + maps $data to bin-ids
##  + really just a wrapper for $data->vsearch($binubs)
BEGIN { *PDL::findbins = \&findbins; }
sub findbins {
  my ($data,$binubs) = @_;
  return $data->vsearch($binubs);
}

## $avg        = binavg($data,$binids,%opts)
## ($avg,$fit) = binavg($data,$binids,%opts)
##  + %opts:
##     missing=>$val : set missing values to $val (default=0)
sub binavg {
  my ($data,$binids,%opts) = @_;
  my $missing = defined($opts{missing}) ? $opts{missing} : 0;
  my $uids = $binids->flat->qsort->uniq;
  my $avg = zeroes($binids->max+1);
  $avg   .= $missing;
  foreach my $id ($uids->list) {
    my $data_i = $data->where($binids==$id);
    next if ($data_i->nelem == 0);
    my $avg_i  = $data_i->flat->average;
    (my $tmp=$avg->slice("($id)")) .= $avg_i;
  }
  return wantarray ? ($avg,$avg->index($binids)) : $avg;
}

## $sd        = binsd($data,$binids,%opts)
## ($sd,$fit) = binsd($data,$binids,%opts)
##  + %opts:
##     missing=>$val  : set missing values to $val (default=0)
##     amissing=>$val : set missing average values to $val (default=0)
sub binsd {
  my ($data,$binids,%opts) = @_;
  my $missing  = defined($opts{missing}) ? $opts{missing} : 0;
  my $uids = $binids->flat->qsort->uniq;
  my $avg = binavg($data,$binids, %opts,missing=>$opts{amissing});
  my $sd  = $avg->zeroes + $missing;
  foreach my $id ($uids->list) {
    my $data_i = $data->where($binids==$id);
    next if ($data_i->nelem == 0);
    my $sd_i   = (($data_i->flat-$avg->slice("($id)"))**2)->average->sqrt;
    (my $tmp=$sd->slice("($id)")) .= $sd_i;
  }
  return wantarray ? ($sd,$sd->index($binids)) : $sd;
}


##======================================================================
## Value Smearing (GT-style)

## ($v_smeared) = $vals->smearvals();
## ($v_smeared) = $vals->smearvals($keys);
## + as for MUDL::Dist::smear() (as used by MUDL::Dist::smoothGTLogLin())
BEGIN { *PDL::smearvals = \&smearvals; }
sub smearvals {
  my ($vals,$r) = @_;

  my ($r_qsi,$Nr);
  if (!defined($r)) {
    $r     = $vals->xvals;
    $r_qsi = $r->sequence;
    $Nr    = $vals;
  } else {
    $r_qsi = $r->qsorti;
    $r     = $r->index($r_qsi);
    $Nr    = $vals->index($r_qsi);
  }
  my $r_lo = $r->slice("0:-2")->append(0)->rotate(1);
  my $r_hi = $r->rotate(-1)->slice("0:-2")->append(0);
  $r_hi->slice("-1") .= 2*$r->slice("-1") - $r->slice("-2");

  #my $Zr  = 2*$Nr->double / ($r_hi-$r_lo);
  ##--
  my $Zrv = $vals->zeroes->double;
  $Zrv->index($r_qsi) .= 2*$Nr / ($r_hi-$r_lo);

  return $Zrv;
}

##======================================================================
## Zipf fit

## ($zipf_constant,$freq_fit) = zipf_fit($freq_pdl)             ##-- array context
## ($zipf_constant,$freq_fit) = zipf_fit($freq_pdl,$rank_pdl)
## $zipf_constant             = zipf_fit($freq_pdl)             ##-- scalar context
## $zipf_constant             = zipf_fit($freq_pdl,$rank_pdl)
## + fits $freq_pdl to best Zipfian distribution (linear)
##     $freq = $zipf_constant / $rank_desc
## + $rank_pdl is optional; if specified it should be a descending-order rank-sort of $freq_pdl, as
##     $ranks = $freq_pdl->ranks(order=>'desc')+1;
## + this looks ok for unigrams, craps out for bigrams
BEGIN { *PDL::zipf_fit = \&zipf_fit; }
sub zipf_fit {
  my ($freq,$f_ranks) = @_;
  $f_ranks    = $freq->ranks(order=>'desc')+1 if (!defined($f_ranks) || $f_ranks->isempty);
  my $total   = ($freq*$f_ranks)->sumover;
  my $nitems  = pdl(double,$freq->nelem);
  my $zipf_c  = $total / $nitems;
  return $zipf_c if (!wantarray);
  my $freq_fit = $zipf_c / $f_ranks;
  return ($zipf_c,$freq_fit);
}

## ($zipf_constant,$freq_fit) = zipf_fit_lm1($freq_pdl,%opts)         ##-- array context
## $zipf_constant             = zipf_fit_lm1($freq_pdl,%opts)         ##-- scalar context
##  + uses PDL::Fit::LM to fit a 1-parameter Zipf distribution from $rank_pdl to $freq_pdl
##     $freq = $zipf_constant / $rank_desc
##  + %opts:
##     ranks => $rank_pdl,     ##-- default = $freq_pdl->ranks1_dsc()
##     weight => $weight_pdl,  ##-- default: uniform (for LM algorithm)
##     $lmOpt => $lmVal,       ##-- passed to PDL::Fit::LM
BEGIN { *PDL::zipf_fit_lm1 = *PDL::zipf_fit_lm = *zipf_fit_lm = \&zipf_fit_lm1; }
sub zipf_fit_lm1 {
  my ($freq,%opts) = @_;

  ##-- MakeMaker tests choke on external 'use PDL::Fit::LM'
  require PDL::Fit::LM;
  PDL::Fit::LM->import();

  ##-- get ranks
  my $ranks = $opts{ranks};
  $ranks = $freq->ranks(order=>'desc')+1 if (!defined($ranks) || $ranks->isempty);
  delete($opts{ranks});

  ##-- get weights
  my $weight = $opts{weight};
  $weight = 1.0 if (!defined($weight) || $weight->isempty);
  delete($opts{weight});

  ##-- fit
  my ($lmfit,$lmpar,@lmrest) = lmfit($ranks,$freq, $weight, \&_zipf_fit_lm1_sub, pdl(10.0), Maxiter=>1000,Eps=>1e-5,%opts);
  return wantarray ? ($lmpar,$lmfit) : $lmpar;
}

## _zipf_fit_lm1_sub($ranks,$zipf_k,$freq_est,$dyda)
##  + low-level fitting sub for 1-parameter Zipf fitting
sub _zipf_fit_lm1_sub {
    my ($x,$par,$ym,$dyda) = @_;

    ##-- get fit parameter(s)
    my $k = $par->slice("(0)");

    ##-- compute function value for these parameters
    my $xi = $x->pow(-1.0);
    $ym .= $k * $xi;

    ##-- get partial derivative output pdls
    my (@dy) = map {$dyda -> slice(",($_)") } (0..0);

    ##-- compute partial derivatives
    $dy[0] .= $xi;
}

## ($zipf_pars,$freq_fit) = zipf_fit_lm2($freq_pdl,%opts)         ##-- array context
## $zipf_pars             = zipf_fit_lm2($freq_pdl,%opts)         ##-- scalar context
##  + uses PDL::Fit::LM to fit a 2-parameter pseudo-Zipf distribution from $rank_pdl to $freq_pdl
##     $freq      = $a * $rank_desc**$b + $c
##     $zipf_pars = pdl [$a,$b,$c];
##  + analagous to:
##     ($freq_fit,$zipf_pars) = $freq_pdl->loglinfit($ranks)
##    ... but uses direct nonlinear fitting rather than log-linear,
##    which may reduce error rates for large values in $freq_pdl
##  + %opts:
##     ranks => $rank_pdl,   ##-- default = $freq_pdl->ranks1_dsc()
##     weight => $weight_pd, ##-- relative weights for LM fitting algorithm (default=uniform)
##     $lmOpt => $lmVal,    ##-- passed to PDL::Fit::LM
BEGIN { *PDL::zipf_fit_lm2 = \&zipf_fit_lm2; }
sub zipf_fit_lm2 {
  my ($freq,%opts) = @_;

  ##-- get ranks
  my $ranks = $opts{ranks};
  $ranks = $freq->ranks(order=>'desc')+1 if (!defined($ranks) || $ranks->isempty);
  delete($opts{ranks});

  my @fit = expfit($freq,$ranks,%opts); ## ($fit,$par,$covar,$iters)=@fit;
  return wantarray ? @fit[1,0] : $fit[1];
}

##======================================================================
## Linear fitting, generic

## $yfit = $y->linfit($x=cat($fit->xvals+1,$fit->ones), %opts)  ##-- scalar context
## %fit  = $y->linfit($x=cat($fit->xvals+1,$fit->ones), %opts)  ##-- list context
##  + pseudo-signature: (y(n); x(n,nx); [o]fit(n); [o]coeffs(nx); %opts)
##  + $x can also be specified as an ARRAY: ($x0,$x1,...,$xn) to set $x=cat($x0,$x1,...,$xn)
##  + $yfit(i) is fitted value for $y(i) as a linear function of $x(i,)
##  + in hash mode, outputs %opts plus:
##     yfit => $yfit,      ##-- $yfit(n)   : as scalar return value
##     coeffs => $coeffs,  ##-- $coeffs(nx): s.t. all($yfit == $coeffs(0)*$x(,(0)) + $coeffs(1)*$x(,(1)) + ... + $coeffs(nx-1)*$x(,(nx-1))
##  + %opts:
##     uniq   => $bool,  ##-- if true, only vector-unique points are fitted
##     logy   => $bool,  ##-- if true, $y will be log-transformed before fit
##     logx   => $mask,  ##-- $mask(nx) of x-indices to be log-transformed before fit
##     epsy   => $eps,   ##-- scalar added to $y() before fit (e.g. for logy=>1)
##     epsx   => $eps,   ##-- $epsx(nx) of eps-values to add to $x() (e.g. for logx)
##     nbins  => $n,     ##-- number of y-bins with which to quantize fit (default=0: use all values)
BEGIN { *PDL::linfit = \&linfit; }
sub linfit {
  my ($y0,$x0,%opts) = @_;

  ##-- MakeMaker tests choke on PDL::Fit::Linfit:
  ## Can't load '/usr/lib/perl5/auto/PDL/Slatec/Slatec.so' for module PDL::Slatec: /usr/lib/perl5/auto/PDL/Slatec/Slatec.so: undefined symbol: _gfortran_concat_string at /usr/lib/perl/5.10/DynaLoader.pm line 196.
  require PDL::Fit::Linfit;
  PDL::Fit::Linfit->import();

  ##-- get x values
  my ($x);
  if (!defined($x0)) {
    $x = cat(($y0->xvals+1)->double,$y0->ones);
  } elsif (!UNIVERSAL::isa($x0,'PDL') && UNIVERSAL::isa($x0,'ARRAY')) {
    $x = cat(@$x0)->double;
  } else {
    $x = pdl($x0)->double; ##-- copy
  }

  ##-- epsy
  my ($y,$epsy,$logy);
  $y  = pdl($y0)->double; ##-- copy
  $y += $opts{epsy} if (defined($epsy=$opts{epsy}));

  ##-- logy
  $y->inplace->log if ($logy=$opts{logy});

  ##-- epsx
  my $epsx = defined($opts{epsx}) ? pdl($opts{epsx}) : zeroes(long,$x->dim($x->dim(1)));
  $x += $epsx->slice("*1,");
  $x->inplace->setnantobad->inplace->setbadtoval(0);

  ##-- logx
  my ($logx);
  if (defined($opts{logx})) {
    $logx = UNIVERSAL::isa($opts{logx},'PDL') || UNIVERSAL::isa($opts{logx},'ARRAY') ? pdl($opts{logx}) : ones(long,$x->dim(1));
    $x->dice_axis(1,$logx->which)->inplace->log() if ($logx->any);
  }

  ##-- bins
  my $nbins = $opts{nbins} || 0;
  my ($ybb,$ybi,$yba,$xba);
  if ($nbins) {
    $ybb = makebins($y,n=>$opts{biny});
    $ybi = findbins($y,$ybb);
    $yba = binavg($y,$ybi);
    $xba = binavg($x,$ybi->slice(",*1"));
  } else {
    ($xba,$yba) = ($x,$y);
  }

  ##-- fit temporaries
  my ($yfit,$coeffs);

  ##-- uniq
  my $uniq = $opts{uniq} || $opts{unique};
  if ($uniq) {
    my $xyv  = $xba->glue(1,$yba)->xchg(0,1);
    my $xyvi  = $xyv->vv_qsortveci;
    my $xyvs  = $xyv->dice_axis(1,$xyvi);
    my ($xyvc,$xyvv) = $xyvs->rleND;
    my $xyui = $xyvv->dice_axis(1,which($xyvc>0))->vsearchvec($xyvs);
    my $xu   = $opts{xu} = $xyv->dice_axis(1,$xyui)->xchg(0,1)->slice(",0:-2");
    my $yu   = $opts{yu} = $xyv->dice_axis(1,$xyui)->xchg(0,1)->slice(",(-1)");

    ##-- fit: unique
    my ($ufit,$ucoeffs) = @opts{qw(ufit ucoeffs)} = $yu->linfit1d($xu);
    $coeffs = $ucoeffs;

    ##-- back-translate: unique -> full
    $yfit = $yba->zeroes;
    (my $tmp=$yfit->index($xyvi)) .= $ufit->index($xyvs->vsearchvec($xyvv->dice_axis(1,$xyui)));
  }
  else {
    ##-- fit: non-unique
    ($yfit,$coeffs) = $yba->linfit1d($xba);
  }

  ##-- back-translate: bins
  if ($nbins) {
    my $yfitb = $opts{yfitb} = $yfit;
    $yfit     = $y0->zeroes;
    $yfit->index($ybi) .= $yfitb;
  }

  ##-- back-translate: logx,epsx
  if (defined($logx) && $logx->any) {
    $x->dice_axis(1,$logx->which)->inplace->exp();
  }
  if (defined($epsx) && any($epsx)) {
    $x -= $epsx->slice("*1,");
  }

  ##-- back-translate: logy,epsy
  if ($logy) {
    $yfit->inplace->exp;
    $y->inplace->exp;
  }
  if ($epsy) {
    $yfit -= $epsy;
    $y    -= $epsy;
  }

  ##-- return: scalar
  return $yfit if (!wantarray);

  ##-- return: hash
  @opts{qw(logy epsy logx epsx)} = ($logy,$epsy,$logx,$epsx);
  @opts{qw(uniq)} = ($uniq);
  @opts{qw(x y x0 y0 yfit coeffs)} = ($x,$y,$x0,$y0,$yfit,$coeffs);
  $opts{nbins} = $nbins;
  if ($nbins) {
    @opts{qw(ybb ybi yba xba)} = ($ybb,$ybi,$yba,$xba);
  }
  return %opts;
}

##======================================================================
## 3d fitting

## %bin2d = $z->bin2d($x,$y,%opts); ##-- list context
## $image = $z->bin2d($x,$y,%opts); ##-- scalar context
##  + pseudo-signature:
##     ( z(n); x(n); y(n); [o]image(nx,ny); %opts )
##
##  + %opts
##     nx => $nx,	 ##-- number of x-bins (default=$x->uniq->nelem)
##     logx => $bool,	 ##-- bin x-values exponentially?
##     epsx => $eps,	 ##-- add to $x before exp-binning?
##     missingx => $val, ##-- missing value for x averaging (default=0)
##
##     ny => $ny,	 ##-- number of y-bins (default=$y->uniq->nelem)
##     logy => $bool,	 ##-- bin y-values exponentially?
##     epsy => $eps,	 ##-- add to $y before exp-binning?
##     missingy => $val, ##-- missing value for x averaging (default=0)
##
##     missingz => $val, ##-- missing value for z averaging (default=0)
##
##  + output %bin2d: %opts plus:
##     image => $imag,	##-- $imag(nx,ny): ($xi,$yi)=>$z
##
##     x0  => $x0,	##-- $x0(n)   : input pdl $x(n)
##     xbi => $xbi,	##-- $xbi(n)  : bin indices for arg $x(n) (0<=$xbi()<=nx)
##     xbb => $xbb,	##-- $xbb(nx) : bin upper-bounds
##     xba => $xba,     ##-- $xba(nx) : bin-wise averages for $x(n)
##     x   => $x,       ##-- $x(nx)   : alias for $xba
##
##     y0  => $y0,	##-- $y0(n)   : input pdl $y(n)
##     ybi => $ybi,	##-- $ybi(n)  : bin indices for arg $y(n) (0<=$ybi()<=ny)
##     ybb => $ybb,	##-- $ybb(ny) : bin upper-bounds
##     yba => $yba,     ##-- $yba(ny) : bin-wise averages for $y(n)
##     y   => $y,       ##-- $y(ny)   : alias for $yba
##
##     zwhich => $zwhich, ##-- $zwhich(nnz,2): whichND-style pdl for non-missing (x-bin,y-bin) index pairs
##     zvals  => $zvals,  ##-- $zvals(nnz)   : non-missing z values a la PDL::CCS::Nd
##     znnz   => $znnz,   ##-- $znnz(nx,ny)  : number of non-missing values per bin
##
BEGIN { *PDL::bin2d = \&bin2d; }
sub bin2d {
  my ($z0,$x0,$y0,%opts) = @_;
  my ($x,$y,$z) = @opts{qw(x0 y0 z0)} = ($x0,$y0,$z0);

  ##-- bins: x
  if ($opts{nx}) {
    $opts{xbb} = ($opts{logx}
		  ? makebins_exp($x,eps=>$opts{epsx},n=>$opts{nx})
		  : makebins($x,n=>$opts{nx}));
    $opts{xbi} = findbins($x,$opts{xbb});
    $opts{xba} = binavg($x,$opts{xbi},missing=>$opts{missingx});
  } else {
    $opts{xbb} = $x->uniq->qsort;
    $opts{xbi} = findbins($x,$opts{xbb});
    $opts{xba} = $opts{xbb};
    $opts{nx} = $opts{xbb}->nelem;
  }

  ##-- bins: y
  if ($opts{ny}) {
    $opts{ybb} = ($opts{logy}
		  ? makebins_exp($y,eps=>$opts{epsy},n=>$opts{ny})
		  : makebins($y,n=>$opts{ny}));
    $opts{ybi} = findbins($y,$opts{ybb});
    $opts{yba} = binavg($y,$opts{ybi},missing=>$opts{missingy});
  } else {
    $opts{ybb} = $y->uniq->qsort;
    $opts{ybi} = findbins($y,$opts{ybb});
    $opts{yba} = $opts{ybb};
    $opts{ny}  = $opts{ybb}->nelem;
  }

  ##-- bin-vectors
  my $missingz = $opts{missingz};
  $missingz = 0 if (!defined($missingz));
  $missingz = $missingz->sclr if (UNIVERSAL::isa($missingz,'PDL'));
  my $xybi  = cat(@opts{qw(xbi ybi)})->xchg(0,1);
  my $xybii = $xybi->vv_qsortveci;
  my $xybis = $xybi->dice_axis(1,$xybii);
  my $zs    = $z->index($xybii);
  my ($zwhich,$zavg) = ccs_accum_dsum($xybis,$zs, $missingz, 0);
  my ($nzwhich,$nnz) = ccs_accum_nnz($xybis, $zs, $missingz, 0);
  $zavg /= $nnz->double;
  $zavg->inplace->setnantobad->inplace->setbadtoval($missingz) if (defined($opts{missingz}));
  my $image = ccs_decode($zwhich,$zavg,$missingz,[@opts{qw(nx ny)}]);
  return $image if (!wantarray);

  ##-- output full hash
  @opts{qw(x y)} = @opts{qw(xba yba)};
  $opts{image} = $image;
  $opts{zwhich} = $zwhich;
  $opts{znnz}   = ccs_decode($zwhich,$nnz,0,[@opts{qw(nx ny)}]);

  return %opts;
}



##======================================================================
## Linear fitting, old

## ($fit,$coeffs) = $vals->mooLinfit()
## ($fit,$coeffs) = $vals->mooLinfit($keys)
## ($fit,$coeffs) = $vals->mooLinfit($keys, %opts)
##  + $keys defaults to $vals->xvals()+1
##  + $fit are linear-fitted values $vals as values for $keys
##  + $coeffs are [$a,$b] such that all($yfit == $a*$keys + $b)
##  + %opts:
##     unique => $bool, ##-- if true, only unique points are fitted
BEGIN { *PDL::mooLinfit = \&mooLinfit; }
sub mooLinfit {
  my ($y,$x,%opts) = @_;

  ##-- MakeMaker tests choke on PDL::Fit::Linfit:
  ## Can't load '/usr/lib/perl5/auto/PDL/Slatec/Slatec.so' for module PDL::Slatec: /usr/lib/perl5/auto/PDL/Slatec/Slatec.so: undefined symbol: _gfortran_concat_string at /usr/lib/perl/5.10/DynaLoader.pm line 196.
  require PDL::Fit::Linfit;
  PDL::Fit::Linfit->import();

  $x = ($y->xvals+1)->double if (!defined($x));
  if (!$opts{unique}) {
    my ($yfit,$coeffs) = $y->linfit1d($y->ones->cat($x->setnantobad->setbadtoval(0)));
    return ($yfit,$coeffs->slice("-1:0"));
  }
  ##-- unique values
  my ($xu,$yu) = $x->cat($y)->xchg(0,1)->vv_uniqvec->xchg(0,1)->dog;
  my ($yufit,$coeffs) = mooLinfit($yu,$xu,%opts,unique=>0);
  my $yfit = $coeffs->slice("(0)")*$x + $coeffs->slice("(1)");
  #my $yfit = $x->interpol( $xu, $yufit ); ##-- causes error: "identical abscissas at /usr/lib/perl5/PDL/Primitive.pm line 1740."
  return ($yfit,$coeffs);
}

##======================================================================
## Log-linear fit, old

## ($fit,$coeffs) = $vals->loglinfit()
## ($fit,$coeffs) = $vals->loglinfit($keys,%opts)
##  + $keys defaults to $vals->xvals()+1
##  + $fit are log-linear fitted values $vals as values for $keys
##  + $coeffs are [$a,$e] such that all($yfit == $a*($keys**$e))
##  + %opts:
##     nologx => $bool,  ##-- if true, $keys are not implicitly log()d
##     nology => $bool,  ##-- if true, $vals are not implicitly log()d
##     unique => $bool,  ##-- if true, only unique points are used for fitting
##  + example:
##     use MUDL::PDL::Smooth;
##     $f1 = corpus_unigram_frequencies();
##     ($v,$c) = $f1->valcounts();
##     $Zc     = $c->smearvals($v);
##     ($fitc,$coeffs) = $Zc->loglinfit($v);
##
##     use PDL::Graphics::PGPLOT;
##     autolog(1);
##     %plot = (axis=>'logxy',xtitle=>'freq',ytitle=>'count(freq)',xrange=>[1,$f1->sum],yrange=>[$Zc->min,$c->max]);
##     points($v, $c, 4,{%plot}); hold;
##     points($v, $Zc,2,{%plot,color=>'red'}); hold;
##     line($v, $coeffs->index(0)*($v**$coeffs->index(1)), {%plot,color=>'blue'});
##     release;
BEGIN { *PDL::loglinfit = \&loglinfit; }
sub loglinfit {
  my ($y,$x,%opts) = @_;
  $x = ($y->xvals+1)->double if (!defined($x));
  $x = $x->log if (!$opts{nologx});
  $y = $y->log if (!$opts{nology});
  my ($yfit,$coeffs) = mooLinfit($y,$x,%opts); #$y->linfit1d($y->ones->cat($x->setnantobad->setbadtoval(0)));
  $yfit->inplace->exp if (!$opts{nology});
  $coeffs = $coeffs->slice("-1:0");
  $coeffs->slice("(0)")->inplace->exp;
  return ($yfit,$coeffs);
}

##======================================================================
## Native exponential fit

## ($fit,$coeffs,$covar,$iters) = $vals->expfit_ab()
## ($fit,$coeffs,$covar,$iters) = $vals->expfit_ab($keys, %opts)
## $coeffs = $vals->expfit(...)
##  + $keys defaults to $vals->xvals()+1
##  + $fit are log-linear fitted values $vals as values for $keys
##  + $coeffs are [$a,$b] such that
##     $yfit = $a * $x**$b
##  + %opts are passed to PDL::Fit::LM::lmfit(), except for:
##     initp => $initial_param_pdl,
##     weight => $weight_pdl
##  + return values are as for loglinfit(), but uses LM fitting internally
BEGIN { *PDL::expfit_ab = *PDL::expfit = *expfit = \&expfit_ab; }
sub expfit_ab {
  my ($y,$x,%opts) = @_;

  ##-- MakeMaker builds choke on PDL::Fit::LM:
  ## Can't load '/usr/lib/perl5/auto/PDL/Slatec/Slatec.so' for module PDL::Slatec: /usr/lib/perl5/auto/PDL/Slatec/Slatec.so: undefined symbol: _gfortran_concat_string at /usr/lib/perl/5.10/DynaLoader.pm line 196.
  require PDL::Fit::LM;
  PDL::Fit::LM->import();

  $x = $y->xvals+1 if (!defined($x) || $x->isempty);

  ##-- initial parameters
  my $initp = defined($opts{initp}) ? $opts{initp} : pdl(10,-1);
  delete($opts{initp});

  ##-- weights
  my $weight = $opts{weight};
  $weight = 1.0 if (!defined($weight) || $weight->isempty);
  delete($opts{weight});

  my @fit = lmfit($x,$y, $weight, \&_expfit_ab_lm_sub, $initp, {Maxiter=>1000,Eps=>1e-5,%opts});
  #my ($lmfit,$lmpar,$lmcovar,$lmiters) = @fit;
  return wantarray ? @fit : $fit[1];
}

## _expfit_ab_lm_sub($x,$par,$y,$dyda)
##  + low-level fitting sub for exponential fitting
sub _expfit_ab_lm_sub {
    my ($x,$par,$ym,$dyda) = @_;
    my $maxpar = 1;

    ##-- get fit parameter(s)
    my ($a,$b) = map {$par->slice("($_)")} (0..$maxpar);

    ##-- compute function value for these parameters
    my $xb = $x**$b;
    $ym .= $a * $xb;

    ##-- get partial derivative output pdls
    my (@dy) = map {$dyda -> slice(",($_)") } (0..$maxpar);

    ##-- compute partial derivatives
    $dy[0] .= $xb;
    $dy[1] .= $a*$xb*log($x);
}


## ($fit,$coeffs,$covar,$iters) = $vals->expfit_abc()
## ($fit,$coeffs,$covar,$iters) = $vals->expfit_abc($keys, %opts)
## $coeffs = $vals->expfit_abc(...)
##  + as for expfit_ab(), but fits $coeffs = [$a,$b,$c] as:
##     $yfit = $a * $x**$b + $c;
BEGIN { *PDL::expfit_abc = \&expfit_abc; }
sub expfit_abc {
  my ($y,$x,%opts) = @_;
  $x = $y->xvals+1 if (!defined($x) || $x->isempty);

  ##-- initial params
  my $initp = defined($opts{initp}) ? $opts{initp} : pdl(10,-1,0);
  delete($opts{initp});

  ##-- weights
  my $weight = $opts{weight};
  $weight = 1.0 if (!defined($weight) || $weight->isempty);
  delete($opts{weight});

  ##-- fit
  my @fit = lmfit($x,$y, 1.0, \&_expfit_abc_lm_sub, $initp, {Maxiter=>1000,Eps=>1e-5,%opts});
  #my ($lmfit,$lmpar,$lmcovar,$lmiters) = @fit;
  return wantarray ? @fit : $fit[1];
}

## _expfit_abc_lm_sub($x,$par,$y,$dyda)
##  + low-level fitting sub for exponential fitting
sub _expfit_abc_lm_sub {
    my ($x,$par,$ym,$dyda) = @_;
    my $maxpar = 2;

    ##-- get fit parameter(s)
    my ($a,$b,$c) = map {$par->slice("($_)")} (0..$maxpar);

    ##-- compute function value for these parameters
    my $xb = $x**$b;
    $ym .= $a * $xb + $c;

    ##-- get partial derivative output pdls
    my (@dy) = map {$dyda -> slice(",($_)") } (0..$maxpar);

    ##-- compute partial derivatives
    $dy[0] .= $xb;
    $dy[1] .= $a*$xb*log($x);
    $dy[2] .= 1;
}


##======================================================================
## Good-Turing smoothing

## ($valfit_pdl,$coeffs_pdl,$zmass) = $pdl->smoothGTLogLin()
## ($valfit_pdl,$coeffs_pdl,$zmass) = $pdl->smoothGTLogLin($minval=1)
##  + $minval is the minimum (1) value for GT-smoothing (default=1)
##  + in scalar context, returns only $valfit_pdl
BEGIN { *PDL::smoothGTLogLin = \&smoothGTLogLin; }
sub smoothGTLogLin {
  my ($pdl,$vmin) = @_;
  $vmin = 1 if (!defined($vmin));

  my ($v,$c) = $pdl->valcounts();
  my $Zc     = $c->double->smearvals($v);
  my ($fitc,$coeffs) = $Zc->loglinfit($v);
  my $S_a = $coeffs->index(0);
  my $S_e = $coeffs->index(1);

  my $p_fit  = zeroes(double,$pdl->dims);
  my $p_mask = ($pdl>=$vmin);
  my $p_srcv = $pdl->where($p_mask);
  my $p_fitv = $p_fit->where($p_mask);
  $p_fitv   .= ($p_srcv+1)*($S_a*($p_srcv+1)**$S_e) / ($S_a * $p_srcv**$S_e);

  my $N1    = $c->where($v<=$vmin)->sumover->double;
  my $zmass = $N1/$c->flat->sumover->double;

  my $z_mask = $p_mask->not;
  $p_fit->where($z_mask) .= $zmass / $z_mask->which->nelem if ($z_mask->any);

  return wantarray ? ($p_fit,$coeffs,$zmass) : $p_fit;
}

##======================================================================
## Gaussian fitting

##--------------------------------------------------------------
## $yfit                                      = smoothGaussian($ydata, $xvals)
## ($yfit,$yfit_peak,$yfit_mean,$yfit_stddev) = smoothGaussian($ydata, $xvals)
##  + best-fit Gaussian
##  + $xvals are independent indices for $ydata; default=$ydata->avgranks()
##  + $yfit should be re-computable by Gaussian function with params:
##       a=$yfit_peak
##       b=$yfit_mean
##       c=$yfit_stddev
##    e.g.:
##       $yfit2 = $yfit_peak * exp( ($xvals-$yfit_mean)**2 / (2*$yfit_stddev**2) );
BEGIN { *PDL::smoothGaussian = *PDL::smoothNormal = *smoothNormal = \&smoothGaussian; }
sub smoothGaussian {
  my ($ydata,$xvals) = @_;
  require PDL::Fit::Gaussian;

  $xvals = $ydata->avgranks if (!defined($xvals));
  my ($xmean,$ypeak,$fwhm, $back,$err,$yfit) = $xvals->fitgauss1d($ydata);

  return $yfit if (!wantarray);

  ##-- get mean & stddev
  # $fwhm  = 2*sqrt(2*log(2))*$sigma
  # $sigma = $fwhm / (2*sqrt(2*log(2)))
  my $yfit_stddev = $fwhm / (2*sqrt(2*log(2)));
  my $yfit_mean   = $xmean;
  #my $yfit_stddev = $yfit->stddev;
  #my $yfit_mean   = $yfit->average;

  return ($yfit,$ypeak,$yfit_mean,$yfit_stddev);
}

##--------------------------------------------------------------
## $yvals = gaussyvals($xvals, $peak,$mu,$sigma);
##  + generalized yvals for gaussian functions, incl. pdf
BEGIN { *PDL::gaussyvals = \&gaussyvals; }
sub gaussyvals {
  my ($x, $peak,$mu,$sigma) = @_;
  our ($pi);
  $sigma = 1 if (!defined($sigma));
  $mu    = 0 if (!defined($mu));
  $peak  = 1/($sigma*sqrt(2*$pi)) if (!defined($peak)); ##-- for pdf
  my $y = $peak * exp( -($x-$mu)**2 / (2*$sigma**2) );
  return $y;
}

## $ypeak = gausspeak($sigma)
##  + y peak of gaussian pdf
##  + should be equiv to gausspdf($mu,$mu,$sigma) == gausspdf(0,0,$sigma)
BEGIN { *PDL::gausspeak = \&gausspeak; }
sub gausspeak {
  #return gausspdf(0,0,@_);
  my $sigma = shift;
  our ($pi);
  return 1/($sigma*sqrt(2*$pi));
}

## $pvals = gausspdf($xvals, $mu,$sigma);
BEGIN { *PDL::gausspdf = \&gausspdf; }
sub gausspdf { return gaussyvals($_[0], undef,@_[1,2]); }

##--------------------------------------------------------------
## $pvals = gausscdf($xvals, $mu,$sigma);
BEGIN { *PDL::gausscdf = \&gausscdf; }
sub gausscdf {
  my ($x,$mu,$sigma) = @_;
  $sigma = 1 if (!defined($sigma));
  $mu    = 0 if (!defined($mu));
  return 0.5*(1 + erf( ($x-$mu) / ($sigma*sqrt(2)) ));
}


##--------------------------------------------------------------
## $width = gausswidth($confidence, $mu,$sigma);                  ##-- scalar context
## ($mu-$width,$mu+$width) = gausswidth($confidence, $mu,$sigma); ##-- array context
##  + In scalar context, returns width around mean corresponding to confidence level $confidence
##  + In array context, returns interval around mean for level $confidence.
BEGIN { *PDL::gausswidth = \&gausswidth; }
sub gausswidth {
  my ($conf,$mu,$sigma) = @_;
  $sigma = 1 if (!defined($sigma));
  $mu    = 0 if (!defined($mu));
  #my $w  = abs(erfi($conf) * sqrt(2) * $sigma);
  my $w  = erfi($conf) * sqrt(2) * $sigma;
  return wantarray ? ($mu-$w,$mu+$w) : $w;
}


##--------------------------------------------------------------
## $qvals = gaussquantiles($pvals, $mu,$sigma);
##  + quantile function for Gaussian distribution, aka cdf^{-1}
BEGIN { *PDL::gaussquantiles = *PDL::gaussqvals = *gaussqvals = *gausscdfi = \&gaussquantiles; }
sub gaussquantiles {
  my ($p,$mu,$sigma) = @_;
  $sigma = 1 if (!defined($sigma));
  $mu    = 0 if (!defined($mu));
  return $mu + $sigma * sqrt(2) * erfi(2*$p-1);
}


##--------------------------------------------------------------
## $centers       = intervals($min,$max,$n) ##-- scalar context
## ($ctr,$lo,$hi) = intervals($min,$max,$n) ##-- list context
##  + returns $n equally-spaced values between $min and $max
sub intervals {
  my ($min,$max,$n) = @_;
  $n   = 100 if (!defined($n));
  $max = $min+1 if (!defined($max));
  $min = $max-1 if (!defined($min));
  my $ctrs = $min + (sequence($n)+0.5)/$n * ($max-$min);
  return $ctrs if (!wantarray);

  my $lo = $ctrs->append($min)->rotate(1)->slice("0:".($n-1));
  my $hi = $ctrs->append($max)->slice("0:".($n-1));
  return ($ctrs,$lo,$hi);
}

##--------------------------------------------------------------
## ($x,$y) = gausspoints($peak,$mu,$sigma, $xmin,$xmax,$nx);
##  + generate points of a gaussian curve
##  + defaults:
##     $sigma = 1
##     $mu    = 0
##     $peak  = 1/($sigma*sqrt(2*$pi)) ##-- e.g. pdf of normal distribution
##     $xmin  = $mu - 2*$sigma
##     $xmax  = $mu + 2*$sigma
##     $nx    = 100
BEGIN { our $pi = 3.14195; }
sub gausspoints {
  my ($peak,$mu,$sigma, $xmin,$xmax,$nx) = @_;
  our ($pi);
  $sigma = 1 if (!defined($sigma));
  $mu    = 0 if (!defined($mu));
  $peak  = 1/($sigma*sqrt(2*$pi)) if (!defined($peak)); ##-- e.g. pdf of normal distribution
  $xmin  = $mu - 2*$sigma if (!defined($xmin));
  $xmax  = $mu + 2*$sigma if (!defined($xmax));
  $nx    = 100 if (!defined($nx));

  my $x = intervals($xmin,$xmax,$nx);
  my $y = $peak * exp( -($x-$mu)**2 / (2*$sigma**2) );  ##-- by hand

  ##-- ... using PDL::GSLSF::ERF
  #require PDL::GSLSF::ERF;
  #$y = ... gsl_sf_erf_Z($x) ...;

  return ($x,$y);
}

##--------------------------------------------------------------
## $probit = $pvals->probit()
## $probit = $pvals->probit($probit)
##  + gets probit() function values for probability points $pvals
##  + signature: $pvals(n), $probit(n)
##  + 0 < $pvals < 1
##  + probit(p) = sqrt(2)*erfi(2*p-1)
BEGIN { *PDL::probit = \&probit; }
sub probit {
  my ($p,$probit) = @_;
  if (any($p<=0) || any($p>=1)) {
    my $p1 = $p->pdl;
    $p1->where($p >= 1) .= $p->where($p<1)->max;
    $p1->where($p <= 0) .= $p->where($p>0)->min;
    $p = $p1;
  }
  $probit = $p->zeroes if (!defined($probit));
  erfi(2*$p-1, $probit);
  $probit *= sqrt(2);
  return $probit;
}

## $uosm = uosm($i)
##   + uniform order statistic medians for index-pdl $i
BEGIN { *PDL::uosm = \&uosm; }
sub uosm {
  my $i = shift;
  my $n = $i->dim(0);
  my $m = (($i+1) - 0.3175) / ($n + 0.365);
  my ($tmp);
  ($tmp=$m->slice("(".($n-1).")")) .= 0.5**(1/$n);
  ($tmp=$m->slice("(0)")) .= 1 - $m->slice("(".($n-1).")");
  return $m;
}

## ($xline,$yline,$ycoeffs) = qqfit($xraw,$yraw,$opts)
##  + see: http://www.nist.gov/stat.handbook
##  + returned values are independently sorted
##  + %$opts:
##     nosort => $bool,  ##-- if true, data is assumed already flat and independently sorted
##     unique => $bool,  ##-- if true, line is fit to unique values only
BEGIN { *PDL::qqfit = \&qqfit; }
sub qqfit {
  my ($xdata,$ydata,$opts) = @_;

  ##-- require (independently) sorted data
  if ( !($opts && $opts->{nosort}) ) {
    $xdata = $xdata->flat->qsort;
    $ydata = $ydata->flat->qsort;
  }

  ##-- line() plot (fit $xdata->$ydata)
  my ($yfit,$coeffs) = $ydata->mooLinfit($xdata,%{$opts||{}});
  return ($xdata,$yfit,$coeffs);
}

##======================================================================
## Deleted Interpolation

## $lambdas             = diLambdas2($f12,%args) ##-- scalar context
## ($lambda2,$lambda12) = diLambdas2($f12,%args) ##-- list context
##  + $f12 is either a dense PDL or a PDL::CCS::Nd
##  + finds $lambdas = pdl([$lambda1,$lambda2]) for estimating values of dim=1 of $f12
##    by $f2 independent probabilities ($lambda2) or dim=0 conditional probabilities ($lambda12)
##  + %args:
##     f1    => $f1,  ##-- optional, dense or CCS::Nd
##     f2    => $f2,  ##-- optional, dense or CCS::Nd
##     N     => $N,   ##-- optional
##     hapax => $how, ##-- '12':add to $lambda12, ('1' or '2'):add to $lambda2, anything else: ignore (default)
BEGIN { *PDL::diLambdas2 = *PDL::CCS::Nd::diLambdas2 = \&diLambdas2; }
sub diLambdas2 {
  my ($f12,%args) = @_;
  my $f1  = defined($args{f1}) ? $args{f1} : $f12->xchg(0,1)->sumover;
  my $f2  = defined($args{f2}) ? $args{f2} : $f12->sumover;
  my $N   = defined($args{N})  ? $args{N}  : $f2->sumover;
  my $hapax_how = defined($args{hapax})  ? $args{hapax}  : 'ignore';
  #$f12 = $f12->toccs if (!UNIVERSAL::isa($f12,'PDL::CCS::Nd'));
  $f1  = $f1->decode if (UNIVERSAL::isa($f1,'PDL::CCS::Nd'));
  $f2  = $f2->decode if (UNIVERSAL::isa($f2,'PDL::CCS::Nd'));

  my $f12i = $f12->whichND;
  my $f12v = UNIVERSAL::isa($f12,'PDL::CCS::Nd') ? $f12->whichVals->double : $f12->indexND($f12i)->double;
  my $f1v  = $f1->double->index( $f12i->slice("(0),") );
  my $f2v  = $f2->double->index( $f12i->slice("(1),") );

  my $p12v = ($f12v-1) / ($f1v-1);
  my $p2v  = ($f2v-1)  / ($N-1);

  my $f1_hapax_mask    = ($f1v <= 1);
  my $f1_nonhapax_mask = $f1_hapax_mask->not;

  ##-- compare
  my $p12_best = $f1_nonhapax_mask & ($p12v > $p2v);
  my $p2_best  = $f1_nonhapax_mask & $p12_best->not;

  ##-- apply hapax preference
  $p12_best |= $f1_hapax_mask if ($hapax_how eq '12');
  $p2_best  |= $f1_hapax_mask if ($hapax_how eq '1' || $hapax_how eq '2');

  ##-- extract frequencies
  my $p12_best_f = $f12v->where($p12_best);
  my $p2_best_f  = $f12v->where($p2_best);

  ##-- compute lambdas
  my $lambda12f = $p12_best_f->sumover;
  my $lambda2f  = $p2_best_f->sumover;
  my $lambdas   = pdl(PDL::double, [$lambda2f,$lambda12f]);
  if (!all($lambdas==0)) {
    $lambdas /= $lambdas->sumover;
  } else {
    ##-- all ignored hapax: hack
    $lambdas .= $lambdas->ones / 2;
  }

  return wantarray ? $lambdas->dog : $lambdas;
}

1;

##======================================================================
## Docs
=pod

=head1 NAME

MUDL - MUDL Unsupervised Dependency Learner

=head1 SYNOPSIS

 use MUDL;

=cut

##======================================================================
## Description
=pod

=head1 DESCRIPTION

...

=cut

##======================================================================
## Footer
=pod

=head1 ACKNOWLEDGEMENTS

perl by Larry Wall.

=head1 AUTHOR

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2008, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
