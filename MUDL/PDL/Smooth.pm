#-*- Mode: CPerl -*-

## File: MUDL::PDL::Smooth.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL Smoothing utilities
##======================================================================

package MUDL::PDL::Smooth;
use MUDL::PDL::Ranks;
use PDL;
use PDL::Math;
use PDL::CCS;

use strict;

our @ISA = qw(Exporter);
our %EXPORT_TAGS =
  (
   'vals'  => ['valcounts','smearvals','intervals'],
   'fit'   => ['zipf_fit','linfit','loglinfit'],
   #'gt'    => ['smoothGTLogLin'],
   'gauss' => ['smoothGaussian', 'gausspoints', 'gaussyvals', 'probit',
	       'gausspdf', 'gausscdf', 'gausspeak',
	       'gaussquantiles', 'gaussqvals', 'gausscdfi', ##-- all aliases for one another
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

## ($values,$valcounts) = $pdl->valcounts()  ##-- array context, suitable for PDL::primitive::interpol(ate)
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
  $vc->index($imissing) .= $nmissing;
  return ($v,$vc);
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

## ($zipf_constant,$freq_fit) = zipf_fit($freq_pdl)   ##-- array context
## $zipf_constant             = zipf_fit($freq_pdl)   ##-- scalar context
## + fits $freq_pdl to best Zipfian distribution
##     $freq = $zipf_constant / $rank_desc
BEGIN { *PDL::zipf_fit = \&zipf_fit; }
sub zipf_fit {
  my $freq    = shift;
  my $f_ranks = $freq->ranks(order=>'desc')+1;
  my $total   = ($freq*$f_ranks)->sumover;
  my $nitems  = pdl(double,$freq->nelem);
  my $zipf_c  = $total / $nitems;
  return $zipf_c if (!wantarray);
  my $freq_fit = $zipf_c / $f_ranks;
  return ($zipf_c,$freq_fit);
}

##======================================================================
## Linear fit

## ($fit,$coeffs) = $vals->mooLinfit()
## ($fit,$coeffs) = $vals->mooLinfit($keys)
##  + $keys defaults to $vals->xvals()+1
##  + $fit are linear-fitted values $vals as values for $keys
##  + $coeffs are [$a,$b] such that all($yfit == $a*$keys + $b)
BEGIN { *PDL::mooLinfit = \&mooLinfit; }
use PDL::Fit::Linfit;
sub mooLinfit {
  my ($Zc,$v) = @_;
  $v = ($Zc->xvals+1)->double if (!defined($v));
  my ($cfit,$coeffs) = $Zc->linfit1d($Zc->ones->cat($v->setnantobad->setbadtoval(0)));
  return ($cfit,$coeffs->slice("-1:0"));
}

##======================================================================
## Log-linear fit

## ($fit,$coeffs) = $vals->loglinfit()
## ($fit,$coeffs) = $vals->loglinfit($keys)
##  + $keys defaults to $vals->xvals()+1
##  + $fit are log-linear fitted values $vals as values for $keys
##  + $coeffs are [$a,$e] such that all($yfit == $a*($keys**$e))
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
use PDL::Fit::Linfit;
sub loglinfit {
  my ($Zc,$v) = @_;
  $v = ($Zc->xvals+1)->double if (!defined($v));
  my ($cfit,$coeffs) = $Zc->log->linfit1d($Zc->ones->cat($v->log->setnantobad->setbadtoval(0)));
  $cfit->inplace->exp;
  $coeffs->slice("(0)")->inplace->exp;
  return ($cfit,$coeffs);
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
##  + Signature: $pvals(n), $probit(n)
##  + 0 < {$pvals,$probit} < 1
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
  $m->slice("(".($n-1).")") .= 0.5**(1/$n);
  $m->slice("(0)") .= 1 - $m->slice("(".($n-1).")");
  return $m;
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
