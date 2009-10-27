#-*- Mode: CPerl -*-

## File: MUDL::SVD.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Singular Value Decomposition
##======================================================================

package MUDL::SVD;
use MUDL::Object;
use PDL;
use PDL::SVDLIBC;
use Carp;

use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## $svd = $class_or_obj->new(%args)
##  + %args
##    ##-- configuration
##    r        => $r,         ##-- number of target dimensions (0 for no svd)
##                            ##   + if ( 1 <= $r <= $d ) : number of reduced dimensions
##                            ##   + if ( 0 <= $r <   1 ) : coefficient: r' = ceil($r*$d)
##                            ##   + if (-1 <  $r <   0 ) : log-coeff  : r' = ceil(exp($r*log($d)))
##    rdims    => $ndims,     ##-- expanded number of dimensions corresponding to $r
##    maxiters => $maxiters,  ##-- max Lanczos iterations (default = 2*$r)
##    kappa    => $kappa,     ##-- tolerance (default=1e-6)
##    endl     => $end_l,     ##-- left interval endpoint for unwanted eigenvalues (-1e-30)
##    endr     => $end_r,     ##-- left interval endpoint for unwanted eigenvalues ( 1e-30)
##
##    ##-- input matrix
##    #$pdl = $matrix2d       ##-- pdl: $d-by-$n
##
##    ##-- output values
##    u        => $u,         ##-- pdl: $r-by-$n  ##-- $r==$ndims==$lr->{r}
##    sigma    => $sigma,     ##-- pdl: $r (diagonal of an $r-by-$r matrix, created with stretcher($sigma))
##    v        => $v,         ##-- pdl: $r-by-$d
##    ##
##    ##-- such that:
##    ## + $inputMatrixApprox = ($u x stretcher($sigma) x $v->xchg(0,1))
##    ## + $a_reduced = $svd->apply($a) = $a x $v
##    ## + $a_approx  = $svd->unapply($a_reduced) = $a_reduced x $v->xchg(0,1)
##    ## + see also built-in 'svd()' in PDL::MatrixOps
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   r=>0,
			   maxiters=>undef,
			   kappa=>1e-6,
			   endl=>-1e-30,
			   endr=> 1e-30,
			   @_,
			  );
}

##======================================================================
## General
##======================================================================

## $svd = $svd->clear()
##  + removes underlying decomposition pdls
sub clear {
  my $svd = shift;
  delete @$svd{qw(u sigma v)};
  return $svd;
}

##======================================================================
## SVD: Computation
##======================================================================

## $svd = $svd->computeccs_nd($ccs_nd)
## $svd = $svd->computeccs_nd($ccs_nd, $d_dimnum=0)
##  + $ccs_nd is a PDL::CCS::Nd object
sub computeccs_nd {
  my ($svd,$ccs,$d_dim) = @_;
  $d_dim    = 0 if (!defined($d_dim));
  my $n_dim = abs(1-$d_dim);
  my ($ptr,$pi2nzi) = $ccs->ptr($d_dim);
  my $rowids = $ccs->_whichND->slice("($n_dim),")->index($pi2nzi);
  my $nzvals = $ccs->_nzvals->index($pi2nzi);
  my ($d,$n) = ($ccs->dims)[$d_dim,$n_dim];
  return $svd->computeccs($ptr->slice("0:-2"),$rowids,$nzvals,$n);
}

## $svd = $svd->computeccs($ptr,$rowids,$nzvals);
## $svd = $svd->computeccs($ptr,$rowids,$nzvals,$n);
## $svd = $svd->computeccs($ptr,$rowids,$nzvals,$n,$r);
##   + ($ptr,$rowids,$vals) is a CCS-encoded pdl $a of dims ($d,$n)
##     as encoded by PDL::CCS::ccsencode
##   + (re-)computes svd for encoded pdl $a
##   + does nothing if $r==0
sub computeccs {
  my ($svd,$ptr,$rowids,$nzvals,$n,$r) = @_;

  ##-- arg parsing
  $r = $svd->{r} if (!defined($r));
  $svd->{r} = $r;
  return $svd if (!$r);

  my $d = $ptr->dim(0);
  $n    = $rowids->max+1 if (!$n);

  ##-- detect $r coefficients
  if ($r < 0) {
    $r = int(0.5+exp(-$r*log($d)));
  } elsif ($r < 1) {
    $r = int(0.5+$r*$d);
  }

  $r = $d if ($r > $d); ##-- weak sanity check
  $svd->{rdims} = $r;   ##-- ... set dims

  ##-- pointer hacking
  $ptr->reshape($ptr->nelem+1);
  $ptr->set(-1, $rowids->nelem);

  ##-- defaults
  my $maxiters = $svd->{maxiters};
  $maxiters ||= 2*$r;

  my $ut = zeroes(double, $n, $r);
  my $s  = zeroes(double, $r);
  my $vt = zeroes(double, $d, $r);

  svdlas2($ptr,$rowids,$nzvals,$n,
	  $maxiters, pdl(double, [@$svd{qw(endl endr)}]), $svd->{kappa},
	  $ut, $s, $vt);

  $svd->{u} = $ut->xchg(0,1);
  $svd->{sigma} = $s;
  $svd->{v} = $vt->xchg(0,1);

  return $svd;
}

## $svd = $svd->compute($a);
## $svd = $svd->compute($a,$r);
##   + $a : pdl ($d-by-$n)
##   + (re-)computes svd for input pdl $a
##   + sets $r=$d if $r>$d
sub compute {
  my ($svd,$a,$r) = @_;
  $r = $svd->{r} if (!defined($r));
  $svd->{r} = $r;
  return $svd if (!$r);

  my ($d,$n) = $a->dims;

  ##-- sanity check: no svd if $r >= $d
  ##   + this gets handled in apply():
  ##     we'll assume here that if you're calling compute(), you really want the SVD
  #if ($r >= $d) {
  #  $svd->{u}     = pdl($a);
  #  $svd->{sigma} = ones(double,$d);
  #  $svd->{v}     = stretcher(ones($d));
  #  return $svd;
  #}

  ##-- detect $r coefficients
  if ($r < 0) {
    $r = int(0.5+exp(-$r*log($d)));
  } elsif ($r < 1) {
    $r = int(0.5+$r*$d);
  }

  ##-- back to ye olde grinde
  $r = $d if ($r > $d); ##-- weak sanity check
  $svd->{rdims} = $r;   ##-- ... set dims

  my $maxiters = $svd->{maxiters};
  $maxiters   ||= 2*$r;

  my $ut = zeroes(double, $n, $r);
  my $s  = zeroes(double, $r);
  my $vt = zeroes(double, $d, $r);

  svdlas2d($a,
	   $maxiters, pdl(double, [@$svd{qw(endl endr)}]), $svd->{kappa},
	   $ut, $s, $vt);

  $svd->{u} = $ut->xchg(0,1);
  $svd->{sigma} = $s;
  $svd->{v} = $vt->xchg(0,1);

  return $svd;
}

##======================================================================
## SVD: Shrinking
##======================================================================

## $svd = $svd->shrink()
## $svd = $svd->shrink($r)
##  + shrinks to reductive dimension $r
##  + $r defaults to index of last (least signifcant) singular value in $svd->{sigma}
sub shrink {
  my ($svd,$r) = @_;
  return $svd if (defined($r) && $svd->{r}==$r);

  confess(ref($svd)."::shrink(): no SVD computed yet!")
    if (grep { !defined($_) } @$svd{qw(u sigma v)});

  confess(ref($svd)."::shrink(): cannot increate size!")
    if (defined($r) && $r > $svd->{r});

  ##-- shrink SVD pdls
  if (!defined($r)) {
    ##-- auto-compute $r (leave at least 1 zero if possible)
    $r = $svd->{sigma}->nnz->sclr ;
    $r++ if ($r < $svd->{r});
  }
  $svd->{u}     = $svd->{u}->slice("0:".($r-1).",:");
  $svd->{sigma} = $svd->{sigma}->slice("0:".($r-1));
  $svd->{v}     = $svd->{v}->slice("0:".($r-1).",:");
  $svd->{r}     = $r;

  return $svd;
}


##======================================================================
## SVD: Application
##======================================================================

## $a_reduced = $svd->apply($a)
##  + applies svd by row to $a, a pdl of dims $d,$na
##  + computes svd for $a if no data is already stored
##  + just returns $a unless $svd->{r} is set to a true value
sub apply {
  my ($svd,$a) = @_;
  return $a if ($svd->{r}==0 || $svd->{rdims} >= $a->dim(0)); ##-- sanity check

  ##-- sanity check(s)
  my ($d,$na) = $a->dims;
  $svd->compute($a)
    if (grep { !defined($_) } @$svd{qw(u sigma v)});
  confess(ref($svd), "::apply(): bad input pdl!")
    if ($d != $svd->{v}->dim(1));

  ##-- apply svd
  my $ar  = $a x $svd->{v};
  #$ar x= stretcher($svd->{sigma})->inv; ##-- this is probably NOT a wise idea: check the definitions...

  return $ar;
}

## $a_approx = $svd->unapply($a_reduced)
##  + un-applies svd by row to $a, a pdl of dims $r,$na
##  + just returns $a_reduced unless $svd->{r} is set to a true value
sub unapply {
  my ($svd,$ar) = @_;
  return $ar if ($svd->{r}==0 || $svd->{rdims} != $ar->dim(0)); ##-- sanity check

  ##-- sanity check(s)
  confess(ref($svd), "::unapply(): bad input pdl!")
    if ($ar->dim(0) != $svd->{v}->dim(0));

  ##-- un-apply svd
  my $a = $ar x $svd->{v}->xchg(0,1);

  return $a;
}

1;
