#-*- Mode: CPerl -*-

## File: MUDL::SVD.pm
## Author: Bryan Jurish <moocow@cpan.org>
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
##    r        => $r,         ##-- number of target dimensions (0 for no svd); 1 <= $r <= min2($d,$n)
##                            ##   + if ( 1 <= $r <= $d ) : number of reduced dimensions
##                            ##   + if ( 0 <= $r <   1 ) : coefficient: r' = ceil($r*$d)
##                            ##   + if (-1 <  $r <   0 ) : log-coeff  : r' = ceil(exp($r*log($d)))
##    rdims    => $ndims,     ##-- expanded number of dimensions corresponding to $r
##    maxiters => $maxiters,  ##-- max Lanczos iterations (default = 2*$r; 0=no max)
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
##    ## + $a_reduced_d2r : $r-by-$n = $u
##    ##                             = $svd->apply0($a)
##    ##                             = (inv(stretcher($sigma)) x $v->xchg(0,1) x $a->xchg(0,1))->xchg(0,1)
##    ##
##    ## + $a_approx_d2r  : $d-by-$n = $svd->unapply0($a_reduced_d2r)
##    ##                             = $a_reduced_d2r x stretcher($sigma) x $v->xchg(0,1)
##    ##
##    ## + $a_reduced_n2r : $d-by-$r = $v->xchg(0,1)
##    ##                             = $svd->apply1($a)
##    ##                             = (inv(stretcher($sigma)) x $u->xchg(0,1) x $a)
##    ##
##    ## + $a_approx_n2r  : $d-by-$n = $svd->unapply1($a_reduced_n2r)
##    ##                             = $u x stretcher($sigma) x $a_reduced_n2r
##    ##
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
## $svd = $svd->computeccs_nd($ccs_nd, $d_dimnum=0, $clearptr=0)
##  + $ccs_nd is a PDL::CCS::Nd object
sub computeccs_nd {
  my ($svd,$ccs,$d_dim,$clearptr) = @_;
  $d_dim      = 0 if (!defined($d_dim));
  $clearptr &&= !defined($ccs->[$PDL::CCS::Nd::PTRS][0]);
  my $n_dim   = abs(1-$d_dim);

  my ($ptr,$pi2nzi) = $ccs->ptr($d_dim);
  my ($rowids,$nzvals);
  if ($d_dim==0 && $ccs->is_physically_indexed) {
    ##-- optimize for physically indexed pdl on 0th dim
    undef $pi2nzi;
    $rowids = $ccs->_whichND->slice("($n_dim),");
    $nzvals = $ccs->_nzvals;
    $ccs->[$PDL::CCS::Nd::PTRS][0]=undef if ($clearptr);
  } else {
    ##-- generic case: use translation indices
    $rowids = $ccs->_whichND->slice("($n_dim),")->index($pi2nzi);
    $nzvals = $ccs->_nzvals->index($pi2nzi);
  }
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
  $maxiters = 2*$r if (!defined($maxiters));

  my $ut = zeroes(double, $n, $r);
  my $s  = zeroes(double, $r);
  my $vt = zeroes(double, $d, $r);

  ##-- DEBUG: weird segfaults in DiaCollo models
  #$ptr->make_physical;
  #$rowids->make_physical;
  #$nzvals->make_physical;
  ##-- /DEBUG

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
  $maxiters    = 2*$r if (!defined($maxiters));

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
  $svd->{r}     = $svd->{rdims} = $r;

  return $svd;
}


##======================================================================
## SVD: Application
##======================================================================

## $sigma_diagonal_matrix = $svd->sigma()
##  + returns diagonal matrix stretcher($svd->{sigma}), checking or populating cache $svd->{sigma_}
sub sigma {
  my $svd = shift;
  return $svd->{sigma_} if (defined($svd->{sigma_}));
  confess("no {sigma} key defined for SVD!") if (!defined($svd->{sigma}));
  return $svd->{sigma_} = stretcher($svd->{sigma});
}

## $inverse_sigma_diagonal_matrix = $svd->isigma()
##  + returns diagonal matrix inv(stretcher($svd->{sigma})), checking or populating cache $svd->{isigma_}
sub isigma {
  my $svd = shift;
  return $svd->{isigma_} if (defined($svd->{isigma_}));
  confess("no {sigma} key defined for SVD!") if (!defined($svd->{sigma}));
  my $isigma = $svd->{sigma}->pdl;
  (my $tmp = $isigma->where($svd->{sigma})) **= -1;
  return $svd->{isigma_} = stretcher( $isigma );
}

## $v_x_isigma = $svd->visigma()
##  + cached $svd->{visigma_} = $svd->{v} x inv(stretcher($svd->{sigma})) (== $svd->isigmaVt->xchg(0,1))
sub visigma {
  my $svd = shift;
  return $svd->{visigma_} if (defined($svd->{visigma_}));
  confess("no {v} key defined for SVD!") if (!defined($svd->{v}));
  return $svd->{visigma_} = $svd->{v}->matmult($svd->isigma);
}

## $u_x_isigma = $svd->uisigma()
##  + cached $svd->{uisigma_} = $svd->{u} x inv(stretcher($svd->{sigma})) (== $svd->isigmaUt->xchg(0,1))
sub uisigma {
  my $svd = shift;
  return $svd->{uisigma_} if (defined($svd->{uisigma_}));
  confess("no {u} key defined for SVD!") if (!defined($svd->{u}));
  return $svd->{uisigma_} = $svd->{u}->matmult($svd->isigma);
}

## $isigma_x_vt = $svd->isigmaVt()
##  + returns $svd->visigma->xchg(0,1) = inv(stretcher($svd->{sigma})) x $svd->{v}->xchg(0,1)
##  + formerly cahced as $svd->{isigmaVt_}
sub isigmaVt {
  return $_[0]->visigma->xchg(0,1);
}

## $isigma_x_ut = $svd->isigmaUt()
##  + returns $svd->uisigma->xchg(0,1) = inv(stretcher($svd->{sigma})) x $svd->{u}->xchg(0,1)
##  + formerly cahced as $svd->{isigmaUt_}
sub isigmaUt {
  return $_[0]->uisigma->xchg(0,1);
}

## $a_reduced_d2r = $svd->apply0($a)
## $a_reduced_d2r = $svd->apply0($a, $abnil)
##  + alias: apply()
##  + applies svd on 0th dim ($d) to $a, a pdl of dims $d,$na
##  + computes svd for $a if no data is already stored
##  + just returns $a unless $svd->{r} is set to a true value
##  + $abnil may be pre-cached & passed in to speed up repeated
##    calls to apply() when $a is passed as a PDL::CCS::Nd object,
##    in which case $abnil should have been computed as:
##      $abnil = $svd->apply1( $a->missing->squeeze->slice("*$d,*1") )->flat;
##    - $abnil will only help (and only be used) if $a->missing != 0
BEGIN { *apply = \&apply0; }
sub apply0 {
  my ($svd,$a,$abnil) = @_;
  return $a if ($svd->{r}==0 || $svd->{rdims} >= $a->dim(0)); ##-- sanity check

  ##-- sanity check(s)
  my ($d,$na) = $a->dims;
  $svd->compute($a)
    if (grep { !defined($_) } @$svd{qw(u sigma v)});
  confess(ref($svd), "::apply0(): bad input pdl!")
    if ($d != $svd->{v}->dim(1));

  ##-- apply svd
  my ($ar);
  if ($a->isa('PDL::CCS::Nd')) {
    ##-- CCS::Nd matmult() calls inner(), produces huge temporary, so we hack things here
    if ($a->missing==0) {
      $ar = $a->matmult2d_zdd($svd->visigma);                ##-- missing is zero: whew!
    } else {
      $ar = $a->matmult2d_sdd($svd->visigma,undef,$abnil);   ##-- missing is nonzero: whoops!
    }
  } else {
    #$ar = $a x $svd->{v}; ##-- OLD
    #$ar = ($svd->isigma x $svd->{v}->xchg(0,1) x $a->xchg(0,1))->xchg(0,1);
    #$ar = $a->xchg(0,1)->matmult($svd->isigmaVt->xchg(0,1))->xchg(0,1);
    $ar = $a->matmult($svd->visigma);
  }

  return $ar;
}

## $a_reduced_n2r = $svd->apply1($a)
## $a_reduced_n2r = $svd->apply1($a, $abnil)
##  + applies svd on 1st dim ($n) to $a, a pdl of dims $da,$n
##  + computes svd for $a if no data is already stored
##  + just returns $a unless $svd->{r} is set to a true value
##  + $abnil may be pre-cached & passed in to speed up repeated
##    calls to apply() when $a is passed as a PDL::CCS::Nd object,
##    in which case $abnil should have been computed as:
##      $abnil = $svd->apply1( $a->missing->squeeze->slice("*1,*$n") )->flat;
##    - $abnil will only help (and only be used) if $a->missing != 0
sub apply1 {
  my ($svd,$a,$abnil) = @_;
  return $a if ($svd->{r}==0 || $svd->{rdims} >= $a->dim(1)); ##-- sanity check

  ##-- sanity check(s)
  my ($da,$n) = $a->dims;
  $svd->compute($a)
    if (grep { !defined($_) } @$svd{qw(u sigma v)});
  confess(ref($svd), "::apply1(): bad input pdl!")
    if ($n != $svd->{u}->dim(1));

  ##-- apply svd
  my ($ar);
  if ($a->isa('PDL::CCS::Nd')) {
    if ($a->missing==0) {
      $ar = $a->xchg(0,1)->matmult2d_zdd($svd->uisigma)->xchg(0,1);
    } else {
      $ar = $a->xchg(0,1)->matmult2d_sdd($svd->uisigma,undef,$abnil)->xchg(0,1);
    }
  } else {
    #$ar = ($svd->isigma x $svd->{u}->xchg(0,1) x $a);
    $ar = $svd->isigmaUt->matmult($a);
  }

  return $ar;
}

##======================================================================
## SVD: Unapplication
##======================================================================

## $a_approx = $svd->unapply0($a_reduced_d2r)
##  + alias: unapply()
##  + un-applies svd over 0th dim ($r~$d) of $a_reduced_d2r, a pdl of dims $r,$na
##  + just returns $a_reduced_d2r unless $svd->{r} is set to a true value
BEGIN { *unapply = \&unapply0; }
sub unapply0 {
  my ($svd,$ar) = @_;
  return $ar if ($svd->{r}==0 || $svd->{rdims} != $ar->dim(0)); ##-- sanity check

  ##-- sanity check(s)
  confess(ref($svd), "::unapply(): bad input pdl!")
    if ($ar->dim(0) != $svd->{v}->dim(0));

  ##-- un-apply svd, by dim=0
  #my $a = $ar x $svd->{v}->xchg(0,1); ##-- OLD, WRONG
  #my $a = $ar x $svd->sigma x $svd->{v}->xchg(0,1);
  my $a = $ar->matmult($svd->sigma)->matmult($svd->{v}->xchg(0,1));

  return $a;
}

## $a_approx = $svd->unapply1($a_reduced_n2r)
##  + alias: unapply()
##  + un-applies svd over 1st dim ($r~$n) of $a_reduced_n2r, a pdl of dims $da,$r
##  + just returns $a_reduced_n2r unless $svd->{r} is set to a true value
sub unapply1 {
  my ($svd,$ar) = @_;
  return $ar if ($svd->{r}==0 || $svd->{rdims} != $ar->dim(1)); ##-- sanity check

  ##-- sanity check(s)
  confess(ref($svd), "::unapply1(): bad input pdl!")
    if ($ar->dim(1) != $svd->{u}->dim(0));

  ##-- un-apply svd, by dim=1
  #my $a = $svd->{u} x $svd->sigma x $ar;
  my $a = $svd->{u}->matmult($svd->sigma)->matmult($ar);

  return $a;
}

##======================================================================
## I/O: mmap
##======================================================================

## $thingy = $obj->TO_JSON()
##   + JSON module wrapper; default just returns anonymous HASH-ref
sub TO_JSON {
  my $svd = shift;
  my $data = { __CLASS__=>ref($svd), map {($_=>$svd->{$_})} grep {!ref($svd->{$_})} keys %$svd };
  foreach (qw(r rdims maxiters kappa endl endr)) {
    $data->{$_} = $svd->{$_}->sclr if (UNIVERSAL::isa($svd->{$_},'PDL'));
  }
  return $data;
}

## $bool = $svd->saveRawFiles($basename)
sub saveRawFiles {
  my ($svd,$base) = @_;
  $svd->saveJsonFile("$base.json")
    or confess(__PACKAGE__, "::saveMMapFile(): failed to save $base.json: $!");
  foreach (qw(u sigma v)) {
    $svd->writePdlFile($svd->{$_}, "$base.$_.pdl")
      or confess(__PACKAGE__, "::saveMMapFile(): failed to save $base.$_.pdl: $!");
  }
  return 1;
}

## $obj = $CLASS_OR_OBJECT->loadRawFiles($basename,$mmap=0)
sub loadRawFiles {
  my ($that,$base,$mmap) = @_;
  my $svd = $that->loadJsonFile("$base.json")
    or confess(__PACKAGE__, "::loadMMapFile(): failed to load $base.json: $!");
  foreach (qw(u sigma v)) {
    defined($svd->{$_} = $svd->readPdlFile("$base.$_.pdl",'PDL',$mmap))
      or confess(__PACKAGE__, "::loadMMapFile(): failed to ".($mmap ? "mmap" : "read")." $base.$_.pdl: $!");
  }
  return $svd;
}


1;
