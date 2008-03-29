##
## File: MUDL::Cluster::LinkMethod::GroupAverage.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: generic clustering (cluster-row & cluster-cluster) distance linkage, native perl: group-average-link
##======================================================================

package MUDL::Cluster::LinkMethod::GroupAverage;
use MUDL::Cluster::LinkMethod;
use PDL;
use PDL::CCS::Nd;
use PDL::CCS::Ufunc;
use PDL::CCS::VectorValued;
use Carp;

use strict;

our @ISA = qw(MUDL::Cluster::LinkMethod);

##======================================================================
## Generic constructor

## $clm = CLASS->new(%args);
##  + basic %args:
##     class    => $className,  # string: class-name or -alias or MUDL::Cluster::LinkMethod:: suffix
##     ...

#(inherited from ../LinkMethod.pm)

##======================================================================
## API: Low-level

##--------------------------------------------------------------
## ($lwhich,$lcmps) = $clm->compare_link(%args)
##  + cluster-row and cluster-cluster linkage utility
##  + %args
##     which   => $whichX, ##-- int (2,$ncmps) : link keys (cluster-ids) as for indexND [REQUIRED]
##     cmps    => $cmps,   ##-- dbl ($ncmps)   : row-row distances                      [REQUIRED]
##  [o]lwhich  => $lwhich, ##-- int (2,$k*$n)  : unique link keys                       [default=new]
##  [o]lcmps   => $lcmp,   ##-- dbl ($k*$n)    : link-distances for unique link keys    [default=new]
sub compare_link {
  my ($clm,%args) = @_;
  croak(ref($clm)."::compare_link(): cowardly refusing to inconsistent request") if (!$clm->compare_link_check(\%args));

  my $qsi   = $args{which}->qsortveci;
  my $which = $args{which}->dice_axis(1,$qsi);
  my $cmps  = $args{cmps}->index($qsi);

  my ($wlens,$wvals)  = $which->rlevec();
  my ($lwhich,$lcmps) = $which->ccs_accum_sum($cmps, 0,0);
  my $which2cmp_ccs   = PDL::CCS::Nd->newFromWhich($lwhich, $lcmps->append(0), sorted=>1,steal=>1);
  my $which2len_ccs   = PDL::CCS::Nd->newFromWhich($wvals,  $wlens->append(0), sorted=>1,steal=>1);
  $which2cmp_ccs     /= $which2len_ccs;

  return $clm->compare_link_set($wc_avg->_whichND, $wc_avg->_nzvals,\%args);
}


1; ##-- be happy
