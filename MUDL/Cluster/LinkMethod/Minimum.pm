##
## File: MUDL::Cluster::LinkMethod::Minimum.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: generic clustering (cluster-row & cluster-cluster) distance linkage, native perl: minimum-link
##======================================================================

package MUDL::Cluster::LinkMethod::Minimum;
use MUDL::Cluster::LinkMethod;
use PDL;
use PDL::CCS::Ufunc;
use PDL::VectorValued;
use Carp;

use strict;

our @ISA = qw(MUDL::Cluster::LinkMethod);

##======================================================================
## Generic constructor

## $clm = MUDL::Cluster::LinkMethod->new(%args);
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
  croak(ref($clm)."::compare_link(): cowardly refusing to inconsistent request") if (!$clm->compare_check(\%args));

  my $qsi   = $args{which}->qsortveci;
  my $which = $args{which}->dice_axis(1,$qsi);
  my $cmps  = $args{cmps}->index($qsi);

  my ($lwhich,$lcmps) = ccs_accum_minimum($which, $cmps, 'inf',0);

  return $clm->compare_link_set($lwhich,$lcmps,\%args);
}


1; ##-- be happy
