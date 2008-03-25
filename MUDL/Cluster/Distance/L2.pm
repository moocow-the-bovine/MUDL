##
## File: MUDL::Cluster::Distance::L2.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: cluster distance function: L2 (Euclidean)
##======================================================================

package MUDL::Cluster::Distance::L2;
use PDL;
use MUDL::Object;
use MUDL::Cluster::Distance;
use Carp;

use strict;

our @ISA = qw(MUDL::Cluster::Distance);


##======================================================================
## API: Low-Level

##--------------------------------------------------------------
## $cmpvec = $cd->compare(%args)
##  + %args:
##     data1  => $data1,   ##-- pdl($d,$n1) : $d=N_features, $n1=N_data1                [REQUIRED]
##     data2  => $data2,   ##-- pdl($d,$n1) : $d=N_features, $n1=N_data2                [REQUIRED]
##     rows1  => $rows1,   ##-- pdl($ncmps) : [$i] -> $data1_rowid_for_cmp_i            [REQUIRED]
##     rows2  => $rows2,   ##-- pdl($ncmps) : [$i] -> $data2_rowid_for_cmp_i            [REQUIRED]
##     mask1  => $mask1,   ##-- pdl($d,$n1) : "feature-is-good" boolean mask for $data1 [default=ones()]
##     mask2  => $mask2,   ##-- pdl($d,$n1) : "feature-is-good" boolean mask for $data2 [default=ones()]
##     weight => $weight,  ##-- pdl($d)     : feature-weight mask (weights distances)   [default=ones()]
##  [o]cmpvec => $cmpvec,  ##-- pdl($ncmps) : output pdl [optional]
sub compare {
  my ($cd,%args) = @_;
  $cd->compare_check(\%args) or croak(ref($cd)."::compare(): cowardly refusing to process inconsistent request");
  #$cd->compare_defaults(\%args); ##-- ensure mask*, weight
  my $cmpvec = $cd->compare_cmpvec(\%args);
  my $dr1 = $args{data1}->dice_axis(1,$args{rows1});
  my $dr2 = $args{data2}->dice_axis(1,$args{rows2});
  ##
  ##-- dist(x,y) = 1/d * \sum_{i=1}^d abs(x[i]-y[i])
  my $d = $dr1->dim(0);

  #$cmpvec .= ($dr1-$dr2)->pow(2)->sumover->sqrt;      ##-- correct
  #$cmpvec .= ($dr1-$dr2)->pow(2)->sumover->sqrt / $d; ##-- correct-ish, normalized by $d
  $cmpvec .= (($dr1-$dr2)->pow(2)->sumover / $d);      ##-- ... but this is what PDL::Cluster 'e' does

  return $cmpvec;
}



1; ##-- make perl happy
