##
## File: MUDL::Cluster::Distance::L1.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL: cluster distance function: L1
##======================================================================

package MUDL::Cluster::Distance::L1;
use PDL;
use MUDL::Object;
use MUDL::Cluster::Distance;
use Carp;

use strict;

our @ISA = qw(MUDL::Cluster::Distance);

##======================================================================
## Constructor
sub new { return $_[0]->SUPER::new(distFlag=>'b',@_[1..$#_]); }

##======================================================================
## API: Low-Level

##--------------------------------------------------------------
## $cmpvec = $cd->compare(%args)
##  + %args:
##     data   => $data,    ##-- pdl($d,$n)  : $d=N_features, $n=N_data                  [REQUIRED]
##     rows1  => $rows1,   ##-- pdl($ncmps) : [$i] -> $data1_rowid_for_cmp_i            [REQUIRED]
##     rows2  => $rows2,   ##-- pdl($ncmps) : [$i] -> $data2_rowid_for_cmp_i            [REQUIRED]
##     mask   => $mask,    ##-- pdl($d,$n)  : "feature-is-good" boolean mask for $data1 [default=$data->isgood()]
##     weight => $weight,  ##-- pdl($d)     : feature-weight mask (weights distances)   [default=ones()]
##  [o]cmpvec => $cmpvec,  ##-- pdl($ncmps) : output pdl [optional]
sub compare {
  my ($cd,%args) = @_;
  $cd->compare_check(\%args) or croak(ref($cd)."::compare(): cowardly refusing to process inconsistent request");
  #$cd->compare_defaults(\%args); ##-- ensure mask*, weight
  my $dr1 = $args{data}->dice_axis(1,$args{rows1});
  my $dr2 = $args{data}->dice_axis(1,$args{rows2});
  ##
  ##-- dist(x,y) = 1/d * \sum_{i=1}^d abs(x[i]-y[i])
  my $cmpvec = ($dr1-$dr2)->inplace->abs->sumover;
  $cmpvec   /= $dr1->dim(0);
  return $cd->compare_set_cmpvec($args{cmpvec},$cmpvec);
}



1; ##-- make perl happy
