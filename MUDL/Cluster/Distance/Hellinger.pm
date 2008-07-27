##
## File: MUDL::Cluster::Distance::Bhat.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: cluster distance function: Hellinger distance
##======================================================================

package MUDL::Cluster::Distance::Hellinger;
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
##  + restrictions:
##    - $data rows (target vectors) should be probability distributions (e.g. conditional)
##    - ... ?
##  + inherited %args:
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
  my $pdata = $args{data} / $args{data}->sumover->dummy(0,1); ##-- implicitly normalize
  my $dr1 = $pdata->dice_axis(1,$args{rows1});
  my $dr2 = $pdata->dice_axis(1,$args{rows2});

  ##-- dist(p,q) = 1/2 * sqrt(2-2*BC(p,q))
  ## + where:
  ##     BC(p,q) =     \sum_{i=1}^d sqrt(p[i]*q[i]) ##-- Bhattacharyya coefficient
  ##   so:
  ##   dist(p,q) = 1/2 * sqrt( 2 - 2*\sum_{i=1}^d sqrt(p[i]*q[i]) )

  my $cmpvec = (($dr1*$dr2)->inplace->sqrt->sumover        ##-- BC(p,q)
		->inplace->mult(2,0)->inplace->minus(2,1)  ##-- 2-2*BC(p,q)
		->inplace->divide(2,0)                     ##-- 1/2 * sqrt(2-2*BC(p,q))
	       );
  return $cd->compare_set_cmpvec($args{cmpvec},$cmpvec);
}



1; ##-- make perl happy
