##
## File: MUDL::Cluster::Distance::Cosine.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: cluster distance function: Vector Cosine
##======================================================================

package MUDL::Cluster::Distance::Cosine;
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

  ##-- dist(x,y) = 1 - 1/d * (\sum_{i=1}^d (x[i]-mean(x))/stddev(x) * (y[i]-mean(y))/stddev(y))
  ##             = 1 - 1/d * 1/stddev(x) * 1/stddev(y) * (\sum_{i=1}^d (x[i]-mean(x)) * (y[i]-mean(y)))
  ##             = 1 - (\sum_{i=1}^d (x[i]-mean(x)) * (y[i]-mean(y))) / (d * stddev(x) * stddev(y))
  ## + where:
  ##     mean(x)   := 0
  ##     stddev(x) := sqrt( E(X^2) )
  my $d = $dr1->dim(0);

  my $sigma  = $args{data}->pow(2)->average->sqrt;
  my $sigma1 = $sigma->index($args{rows1});
  my $sigma2 = $sigma->index($args{rows2});

  my $cmpvec = (
		($dr1*$dr2)->sumover
		->inplace->divide($sigma1,0)
		->inplace->divide($sigma2,0)
		->inplace->divide($d,0)
	       )->todense;
  $cmpvec->minus(1,$cmpvec,1);
  #my $cmpvec =  1 - ($dr1/$sigma1r * $dr2/$sigma2r)->sumover / $d;

  return $cd->compare_set_cmpvec($args{cmpvec},$cmpvec);
}



1; ##-- make perl happy
