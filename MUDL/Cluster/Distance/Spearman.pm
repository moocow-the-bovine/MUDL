##
## File: MUDL::Cluster::Distance::Spearman.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: cluster distance function: Spearman Rank Correlation Coefficient
##======================================================================

package MUDL::Cluster::Distance::Spearman;
use PDL;
use MUDL::Object;
use MUDL::Cluster::Distance;
use MUDL::Cluster::Distance::Pearson;
use MUDL::PDL::Ranks;
use Carp;

use strict;
our @ISA = qw(MUDL::Cluster::Distance::Pearson);

##======================================================================
## Constructor

## $cd = $CLASS_OR_OBJ->new(%args)
##  + new %args:
##     rank_min => $minimum_rank,  [default=0]
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   rank_min=>0, ##-- minimum rank
			   @_
			  );
}


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
  my $rdata1 = $args{data1}->avgranks()+$cd->{rank_min};
  my $rdata2 = $args{data2}->avgranks()+$cd->{rank_min};
  return $cd->SUPER::compare(%args, data1=>$rdata1, data2=>$rdata2);
}



1; ##-- make perl happy
