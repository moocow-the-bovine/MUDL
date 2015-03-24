##
## File: MUDL::Cluster::Distance::Spearman.pm
## Author: Bryan Jurish <moocow@cpan.org>
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
##     data   => $data,    ##-- pdl($d,$n)  : $d=N_features, $n=N_data                  [REQUIRED]
##     rows1  => $rows1,   ##-- pdl($ncmps) : [$i] -> $data1_rowid_for_cmp_i            [REQUIRED]
##     rows2  => $rows2,   ##-- pdl($ncmps) : [$i] -> $data2_rowid_for_cmp_i            [REQUIRED]
##     mask   => $mask,    ##-- pdl($d,$n)  : "feature-is-good" boolean mask for $data1 [default=$data->isgood()]
##     weight => $weight,  ##-- pdl($d)     : feature-weight mask (weights distances)   [default=ones()]
##  [o]cmpvec => $cmpvec,  ##-- pdl($ncmps) : output pdl [optional]
sub compare {
  my ($cd,%args) = @_;
  $cd->compare_check(\%args) or croak(ref($cd)."::compare(): cowardly refusing to process inconsistent request");
  my $rdata = $args{data}->avgranks()+$cd->{rank_min};
  return $cd->SUPER::compare( %args,
			      data=>$rdata,
			      #pearson_mu =>(0.5*($rdata->dim(0)-1)), ##-- a priori for ranks in [0..($d-1)]
			      #pearson_mu =>(0.5*($rdata->dim(0)+1)), ##-- a priori for ranks in [1..$d] ~ PDL::Cluster 's','S'
			      pearson_mu=>(sequence($rdata->dim(0))+$cd->{rank_min})->average, ##-- a posteriori
			    );
}



1; ##-- make perl happy
