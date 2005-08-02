##-*- Mode: Perl -*-

## File: MUDL::Cluster::Buckshot.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: buckshot clustering
##======================================================================

package MUDL::Cluster::Buckshot;
use PDL;
use PDL::Cluster;
use MUDL::Cluster::Method;
use MUDL::Cluster::Tree;
use Carp;

use strict;

our @ISA = qw(MUDL::Cluster::Method);
our @EXPORT_OK = qw();

##======================================================================
## Buckshot clustering: Constructor

## $args = MUDL::Cluster::Buckshot->new(%args);
##   + %args: general:
##       data     => $data,     # pdl($d,$n)
##       mask     => $mask,     # pdl($d,$n) boolean-valued matrix: true iff $data->at($i,$j) is valid
##       weight   => $wts,      # pdl($d) $d-ary weight vector
##       nclusters=> $k,        # number of desired clusters     (default=2)
##   + %args: prototypes [cluster(), protocluster()]:
##       protos   => $rowids,   # pdl(long,$np) (default=random)
##       nprotos  => $np,       # number of protos (default=rint(sqrt($k*$n)))
##       tree     => $tree,     # a MUDL::Cluster::Tree object for clustering prototypes (default=new)
##       ##-- subdata:
##       {tree}{data}  => $pdata,     # pdl($d,$np): prototype data
##       {tree}{dist}  => $metric,    # distance metric character flag (default='b') : default=$cm->{dist}
##       {tree}{method} => $method,   # treecluster link-method flag   (default='m') : default=$cm->{method}
##   + %args: for getcenters():
##       ctrmethod => $cmethod, # see getcenters()
##       ctrmode   => $cmode,   # 'hard' or 'soft' [default='hard']
##       ctrm      => $m,       # m-best
##   + %args: attachment [cut(), attach()]:
##       niters    => $niters,  # maximum number of attachment iterations (default=0)
##   + optional data:
##       enum     => $enum,    # leaf-id enumerator
##       cenum    => $enum,    # cluster-id enumerator
##   + additional data:
##     - on cut() [attachment]:
##       clusterids  => $cids,  # pdl(long,   $n)    cluster-id by target index
##       clusterdists=> $cdist, # pdl(double, $n)    distance to best cluster
##       cdata    => $cdata,    # pdl(double, $d,$k) centroid profile data
##       cmask    => $cmask,    # pdl(long,   $d,$k) centroid profile mask
sub new {
  my ($that,%args) = @_;

  my ($bc);
  $bc = $that->SUPER::new(
			  ##-- general
			  data=>undef,
			  mask=>undef,
			  weight=>undef,
			  nclusters=>2,
			  ##-- prototypes
			  protos=>undef,
			  nprotos=>undef,
			  tree=>MUDL::Cluster::Tree->new(),
			  dist  =>'b',   ##-- Manhattan distance
			  method=>'m',   ##-- maximum link
			  ##-- clusterdistance() flags
			  cddist  =>'u',  ##-- uncentered correlation (vector cosine)
			  cdmethod=>'v',  ##-- pairwise average
			  ##-- centroid acquisition
			  ctrmethod => 'mean',
			  ctrm    => 4,   ##-- m-best
			  ctrmode => 'hard',
			  ##-- attachment
			  niters => 0,
			  ##-- optional data
			  enum=>undef,
			  cenum=>undef,
			  %args
			 );

  ##-- setup tree
  $bc->{tree}{$_} = $bc->{$_} foreach (grep { $_ ne 'class' && $_ ne 'tree' } keys(%$bc));
  delete($bc->{tree}{data});

  return $bc;
}

##======================================================================
## @keys = $cm->datakeys()
##   + return data-related keys: not copied by shadow(),
##     deleted on set data
sub datakeys {
  my $bc = shift;
  return
    ($bc->SUPER::datakeys,
     #qw(tree
	#pdata pmask clusterids clusterdists
	#cdata cmask cdmatrix cdweight cdsizes cdelts)
     qw(tree protos),
    );

}

##======================================================================
## $bc2 = $bc->shadow(%args)
##
sub shadow {
  my ($bc,%args) = @_;
  my $bc2 = $bc->SUPER::shadow(%args);
  $bc2->{tree} = $bc->{tree}->shadow();
  return $bc2;
}

##======================================================================
## $data = $bc->data()
## $data = $bc->data($data)
##   + get/set data -- reset related pdls on set
sub data {
  my $bc = shift;
  return $bc->{data} if (!@_);
  my $tree = $bc->{tree}->shadow;
  my $data = $bc->SUPER::data(@_);
  $bc->{tree} = $tree;
  return $bc;
}

########################################################################
## Utilities: Clustering
########################################################################

##======================================================================
## Utilities: Clustering: prototypes
##======================================================================

##======================================================================
## $pdata = $bc->protodata()
##  + get prototype ids
##  + returns $bc->{pdata} if defined
##  + otherwise, calls $bc->getprotodata()
sub protodata {
  my $bc = shift;
  return ((defined($bc->{nprotos})
	   && defined($bc->{tree}{data})
	   && $bc->{nprotos}==$bc->{tree}{data}->dim(1))
	  ? $bc->{tree}{data}
	  : $bc->getprotodata());
}

##======================================================================
## $pdata = $bc->getprotodata()
## $pdata = $bc->getprotodata($protos)
##  + initialized prototype data: @$bc{qw(protos nprotos pdata)}
##  + chooses $bc->{nprotos} prototypes randomly
##  + $bc->{nprotos} defaults to sqrt($k*$n)
##  + sets @{$bc->{tree}}{data, mask, weight}
sub getprotodata {
  my ($bc,$protos) = @_;
  $protos = $bc->{protos} if (!defined($protos));

  my ($np,$k,$d,$n) = (@$bc{qw(nprotos nclusters)}, $bc->{data}->dims);
  if (!defined($protos)) {
    ##-- choose random prototypes
    $np = $bc->{nprotos} = sclr(rint(sqrt($k*$n))) if (!$np);
    ##--
    #random(float,$n)->minimum_n_ind($protos=zeroes(long,$np));
    #$protos .= qsort($protos);     ##-- for ease of human inspection
    ##--
    my $ri = random(float,$n)->qsorti;
    $protos = qsort($ri->slice("0:".($np-1)));
  }

  ##-- get prototype data
  $bc->{protos} = $protos;
  $bc->{tree}->data($bc->{data}->dice_axis(1,$protos));
  $bc->{tree}{mask}   = $bc->{mask}->dice_axis(1,$protos) if (defined($bc->{mask}));
  $bc->{tree}{weight} = $bc->{weight};

  return $bc->{tree}{data};
}


##======================================================================
## $bc = $bc->protocluster(%args)
##  + runs tree clustering algorithm on prototypes
sub protocluster {
  my ($bc,%args) = @_;
  @$bc{keys(%args)} = values(%args);
  $bc->protodata();

  $bc->{weight} =
    $bc->{tree}{weight} =
      ones(double, $bc->{data}->dim(0)) if (!defined($bc->{weight}));

  $bc->{tree}{nclusters} = $bc->{nclusters};
  $bc->{tree}->cluster(%args);
  $bc->{tree}->cut();

  return $b;
}


##======================================================================
## ($pcdata,$pcmask) = $bc->getprotocenters(%args)
##  + gets prototype centers (top-level)
##  + just dispatches to $bc->{tree} object
sub getprotocenters {
  my $bc = shift;

  $bc->protocluster() if (!defined($bc->{tree}{clusterids}));

  return @{$bc->{tree}}{qw(cdata cmask)}
    if (defined($bc->{tree}{cdata}) && defined($bc->{tree}{cmask}));

  return $bc->{tree}->getcenters(@_);   ##-- dispatch to {tree} object
}


##======================================================================
## Utilities: Clustering: attachment
##======================================================================

## $bc = $bc->attach0(%args)
##  + attaches non-prototype data to nearest prototype center
##  + sets %$bc keys:
##     clusterids => $cids, # pdl($n) : as for Cluster::Method
sub attach0 {
  my $bc = shift;

  ##-- get prototype centroid data
  my ($cdata,$cmask) = $bc->getprotocenters();

  ##-- get attachment targets
  my ($d,$k,$n) = ($cdata->dims, $bc->{data}->dim(1));
  my $atgmask = ones(byte,$n);
  $atgmask->index($bc->{protos}) .= 0;
  my $atgids = $atgmask->which;
  my $natgs  = $atgids->nelem;

  ##-- call superclass attach()
  my ($acids,$acdist) = $bc->SUPER::attach(rowids=>$atgids,
					   mask=>$bc->{mask},
					   cdata=>$cdata,
					   cmask=>$cmask);

  ##-- get grand total output
  my $cids = $bc->{clusterids} = zeroes(long,$n);
  $cids->index($bc->{protos}) .= $bc->{tree}{clusterids};
  $cids->index($atgids)       .= $acids;

  return $bc;
}


## $bc = $bc->attachN(%args)
##  + attaches all data to nearest center
##  + performs at most $bc->{niters} iterations
##  + required keys:
##      data, mask, weight, ##-- base data
##      clusterids,         ##-- initial cluster assignment
##      ctrXXX,             ##-- centroid data acquisition flags
##      cdmatrix,           ##-- used for fast attachment if present
sub attachN {
  my $bc = shift;

  ##-- iteration control params
  my $niters = $bc->{niters};

  ##-- centroid data
  my ($cdata,$cmask);

  ##-- target data
  my $atgids = sequence(long,$bc->{data}->dim(1));

  ##-- output data
  my $cids_1 = $bc->{clusterids}; ##-- previous assignment, to check for changes
  my $cdist  = zeroes(double, $atgids->nelem);
  my $cids   = pdl($cids_1);

  my ($i);
  for ($i=0; $niters>0 && $i<$niters; $i++) {
    ($cdata,$cmask) = $bc->getcenters(@_);   ##-- get centroid data

    #if (!defined($bc->{cdmatrix})) {
      ##-- compute cluster-to-centroid distances on-the-fly
      attachtonearest(@$bc{qw(data mask weight)},
		      $atgids,
		      $cdata, $cmask,
		      $cids,  $cdist,
		      $bc->cddist, $bc->cdmethod);
    #} else {
    #  ##-- we already have row-to-centroid distances: use 'em
    #  attachtonearestd($bc->{cdmatrix}, $atgids, $cids, $cdist);
    #}

    ##-- BUG: we get empty clusters here sometimes!

    last if (all($cids==$cids_1));     ##-- check for changes
    $bc->{clusterids} = $cids;         ##-- update {clusterids} flag
    ($cids_1,$cids) = ($cids,$cids_1); ##-- swap temps

    ##-- invalidate data depending on previous cluster assignment
    delete(@$bc{qw(csizes celtmask celts cdmatrix cweights),
		qw(rprobs r2cprobs beta),
	       });
  }

  return $bc;
}


########################################################################
## Required Methods: Clustering
########################################################################

##======================================================================
## $bc = $bc->cluster(%args)
##  + actually runs clustering algorithm
sub cluster {
  my ($bc,%args) = @_;
  @$bc{keys(%args)} = values(%args);

  ##-- sanity check(s)
  #$bc->{mask}   = ones(long,$bc->{data}->dims)     if (!defined($bc->{mask}));
  $bc->{mask}   = !$bc->{data}->isbad              if (!defined($bc->{mask}));
  $bc->{weight} = ones(double,$bc->{data}->dim(0)) if (!defined($bc->{weight}));

  $bc->protocluster();   ##-- cluster prototypes
  $bc->attach0();        ##-- attach non-protos to proto centroids
  $bc->attachN();        ##-- re-attachment EM

  return $bc;
}


##======================================================================
## $clusterids = $tc->cut()
## $clusterids = $tc->cut($nclusters)
##   + cut tree, returns vector clusterids($n)
sub cut {
  my ($bc,$nclusters) = @_;
  $bc->{nclusters} = $nclusters if (defined($nclusters));
  $bc->{nclusters} = 2 if (!defined($bc->{nclusters}));

  ##-- maybe re-cluster?
  if (!defined($bc->{clusterids}) || $bc->{clusterids}->dim(0) != $bc->{data}->dim(1)) {
    $bc->cluster();
  }

  return $bc->{clusterids};
}



########################################################################
## Conversion
########################################################################

## (inherited)


########################################################################
## Viewing
########################################################################

## (inherited)


1;

##======================================================================
## Docs
=pod

=head1 NAME

MUDL - MUDL Unsupervised Dependency Learner

=head1 SYNOPSIS

 use MUDL;

=cut

##======================================================================
## Description
=pod

=head1 DESCRIPTION

...

=cut

##======================================================================
## Footer
=pod

=head1 ACKNOWLEDGEMENTS

perl by Larry Wall.

=head1 AUTHOR

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
