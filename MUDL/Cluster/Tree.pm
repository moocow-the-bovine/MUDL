#-*- Mode: Perl -*-

## File: MUDL::Cluster::Tree.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: hierarchical clustering
##======================================================================

package MUDL::Cluster::Tree;
use PDL;
use PDL::Cluster;
use MUDL::Cluster::Method;
use Carp;

our @ISA = qw(MUDL::Cluster::Method);
our @EXPORT_OK = qw();

##======================================================================
## Hierartchical clustering: Constructor

## $args = MUDL::Cluster::Tree->new(%args);
##   + %args:
##       data     => $data,    # pdl($d,$n)
##       mask     => $mask,    # pdl($d,$n) boolean-valued matrix: true iff $data->at($i,$j) is missing
##       weight   => $wts,     # pdl($d) $d-ary weight vector
##       dist     => $metric,  # distance metric character flag (default='e')
##       method   => $method,  # center computation method flag (default='a')
##   + optional data:
##       enum     => $enum,    # leaf-id enumerator
##       cenum    => $enum,    # cluster-id enumerator
##   + additional data:
##     - post-cluster():
##         tree     => $ctree,   # pdl(2,$n) gives structure of clustering tree (see below) [(2,n-1) used]
##         linkdist => $vector,  # pdl($n) array giving distances between sister nodes [(n-1) used]
##     - post-cut($k):
##         nclusters  => $k,                # number of clusters
##         clusterids => $rowid2clusterid,  # pdl($n) maps data rows to cluster-id (range 0..($k-1))
##     - post-leafdistances():
##         leafdist   => $leaf2cid2dist, # pdl($k,$n) maps (clusterid,leaf) to distance(leaf,clusterid)
##   + where:
##       $n : number of data instances (rows)
##       $d : number of features per datum (columns)
##   + methods, metrics, structures: see PDL::Cluster and cluster-3.0 documentation
sub new {
  my ($that,%args) = @_;

  my ($tc);
  if (defined($args{method}) && $args{method} =~ /^k/i) {
    ##-- kmeans clustering
    require MUDL::Cluster::KMeans;
    $args{method} = substr($args{method},1);
    $tc = MUDL::Cluster::KMeans->new(%args);
  }
  else {
    $tc = $that->SUPER::new(
			    data=>null,
			    mask=>undef,
			    weight=>undef,
			    method=>'a',
			    dist=>'e',
			    ##-- output data
			    ctree=>undef,
			    linkdist=>undef,
			    ##-- for cut
			    nclusters=>2,
			    %args
			    );
  }

  return $tc;
}


##======================================================================
## $data = $tc->data()
## $data = $tc->data($data)
##   + get/set data -- reset related pdls on set
##
## (inherited)


##======================================================================
## $tc = $tc->cluster(%args)
##  + actually runs clustering algorithm
sub cluster {
  my ($tc,%args) = @_;
  @$tc{keys(%args)} = values(%args);

  PDL::Cluster::treecluster
    (
     $tc->{data},
     (defined($tc->{mask})     ? $tc->{mask}     : ($tc->{mask}=ones(long,$tc->{data}->dims))),
     (defined($tc->{weight})   ? $tc->{weight}   : ($tc->{weight}=ones(double,$tc->{data}->dim(0)))),

     (defined($tc->{ctree})    ? $tc->{ctree}    : ($tc->{ctree}=zeroes(long,2,$tc->{data}->dim(1)))),
     (defined($tc->{linkdist}) ? $tc->{linkdist} : ($tc->{linkdist}=zeroes(double,$tc->{data}->dim(1)))),

     (defined($tc->{dist})     ? $tc->{dist}     : ($tc->{dist}='e')),
     (defined($tc->{method})   ? $tc->{method}   : ($tc->{method}='a')),
    );

  return $tc;
}

##======================================================================
## $clusterids = $tc->cut()
## $clusterids = $tc->cut($nclusters)
##   + cut tree, returns vector clusterids($n)
sub cut {
  my ($tc,$nclusters) = @_;
  $tc->{nclusters} = $nclusters if (defined($nclusters));
  $tc->{nclusters} = 2 if (!defined($tc->{nclusters}));

  if (!defined($tc->{clusterids}) || $tc->{clusterids}->dim(0) != $tc->{ctree}->dim(1)) {
    $tc->{clusterids} = zeroes(long, $tc->{ctree}->dim(1));
  }

  PDL::Cluster::cuttree($tc->{ctree},
			$tc->{nclusters},
			$tc->{clusterids});

  return $tc->{clusterids};
}

##======================================================================
## $pdl = $tc->leafdistances()
## $pdl = $tc->leafdistances($pdl)
##   + populates returns a $k-by-$n pdl representing distances
##     between each (cluster,leaf) pair.
sub leafdistances {
  my ($tc,$pdl) = @_;

  $tc->cluster() if (!defined($tc->{ctree}) || !defined($tc->{linkdist}));
  $tc->cut() if (!defined($tc->{clusterids}));

  my $cids = $tc->{clusterids};
  my $k = $tc->{nclusters};
  my $n = $tc->{data}->dim(1);

  $pdl = $tc->{leafdist} if (!defined($pdl));
  $pdl = zeroes(double,1,1) if (!defined($pdl));
  $pdl->reshape($k, $n) if ($pdl->dim(0) != $k || $pdl->dim(1) != $n);

  my $rowids=sequence(long,$n);
  foreach $cid (0..($k-1)) {
    PDL::Cluster::rowdistances($tc->{data},
			       $tc->{mask},
			       $tc->{weight},
			       $rowids,
			       which($cids==$cid),
			       $pdl->slice("($cid)"),
			       $tc->{dist},
			       $tc->{method});
  }

  return $tc->{leafdist}=$pdl;
}


########################################################################
## Conversion
########################################################################

## (inherited)


########################################################################
## Viewing
########################################################################

##======================================================================
## $tree = $tc->toTree(%args)
##  + returns a MUDL::Tree representing the clusters
##  + %args are passed to MUDL::Tree->fromClusters()
sub toTree {
  my $tc = shift;
  require MUDL::Tree;
  return MUDL::Tree->fromClusterPDL($tc->{ctree},
				    enum=>$tc->{enum},
				    dists=>$tc->{linkdist},
				    groups=>$tc->{clusterids},
				    #dmult=>100,
				    @_);
}


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
