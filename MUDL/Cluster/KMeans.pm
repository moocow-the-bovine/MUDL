#-*- Mode: Perl -*-

## File: MUDL::Cluster::Kmeans.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner
##======================================================================

package MUDL::Cluster::KMeans;
use PDL;
use PDL::Cluster;
use MUDL::Cluster::Method;
use Carp;

our @ISA = qw(MUDL::Cluster::Method);
our @EXPORT_OK = qw();

##======================================================================
## K-Means clustering: Constructor

## $args = KMeans->new(%args);
##   + %args:
##       data     => $data,    # pdl($d,$n)
##       nclusters=> $k,       # number of desired clusters (default=2)
##       npass    => $npasses, # number of full k-means passes (default=1)
##       initialid=> $ids,     # pdl($n): initial cluster ids ($n-ary vector, values in [0..($k-1)])
##                             # - if given, you should also specifiy npass=>0 (single pass only)
##       mask     => $mask,    # pdl($d,$n) boolean-valued matrix: true iff $data->at($i,$j) is missing
##       weight   => $wts,     # pdl($d) $d-ary weight vector
##       dist     => $metric,  # distance metric character flag (default='e')
##       method   => $method,  # center computation method flag (default='a')
##   + additional data (after running):
##       clusterids => $ids,     # pdl($n): $n-ary vector, values in [0..($k-1)], gives cluster assignment
##       error      => $error,   # within-cluster sum of distances of the "optimal" solution found
##       nfound     => $nfound,  # number of times the "optimal" solution was found
##   + where:
##       $n : number of data instances (rows)
##       $d : number of features per datum (columns)
##   + methods:
##       'a' : arithmetic mean (default)
##       'm' : median
##   + metrics:
##       'c' : correlation
##       'a' : abs(correlation)
##       'u' : uncentered correlation
##       'x' : abs(uncentered correlation)
##       's' : Spearman's rank correlation
##       'k' : Kendalls tau
##       'e' : Euclidean distance
##       'b' : city-block (L1) distance
sub new {
  my $km = $_[0]->SUPER::new(
			     data=>null,
			     nclusters => 2,
			     npass=>1,
			     initialid=>undef,
			     mask=>undef,
			     weight=>undef,
			     dist=>'e',
			     method=>'a',
			     ##-- output data
			     clusters=>undef,
			     error=>undef,
			     nfound=>0,
			     @_[1..$#_]
			    );

  if (defined($km->{initialid})) {
    $km->{clusters} = $km->{initialid};
    delete($km->{initialid});
  }

  return $km;
}

##======================================================================
## @keys = $cm->datakeys()
##   + return data-related keys: not copied by shadow(),
##     deleted on set data
sub datakeys {
  my $cm = shift;
  return ($cm->SUPER::datakeys,
	  qw(centroids cmask));
}

##======================================================================
## $data = $km->data()
## $data = $km->data($data)
##   + resets data and related elements
##
##-- (inherited)


##======================================================================
## $ids = $km->initialid()
## $ids = $km->initialid($ids)
##   + get/set initial ids
*clusters = *initialids = *clusterids = \&initialid;
sub initialid {
  my $km = shift;
  return $km->{clusterids} if (!@_);
  return $km->{clusterids} = shift;
}


##======================================================================
## $km = $km->cluster(%args)
##  + actually runs clustering alg
##  + adopts %args into %$km
sub cluster {
  my ($km,%args) = @_;
  @$km{keys(%args)} = values(%args);

  PDL::Cluster::kcluster
      (
       $km->{nclusters},
       $km->{data},
       (defined($km->{mask})       ? $km->{mask}       : ($km->{mask}=ones(long,$km->{data}->dims))),
       (defined($km->{weight})     ? $km->{weight}     : ($km->{weight}=ones(double,$km->{data}->dim(0)))),
       (defined($km->{npass})      ? $km->{npass}      : ($km->{npass}=1)),
       (defined($km->{clusterids}) ? $km->{clusterids} : ($km->{clusterids}=zeroes(long,$km->{data}->dim(1)))),
       ($km->{error}=null),
       ($km->{nfound}=null),
       (defined($km->{dist})       ? $km->{dist}       : ($km->{dist}='b')),
       (defined($km->{method})     ? $km->{method}     : ($km->{method}='v')),
      );

  return $km;
}

##======================================================================
## $clusterids = $km->cut()
## $clusterids = $km->cut($nclusters)
##   + cut tree, returns vector clusterids($n)
sub cut {
  my ($km,$nclusters) = @_;
  $km->{nclusters} = $nclusters if (defined($nclusters));
  $km->{nclusters} = 2 if (!defined($km->{nclusters}));

  if (!defined($km->{clusterids})
      || $km->{clusterids}->dim(0) != $km->{data}->dim(1)
      || $km->{clusterids}->max    != $km->{nclusters}-1)
    {
      $km->cluster();
    }

  return $km->{clusterids};
}

##======================================================================
## Utilities: cluster <-> datum distances
##======================================================================

## (inherited)

##======================================================================
## Utilities: m-best (min|max)
##======================================================================

## (inherited)

##======================================================================
## Utilities: get centroid data
##======================================================================

## (inherited)

##======================================================================
## Utilities: distance-to-probability
##======================================================================

## (inherited)


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
