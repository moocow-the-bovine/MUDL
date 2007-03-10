##-*- Mode: CPerl -*-

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

use strict;

our @ISA = qw(MUDL::Cluster::Method);
our @EXPORT_OK = qw();

##======================================================================
## Hierarchical clustering: Constructor

## $args = MUDL::Cluster::Tree->new(%args);
##   + %args:
##       data     => $data,     # pdl($d,$n)
##       mask     => $mask,     # pdl($d,$n) boolean matrix: true iff $data->at($i,$j) is valid [default=!$data->isbad]
##       weight   => $wts,      # pdl($d) $d-ary weight vector
##       dist     => $metric,   # distance metric character flag (default='b')
##       method   => $method,   # treecluster link-method flag (default='m')
##       cddist   => $cddist,   # clusterdistance() dist   flag (default='u')
##       cdmethod => $cdmethod, # clusterdistance() method flag (default='v')
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
##     - post-():
##         #leafdist   => $leaf2cid2dist, # pdl($k,$n) maps (clusterid,leaf) to distance(leaf,clusterid)
##         # ^-- OBSOLETE: now use 'cdmatrix' (same thing)
##   + where:
##       $n : number of data instances (rows)
##       $d : number of features per datum (columns)
##   + methods, metrics, structures: see PDL::Cluster and cluster-3.0 documentation
sub new {
  my ($that,%args) = @_;
  my $tc = $that->SUPER::new(
			     data=>undef,
			     mask=>undef,
			     weight=>undef,
			     dist=>'b',	##-- Manhattan distance
			     method=>'m', ##-- maximum link
			     cddist=>'u', ##-- uncentered correlation (vector cosine)
			     cdmethod=>'v', ##-- pairwise average
			     ##-- output data
			     ctree=>undef,
			     linkdist=>undef,
			     ##-- for cut
			     nclusters=>2,
			     %args
			    );
#  print STDERR
#    ("<<DEBUG>> ", __PACKAGE__, "::new() got args:\n",
#     (map {
#       "\t '$_' => '$args{$_}'\n"
#     } sort(keys(%args))),
#    );

  return $tc;
}

##======================================================================
## @keys = $cm->datakeys()
##   + return data-related keys: not copied by shadow(),
##     deleted on set data
sub datakeys {
  my $cm = shift;
  return ($cm->SUPER::datakeys,
	  qw(ctree linkdist),
	 );
}

##======================================================================
## @keys = $cm->cachekeys()
##   + return cache-related keys: cleared on 'flushCache()'
##
## (inherited)

##======================================================================
## $tree2 = $tree->shadow(%args)
##   + return a new tree of same type:
##
## (inherited)


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
     (defined($tc->{mask})     ? $tc->{mask}     : ($tc->{mask}=!$tc->{data}->isbad)),
     (defined($tc->{weight})   ? $tc->{weight}   : ($tc->{weight}=ones(double,$tc->{data}->dim(0)))),

     (defined($tc->{ctree})    ? $tc->{ctree}    : ($tc->{ctree}=zeroes(long,2,$tc->{data}->dim(1)))),
     (defined($tc->{linkdist}) ? $tc->{linkdist} : ($tc->{linkdist}=zeroes(double,$tc->{data}->dim(1)))),

     (defined($tc->{dist})     ? $tc->{dist}     : ($tc->{dist}='e')),
     (defined($tc->{method})   ? $tc->{method}   : ($tc->{method}='a')),
    );

  ##-- sanity check
  confess(ref($tc), "::cluster() -- treecluster() returned zero matrix: something went wahooni-shaped")
    if (all($tc->{ctree}==zeroes(long,$tc->{ctree}->dims)));

  ##-- update size flags
  @$tc{qw(nfeatures ndata)} = $tc->{data}->dims;

  return $tc;
}

##======================================================================
## $clusterids = $tc->cut()
## $clusterids = $tc->cut($nclusters)
##   + cut tree, returns vector clusterids($n)
sub cut {
  my ($tc,$nclusters) = @_;

  ##-- ensure clustered
  $tc->cluster() if (!defined($tc->{ctree}));

  ##-- cut: params
  $tc->{nclusters} = $nclusters if (defined($nclusters));
  $tc->{nclusters} = 2 if (!defined($tc->{nclusters}));

  if (!defined($tc->{clusterids}) || $tc->{clusterids}->dim(0) != $tc->{ctree}->dim(1)) {
    $tc->{clusterids} = zeroes(long, $tc->{ctree}->dim(1));
  }
  
  print STDERR
      ("<<<DEBUG>>>: ", ref($tc),
       "::cut(): cutting tree into $tc->{nclusters} clusters\n"
      );
  PDL::Cluster::cuttree($tc->{ctree},
			$tc->{nclusters},
			$tc->{clusterids});

  return $tc->{clusterids};
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
  if (defined($tc->{ctree})) {
    return MUDL::Tree->fromClusterPDL($tc->{ctree},
				      enum=>$tc->{enum},
				      dists=>$tc->{linkdist},
				      groups=>$tc->{clusterids},
				      #dmult=>100,
				      @_);
  }
  return $tc->SUPER::toTree(@_);
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
