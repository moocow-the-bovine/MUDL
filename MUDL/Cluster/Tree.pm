#-*- Mode: Perl -*-

## File: MUDL::Cluster::Tree.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: hierarchical clustering
##======================================================================

package MUDL::Cluster::Tree;
use PDL;
use PDL::Cluster;
use MUDL::Object;
use MUDL::Tk::Dendogram;
use Carp;

our @ISA = qw(MUDL::Object);
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
##   + additional data (after running):
##       tree     => $ctree,   # pdl(2,$n) gives structure of clustering tree (see below) [(2,n-1) used]
##       linkdist => $vector,  # pdl($n) array giving distances between sister nodes [(n-1) used]
##   + where:
##       $n : number of data instances (rows)
##       $d : number of features per datum (columns)
##   + tree structure:
##       [ $node(0), $node(1), ..., $node($N-1) ]
##   + node structure:
##       [ $dtr1, $dtr2 ]
##     - $dtr1, $dtr2 are integers
##     - ($dtri >= 0) refers to datum $dtri
##       ~ i.e. leaf nodes have nonnegative ids numbered from 0..($n-1)
##     - ($dtri < 0) refers to $node( -($dtri+1) )
##       ~ i.e. nonterminal nodes have negative ids, and are numbered from -1 to -($n-1)
##   + distance structure:
##     - $dists->at($i) is the distance between nodes merged in $ctree->at($i)
##   + methods:
##       's' : single-link
##       'm' : maximum- (complete-) link
##       'a' : average-link (default)
##       'c' : centroid-link
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
  my $tc = $_[0]->SUPER::new(
			     data=>null,
			     mask=>undef,
			     weight=>undef,
			     method=>'a',
			     dist=>'e',
			     ##-- output data
			     ctree=>undef,
			     linkdist=>undef,
			     @_[1..$#_]
			    );

  return $tc;
}


##======================================================================
## $data = $tc->data()
## $data = $tc->data($data)
##   + get/set data -- reset related pdls on set
sub data {
  my $tc = shift;
  return $tc->{data} if (!@_);

  my $data = $tc->{data} = shift;

  ##-- sanity checks
  delete($tc->{mask})
    if (defined($tc->{mask}) && ($tc->{mask}->dim(0) != $data->dim(0)
				 ||
				 $tc->{mask}->dim(1) != $data->dim(1)));

  delete($tc->{weight})
    if (defined($tc->{weight}) && $tc->{weight}->dim(0) != $data->dim(0));

  delete($tc->{ctree})
    if (defined($tc->{ctree}) && $tc->{ctree}->dim(1) != $data->dim(1));

  delete($tc->{linkdist})
    if (defined($tc->{linkdist}) && $tc->{linkdist}->dim(0) != $data->dim(1));

  return $data;
}


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
## $tree = $tc->toTree(%args)
##  + returns a MUDL::Tree representing the clusters
##  + %args are passed to MUDL::Tree->toTree(), fromClusters()
sub toTree {
  my $tc = shift;
  require MUDL::Tree;
  return MUDL::Tree->fromClusterPDL($tc->{ctree},
				    enum=>$tc->{enum},
				    dists=>$tc->{linkdist},
				    @_);
}

##======================================================================
## $tree = $tc->viewTree(%args)
##  + view a tree
##  + %args are passed to MUDL::Tree->toTree(), fromClusters()
sub viewTree {
  my $tc = shift;
  my $t = $tc->toTree(@_)->view(@_);
  return $t;
}


##======================================================================
## $dg = $tc->toDendogram(%args)
##  + get a dendogram of the clustering results
##  + %args are passed to MUDL::Tk::Dendogram->new()
sub toDendogram {
  my $tc = shift;
  return $tc->toTree(@_)->toDendogram(@_);
}

##======================================================================
## undef = $tc->view(%args)
##  + view a dendogram of the clustering results
##  + %args are passed to MUDL::Tk::Dendogram->new()
sub view {
  $_[0]->toDendogram(@_[1..$#_])->view;
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
