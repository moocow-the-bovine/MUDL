#-*- Mode: Perl -*-

## File: MUDL::Cluster::Tree.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: hierarchical clustering
##======================================================================

package MUDL::Cluster::Tree;
use Algorithm::Cluster qw(treecluster);
use MUDL::Object;
use MUDL::Tk::Dendogram;
use Carp;

our @ISA = qw(MUDL::Object);
our @EXPORT_OK = qw();

##======================================================================
## Hierartchical clustering: Constructor

## $args = MUDL::Cluster::Tree->new(%args);
##   + %args:
##       data     => \@data,   # 2d array ref (matrix), $n-by-$d
##       mask     => \@mask,   # either '' or $n-by-$d boolean-valued matrix: true iff $data->[$i][$j] is missing
##       weight   => \@wts,    # either '' or $d-ary weight vector
##       tranpose => $bool,    # whether $data is row-primary (0,default) or column-primary (1)
##       dist     => $metric,  # distance metric character flag (default='e')
##       method   => $method,  # center computation method flag (default='a')
##   + additional data (after running):
##       tree     => \@tree,   # ($n-1)-by-2 array giving structure of clustering tree (see below)
##       linkdist => \@dists,  # ($n-1)-ary array giving distances between sister nodes
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
##     - $dists[$i] is the distance between nodes merged in $tree[$i]
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
			     data=>[[]],
			     mask=>'',
			     weight=>'',
			     transpose=>0,
			     method=>'a',
			     dist=>'e',
			     ##-- output data
			     tree=>undef,
			     linkdist=>undef,
			     @_[1..$#_]
			    );

  return $tc;
}


##======================================================================
## $tc = $tc->cluster(%args)
##  + actually runs clustering algorithm
sub cluster {
  my ($tc,%args) = @_;
  @$tc{keys(%args)} = values(%args);
  @$tc{qw(tree linkdist)} = Algorithm::Cluster::treecluster(%$tc);
  return $tc;
}


##======================================================================
## $dg = $tc->toDendogram(%args)
##  + get a dendogram of the clustering results
##  + %args are passed to MUDL::Tk::Dendogram->new()
sub toDendogram {
  return MUDL::Tk::Dendogram->new(tree=>$_[0]{tree},
				  dist=>$_[0]{linkdist},
				  @_[1..$#_]);
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
