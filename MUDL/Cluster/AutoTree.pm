##-*- Mode: CPerl -*-

## File: MUDL::Cluster::AutoTree.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: clustering method: auto
##  + uses Buckshot for large datasets, otherwise Tree (hierarchical agglomerative)
##======================================================================

package MUDL::Cluster::AutoTree;
use PDL;
#use PDL::GA;
use PDL::Cluster;
use MUDL::Cluster::Method;
use MUDL::Cluster::Buckshot;
use MUDL::Cluster::Tree;
use Carp;

use strict;

our @ISA = qw(MUDL::Cluster::Buckshot);
our @EXPORT_OK = qw();

##======================================================================
## AutoTree clustering: Constructor

## $args = MUDL::Cluster::AutoTree->new(%args);
##   + %args: new in AutoTree
##       nlimit      => $nmax,     # maximum $n for which to use Tree clustering (default=1024)
##       protomethod => $method,   # how to acquire prototypes: see getprotoids(); default=guessed
##       protoweights=> $weights,  # pdl($n): weights for stochastic prototype selection [optional]
##                                 # - if undefined, 'rprobs' key will be tried as well
##                                 #   (see MUDL::Cluster::Method::getcenters() for other uses of 'rprobs')
##
##   + %args: general:
##       data     => $data,     # pdl($d,$n)
##       mask     => $mask,     # pdl($d,$n) boolean-valued matrix: true iff $data->at($i,$j) is valid
##       weight   => $wts,      # pdl($d) $d-ary weight vector
##       nclusters=> $k,        # number of desired clusters     (default=2)
##
##   + %args: prototypes [cluster(), protocluster()]:
##       protos   => $rowids,   # pdl(long,$np) (default=random)
##       nprotos  => $np,       # number of protos (default=rint(sqrt($k*$n)))
##       tree     => $tree,     # a MUDL::Cluster::Tree object for clustering prototypes (default=new)
##       ##-- subdata:
##       {tree}{data}  => $pdata,     # pdl($d,$np): prototype data
##       {tree}{dist}  => $metric,    # distance metric character flag (default='b') : default=$cm->{dist}
##       {tree}{method} => $method,   # treecluster link-method flag   (default='m') : default=$cm->{method}
##
##   + %args: for getcenters():
##       ctrmethod => $cmethod, # see MUDL::Cluster::Method::getcenters()
##       ctrmode   => $cmode,   # 'hard' or 'soft' [default='hard']
##       ctrm      => $m,       # m-best [default=4]
##
##   + %args: attachment [cut(), attach()]:
##       niters    => $niters,  # maximum number of attachment iterations (default=0) --> BUSTED?
##
##   + optional data:
##       enum     => $enum,    # leaf-id enumerator
##       cenum    => $enum,    # cluster-id enumerator
##
##   + additional data:
##     - on cut() [attachment]:
##       clusterids  => $cids,  # pdl(long,   $n)    cluster-id by target index
##       clusterdists=> $cdist, # pdl(double, $n)    distance to best cluster
##       cdata    => $cdata,    # pdl(double, $d,$k) centroid profile data
##       cmask    => $cmask,    # pdl(long,   $d,$k) centroid profile mask
sub new {
  my ($that,%args) = @_;

  my ($ac);
  $ac = $that->SUPER::new(
			  ##
			  ##-- AutoTree new
			  nlimit      => 1024,
			  protomethod => 'auto',
			  protoweights=> undef,
			  ##
			  ##-- general
			  data=>undef,
			  mask=>undef,
			  weight=>undef,
			  nclusters=>2,
			  ##
			  ##-- Buckshot: prototypes
			  protos=>undef,
			  nprotos=>undef,
			  #tree=>MUDL::Cluster::Tree->new(),    ##-- default
			  dist  =>'u',   ##-- uncentered correlation (vector cosine)
			  method=>'a',   ##-- group-average link
			  ##
			  ##-- clusterdistance() flags
			  cddist  =>'u',  ##-- uncentered correlation (vector cosine)
			  cdmethod=>'v',  ##-- group-average
			  ##
			  ##-- Buckshot: centroid acquisition: DO WE REALLY NEED THIS?!
			  ctrmethod => 'mean',
			  ctrm      => 4,   ##-- m-best
			  ctrmode   => 'hard',
			  ##
			  ##-- Buckshot: attachment
			  niters => 0,
			  ##
			  ##-- optional data
			  enum=>undef,
			  cenum=>undef,
			  %args
			 );

  ##-- setup tree
  $ac->{tree}{$_} = $ac->{$_} foreach (grep { $_ ne 'class' && $_ ne 'tree' } keys(%$ac));
  delete($ac->{tree}{data});

  return $ac;
}

##======================================================================
## @keys = $cm->datakeys()
##   + return data-related keys: not copied by shadow(),
##     deleted on set data
sub datakeys {
  my $bc = shift;
  return
    ($bc->SUPER::datakeys,
     ##-- super
     #qw(tree
	#pdata pmask clusterids clusterdists
	#cdata cmask cdmatrix cdweight cdsizes cdelts)
     #qw(tree protos),
     qw(protoweights),
    );

}

## $bc2 = $bc->shadow(%args)
#(inherited)

##======================================================================
## $data = $bc->data()
## $data = $bc->data($data)
##   + get/set data -- reset related pdls on set
#(inherited)

########################################################################
## Utilities: Clustering
########################################################################

##======================================================================
## Utilities: Clustering: prototypes
##======================================================================

##======================================================================
## $pdata = $bc->protodata()
##  + get prototyping data matrix
##  + returns $bc->{tree}{data} if it's defined and has $bc->{nprotos} rows
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
## $pids = $ac->getprotoids()
##  + just gets prototype ids
##  + recognizes the following '$ac->{protomethod}' values:
##     'auto'  : ranks if available, otherwise random-uniform
##     'ranks' : select maximum-ranked 'protoweights' elements
##     'random': uniform random selection
*getprotos = \&getprotoids;
sub getprotoids {
  my $ac = shift;

  die(ref($ac), "::getprotoids(): cowardly refusing to return an empty prototype set")
    if (!defined($ac->{data}) || $ac->{data}->isnull);

  ##-- check whether we need to select prototypes
  my $n = $ac->{data}->dim(1);
  my $k = $ac->{nclusters};
  return sequence(long,$n) if ($n <= $ac->{nlimit}); ##-- nope: tree-mode clustering

  ##-- get number of prototypes
  my $nprotos = $ac->{nprotos};
  $nprotos = sclr(rint(sqrt($k*$n))) if (!defined($nprotos));

  ##-- check for prototype acquisition method
  my $pmethod = $ac->{protomethod}||'auto';
  $pmethod = 'ranks' if ($pmethod eq 'auto');
  if ($pmethod =~ /^rank/ && !defined($ac->{protoweights}) && !defined($ac->{rprobs})) {
    warn(ref($ac),"::getprotoids(): cannot select by ranks without 'protoweights': using random selection");
    $pmethod = 'random';
  }

  my ($ranks);
  if ($pmethod =~ /^rank/) {
    ##-- rank selection
    my $pweights = defined($ac->{protoweights}) ? $ac->{protoweights} : $ac->{rprobs};
    $ranks = $pweights->qsorti; ##-- sort in ascending order (e.g. reversed)
  } else {
    ##-- uniform-random selection
    $ranks = random($n)->qsorti;
  }

  return $ranks->slice("-1:-$nprotos")->qsort; ##-- be nice and sort the returned ids
}

##======================================================================
## $pdata = $ac->getprotodata()
## $pdata = $ac->getprotodata($protos)
## (new)
##  + calls $ac->getprotoids() if $protos and $ac->{protos} are undefined
## (inherited):
##  + initializes prototype data: @$bc{qw(protos nprotos pdata)}
##  + chooses $bc->{nprotos} prototypes uniform-randomly if $bc->{protos} is undefined
##  + $bc->{nprotos} defaults to sqrt($k*$n)
##  + sets @{$bc->{tree}}{data, mask, weight}
sub getprotodata {
  my ($ac,$protos) = @_;
  $protos = $ac->{protos}      if (!defined($protos));
  $protos = $ac->getprotoids() if (!defined($protos));
  $ac->{nprotos} = $protos->nelem;

  ##-- call inherited function to set things up
  return $ac->SUPER::getprotodata($protos);
}


##======================================================================
## $bc = $bc->protocluster(%args)
##  + runs tree clustering algorithm on prototypes
##  + calls protodata() to ensure prototypes have been selected
#(inherited)


##======================================================================
## ($pcdata,$pcmask) = $bc->getprotocenters(%args)
##  + gets prototype centers (top-level)
##  + just dispatches to $bc->{tree} object
#(inherited)


##======================================================================
## Utilities: Clustering: attachment
##======================================================================

## $bc = $bc->attach0(%args)
## (new)
##  + dispatch according to size
## (inherited)
##  + attaches non-prototype data to nearest prototype center
##  + sets %$bc keys:
##     clusterids => $cids, # pdl($n) : as for Cluster::Method
##
sub attach0 {
  my $ac = shift;

  if ($ac->{nprotos}==$ac->{data}->dim(1)) {
    ##-- Tree mode: just copy
    $ac->{clusterids} = pdl(long,$ac->{tree}{clusterids}->nelem);
    $ac->{clusterids}->index($ac->{protos}) .= $ac->{tree}{clusterids};
    return $ac;
  }

  ##-- Buckshot mode
  return $ac->SUPER::attach0(@_);
}


## $bc = $bc->attachN(%args)
## (new)
##  + dispatch method
## (inherited)
##  + attaches all data to nearest center
##  + performs at most $bc->{niters} iterations
##  + required keys:
##      data, mask, weight, ##-- base data
##      clusterids,         ##-- initial cluster assignment
##      ctrXXX,             ##-- centroid data acquisition flags
##      cdmatrix,           ##-- used for fast attachment if present
sub attachN {
  my $ac = shift;
  return $ac if ($ac->{nprotos}==$ac->{data}->dim(1));
  return $ac->SUPER::attachN(@_);
}


########################################################################
## Required Methods: Clustering
########################################################################

##======================================================================
## $bc = $bc->cluster(%args)
##  + actually runs clustering algorithm
##  + calls protocluster(), attach0(), attachN()
#(inherited)


##======================================================================
## $clusterids = $tc->cut()
## $clusterids = $tc->cut($nclusters)
##   + cut tree, returns vector clusterids($n)
##   + may call cluster() if dimensions don't match up correctly


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
