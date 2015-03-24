##-*- Mode: CPerl -*-

## File: MUDL::Cluster::Tree.pm
## Author: Bryan Jurish <moocow@cpan.org>
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
##       data     => $data,     # double ($d,$n) : data
##       mask     => $mask,     # long   ($d,$n) : boolean matrix: true iff $data->at($i,$j) is valid [default=$data->isgood]
##       weight   => $wts,      # double ($d)    : weight vector (weights distances)
##   + (DISTANCE:OLD) for clusterdistance():
##       cddist   => $cddist,   # clusterdistance() dist   flag (default={dist})     : for clusterdistance()
##       cdmethod => $cdmethod, # clusterdistance() method flag (default=from method): for clusterdistance()
##                              # - may contain suffix '+b' to indicate bonus clustering
##   + (DISTANCE:NEW) for distance computations:
##       distf    => $distf,    # MUDL::Cluster::Distance object (cached)
##       dclass   => $dclass,   # as accepeted by the 'class' argument to MUDL::Cluster::Disance()
##       cdbonus  => $bool,     # whether to apply hard-clustering bonus (bash distance to zero)
##                              # - (NEW) bonus distance is applied if $cdbonus is a true value (default=1)
##                              # - (OLD) used to require that also ($cdmethod =~ /\+b/)
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
			     #dist=>'b',	##-- Manhattan distance
			     #method=>'m', ##-- maximum link
			     #cddist=>'u', ##-- uncentered correlation (vector cosine)
			     #cdmethod=>'v', ##-- pairwise average
			     ##-- output data
			     ctree=>undef,
			     linkdist=>undef,
			     ##-- for cut
			     nclusters=>2,
			     %args
			    );
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

  ##-- common args
  my $data   = $tc->{data};
  my $mask   = defined($tc->{mask})     ? $tc->{mask}     : ($tc->{mask}=$data->isgood);
  my $weight = defined($tc->{weight})   ? $tc->{weight}   : ($tc->{weight}=ones(double,$data->dim(0)));
  my $ctree  = defined($tc->{ctree})    ? $tc->{ctree}    : ($tc->{ctree}=zeroes(long,2,$data->dim(1)));
  my $linkd  = defined($tc->{linkdist}) ? $tc->{linkdist} : ($tc->{linkdist}=zeroes(double,$data->dim(1)));
  my $distf  = $tc->distance();

  my ($pcfunc); ##-- underlying PDL::Cluster function name, for error reporting

  if (UNIVERSAL::isa($distf,'MUDL::Cluster::Distance::Builtin')) {
    ##-- builtin distance function: direct call to PDL::Cluster::treecluster()
    $pcfunc = 'treecluster';
    PDL::Cluster::treecluster($data,$mask,$weight, $ctree,$linkd, $distf->distFlag,$distf->tcLinkFlag);
  }
  else {
    ##-- perl distance function: get our own distance matrix
    $pcfunc     = 'treeclusterd';
    my $dmatrix = $distf->distanceMatrix(data=>$data, mask=>$mask, weight=>$weight);
    PDL::Cluster::treeclusterd($data,$mask,$weight, $dmatrix, $ctree,$linkd, '?',$distf->tcLinkFlag);
  }

  ##-- sanity check
  confess(ref($tc), "::cluster() -- PDL::Cluster::${pcfunc}() returned zero matrix: something went wahooni-shaped")
    if (!$ctree->any);

  ##-- update size flags
  @$tc{'nfeatures','ndata'} = $tc->{data}->dims;

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
