##-*- Mode: CPerl -*-

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
##
##   + %args: general:
##       data     => $data,     # pdl($d,$n)
##       mask     => $mask,     # pdl($d,$n) boolean-valued matrix: true iff $data->at($i,$j) is valid
##       weight   => $wts,      # pdl($d) $d-ary weight vector
##       nclusters=> $k,        # number of desired clusters     (default=2)
##
##   + %args: prototypes [cluster(), protocluster()]:
##       protos   => $rowids,      # pdl(long,$np) (default=random)
##       nprotos  => $np,          # number of protos (default=rint(sqrt($k*$n)))
##       protomethod => $method,   # how to acquire prototypes: see getprotoids(); default=guessed
##       protoweights=> $weights,  # pdl($n): weights for stochastic prototype selection [optional]
##                                 # - if undefined, 'dataweights' key will be tried as well
##                                 #   (see MUDL::Cluster::Method::getcenters() for other uses of 'dataweights')
##       tree     => $tree,        # a MUDL::Cluster::Tree object for clustering prototypes (default=new)
##       ##
##       ##-- subdata:
##       {tree}{data}  => $pdata,     # pdl($d,$np): prototype data
##       {tree}{dist}  => $metric,    # distance metric character flag (default='b') : default=$cm->{dist}
##       {tree}{method} => $method,   # treecluster link-method flag   (default='m') : default=$cm->{method}
##
##   + %args: for getcenters():
##       ctrmethod => $cmethod, # see MUDL::Cluster::Method::getcenters()
##       ctrmode   => $cmode,   # 'hard' or 'soft' [default='hard']
##       ctrm      => $m,       # m-best
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

  my ($bc);
  $bc = $that->SUPER::new(
			  ##
			  ##-- general
			  data=>undef,
			  mask=>undef,
			  weight=>undef,
			  nclusters=>2,
			  ##
			  ##-- prototypes
			  protos=>undef,
			  nprotos=>undef,
			  protomethod=>'random',
			  #protoweights=>undef,

			  ##-- tree clustering object
			  tree=>undef, ##--MUDL::Cluster::Tree->new(),

			  ##-- DISTANCE:OLD
			  #dist  =>'b',   ##-- Manhattan distance
			  #method=>'m',   ##-- maximum link
			  ###
			  ###-- clusterdistance() flags
			  #cddist  =>'u',  ##-- uncentered correlation (vector cosine)
			  #cdmethod=>'v',  ##-- pairwise average
			  ##-- /DISTANCE:OLD

			  ##
			  ##-- centroid acquisition
			  ctrmethod => 'mean',
			  ctrm    => 4,   ##-- m-best
			  ctrmode => 'hard',
			  ##
			  ##-- attachment
			  niters => 0,
			  ##
			  ##-- optional data
			  enum=>undef,
			  cenum=>undef,
			  %args
			 );

  ##-- setup tree
  if (!defined($bc->{tree})) {
    $bc->{tree} = MUDL::Cluster::Tree->new(
					   map  { ($_=>$bc->{$_}) }
					   grep { $_ ne 'class' && $_ ne 'tree' }
					   keys (%$bc)
					  );
  } else {
    $bc->{tree}{$_} = $bc->{$_} foreach (grep { $_ ne 'class' && $_ ne 'tree' } keys(%$bc));
  }
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
  my $data = $bc->SUPER::data(@_); ##-- maybe compute and/or apply SVD
  $bc->{tree} = $tree;
  return $data;
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
## $pdata = $bc->getprotodata()
## $pdata = $bc->getprotodata($protos)
##  + initializes prototype data: @$bc{qw(protos nprotos pdata)}
##  + chooses $bc->{nprotos} prototypes uniform-randomly if $bc->{protos} is undefined
##  + $bc->{nprotos} defaults to sqrt($k*$n)
##  + sets @{$bc->{tree}}{data, mask, weight}
##  + sets $bc->{tree}{rprobs}, if available
sub getprotodata {
  my ($bc,$protos) = @_;
  $protos = $bc->{protos}      if (!defined($protos));
  $protos = $bc->getprotoids() if (!defined($protos));
  $bc->{nprotos} = $protos->nelem;

  my ($np,$k,$d,$n) = (@$bc{qw(nprotos nclusters)}, $bc->{data}->dims);

  ##-- get prototype data
  $bc->{protos}       = $protos;
  $bc->{tree}->data($bc->{data}->dice_axis(1,$protos));
  $bc->{tree}{mask}   = $bc->{mask}->dice_axis(1,$protos) if (defined($bc->{mask}));
  $bc->{tree}{weight} = $bc->{weight};
  $bc->{tree}{dataweights} = $bc->{dataweights}->index($protos) if (defined($bc->{dataweights}));

  return $bc->{tree}{data};
}

##======================================================================
## $proto_ids = $bc->getprotoids()
##  + gets & returns prototype ids
##  + recognizes the following '$bc->{protomethod}' values:
##     'auto'  : ranks if available, otherwise random-uniform
##     'ranks' : select maximum-ranked 'protoweights' elements
##     'random': uniform random selection
*getprotos = \&getprotoids;
sub getprotoids {
  my $bc = shift;

  die(ref($bc), "::getprotoids(): cowardly refusing to return an empty prototype set")
    if (!defined($bc->{data}) || $bc->{data}->isnull);

  ##-- check whether we need to select prototypes
  my $n = $bc->{data}->dim(1);
  my $k = $bc->{nclusters};
  #return sequence(long,$n) if ($n <= $bc->{nlimit}); ##-- nope: tree-mode clustering: AutoTree only!

  ##-- get number of prototypes
  my $nprotos = $bc->{nprotos};
  $nprotos = sclr(rint(sqrt($k*$n))) if (!defined($nprotos));

  ##-- check for prototype acquisition method
  my $pmethod = $bc->{protomethod}||'auto';
  #$pmethod = 'ranks' if ($pmethod eq 'auto');
  if (!defined($bc->{protoweights}) && !defined($bc->{dataweights})) {
    warn(ref($bc),"::getprotoids(): cannot select by ranks without 'protoweights': using random selection")
      if ($pmethod =~ /^rank/);
    $pmethod = 'random';
  } else {
    $pmethod = 'ranks' if ($pmethod eq 'auto');
  }

  my ($ranks);
  if ($pmethod =~ /^rank/) {
    ##-- rank selection
    my $pweights = defined($bc->{protoweights}) ? $bc->{protoweights} : $bc->{dataweights};
    $ranks = $pweights->qsorti; ##-- sort in ascending order (e.g. reversed)
  } else {
    ##-- uniform-random selection
    $ranks = random($n)->qsorti;
  }

  return $ranks->slice("-1:-$nprotos")->qsort; ##-- be nice and sort the returned ids
}



##======================================================================
## $bc = $bc->protocluster(%args)
##  + runs tree clustering algorithm on prototypes
##  + calls protodata() to ensure prototypes have been selected
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
## ($tpdata,$tpmask,$tpcids) = $bc->getprotoprofile(%args)
##  + gets prototype trimmed profile (top-level)
##  + just dispatches to $bc->{tree} object
sub getprotoprofile {
  my $bc = shift;
  $bc->protocluster() if (!defined($bc->{tree}{clusterids}));

  return @{$bc->{tree}}{qw(tpdata tpmask tpcids)}
    if (defined($bc->{tree}{tpdata}) && defined($bc->{tree}{tpmask}) && defined($bc->{tree}{tpcids}));

  return $bc->{tree}->getprofile(@_);   ##-- dispatch to {tree} object
}


##======================================================================
## Utilities: Clustering: attachment
##======================================================================

##======================================================================
## $bc = $bc->attach0(%args)
##  + attaches non-prototype data to nearest prototype center
##  + sets %$bc keys:
##     clusterids => $cids, # pdl($n) : as for Cluster::Method
sub attach0 {
  my $bc = shift;

  ##-- get prototype centroid data
  my ($tpdata,$tpmask,$tpcids) = $bc->getprotoprofile(@_);

  ##-- get attachment targets
  my ($d,$n,$k) = ($bc->{data}->dims, $bc->{nclusters});
  my $atgmask = ones(byte,$n);
  $atgmask->index($bc->{protos}) .= 0;
  my $atgids = $atgmask->which;
  my $natgs  = $atgids->nelem;

  ##-- call superclass attach()
  my ($acids,$acdist) = $bc->SUPER::attach(rowids=>$atgids,
					   mask=>$bc->{mask},
					   tpdata=>$tpdata,
					   tpmask=>$tpmask,
					   tpcids=>$tpcids,
					   @_,
					  );

  ##-- get grand total output
  my $cids = $bc->{clusterids} = zeroes(long,$n);
  $cids->index($bc->{protos}) .= $bc->{tree}{clusterids};
  $cids->index($atgids)       .= $acids;

  ##-- clear cache
  delete($bc->{tpcdmatrix});

  return $bc;
}


##======================================================================
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

  ##-- target data
  my $atgids = sequence(long,$bc->{data}->dim(1));

  ##-- output data
  my $cids_1 = $bc->{clusterids}; ##-- previous assignment, to check for changes
  #my $cdist  = zeroes(double, $atgids->nelem);
  #my $cids   = pdl($cids_1);
  my ($cids,$cdist);

  my ($i);
  for ($i=0; $niters>0 && $i<$niters; $i++) {
    ($cids,$cdist) = $bc->attach(@_, rowids=>$atgids);  ##-- underlying attach()

    last if (all($cids==$cids_1));     ##-- check for changes
    $bc->{clusterids} = $cids;         ##-- update {clusterids} flag
    ($cids_1,$cids) = ($cids,$cids_1); ##-- swap temps

    ##-- invalidate data depending on previous cluster assignment
    $bc->flushCache();
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
