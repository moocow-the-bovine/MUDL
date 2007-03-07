##
## File: MUDL::Cluster::Method.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: clustering methods
##======================================================================

package MUDL::Cluster::Method;
use PDL;
use PDL::Cluster;
use MUDL::Object;
use MUDL::CmdUtils qw();
use Carp;

use strict;

our @ISA = qw(MUDL::Object);
our @EXPORT_OK = qw();

##======================================================================
## Generic Clustering Method : Constructor

## $cm = MUDL::Cluster::Method->new(%args);
##   + %args:
##       class   => $classname, # string: class-name or MUDL::Cluster:: suffix
##   + data %args:
##       data     => $data,    # pdl($d,$n)
##       nclusters=> $k,       # number of desired clusters
##       ndata    => $n,       # number of data elements
##       nfeatures=> $d,       # number of features
##   + svd %args:
##       svd      => $svd,     # a MUDL::SVD object (optional), called on data($data)
##       svdr     => $r,       # number of reduced dimensions (==$svd->{r})
##       svd_save => $bool,    # whether to try and re-use same SVD for different data()
##   + for clusterdistance():
##       cddist   => $cddist,   # clusterdistance() dist   flag (default={dist})     : for clusterdistance()
##       cdmethod => $cdmethod, # clusterdistance() method flag (default=from method): for clusterdistance()
##                              # - may contain suffix '+b' to indicate bonus clustering
##       cdbonus  => $bool,     # whether to apply hard-clustering bonus (bash distance to zero)
##                              # - bonus distance is only applied if:
##                              #     $cdmethod =~ /\+b/
##                              #   AND
##                              #     $cdbonus is a true value (default=true)
##   + for getcenters():
##       ctrmethod => $cmethod, # see getcenters()
##       ctrmode   => $cmode,   # 'hard' or 'soft' [default='hard']
##       ctrm      => $m,       # m-best
##   + optional data:
##       enum     => $enum,    # leaf-id enumerator
##       cenum    => $enum,    # cluster-id enumerator
##   + additional data:
##     - post-cluster():
##         #tree     => $ctree,   # pdl(2,$n) gives structure of clustering tree (see below) [(2,n-1) used]
##         #linkdist => $vector,  # pdl($n) array giving distances between sister nodes [(n-1) used]
##     - post-cut($k):
##         nclusters  => $k,                # number of clusters
##         clusterids => $rowid2clusterid,  # pdl($n) maps data rows to cluster-id (range 0..($k-1))
##     - post-leafdistances():
##         cdmatrix   => $cdmatrix, # pdl($k,$n) maps (clusterid,leaf) to distance(leaf,clusterid)
##   + where:
##       $n : number of data instances (rows)
##       $d : number of features per datum (columns)
##   + methods, metrics, structures: see PDL::Cluster and cluster-3.0 documentation
sub new {
  my ($that,%args) = @_;

  ##-- optional class argument: dispatch
  if (!ref($that) && exists($args{class})) {
    $that = $args{class};
    delete($args{class});
    $that = "MUDL::Cluster::$that" if ($that !~ /::/);
    MUDL::CmdUtils::loadModule($that);
    return $that->new(%args);
  }

  ##-- svd
  if ($args{svdr}) {
    my $r = $args{svdr};
    my $svd = $args{svd};
    if (!defined($svd)) {
      require MUDL::SVD;
      $svd = MUDL::SVD->new(r=>$r);
    } else {
      $svd->{r} = $r;
    }
    delete($args{svdr});
    $args{svd} = $svd;
  }

  my $cm = $that->SUPER::new(
			     data=>undef,
			     ##-- svd data
			     svd=>undef,
			     svd_save=>0,
			     ##-- flags
			     dist=>'b',
			     method=>'m',
			     ##-- clusterdistance() flags
			     cddist => 'u',
			     cdmethod => 'v',
			     cdbonus => 1, ##-- hard-clustering bonus
			     ##-- output data
			     clusterids=>undef,
			     nclusters=>2,
			     %args,
			    );
  $cm->data($args{data}) if (defined($args{data}));

#  print STDERR "<<DEBUG>> ", __PACKAGE__, "::new() returning nclusters=$cm->{nclusters}\n";

  return $cm;
}


##======================================================================
## @keys = $cm->datakeys()
##   + return data-related keys: not copied by shadow(),
##     deleted on set data
sub datakeys {
  my $cm = shift;
  return
    (qw(data mask weight ctree linkdist clusterids),  #leafdist
     qw(csizes celtmask celts cdmatrix cweights),
     qw(rprobs r2cprobs beta),
     qw(protos protoweights),
     #qw(svd),
    );
}

##======================================================================
## $data = $cm->data()
## $data = $cm->data($data)
##   + get/set data -- reset related pdls on set
sub data {
  my $cm = shift;
  return $cm->{data} if (!@_);

  my $data = shift;

  ##-- sanity check
  if (defined($cm->{data})) {
    confess(ref($cm), "::data(): cowardly refusing bad data!")
      if ($data->nbad > 0);
  }

  ##-- consistency paranoia
  delete(@$cm{grep { $_ ne 'data' } $cm->datakeys});

  ##-- svd
  if (defined($cm->{svd}) && $cm->{svd}{r} && $cm->{svd}{r} < $data->dim(0)) {
    require MUDL::SVD;
    $cm->{svd}->compute($data) if (!defined($cm->{svd}{v})                   ##-- new SVD
				   || $data->dim(0) != $cm->{svd}{v}->dim(1) ##-- dimension mismatch
				   || !$cm->{svd_save}                       ##-- user request
				  );
    #$data = $cm->{svd}{u};
    $data = $cm->{svd}->apply($data);
  }

  ##-- update dimensional args
  @$cm{qw(nfeatures ndata)} = $data->dims;

  return $cm->{data} = $data;
}

##======================================================================
## $cm2 = $cm->shadow()
##   + return a new clustering object of same type:
##     ~ no data
##     ~ same distance-metric, link-method, nclusters, etc.
##     ~ copied enum, if present
##   + %args are passed to ref($cm)->new();
sub shadow {
  my ($cm1,%args) = @_;
  my %nocopy = map {$_=>undef} ($cm1->datakeys,keys(%args));
  return ref($cm1)->new(
			(map {
			  ($_=>$cm1->{$_})
			} grep { !exists($nocopy{$_}) } keys(%$cm1)),
			(map {
			  (defined($cm1->{$_})
			   ? ($_=>$cm1->{$_}->copy)
			   : qw())
			} grep { !exists($nocopy{$_}) } qw(enum)),
			%args,
		       );
}


##======================================================================
## $cm = $cm->cluster(%args)
##  + actually runs clustering algorithm
*cluster = MUDL::Object::dummy('cluster');


##======================================================================
## $clusterids = $cm->cut()
## $clusterids = $cm->cut($nclusters)
##   + cut tree, returns vector clusterids($n)
*cut = MUDL::Object::dummy('cut');


##======================================================================
## $pdl = $cm->leafdistances()
## $pdl = $cm->leafdistances($pdl)
##   + populates & returns a $k-by-$n pdl representing distances
##     between each (cluster,leaf) pair.
##   + sets $cm->{leafdist}=$pdl
##   + now just a wrapper for clusterDistanceMatrix() : OBOSLETE!
*leafdistances = MUDL::Object::dummy('leafdistances');

#*leafdistances = MUDL::Object::dummy('leafdistances');
#sub leafdistances { return shift->clusterDistanceMatrix(@_); }
#sub leafdistances {
#  my ($cm,$pdl) = @_;
#  my $cdm = $cm->clusterDistanceMatrix();
#  #return $cm->{leafdist} = $cdm; ##-- REDUNDANT
#  return $cdm;
#}

########################################################################
## Attachment
########################################################################

## ($cids, $cdists, @other) = $cm->attach(%args)
##  + %args:
##      data=>$data,     # double($d,$n2) (default: $cm->data() [implied svd application])
##      mask=>$mask,     # long($d,$n2)   (default: !$data->isbad)
##      rowids=>$rowids  # long($nrows)   (default: sequence($n2))
##      cdata=>$cdata,   # double($d,$k)  (default: $cm->getcenters)
##      cmask=>$cmask,   # long($d,$k)    (default: $cm->getcenters)
##      cddist=>$cdd,    # as for getcenters()  (default: $cm->cddist)
##      cdmethod=>$cdm,  # as for getcenters()  (default: $cm->cdmethod)
##  + attaches $rowids rows of $data to nearest cluster center,
##    as returned by $cm->getcenters()
sub attach {
  my ($cm,%args) = @_;

  ##-- arg parsing
  my ($data,$mask,$rowids,$cdata,$cmask) = @args{qw(data mask rowids cdata cmask)};
  delete @args{qw(data mask rowids cdata cmask)};
  @$cm{keys %args} = values %args;

  ##-- defaults
  if (!defined($data)) {
    $data   = $cm->{data};
  } elsif (defined($cm->{svd}) && $cm->{svd}{r} && $cm->{svd}{r} < $data->dim(0)) {
    ##-- apply svd
    require MUDL::SVD;
    $data = $cm->{svd}->apply($data);
    $mask = !$data->isbad;
  }
  $mask   = !$data->isbad if (!defined($mask));
  $rowids = sequence(long, $data->dim(1)) if (!defined($rowids));
  if (!defined($cdata)) {
    my @ctrs = $cm->getcenters();
    $cdata = shift(@ctrs);
    $cmask = shift(@ctrs) if (!defined($cmask));
  }

  ##-- output data
  my $nrows = $rowids->nelem;
  my $cids  = zeroes(long,$nrows);
  my $cdist = zeroes(double,$nrows);

  ##-- attachment
  attachtonearest($data,$mask,
		  (defined($cm->{weight}) ? $cm->{weight} : ($cm->{weight}=ones(double,$data->dim(0)))),
		  $rowids,
		  $cdata, $cmask,
		  $cids,  $cdist,
		  $cm->cddist, $cm->cdmethod);

  ##-- return
  return ($cids,$cdist);
}

########################################################################
## Utilities
########################################################################


##======================================================================
## Utilities: cluster <-> datum distances
##======================================================================

## $cdmethod = $cm->cdmethod()
##   + gets {cdmethod} member if defined, otherwise tranlsates {method} flag
sub cdmethod {
  my $cm=shift;
  return $cm->{cdmethod} if ($cm->{cdmethod});
  my $method = $cm->{method} ? $cm->{method} : 'a';
  if    ($method =~ /^f(.*)/) { return "a$1"; }
  elsif ($method =~ /^c(.*)/) { return "m$1"; }
  elsif ($method =~ /^s(.*)/) { return "s$1"; }
  elsif ($method =~ /^m(.*)/) { return "x$1"; }
  elsif ($method =~ /^a(.*)/) { return "v$1"; }
  else {
    croak(ref($cm), "::cdmethod(): no clusterdistance() equivalent known for method='$method'!");
  }
}

## $cddist = $cm->cddist()
##   + gets {cddidst} member if defined, otherwise returns {dist}, else 'a'
sub cddist {
  my $cm = shift;
  return (defined($cm->{cddist})
	  ? $cm->{cddist}
	  : (defined($cm->{dist})
	     ? $cm->{dist}
	     : 'a'));
}


## $csizes = $cm->clusterSizes(%args);
##   + sets $cm->{csizes}=$csizes # pdl(long, $k) : number of elts per cluster
##   + requires:
##      $cm->{clusterids}
sub clusterSizes {
  my $cm = shift;
  return $cm->{csizes} if (defined($cm->{csizes}));
  $cm->{csizes} = zeroes(long,$cm->{nclusters});
  clustersizes($cm->{clusterids}, $cm->{csizes});
  return $cm->{csizes};
}

## $cemask = $cm->clusterElementMask(%args)
##  + %args: (none)
##  + boolean cluster-element mask $cemask, sets $cm->{celtmask}=$cemask
##   + requires:
##      $cm->{clusterids}
##  + output:
##    - $cemask: pdl(byte, $k, $n) : true iff $row_n \in $cluster_k
sub clusterElementMask {
  my $cm = shift;
  return $cm->{celtmask} if (defined($cm->{celtmask}));
  #$cm->{celtmask} = zeroes(byte, $cm->{nclusters}, $cm->{ndata});
  $cm->{celtmask} = zeroes(byte, $cm->{nclusters}, $cm->{clusterids}->dim(0));
  clusterelementmask($cm->{clusterids}, $cm->{celtmask});
  return $cm->{celtmask};
}

## $celts = $cm->clusterElements(%args)
##  + %args: (none)
##  + gets cluster-element enumeration
##  + sets $cm->{celts}=$celts # pdl(long, max($csizes), $k)
sub clusterElements {
  my $cm = shift;
  return $cm->{celts} if (defined($cm->{celts}));
  my $csizes = $cm->clusterSizes();
  $cm->{celts} = zeroes(long, $csizes->max, $cm->{nclusters})-1;
  clusterelements($cm->{clusterids}, $csizes, $cm->{celts});
  return $cm->{celts};
}

## $cdmatrix = $cm->clusterDistanceMatrix(%args)
##  + gets cluster distance matrix $cdmatrix = $cm->{cdmatrix}
##  + %args:
##     rowids   => $pdl,    ## rows to populate (default: sequence($n))
##     cddist   => $dist,   ## default/clobber: $cm->{cddist}
##     cdmethod => $method, ## default/clobber: $cm->{cdmethod}
##     cdbonus  => $bool,   ## default/clobber: $cm->{cdbonus} : apply hard bonus
##  + ... any args for clusterSizes(), clusterElements()
##  + output matrix $cdmatrix has dimensions ($k,$nr=$rowids->nelem)
##    and has values distance($cluster_k, $rowids_nr)
sub clusterDistanceMatrix {
  my ($cm,%args) = @_;

  ##-- just return pre-computed matrix if defined
  return $cm->{cdmatrix} if (defined($cm->{cdmatrix}));

  ##-- arg parsing
  $args{cddist}   = $cm->cddist()   if (!defined($args{cddist}));
  $args{cdmethod} = $cm->cdmethod() if (!defined($args{cdmethod}));
  $args{cdbonus}  = $cm->{cdbonus}  if (!defined($args{cdbonus}));
  @$cm{qw(cddist cdmethod cdbonus)} = @args{qw(cddist cdmethod cdbonus)};

  ##-- sanity checks
  confess(ref($cm), "::clusterDistanceMatrix(): no data!") if (!defined($cm->{data}));
  confess(ref($cm), "::clusterDistanceMatrix(): cowardly refusing bad data!")
      if ($cm->{data}->inplace->setnantobad->nbad > 0);


  my $rowids = (defined($args{rowids})
		? $args{rowids}
		: sequence(long,$cm->{data}->dim(1)));

  ##-- get base data
  my $csizes = $cm->clusterSizes();
  my $celts  = $cm->clusterElements();
  $cm->{cdmatrix} = zeroes(double,$cm->{nclusters},$rowids->nelem);

  clusterdistancematrix(@$cm{qw(data mask weight)},
			$rowids, $csizes, $celts,
			$cm->{cdmatrix},
			@args{qw(cddist cdmethod)});

  confess(ref($cm), "::clusterDistanceMatrix(): bad data in output pdl!")
      if ($cm->{cdmatrix}->inplace->setnantobad->nbad > 0);

  ##-- apply hard-clustering bonus ?
  if ($args{cdmethod} =~ /\+b/ && $args{cdbonus}) {
    print STDERR
      ("<<<DEBUG>>>: ", ref($cm),
       "::clusterDistanceMatrix() adding bonus for ", $rowids->nelem, " rowids.\n",
      );

    my $cemask     = $cm->clusterElementMask();
    my $row_cemask = $cemask->dice_axis(1, $rowids);
    $cm->{cdmatrix}->where($row_cemask) .= 0;
  }

  return $cm->{cdmatrix};
}


##======================================================================
## Utilities: m-best (min|max)
##======================================================================

## $mmini = $cm->mmini($matrix,$m);
##   + get m-minimum ROW-indices for each COLUMN in matrix $matrix
##   + $matrix is a pdl($nc,$nr), then
##     $mmini  is a pdl($nr,$m)   (as required by minimum_n_ind)
##   + you can index columns by rows by passing $matrix->xchg(0,1)
##     instead of $matrix, (and also xchg()ing the result)
sub mmini {
  my ($cm,$matrix,$m) = @_;
  my $rowi = zeroes(long, $m, $matrix->dim(1));
  $matrix->minimum_n_ind($rowi);
  return $rowi;
}

## $mmaxi = $cm->mmaxi($matrix,$m);
##   + get m-maximum ROW-indices for each COLUMN in matrix $matrix
##   + $matrix is a pdl($nc,$nr), then
##     $mmaxi  is a pdl($nr,$m)   (as required by maximum_n_ind)
##   + you can index columns by rows by passing $matrix->xchg(0,1)
##     instead of $matrix
sub mmaxi {
  my ($cm,$matrix,$m) = @_;
  my $rowi = zeroes(long, $m, $matrix->dim(1));
  $matrix->maximum_n_ind($rowi);
  return $rowi;
}


## $rowiND = $cm->rowi2nd($rowi);
##   + turn minimum ROW-indices into full n-dimensional index PDL
##   + $rowi is a pdl($m,$nr) for indexing a matrix pdl ($nc,$nr)
##   + $rowiND is an n-dimensional index pdl (2, $m*$nr)
sub rowi2nd {
  my ($cm,$rowi) = @_;
  return cat($rowi->flat, yvals($rowi)->flat)->xchg(0,1);
}

## $coli2nd = $cm->coli2nd($coli);
##   + turn minimum COL-indices into full n-dimensional index PDL
##   + $coli is a pdl($m,$nc) for indexing a matrix pdl $mat($nc,$nr)
##   + $coliND is an n-dimensional index pdl (2, $m*$nc)
##   + you can get the same effect by doing:
##     $mat->indexND(coli2nd($coli)) == $mat->xchg(0,1)->indexND(rowi2nd($coli))
sub coli2nd {
  my ($cm,$coli) = @_;
  return cat(yvals($coli)->flat, $coli->flat)->xchg(0,1);
}

## $mask = $cm->mminmask($matrix,$m)
##   + get mask of $m-minimal values per row for matrix $matrix
sub mminmask {
  my ($cm,$matrix,$m) = @_;
  my $rowi = $cm->mmini($matrix,$m);
  my $mask  = zeroes(byte, $matrix->dims);
  $mask->indexND($cm->rowi2nd($rowi)) .= 1;
  return $mask;
}

## $mask = $cm->mmaxmask($matrix,$m)
##   + get mask of $m-maximal values per row for matrix $matrix
sub mmaxmask {
  my ($cm,$matrix,$m) = @_;
  my $rowi = $cm->mmaxi($matrix,$m);
  my $mask = zeroes(byte, $matrix->dims);
  $mask->indexND($cm->rowi2nd($rowi)) .= 1;
  return $mask;
}

##======================================================================
## Utilities: centroid acquisition
##======================================================================

##----------------------------------------------------------------------
## ($cdata,$cmask) = $cm->getcenters(%args)
##  + %args : clobbers defaults from/to %$cm
##      ctrmethod => $method,  # method-name or -suffix for prefix 'd2c_'
##      ctrm => $mbest,        # for m-best methods (default=3)
##      ctrmode => $which,     # 'hard' or 'soft', for m-best methods: default='hard'
##      cddist => $cddist,     # distance method (default='u')
##      cdmethod => $cdmethod, # link type (default='v')
##      cweights => $cweights, # pdl($k,$n) ~ p(t_n|c_k) for 'wsum' method
##      rprobs  => $rowprobs,  # pdl($n) ~ p(t_n) : for 'weighted' method
##  + known methods:
##      mean           # arithmetic mean of 'hard' cluster elements
##      median         # median of 'hard' cluster elements
##      wsum           # general weighted sum, req. 'cweights'=>pdl($k,$n)~p(t_n|c_k)
##      mbest_mean     # arithmetic mean of m-best cluster elements, via clusterDistanceMatrix()
##      mbest_inverse  # weighted m-best sum using clusterDistanceMatrix() inverse
##      weighted       # weighted sum using clusterDistanceMatrix() & 'rprobs'=>pdl($n)~p(t_n)
##      mbest_weighted # weighted m-best using clusterDistanceMatrix() & 'rprobs'=>pdl($n)~p(t_n)
##    TODO:
##      bayes          # Bayesian inversion, hopefully using membershipProbPdl() and 'rprobs'
sub getcenters {
  my ($cm,%args) = @_;

  ##-- parse arguments
  @$cm{keys(%args)} = values(%args); ##-- clobber

  ##-- ensure custerids() is defined
  $cm->cut()  if (!defined($cm->{clusterids}));

  ##-- get method
  my $ctrmethod = $cm->{ctrmethod};
  $ctrmethod = $cm->{ctrmethod} = 'mean' if (!defined($ctrmethod));

  ##-- get method subroutine
  my $msub = $cm->can($ctrmethod);
  $msub = $cm->can("d2c_$ctrmethod") if (!defined($msub));
  croak(ref($cm), "::getcenters(): unknown centroid discovery method '$ctrmethod'")
    if (!defined($msub));

  return $msub->($cm);
}

##----------------------------------------------------------------------
## Utilities: centroid data: Utilties
##----------------------------------------------------------------------

## ($cdata,$cmask) = $cm->d2c_pdls()
##   + gets initial (empty) output pdls of proper size
##   + $cm keys:
##       cdata => $cdata,  ##-- pdl(double,$d,$k)
##       cmask => $cmask,  ##-- pdl(long,  $d,$k)
sub d2c_pdls {
  my $cm = shift;
  my $d = $cm->{data}->dim(0);
  my $k = $cm->{nclusters};
  my ($cdata,$cmask) = @$cm{qw(cdata cmask)};
  $cdata = zeroes(double,$d,$k) if (!defined($cdata));
  $cmask = ones(long,$d,$k)     if (!defined($cmask));
  return @$cm{qw(cdata cmask)} = ($cdata,$cmask);
}

##----------------------------------------------------------------------
## Utilities: centroid data: libcluster built-in methods
##----------------------------------------------------------------------

## ($cdata,$cmask) = $cm->d2c_mean()
##   + get cluster centroids by arithmetic mean
##   + $cm keys: (none)
sub d2c_mean {
  my $cm = shift;
  my ($cdata,$cmask) = $cm->d2c_pdls(@_);
  getclustermean(@$cm{qw(data mask clusterids)}, $cdata,$cmask);
  ##-- stupidity check
  croak(__PACKAGE__, "::d2c_mean(): PDL::Cluster::getclustermean() returned zero matrix!")
    if (all($cdata==0));
  return ($cdata,$cmask);
}

## ($cdata,$cmask) = $cm->d2c_median()
##   + $cm keys: (none)
sub d2c_median {
  my $cm = shift;
  my ($cdata,$cmask) = $cm->d2c_pdls(@_);
  getclustermedian(@$cm{qw(data mask clusterids)}, $cdata,$cmask);
  ##-- sanity check
  croak(__PACKAGE__, "::d2c_mean(): PDL::Cluster::getclustermedian() returned zero matrix!")
    if (all($cdata==0));
  return ($cdata,$cmask);
}


##----------------------------------------------------------------------
## Utilities: centroid data: weighted sum methods
##----------------------------------------------------------------------

## $ctrmode = $cm->ctrmode()
##  + get centroid discovery mode ('hard' or 'soft')
##  + $cm keys:
##     ctrmode => $mode, ## default
sub ctrmode {
  my $cm = shift;
  return (defined($cm->{ctrmode}) ? $cm->{ctrmode} : ($cm->{ctrmode}='hard'));
}

## ($cdata,$cmask) = $cm->d2c_wsum(%args)
##  + general abstract method for weighted sums
##  + %$cm keys:
##     cweights => $cweights, # pdl($k,$n) ~ p(t_n|c_k)
##  + returns d2c_mean() if $cweights is not defined
sub d2c_wsum {
  my $cm = shift;
  return $cm->d2c_mean(@_) if (!defined($cm->{cweights}));
  my ($cdata,$cmask) = $cm->d2c_pdls(@_);
  getclusterwsum(@$cm{qw(data mask cweights)}, $cdata,$cmask);
  return ($cdata,$cmask);
}

## $mbestmask = $cm->d2c_mbest_mask(%args)
##  + Utility method: returns boolean mask of m-best elts per cluster
##    $mbestmask = pdl(byte, $k, $n)
##  + %$cm keys:
##    ctrm => $mbest,   # natural number > 0
##    ctrmode => $mode, # 'hard' or 'soft' [default='hard']
sub d2c_mbest_mask {
  my $cm = shift;

  ##-- get distance matrix
  my $cdm = $cm->clusterDistanceMatrix(@_);

  ##-- get m-best mask (respect hard/soft membership arg)
  my $mbestmask = $cm->mminmask($cdm->xchg(0,1), $cm->{ctrm})->xchg(0,1);
  $mbestmask   &= $cm->clusterElementMask if ($cm->ctrmode(@_) ne 'soft');

  return $mbestmask;
}


## ($cdata,$cmask) = $cm->d2c_mbest_mean();
##  + returns "trimmed profile" as for Cutting et al (1989)
##  + %$cm keys:
##    ctrm => $mbest,   # > 0
##    ctrmode => $mode, # 'hard' or 'soft'
sub d2c_mbest_mean {
  my $cm = shift;
  my $cdm       = $cm->clusterDistanceMatrix(@_);
  my $mbestmask = $cm->d2c_mbest_mask(@_);

  ##-- get weight matrix (uniform)
  my $cweights  = $cm->{cweights}
                = ones(double, $cdm->dims);
  $cweights    *= $mbestmask;
  $cweights    /= $cweights->xchg(0,1)->sumover;

  ##-- get centroid data as weighted sum
  return $cm->d2c_wsum(@_);
}


## ($cdata,$cmask) = $cm->d2c_mbest_inverse();
##  + returns weighted trimmed profile using m-best elements per cluster
##  + %$cm keys:
##     ctrm    => $mbest, # > 0
##     ctrmode => $mode,  # 'hard' or 'soft'
sub d2c_mbest_inverse {
  my $cm        = shift;
  my $cdm       = $cm->clusterDistanceMatrix(@_);
  my $mbestmask = $cm->d2c_mbest_mask(@_);

  ##-- get weight matrix

  ##--
  ## my $cweights = $cdmin/($cdmin + $cdm);
  ## $cweights   /= $cweights->xchg(0,1)->sumover;
  ##--
  my $cdmin    = $cdm->where($cdm!=0)->flat->minimum;
  my $cweights = $cm->{cweights}
               = $cdm + $cdmin; ##-- denominator
  $cweights   /= $cdmin;        ##-- numerator
  $cweights->inplace->pow(-1);  ##-- invert
  $cweights   *= $mbestmask;    ##-- apply m-best mask
  $cweights   /= $cweights->xchg(0,1)->sumover; ##-- ... and normalize by col

  ##-- get centroid data as weighted sum
  return $cm->d2c_wsum(@_);
}

## ($cdata,$cmask) = $cm->d2c_weighted(%args);
## + gets centers as: c(i) = \sum_{w} p(w|c) w(i)
##   - where:
##     p(w|c) = p(c|w)p(w) / \sum_w p(c|w)p(w)
##     p(c|w) = / 1 if w \in c
##              \ 0 otherwise
##     p(w)   = $rprobs(w)
##   - so:
##     c(i)   = \sum_{w \in c} p(w) w(i)
## + %$cm keys:
##     rprobs  => $rprobs,  # row-probabilities: pdl($n)    ~ p(w)
##                          # if undefined, returns d2c_mean()
sub d2c_weighted {
  my ($cm,%args) = @_;
  my $rprobs   = $args{rprobs};
  $rprobs      = $cm->{rprobs} if (!defined($rprobs));
  return $cm->d2c_mean(%args) if (!defined($rprobs));

  my $cdm      = $cm->clusterDistanceMatrix(%args);
  my $celtmask = $cm->clusterElementMask(%args);

  ###-- get weight matrix
  #my $cdmin    = $cdm->where($cdm!=0)->flat->minimum;
  #my $cweights = $cm->{cweights}
  #             = $cdm + $cdmin; ##-- denominator
  #$cweights   /= $cdmin;        ##-- numerator
  #$cweights->inplace->pow(-1);  ##-- invert
  #$cweights   *= $celtmask;     ##-- apply mask
  #
  ###-- convert to prob (?)
  ##$cweights   /= $cweights->xchg(0,1)->sumover; ##-- this would get us ~ p(w|c)
  #$cweights->xchg(0,1) /= $cweights->sumover;   ##-- ... normalize by cluster
  ##-- ... for hard mask, at this point, $cweights==$celtmask!

  my $cweights = $celtmask * $rprobs->slice("*1,:"); ##-- apply row-weights
  $cweights   /= $cweights->xchg(0,1)->sumover;      ##-- ... and normalize

  ##-- get centroid data as weighted sum
  return $cm->d2c_wsum(%args);
}

## ($cdata,$cmask) = $cm->d2c_mbest_weighted(%args);
## + gets centers as: c(i) = \sum_w p(w|c) w(i)
##   - where:
##     p(w|c) = p(c|w)p(w) / \sum_w p(c|w)p(w)
##     p(c|w) = / 1/m if w \in mbest(c)
##              \ 0   otherwise
##     p(w)   = $rprobs(w)
## + %$cm keys:
##     rprobs  => $rprobs,  # row-probabilities: pdl($n)    ~ p(w)
##                          # if undefined, returns d2c_mbest_mean()
##     ctrmode => $mode,    # 'hard' or 'soft'
sub d2c_mbest_weighted {
  my ($cm,%args) = @_;
  my $rprobs   = $args{rprobs};
  $rprobs      = $cm->{rprobs} if (!defined($rprobs));
  return $cm->d2c_mean(%args) if (!defined($rprobs));

  my $cdm       = $cm->clusterDistanceMatrix(%args);
  my $mbestmask = $cm->d2c_mbest_mask(%args);

  ###-- get weight matrix
  my $cweights = $mbestmask->convert(double);
  $cweights   /= $cweights->xchg(0,1)->sumover;  ##-- normalize by cluster ~ p(c)
  $cweights   *= $rprobs->slice("*1,:");         ##-- apply row-weights
  $cweights   /= $cweights->xchg(0,1)->sumover;  ##-- ... and normalize

  ##-- get centroid data as weighted sum
  return $cm->d2c_wsum(%args);
}

##======================================================================
## Utilities: distance-to-probability
##======================================================================

##----------------------------------------------------------------------
## $probPdl            = $cm->membershipProbPdl(%args)
## ($probPdl,$rowsums) = $cm->membershipProbPdl(%args)
##  + as for membershipSimPdl(), output is normalized by row
##  + output: $prob_pdl = $cm->{r2cprobs} : $k by $n
##    - $prob_pdl->at($ki,$ni) ~= p($class_ki | $target_ni)
sub membershipProbPdl {
  my $cm = shift;
  my $pdl  = $cm->membershipSimPdl(@_);
  #my $nbmask = $cm->membershipSimMask(@_);
  #$pdl *= $nbmask;
  my $rowsums = $pdl->sumover;
  $pdl /= $rowsums->slice("*1,");
  $pdl->inplace->setnantobad->inplace->setbadtoval(0); ##-- hack
  return wantarray ? ($pdl,$rowsums) : $pdl;
}

##----------------------------------------------------------------------
## $mask = $cm->membershipSimMask()
## $mask = $cm->membershipSimMask($simpdl,%args)
##  + returns byte-mask of n-best elements if this is an n-best method,
##    all ones otherwise
##  + output: $mask : $k by $n
##    - $prob_mask->at($ki,$ni) == 1 iff ^p($class_ki | $target_ni) > 0
sub membershipSimMask {
  my ($cm,$simPdl,%args) = @_;
  $simPdl  = $cm->membershipSimPdl(%args) if (!defined($simPdl));
  if ($cm->{d2p_isnbest}) {
    my $cdm     = $cm->clusterDistanceMatrix();
    my $simPdls = $cm->d2p_slicePdl($cdm, $simPdl);
    my $nbmasks = $cm->d2p_nbest_mask($cdm);
    my $nbmask  = zeroes(byte, $simPdl->dims);
    $nbmask->slice("0:".($simPdls->dim(0)-1).",0:".($simPdls->dim(1)-1)) .= $nbmasks;
    return $nbmask;
  }
  return ones(byte,$simPdl->dims);
}

##----------------------------------------------------------------------
## $simPdl = $cm->membershipSimPdl(%args)
##  + %args : clobbers defaults from/to %$cm
##      d2pmethod => $dmethod,  # technique: method-name or -prefix for 'd2p_'
##      d2pn      => $nbest,    # for n-best methods
##      d2pb      => $base,     # for base-specific methods
##      d2pbeta   => $beta,     # pdl($n) for sample-size dependent methods
##      d2pq      => $coeff,    # for coefficient-controlled methods (i.e. Gath & Geva)
##      d2ppow    => $pwr,      # distance exponent (for Gath & Geva method)
##      cddist    => $cddist,   # distance method (uses $cm->{cddist})
##      cdmethod  => $cdmethod, # link type (uses $cm->{cdmethod})
##      r2cprobs  => $probPdl,  # specify output pdl ~ p(c|row)
##      cdmatrix  => $cdmatrix, # cluster distance matrix (formerly 'leafdist')
##      donbest   => $bool,     # whether to do n-best masking here
##  + also sets $cm->{d2p_isnbest}
##  + known {d2pmethod}s:
##      'mean', 'median', ...
##  + output: $prob_pdl = $cm->{r2cprobs} : $k by $n
##    - $prob_pdl->at($ki,$ni) ~= sim($class_ki , $target_ni)
sub membershipSimPdl {
  my ($cm,%args) = @_;

  ##-- parse arguments
  @$cm{keys(%args)} = values(%args); ##-- clobber

  ##-- ensure custerids() is defined
  $cm->cut()  if (!defined($cm->{clusterids}));

  ##-- get method
  my $d2pmethod = $cm->{d2pmethod};
  $d2pmethod = $cm->{d2pmethod} = 'hard' if (!defined($d2pmethod));

  ##-- get method subroutine
  my $msub = $cm->can($d2pmethod);
  $msub = $cm->can("d2p_$d2pmethod") if (!defined($msub));

  ##-- n-best method dispatch
  $cm->{d2p_isnbest} = 0;
  $cm->{d2p_isnbest} = 1 if (!defined($msub)
			     && $d2pmethod =~ /^nbest_(.*)/
			     && defined($msub=$cm->can("d2p_$1")));

  confess(ref($cm), "::membershipSimPdl(): unknown estimation method '$d2pmethod'")
    if (!defined($msub));

  return $msub->($cm);
}



##----------------------------------------------------------------------
## Utilities: p(c|w) : common
##----------------------------------------------------------------------

## $r2cprobs = $cm->d2p_getPdl()
## $r2cprobs = $cm->d2p_getPdl($cdmatrix)
## $r2cprobs = $cm->d2p_getPdl($cdmatrix,$r2cprobs)
##  + esures that $r2cprobs=$cm->{r2cprobs} is at least as large as $cdmatrix ($k by $n)
##  + basically just gets empty pdl
##  + sets $cm->{r2cprobs}
sub d2p_getPdl {
  my ($cm,$cdm,$r2c) = @_;
  $cdm = $cm->clusterDistanceMatrix() if (!defined($cdm));
  $r2c = $cm->{r2cprobs}    if (!defined($r2c));
  $r2c = zeroes($cdm->dims) if (!defined($r2c));

  $r2c->reshape($cdm->dims)
    if ($r2c->ndims != 2
	|| $r2c->dim(0) < $cdm->dim(0)
	|| $r2c->dim(1) < $cdm->dim(1));

  return $cm->{r2cprobs}=$r2c;
}

## $r2c_slice = $cm->d2p_slicePdl($cdmatrix,$r2cprobs)
##   + returns slice of $r2cprobs to dimensions of $cdmatrix
sub d2p_slicePdl {
  my ($cm,$cdm,$r2c) = @_;
  return $r2c->slice("0:".($cdm->dim(0)-1).",0:".($cdm->dim(1)-1));
}

##----------------------------------------------------------------------
## Utilities: p(c|w) : hard
##----------------------------------------------------------------------

## $r2cprobs = $cm->d2p_hard()
##  + %$cm keys:
##      celtmask : cluster element mask
##      r2cprobs : output pdl ($k by $n)
sub d2p_hard {
  my $cm = shift;
  my $celtmask = $cm->clusterElementMask;
  my $r2c      = $cm->d2p_getPdl($celtmask);
  my $r2cs     = $cm->d2p_slicePdl($celtmask,$r2c);
  $r2cs       .= $celtmask;
  return $r2c;
}

##----------------------------------------------------------------------
## Utilities: p(c|w) : full
##----------------------------------------------------------------------

## $r2cprobs = $cm->d2p_linear()
##  + %$cm keys:
##      cdmatrix : cluster distance matrix ($k by $n)
##      r2cprobs : output pdl ($k by $n)
sub d2p_linear {
  my $cm   = shift;
  my $cdm  = $cm->clusterDistanceMatrix(@_);
  my $r2c  = $cm->d2p_getPdl($cdm);
  my $r2cs = $cm->d2p_slicePdl($cdm,$r2c);

  ##-- compute: p(c|w) = d_min + d_max-d(c,w)
  my $cdmmin = $cdm->where($cdm!=0)->flat->minimum;
  my $cdmmax = $cdm->flat->maximum;
  $r2cs .= $cdmmax + $cdmmin;
  $r2cs -= $cdm;

  #$r2cs    /= $r2cs->sumover->slice("*1,:"); ##-- normalize by row
  return $r2c;
}

## $probPdl = $cm->d2p_inverse()
##  + %$cm keys:
##      cdmatrix : cluster distance matrix ($k by $n)
##      r2cprobs : output pdl ($k by $n)
sub d2p_inverse {
  my $cm   = shift;
  my $cdm  = $cm->clusterDistanceMatrix(@_);
  my $r2c  = $cm->d2p_getPdl($cdm);
  my $r2cs = $cm->d2p_slicePdl($cdm,$r2c);
  my $cdmmin = $cdm->where($cdm!=0)->flat->minimum;

  $r2cs .= $cdm;
  $r2cs += $cdmmin;
  $r2cs /= $cdmmin;
  $r2cs->inplace->pow(-1);

  return $r2c;
}

## $simPdl = $cm->d2p_pinskerL1()
##  + sim(c,w) = b^( -beta/2 * dist(c,w)^2 )
##  + %$cm keys:
##      cdmatrix : cluster distance matrix ($k by $n)
##      r2cprobs : output pdl ($k by $n)
##      d2pb     : base (default=2)
##      d2pbeta  : pdl ($n) : exp coefficients (sample sizes by target index)
##  + theoretically motivated for L1 distance between probability distributions
sub d2p_pinskerL1 {
  my $cm  = shift;
  my $cdm = $cm->clusterDistanceMatrix(@_);
  my $r2c = $cm->d2p_getPdl($cdm);
  my $r2cs = $cm->d2p_slicePdl($cdm,$r2c);

  my $beta   = defined($cm->{d2pbeta}) ? $cm->{d2pbeta} : ones(double,$cdm->dim(1));
  #my $beta   = $cm->{d2pbeta} ? $cm->{d2pbeta} : 2;

  my $b      = $cm->{d2pb}    ? $cm->{d2pb}    : 2;


  ##-- compute sim(c,w) = b^( -beta/2 * dist(c,w)^2 )
  ##   + for L1 distance, this is Pinsker's inequality
  $r2cs .= $cdm;
  $r2cs  *= $cm->{weight}->sumover if (defined($cm->{weight})); ##-- factor out contribution of *weights*...
  $r2cs->inplace->pow(2);
  $r2cs *= $beta->slice("*1,:");
  $r2cs /= -2;
  PDL::pow($b, $r2cs, $r2cs);

  return $r2c;
}

## $r2cprobs = $cm->d2p_jaynes()
## $r2cprobs = $cm->d2p_pinskerD()
##  + sim(c,w) = b^( -beta * dist(c,w) )
##  + %$cm keys:
##      cdmatrix : cluster distance matrix ($k by $n)
##      r2cprobs : output pdl ($k by $n)
##      d2pb     : base : UNUSED (always=exp(1))
##      d2pbeta  : pdl ($n) : exp coefficients (sample sizes by target index)
##  + theoretically motivated for D(empirical||source) distance between probability
##    distributions [according to Lee (1997)]
*d2p_pinskerD = \&d2p_jaynes;
sub d2p_jaynes {
  my $cm  = shift;
  my $cdm = $cm->clusterDistanceMatrix(@_);
  my $r2c = $cm->d2p_getPdl($cdm);
  my $r2cs = $cm->d2p_slicePdl($cdm,$r2c);

  my $beta   = defined($cm->{d2pbeta}) ? $cm->{d2pbeta} : ones(double,$cdm->dim(1));
  #my $beta   = $cm->{d2pbeta} ? $cm->{d2pbeta} : 2;

  my $b      = exp(1);

  ##-- compute P(c|w) = b^( -beta * dist(c,w) )
  ##   + for dist(w,c)=D(w||c), this is Pinsker's inequality
  $r2cs .= $cdm;
  $r2cs *= $beta->slice("*1,:");
  $r2cs *= -1;
  PDL::pow($b, $r2cs, $r2cs);

  return $r2c;
}


## $r2cprobs = $cm->d2p_gath()
##  + sim(c,w) = abs(1 / d(c,w)^$pow) ^ (1/($q-1))
##  + %$cm keys:
##      cdmatrix : cluster distance matrix ($k by $n)
##      r2cprobs : output pdl ($k by $n)
##      d2pq     : fuzziness control parameter, > 1 (2 ==> nbest_inverse) [alt='b'] [default=4]
##      d2ppow   : distance exponent (default=1)
##  + Gath & Geva technique
##  + Notes:
##    * q > 1
##    * high $q   ~ high-entropy output dist (very uniform)
##    * low  $q   ~ low-entropy output dist  (very certain)
##    * high $pow ~ low-entropy output dist (distances contribute more)
##    * low  $pow ~ high-entropy output dist (distances contribute more)
sub d2p_gath {
  my $cm  = shift;
  my $cdm = $cm->clusterDistanceMatrix(@_);
  my $r2c = $cm->d2p_getPdl($cdm);
  my $r2cs = $cm->d2p_slicePdl($cdm,$r2c);

  my $q    = ($cm->{d2pq} ? $cm->{d2pq}
	      : ($cm->{d2pb} ? $cm->{d2pb}
		 : 4));

  my $pow  = $cm->{d2ppow} ? $cm->{d2ppow} : 1;

  ##-- compute p(c|w) = abs(1 / d(c,w)^$pow) ^ (1/($q-1))
  $r2cs .= $cdm;                 ##-- initialize : denominator (inverted)

  ##-- hack: handle zeroes (works badly)
  #my $r2czmask = $r2cs->where($r2cs!=0);
  #my $zero     = $r2cs->where(!$r2czmask)->min/2;
  #$r2cs->where($r2czmask) .= $zero;
  ##-- /hack

  $r2cs->inplace->pow($pow);     ##-- eponentiate: denominator (inverted)
  $r2cs->inplace->pow(-1);       ##-- invert
  $r2cs->inplace->abs();         ##-- abs value
  $r2cs->inplace->pow(1/($q-1)); ##-- apply fuzziness control parameter

  $r2cs->inplace->setnantobad->inplace->setbadtoval($r2cs->max+1);  ##-- hack: handle zeroes (works pretty well)

  return $r2c;
}

## $r2cprobs = $cm->d2p_irank()
##  + sim(c,w) = rank_w( d(c|w) ) ** - beta
##  + %$cm keys:
##      d2pbeta  : pdl ($n) : exp coefficients (sample sizes by target index)
##      cdmatrix : cluster distance matrix ($k by $n)
##      r2cprobs : output pdl ($k by $n)
sub d2p_irank {
  my $cm  = shift;
  my $cdm = $cm->clusterDistanceMatrix(@_);
  my $r2c = $cm->d2p_getPdl($cdm);
  my $r2cs = $cm->d2p_slicePdl($cdm,$r2c);

  my $beta   = defined($cm->{d2pbeta}) ? $cm->{d2pbeta} : ones(double,$cdm->dim(1));
  #my $beta   = $cm->{d2pbeta} ? $cm->{d2pbeta} : 2;

  ##-- compute sim(c,w) = rank_w( d(c|w) ) ** - beta
  my $cdmi  = $cdm->qsorti;                                    ##-- initialize: sorted cluster indices by row
  my $cdmii = $cdmi->flat->cat($cdmi->yvals->flat)->xchg(0,1); ##-- n-dimensional index pdl
  $r2cs->indexND($cdmii) .= flat(1+sequence($r2cs->dim(1))->slice(",*".($r2cs->dim(0)))); ##-- assign ranks (from 1)
  $r2cs->inplace->pow(-$beta);                                                            ##-- invert via beta

  return $r2c;
}

## $r2cprobs = $cm->d2p_icrank()
##  + sim(c,w) = rank_w( d(c|w) ) ** - beta
##  + forces 'hard' clustering solution to be best for each word
##  + %$cm keys:
##      d2pbeta  : pdl ($n) : exp coefficients (sample sizes by target index)
##      cdmatrix : cluster distance matrix ($k by $n)
##      celtmask : cluster element mask
##      r2cprobs : output pdl ($k by $n)
sub d2p_icrank {
  my $cm  = shift;
  my $cdm = $cm->clusterDistanceMatrix(@_);
  my $r2c = $cm->d2p_getPdl($cdm);
  my $r2cs = $cm->d2p_slicePdl($cdm,$r2c);

  my $beta   = defined($cm->{d2pbeta}) ? $cm->{d2pbeta} : ones(double,$cdm->dim(1));
  #my $beta   = $cm->{d2pbeta} ? $cm->{d2pbeta} : 2;

  ##-- compute sim(c,w) = rank_w( d(c|w) ) ** - beta
  my $cemask  = $cm->clusterElementMask;                    ##-- get hard membership mask
  my $cdhard  = pdl($cdm->where($cemask));                  ##-- save distances
  my $cdmax   = $cdm->max;                                  ##-- save max distance
  $cdm->where($cemask) -= 2*$cdmax;                         ##-- hack: enforce "best"-cluster criterion

  my $cdmi  = $cdm->qsorti;                                    ##-- initialize: sorted cluster indices by row
  my $cdmii = $cdmi->flat->cat($cdmi->yvals->flat)->xchg(0,1); ##-- n-dimensional index pdl
  $r2cs->indexND($cdmii) .= flat(1+sequence($r2cs->dim(1))->slice(",*".($r2cs->dim(0)))); ##-- assign ranks (from 1)
  $r2cs->inplace->pow(-$beta);                                                            ##-- invert via beta

  $cdm->where($cemask) += 2*$cdmax;                         ##-- accomodate the hack

  return $r2c;
}


##----------------------------------------------------------------------
## Utilities: p(c|w) : n-best
##----------------------------------------------------------------------

## $mask = $cm->d2p_nbest_mask()
## $mask = $cm->d2p_nbest_mask($cdm)
## $mask = $cm->d2p_nbest_mask($cdm,$n)
##  + %$cm keys:
##      cdmatrix : cluster distance matrix ($k by $n)
##      d2pn     : n-best (default=1)
### + returns boolean mask of n-best clusters per datum ($k by $n)
sub d2p_nbest_mask {
  my ($cm,$cdm,$nb) = @_;
  $cdm = $cm->clusterDistanceMatrix() if (!defined($cdm));
  $nb  = $cm->{d2pn} if (!defined($nb));
  $nb  = 1           if (!defined($nb));
  return $cm->mminmask($cdm, $nb);
}


########################################################################
## Conversion
########################################################################

##======================================================================
## ($leafEnum,$clusterEnum) = $cm->toEnums(%args)
##  + returns enums representing the clustering solution
sub toEnums {
  require MUDL::Enum;
  my ($cm,%args) = @_;
  return ($cm->leafEnum(%args), $cm->clusterEnum(%args));
}

## $leafEnum = $cm->leafEnum()
##  + returns enums representing the leaves
sub leafEnum {
  require MUDL::Enum;
  my ($cm,%args) = @_;
  my $n = defined($cm->{data}) ? $cm->{data}->dim(1) : $cm->{clusterids}->dim(0);

  ##-- generate enums
  my $lenum = $cm->{enum};
  if (!defined($lenum)) {
    $lenum = MUDL::Enum->new(%args);
    $lenum->addIndexedSymbol('x'.$_, $_) foreach (0..($n-1));
  }

  return $lenum;
}

## $clusterEnum = $cm->clusterEnum(%args)
##  + returns enums representing the clusters
##  + additional %args:
##     offset=>$offset # added to label & index for each cluster
##     prefix=>$str,   # cluster prefix (default='c')
##  + other %args are passed to MUDL::Enum->new() if it is called
sub clusterEnum {
  require MUDL::Enum;
  my ($cm,%args) = @_;
  my $k = $cm->{nclusters};
  my $prefix = defined($args{prefix}) ? $args{prefix} : 'c';
  my $offset = $args{offset}||0;
  delete(@args{qw(prefix offset)});

  ##-- generate enums
  my $cenum = $cm->{cenum};
  if (!defined($cenum)) {
    $cenum = MUDL::Enum->new(%args);
    $cenum->addIndexedSymbol($prefix.($_+$offset), $_+$offset) foreach (0..($k-1));
  }

  return $cenum;
}

## $clusterEnum = $cm->clusterEnumFull()
##  + returns enum representing the clusters
sub clusterEnumFull {
  require MUDL::Enum;
  my ($cm,%args) = @_;
  my $k = $cm->{nclusters};
  #my $n = $cm->{data}->dim(1);
  my $n = $cm->{ndata};

  ##-- generate enum
  my $lenum = $cm->leafEnum(%args);
  my ($cenum,$cid);
  if (!defined($cenum=$cm->{cenum})) {
    $cenum = MUDL::Enum->new(%args);

    ##-- add cluster ids
    foreach (0..($k-1)) {
      $cenum->{id2sym}[$_] = 'c'.$_.':';
    }

    ##-- add all old leaf labels
    foreach (0..($n-1)) {
      $cenum->{id2sym}[$cm->{clusterids}->at($_)] .= '_'.$lenum->symbol($_);
    }

    ##-- build reverse index
    foreach (0..$#{$cenum->{id2sym}}) {
      $cenum->{sym2id}{$cenum->{id2sym}[$_]} = $_;
    }
  }

  return $cenum;
}




##======================================================================
## Conversion: distance-to-probability (old)
##======================================================================

##======================================================================
## $pdist = $cm->toJointPdlDist(%args)
##  + returns a MUDL::PdlDist representing the clusters
##  + returned dist has dimensions ($d,$n): $pdist->at($cid,$wid) = p($cid,$wid)
##  + %args are passed to MUDL::PdlDist->new()
*toPdlDist = \&toJointPdlDist;
sub toJointPdlDist {
  my $cm = shift;
  require MUDL::PdlDist;

  my ($k,$n) = ($cm->{nclusters}, $cm->{ndata});
  my ($lenum,$cenum) = $cm->toEnums();

  ##-- get (cluster,leaf) distance matrix
  my ($ld);
  #$ld    = $cm->leafdistances() if (!defined($ld=$cm->{leafdist}));
  $ld    = $cm->clusterDistanceMatrix();

  ##-- gnerate PdlDist
  my $pd  = $ld->max - $ld; #(+1 ?) (+ $ld->where($ld!=0)->min ???);
  $pd    /= $pd->sum;

  return MUDL::PdlDist->new(pdl=>$pd,
			    enum=>MUDL::Enum::Nary->new(enums=>[$cenum,$lenum]));
}

##======================================================================
## $edist = $cm->toJointEDist(%args)
##  + returns a MUDL::EDist::Nary representing the clusters
##  + returned dist has entries of the form "${target}${sep}${cluster}"=>f($target,$cluster)
##  + %args are passed to MUDL::EDist::Nary->new()
##  + returned dist structure:
##      ($datum_id, $clusterid) = $edist->split($event)
sub toJointEDist {
  my $cm = shift;
  require MUDL::Dist::Nary;

  my ($k,$n) = ($cm->{nclusters}, $cm->{ndata});
  my ($lenum,$cenum) = $cm->toEnums();

  ##-- get (cluster,leaf) distance matrix
  my ($ld,$ldmax);
  #$ld    = $cm->leafdistances() if (!defined($ld=$cm->{leafdist}));
  $ld    = $cm->clusterDistanceMatrix();
  $ldmax = $ld->max;

  ##-- gnerate edist
  my $edist = MUDL::EDist::Nary->new(enum=>MUDL::Enum::Nary->new(enums=>[$lenum,$cenum],nfields=>2),
				     nfields=>2,
				     @_);
  my ($eid,$cid);
  foreach $eid (0..($n-1)) {
    foreach $cid (0..($k-1)) {
      $edist->{nz}{$eid.$edist->{sep}.$cid} = $ldmax - $ld->at($cid,$eid)
	if ($ldmax - $ld->at($cid,$eid) > 0);
    }
  }

  return $edist;
}


##======================================================================
## $edist = $cm->toConditionalEDist(%args)
## $edist = $cm->toEDist(%args)
##  + returns a MUDL::EDist::Nary representing the clusters
##  + returned dist has entries of the form "${target}${sep}${cluster}"=>p($cluster|$target)
##  + %args are passed to MUDL::EDist::Nary->new()
##  + returned dist structure:
##      ($datum_id, $clusterid) = $edist->split($event)
*toEDist = \&toConditionalEDist;
sub toConditionalEDist {
  my $cm = shift;
  my $ed = $cm->toJointEDist(@_);
  return $ed->conditionalize([1]);
}


##======================================================================
## $lex = $cm->toLex(%args)
##  + returns a MUDL::Lex representing the clusters
##  + %args are passed to MUDL::EDist::Nary->new()
sub toLex {
  my $cm = shift;
  return $cm->toJointEDist(@_)->toDist->toLex();
}

##======================================================================
## $lex = $cm->toSupLex(%args)
##  + returns a MUDL::SupLex representing the clusters
##  + %args are passed to MUDL::Corpus::Profile::SupLex->new()
sub toSupLex {
  require MUDL::Corpus::Profile::SupLex;
  my $cm = shift;
  return MUDL::Corpus::Profile::SupLex->new(nz=>$cm->toJointEDist(@_)->{nz},
					    @_);
}

##======================================================================
## $map = $cm->toMap(%args)
##  + returns a MUDL::Map representing the clustering solution
##  + %args are passed to MUDL::Map->new()
sub toMap {
  require MUDL::Map;
  my $cm = shift;

  my ($k,$n) = ($cm->{nclusters}, $cm->{clusterids}->dim(0));
  my ($lenum,$cenum) = $cm->toEnums();
  my ($cids);
  if (defined($cm->{cdmatrix})) {
    ##-- soft clusters
    $cids = $cm->{cdmatrix}->minimum_ind;
  } else {
    ##-- hard clusters
    $cids = $cm->{clusterids};
  }

  ##-- generate map
  my $map = MUDL::Map->new(@_);
  my ($i);
  foreach $i (0..($n-1)) {
    $map->{$lenum->symbol($i)} = $cenum->symbol($cids->at($i));
  }

  return $map;
}

##======================================================================
## $map = $cm->toMetaMap(%args)
##  + returns a MUDL::Map representing the clustering solution,
##    retaining all leaf labels on each cluster label
##  + %args are passed to MUDL::Map->new()
sub toMetaMap {
  require MUDL::Map;
  my $cm = shift;

  my ($k,$n) = ($cm->{nclusters}, $cm->{clusterids}->dim(0));
  my $lenum  = $cm->leafEnum;
  my $cenum  = $cm->clusterEnumFull;
  my $cids   = $cm->{clusterids};

  ##-- generate map
  my $map = MUDL::Map->new(@_);
  my ($i);
  foreach $i (0..($n-1)) {
    $map->{$lenum->symbol($i)} = $cenum->symbol($cids->at($i));
  }

  return $map;
}



########################################################################
## Viewing
########################################################################

##======================================================================
## $tree = $cm->toTree(%args)
##  + returns a MUDL::Tree representing the clusters
##  + %args are passed to MUDL::Tree->new();
##  + default implementation is suitable for 'flat' clustering algorithms
sub toTree {
  my $cm = shift;
  require MUDL::Tree;
  my $tree = MUDL::Tree->new(@_);

  ##-- add cluster nodes
  my $k       = $cm->{nclusters};
  my @cid2nid = qw();
  my ($cid);
  foreach $cid (0..($k-1)) {
    $cid2nid[$cid] = $tree->addDaughter($tree->root, $cid);
  }

  ##-- add leaves
  my $groups = $tree->{groups} = {};
  my $cids   = $cm->{clusterids};
  my $n      = $cids->dim(0);
  my ($tlab,$tnid,$t);
  foreach $t (0..($n-1)) {
    $tlab = defined($cm->{enum}) ? $cm->{enum}->symbol($t) : $t;
    $tlab = "leaf:$t" if (!defined($tlab));
    $cid  = $cids->at($t);
    $tnid = $tree->addDaughter($cid2nid[$cid], $tlab);
    $groups->{$tnid} = $cid;
  }

  return $tree;
}

##======================================================================
## $tree = $cm->viewTree(%args)
##  + view a tree
##  + %args are passed to MUDL::Tree->toTree(), fromClusters()
sub viewTree {
  my $cm = shift;
  my $t = $cm->toTree(@_)->view(@_);
  return $t;
}


##======================================================================
## $dg = $cm->toDendogram(%args)
##  + get a dendogram of the clustering results
##  + %args are passed to MUDL::Tk::Dendogram->new()
sub toDendogram {
  my $cm = shift;
  require MUDL::Tk::Dendogram;
  return $cm->toTree(@_)->toDendogram(@_);
}

##======================================================================
## undef = $cm->view(%args)
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
