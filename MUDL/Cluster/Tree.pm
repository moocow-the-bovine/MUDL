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
  my $tc = $_[0]->SUPER::new(
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

  delete($tc->{leafdist})
    if (defined($tc->{leafdist}) && $tc->{leafdist}->dim(1) != $data->dim(1));

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
    rowdistances($tc->{data},
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

##======================================================================
## ($leafEnum,$clusterEnum) = $tc->toEnums(%args)
##  + returns enums representing the clustering solution
sub toEnums {
  require MUDL::Enum;
  my ($tc,%args) = @_;
  return ($tc->leafEnum(%args), $tc->clusterEnum(%args));
}

## $leafEnum = $tc->leafEnum()
##  + returns enums representing the leaves
sub leafEnum {
  require MUDL::Enum;
  my ($tc,%args) = @_;
  my $n = $tc->{data}->dim(1);

  ##-- generate enums
  my ($lenum);
  if (!defined($lenum=$tc->{enum})) {
    $lenum = MUDL::Enum->new(%args);
    $lenum->addIndexedSymbol('x'.$_, $_) foreach (0..($n-1));
  }

  return $lenum;
}

## $clusterEnum = $tc->clusterEnum()
##  + returns enums representing the clusters
sub clusterEnum {
  require MUDL::Enum;
  my ($tc,%args) = @_;
  my $k = $tc->{nclusters};

  ##-- generate enums
  my ($cenum);
  if (!defined($cenum=$tc->{cenum})) {
    $cenum = MUDL::Enum->new(%args);
    $cenum->addIndexedSymbol('c'.$_, $_) foreach (0..($k-1));
  }

  return $cenum;
}

## $clusterEnum = $tc->clusterEnumFull()
##  + returns enum representing the clusters
sub clusterEnumFull {
  require MUDL::Enum;
  my ($tc,%args) = @_;
  my $k = $tc->{nclusters};
  my $n = $tc->{data}->dim(1);

  ##-- generate enum
  my $lenum = $tc->leafEnum(%args);
  my ($cenum,$cid);
  if (!defined($cenum=$tc->{cenum})) {
    $cenum = MUDL::Enum->new(%args);

    ##-- add cluster ids
    foreach (0..($k-1)) {
      $cenum->{id2sym}[$_] = 'c'.$_.':';
    }

    ##-- add all old leaf labels
    foreach (0..($n-1)) {
      $cenum->{id2sym}[$tc->{clusterids}->at($_)] .= '_'.$lenum->symbol($_);
    }

    ##-- build reverse index
    foreach (0..$#{$cenum->{id2sym}}) {
      $cenum->{sym2id}{$cenum->{id2sym}[$_]} = $_;
    }
  }

  return $cenum;
}


##======================================================================
## Conversion: distance-to-probability (new)
##======================================================================

## %d2pMethods = ($name => \&sub, ...)
##   + methods such that &sub($leafdistance_pdl,%args) = $prob_pdl
##   + $leafdistance_pdl : $k by $n
##   + $prob_pdl         : $k by $n
our %d2pMethods =
  (
   nbest_hughes => \&d2p_nbest_hughes,
   nbest_base   => \&d2p_nbest_base,
   linear       => \&d2p_linear,
   inverse      => \&d2p_inverse,
   DEFAULT      => \&d2p_nbest_base,
  );

## $probPdl = $tc->membershipProbPdl(%args)
##   + %args:
##       pdl    => $probPdl,
##       method => $dist2probMethod,
##   + $dist2probMethod is a key %d2pMethods
##   + $leafdistance_pdl : $k by $n
##   + $prob_pdl         : $k by $n
##     - $prob_pdl->at($ki,$ni) ~= p($class_ki | $target_ni)
*membershipProbs = \&membershipProbPdl;
sub membershipProbPdl {
  my ($tc,%args) = @_;
  require PDL;

  my $ld = $tc->{leafdist};
  $ld = $tc->leafdistances() if (!defined($ld));

  my $method = $args{method} ? $args{method} : 'DEFAULT';
  my $d2psub = $tc->can($method);

  croak(ref($tc), "::membershipProbs(): unknown conversion method '$method'!")
    if (!defined($d2psub) && !defined($d2psub=$d2pMethods{$method}));

  return $d2psub->($tc,$ld,%args);
}

##----------------------------------------------------------------------
## Conversion: distance-to-probability: full
##----------------------------------------------------------------------

## $pdl = $tc->d2p_getPdl($leafdists,$args_pdl)
##  + %args:
##     pdl => $probPdl,
##  + returns a pdl at least as large as $leafdists
sub d2p_getPdl {
  my ($tc,$ld,$pdl) = @_;
  return zeroes($ld->dims) if (!defined($pdl));

  $pdl->reshape($ld->dims)
    if ($pdl->ndims != 2 || $pdl->dim(0) < $ld->dim(0) || $pdl->dim(1) < $ld->dim(1));

  return $pdl;
}

## $pdl_slice = $tc->d2p_slicePdl($leafdists,$pdl)
sub d2p_slicePdl {
  my ($tc,$ld,$pdl) = @_;
  return $pdl->slice("0:".($ld->dims(0)-1).",0:".($ld->dims(1)-1));
}

## $probPdl = $tc->d2p_linear($leafdists,%args)
##  + %args:
##     pdl => $probPdl,
sub d2p_linear {
  my ($tc,$ld,%args) = @_;
  my $pdl   = $tc->d2p_getPdl($ld,$args{pdl});
  my $pdls  = $tc->d2p_slicePdl($ld,$pdl);
  $pdls    .= $ld->max - $ld; # (+1?) ??? (+ $ld->where($ld!=0)->min ???);
  $pdls    /= $pdls->sumover->transpose;
  return $pdl;
}

## $probPdl = $tc->d2p_inverse($leafdists,%args)
##  + %args:
##     pdl => $probPdl,
sub d2p_inverse {
  my ($tc,$ld,%args) = @_;
  my $pdl    = $tc->d2p_getPdl($ld,$args{pdl});
  my $pdls   = $tc->d2p_slicePdl($ld,$pdl);
  my $ldmin  = $ld->where($ld!=0)->min;

  $pdls     .= $ldmin / ($ldmin + $ld);
  $pdls     /= $pdls->sumover->transpose;

  return $pdl;
}


##----------------------------------------------------------------------
## Conversion: distance-to-probability: n-best
##----------------------------------------------------------------------

##------------------------------------------------------
## $probPdl = $tc->d2p_nbest_hughes($leafdists,%args)
##  + %args:
##    n => $nbest,
##    pdl => $probPdl,
sub d2p_nbest_hughes {
  my ($tc,$ld,%args) = @_;
  my $n = $args{n} || 1;

  my $pdl   = $tc->d2p_getPdl($ld,$args{pdl});
  my $ipsum = (sequence($n)+1)->pow(-1)->sum;
  my $ipdl  = sequence(long, $n);
  foreach $ni (0..($ld->dim(1)-1)) {
    $ld->slice(",($ni)")->minimum_n_ind($ipdl);
    foreach $ki (0..($n-1)) {
      $pdl->set($ipdl->at($ki), $ni, 1/($ki+1)/$ipsum );
    }
  }

  return $pdl;
}

##------------------------------------------------------
## $probPdl = $tc->d2p_nbest_base($leafdists,%args)
##  + %args:
##     n => $nbest, # =1
##     b => $base   # =2
##    pdl => $probPdl,
sub d2p_nbest_base {
  my ($tc,$ld,%args) = @_;
  my $n = $args{n} || 1;
  my $b = $args{b} || 2;

  my $pdl   = $tc->d2p_getPdl($ld,$args{pdl});
  my $ipsum = sum(pow(pdl($b), -(sequence($n)+1)));
  my $ipdl  = sequence(long, $n);
  foreach $ni (0..($ld->dim(1)-1)) {
    $ld->slice(",($ni)")->minimum_n_ind($ipdl);
    foreach $ki (0..($n-1)) {
      $pdl->set($ipdl->at($ki), $ni, $b**-($ki+1)/$ipsum );
    }
  }

  return $pdl;
}

##======================================================================
## Conversion: distance-to-probability (old)
##======================================================================

##======================================================================
## $pdist = $tc->toJointPdlDist(%args)
##  + returns a MUDL::PdlDist representing the clusters
##  + returned dist has dimensions ($d,$n): $pdist->at($cid,$wid) = p($cid,$wid)
##  + %args are passed to MUDL::PdlDist->new()
*toPdlDist = \&toJointPdlDist;
sub toJointPdlDist {
  my $tc = shift;
  require MUDL::PdlDist;

  my ($k,$n) = ($tc->{nclusters}, $tc->{data}->dim(1));
  my ($lenum,$cenum) = $tc->toEnums();

  ##-- get (cluster,leaf) distance matrix
  my ($ld);
  $ld    = $tc->leafdistances() if (!defined($ld=$tc->{leafdist}));

  ##-- gnerate PdlDist
  my $pd  = $ld->max - $ld; #(+1 ?) (+ $ld->where($ld!=0)->min ???);
  $pd    /= $pd->sum;

  return MUDL::PdlDist->new(pdl=>$pd,
			    enum=>MUDL::Enum::Nary->new(enums=>[$cenum,$lenum]));
}

##======================================================================
## $edist = $tc->toJointEDist(%args)
##  + returns a MUDL::EDist::Nary representing the clusters
##  + returned dist has entries of the form "${target}${sep}${cluster}"=>f($target,$cluster)
##  + %args are passed to MUDL::EDist::Nary->new()
##  + returned dist structure:
##      ($datum_id, $clusterid) = $edist->split($event)
sub toJointEDist {
  my $tc = shift;
  require MUDL::Dist::Nary;

  my ($k,$n) = ($tc->{nclusters}, $tc->{data}->dim(1));
  my ($lenum,$cenum) = $tc->toEnums();

  ##-- get (cluster,leaf) distance matrix
  my ($ld,$ldmax);
  $ld    = $tc->leafdistances() if (!defined($ld=$tc->{leafdist}));
  $ldmax = $ld->max;

  ##-- gnerate edist
  my $edist = MUDL::EDist::Nary->new(enum=>MUDL::Enum::Nary->new(enums=>[$lenum,$cenum],nfields=>2),
				     nfields=>2,
				     @_);
  foreach $eid (0..($n-1)) {
    foreach $cid (0..($k-1)) {
      $edist->{nz}{$eid.$edist->{sep}.$cid} = $ldmax - $ld->at($cid,$eid)
	if ($ldmax - $ld->at($cid,$eid) > 0);
    }
  }

  return $edist;
}


##======================================================================
## $edist = $tc->toConditionalEDist(%args)
## $edist = $tc->toEDist(%args)
##  + returns a MUDL::EDist::Nary representing the clusters
##  + returned dist has entries of the form "${target}${sep}${cluster}"=>p($cluster|$target)
##  + %args are passed to MUDL::EDist::Nary->new()
##  + returned dist structure:
##      ($datum_id, $clusterid) = $edist->split($event)
*toEDist = \&toConditionalEDist;
sub toConditionalEDist {
  my $tc = shift;
  my $ed = $tc->toJointEDist(@_);
  return $ed->conditionalize([1]);
}


##======================================================================
## $lex = $tc->toLex(%args)
##  + returns a MUDL::Lex representing the clusters
##  + %args are passed to MUDL::EDist::Nary->new()
sub toLex {
  my $tc = shift;
  return $tc->toJointEDist(@_)->toDist->toLex();
}

##======================================================================
## $lex = $tc->toSupLex(%args)
##  + returns a MUDL::SupLex representing the clusters
##  + %args are passed to MUDL::Corpus::Profile::SupLex->new()
sub toSupLex {
  require MUDL::Corpus::Profile::SupLex;
  my $tc = shift;
  return MUDL::Corpus::Profile::SupLex->new(nz=>$tc->toJointEDist(@_)->{nz},
					    @_);
}

##======================================================================
## $map = $tc->toMap(%args)
##  + returns a MUDL::Map representing the clustering solution
##  + %args are passed to MUDL::Map->new()
sub toMap {
  require MUDL::Map;
  my $tc = shift;

  my ($k,$n) = ($tc->{nclusters}, $tc->{data}->dim(1));
  my ($lenum,$cenum) = $tc->toEnums();
  my $cids = $tc->{clusterids};

  ##-- generate map
  my $map = MUDL::Map->new(@_);
  foreach $i (0..($n-1)) {
    $map->{$lenum->symbol($i)} = $cenum->symbol($cids->at($i));
  }

  return $map;
}

##======================================================================
## $map = $tc->toMetaMap(%args)
##  + returns a MUDL::Map representing the clustering solution,
##    retaining all leaf labels on each cluster label
##  + %args are passed to MUDL::Map->new()
sub toMetaMap {
  require MUDL::Map;
  my $tc = shift;

  my ($k,$n) = ($tc->{nclusters}, $tc->{data}->dim(1));
  my $lenum  = $tc->leafEnum;
  my $cenum  = $tc->clusterEnumFull;
  my $cids   = $tc->{clusterids};

  ##-- generate map
  my $map = MUDL::Map->new(@_);
  foreach $i (0..($n-1)) {
    $map->{$lenum->symbol($i)} = $cenum->symbol($cids->at($i));
  }

  return $map;
}



########################################################################
## Viewing
########################################################################

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
				    groups=>$tc->{clusterids},
				    #dmult=>100,
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
