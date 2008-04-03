##
## File: MUDL::Cluster::Distance.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: generic clustering (row-row) and (cluster-row) distance functions
##======================================================================

package MUDL::Cluster::Distance;
use PDL;
#use PDL::CCS;
use MUDL::Object;
use MUDL::CmdUtils qw();
#use MUDL::Cluster::Distance::Builtin;
use MUDL::Cluster::LinkMethod;
use Carp;

use strict;

our @ISA = qw(MUDL::Object);

##======================================================================
## Globals: builtin distance functions

## %DIST_ALIAS = ($alias => [$CLASS_OR_SUFFIX, %class_new_args], ...)
##  + maps builtin suffixes to constructor arguments
our (%DIST_ALIAS);
BEGIN {
  %DIST_ALIAS =
    (
     ##-- Native perl distance functions
     'l1'=>'L1',
     'l2'=>'L2',
     'euclid'=>'L2',
     'Euclid'=>'L2',
     'pearson'=>'Pearson',
     'spearman'=>'Spearman',
     'cosine'=>'Cosine',
     'cos'=>'Cosine',
     ##
     ##-- PDL::Cluster built-in distance functions
     'e'=>['Builtin', distFlag=>'e', distName=>'Euclid'],
     'b'=>['Builtin', distFlag=>'b', distName=>'L1'],
     's'=>['Builtin', distFlag=>'s', distName=>'Spearman'],
     'S'=>['Builtin', distFlag=>'S', distName=>'SpearmanPre'],
     'c'=>['Builtin', distFlag=>'c', distName=>'Pearson'],
     'u'=>['Builtin', distFlag=>'u', distName=>'Cosine'],
     'D'=>['Builtin', distFlag=>'D', distName=>'KLD'],
     'A'=>['Builtin', distFlag=>'A', distName=>'KLD_avg'],
     'h'=>['Builtin', distFlag=>'h', distName=>'Hoeffding_total'],
     'o'=>['Builtin', distFlag=>'o', distName=>'Hoeffding_max'],
     'H'=>['Builtin', distFlag=>'H', distName=>'Hoeffding_safe'],
     'O'=>['Builtin', distFlag=>'O', distName=>'Hoeffding_max_safe'],
     'a'=>['Builtin', distFlag=>'a', distName=>'Pearson_abs'],
     'x'=>['Builtin', distFlag=>'x', distName=>'Cosine_abs'],
     ##... and maybe more ...
    );
}

##======================================================================
## Generic constructor

## $cd = MUDL::Cluster::Distance->new(%args);
##  + basic %args:
##     class    => $className,  # string: class-name or -alias or MUDL::Cluster::Distance:: suffix
##     #link    => $linkSpec,   # MUDL::Cluster::LinkMethod object or 'class' specification : TODO
##  + for PDL::Cluster built-in methods, see MUDL::Cluster::Distance::Builtin
sub new {
  my ($that,%args) = @_;

  ##-- optional class argument: dispatch
  if (!ref($that) && defined($args{class})) {
    $that = $args{class};
    my @alias_args = qw();
    while ( (ref($that) && ref($that) eq 'ARRAY') || exists($DIST_ALIAS{$that}) ) {
      ($that,@alias_args) = @$that if (ref($that) && ref($that) eq 'ARRAY');
      $that = $DIST_ALIAS{$that} if (defined($DIST_ALIAS{$that}));
      %args = (@alias_args,%args);
    }
    delete($args{class});
    $that = "MUDL::Cluster::Distance::$that" if ($that !~ /::/);
    MUDL::CmdUtils::loadModule($that);
    return $that->new(%args);
  }

  return $that->SUPER::new(%args);
}


##======================================================================
## API: High-level

##--------------------------------------------------------------
## $dmat = $cd->distanceMatrix(%args)
##  + row-row distances
##  + %args:
##     data   => $data,   ##-- dbl ($d,$n) : $d=N_features, $n=N_data                [REQUIRED]
##     mask   => $mask,   ##-- int ($d,$n) : "feature-is-good" boolean mask          [default=$data->isgood()]
##     weight => $weight, ##-- dbl ($d)    : feature-weight mask (weights distances) [default=ones($d)]
##  [o]dmat  => $dmat,    ##-- dbl ($n,$n) : output matrix [optional]
##  + default implementation calls $cd->compare()
sub distanceMatrix {
  my ($cd,%args) = @_;
  croak(ref($cd)."::distanceMatrix(): cowardly refusing undefined 'data' matrix") if (!defined($args{data}));

  ##-- get non-redundant comparison vector
  my $n = $args{data}->dim(1);
  my ($rows1,$rows2) = $cd->cmp_pairs($n);
  my $cmpvec = $cd->compare(data=>$args{data},
			    rows1=>$rows1,
			    rows2=>$rows2,
			    mask=>$args{mask},
			    weight=>$args{weight},
			   );

  ##-- roll $cmpvec into square distance matrix
  my $dmat = $args{dmat};
  $dmat = zeroes($cmpvec->type, $n,$n) if (!defined($dmat));
  $dmat->index2d($rows1,$rows2) .= $cmpvec;
  $dmat->index2d($rows2,$rows1) .= $cmpvec;
  $dmat->diagonal(0,1)          .= 0;
  return $dmat;
}

##--------------------------------------------------------------
## $cdmat = $cd->clusterDistanceMatrix(%args)
##  + returns distance matrix between each cluster-id listed in $cids() and each row in $rids()
##  + %args
##     data   => $data,     ##-- dbl ($d,$nde)   : $d=N_features, $nde=N_data_elts           [REQUIRED]
##     cdata  => $cdata,    ##-- dbl ($d,$nce)   : $d=N_features, $nce=N_clustered_elts      [default=$data]
##     cids   => $cids,     ##-- int ($nce)      : $cdata cluster-ids by row-id, 0<=$cid<$k  [default=sequence($nce)]
##     rids   => $rids,     ##-- int ($nde)      : $data  cluster-ids by row-id, 0<=$rid<$nr [default=sequence($nde)]
##     mask   => $mask,     ##-- int ($d,$nde)   : boolean mask for $data                    [default=$data->isgood]
##     cmask  => $cmask,    ##-- int ($d,$nce)   : boolean mask for $cdata                   [default=$cdata->isgood]
##     weight => $weight,   ##-- dbl ($d)        : feature-weight mask (weights distances)   [default=ones($d)]
##     #k      => $k,        ##-- int ()          : number of clusters                        [default=$cids->max+1]
##     #nr     => $nr,       ##-- int ()          : number of target rows                     [default=$rids->max+1]
##  [o]cdmat  => $cdmat,    ##-- dbl ($k,$nr)    : output matrix                             [optional]
sub clusterDistanceMatrix {
  my ($cd,%args) = @_;
  croak(ref($cd)."::clusterDistanceMatrix(): cowardly refusing inconsistent request!") if (!$cd->cdm_check(\%args));

  ##-- default: dispatch to $cd->compare(), $cd->linker->compare_link()
  $cd->cdm_defaults(\%args);

  ##-- get row-row distances
  my $cmp_which = $cd->crossproduct($args{cids}->nelem, $args{rids}->nelem);
  $cmp_which->slice("(1),") .= $args{gurids_rows}->index($cmp_which->slice("(1),"));
  my $cmp_vals  = $cd->compare(%args,
			       data=>$args{gudata},
			       mask=>$args{gumask},
			       weight=>$args{weight},
			       rows1=>$cmp_which->slice("(0),"),
			       rows2=>$cmp_which->slice("(1),"),
			      );

  ##-- link row-row to cluster-cluster distances
  my $linker = $cd->linker();
  my $link_which_in = (
		       $args{cids}->index($cmp_which->slice("(0),"))
		       ->cat(
			     $args{gurids_cids}->index($cmp_which->slice("(1),"))
			    )
		       ->xchg(0,1)
		      );
  my ($link_which,$link_cmps) = $linker->compare_link(%args,
						      which=>$link_which_in,
						      cmps =>$cmp_vals,
						     );

  ##-- build output matrix
  my $cdmat = $args{cdmat};
  if (!defined($cdmat)) {
    my ($k,$nr) = @args{'k','nr'};
    $k  = $link_which->slice("(0),")->max+1 if (!defined($k));
    $nr = $link_which->slice("(1),")->max+1 if (!defined($nr));
    $cdmat = zeroes($link_cmps->type, $k,$nr);
    $cdmat .= 'inf'; ##-- default distance: infinite
  }
  $cdmat->indexND($link_which) .= $link_cmps;

  return $cdmat;
}

##======================================================================
## API: linker & flag access

##--------------------------------------------------------------
## $flag = $cd->distFlag()
##  + returns $cd->{distFlag} if defined
##  + returns equivalent builtin "distance" flag for PDL::Cluster::treecluster(), PDL::Cluster::clusterdistance()
##  + croak()s if no known method exists [default]
sub distFlag {
  my $cd   = shift;
  return $cd->{distFlag} if (defined($cd->{distFlag}));
  croak(ref($cd)."::distFlag(): no equivalent built-in distance flag known!");
}
BEGIN { *tcDistFlag = *cdDistFlag = \&distFlag; }


##--------------------------------------------------------------
## $clm = $cd->linker()
##  + returns MUDL::Cluster::LinkMethod object associated with this distance function
##  + default implementation:
##    - return $cd->{linker} if defined
##    - otherwise, cache & return LinkMethod->new(class=>$cd->{link}) if $cd->{link} is defined
##    - otherwise, croak()s
sub linker {
  my $cd = shift;
  return $cd->{linker} if (UNIVERSAL::isa($cd->{linker},'MUDL::Cluster::LinkMethod'));
  if (defined($cd->{link})) {
    return $cd->{linker}=$cd->{link}
      if (UNIVERSAL::isa($cd->{link},'MUDL::Cluster::LinkMethod')); ##-- allow {link} to contain {linker} itself

    $cd->{linker} = MUDL::Cluster::LinkMethod->new(class=>$cd->{link})
      or croak(ref($cd)."::linker(): could not create new linker from {link}=$cd->{link}");

    return $cd->{linker};
  }
  croak(ref($cd)."::linker(): no {linker} object or {link} class defined!");
}

## $flag = $cd->cdLinkFlag()
##  + returns $cd->{cdLinkFlag} if defined
##  + returns equivalent "link-method" flag for PDL::Cluster::clusterdistance()
##  + croak()s if no equivalent link method exists [default]
sub cdLinkFlag {
  my $cd   = shift;
  return $cd->{cdLinkFlag} if (defined($cd->{cdLinkFlag}));
  my $flag = $cd->linker->cdLinkFlag();
  croak(ref($cd)."::cdLinkFlag(): no equivalent built-in link method known!") if (!defined($flag));
  return $flag;
}

## $flag = $cd->tcLinkFlag()
##  + returns equivalent "link-method" flag for PDL::Cluster::treecluster(), PDL::Cluster::treeclusterd()
##  + returns $cd->{tcLinkFlag} if defined
##  + croak()s if no equivalent link method exists [default]
sub tcLinkFlag {
  my $cd   = shift;
  return $cd->{tcLinkFlag} if (defined($cd->{tcLinkFlag}));
  my $flag = $cd->linker->tcLinkFlag();
  croak(ref($cd)."::tcLinkFlag(): no equivalent built-in link method known!") if (!defined($flag));
  return $flag;
}


##======================================================================
## Utils: for high-level API

##--------------------------------------------------------------
## $bool = $cd->cdm_check(\%args)
##  + checks argument sanity for clusterDistanceMatrix()
sub cdm_check {
  my ($cd,$args) = @_;
  my $rc=1;
  if (!defined($args->{data})) { carp(ref($cd)."::cdm_check(): no 'data' matrix specified!"); $rc=0; }
  return $rc;
}

##--------------------------------------------------------------
## \%args = $cd->cdm_defaults(\%args)
##  + sets defaults in %args for clusterDistanceMatrix()
##  + "defaults" include setting up "Grand Unified Data Matrix"
##  + checks/sets following %args keys:
##     gudata => $gudata, ##-- dbl ($d,$N) : grand unified data matrix, $N=($nce+$nde)
##     gumask => $gumask, ##-- int ($d,$N) : grand unified data mask
##     gucids => $gucids, ##-- ==$cids
##     gurids_rows => $gurids_rows, ##-- int($nr)      : $i => ($rids[$i] + $nce)
##     gurids_cids => $gurids_cids, ##-- int($nr+$nce) : $rids[$i]+$nce => $i : pseudo-clusterids for $rids
##     cids   => $cids,
##     rids   => $rids,
##     weight => $weight,
##     #k      => $k,
##     #nr     => $nr,
sub cdm_defaults {
  my ($cd,$args) = @_;
  my $data = $args->{data};
  my $cdata = defined($args->{cdata}) ? $args->{cdata} : $data;

  ##-- common data: mask, weight
  $args->{mask}   = $data->isgood()               if (!defined($args->{mask}));
  $args->{weight} = ones(double,$data->dim(0))    if (!defined($args->{weight}));

  ##-- common data: $cids: cdata cluster-ids by cdata row-id
  $args->{cids}   = sequence(long,$cdata->dim(1)) if (!defined($args->{cids}));

  ##-- common data: $rids: data "cluster"-ids by data row-id
  $args->{rids}   = sequence(long,$data->dim(1))  if (!defined($args->{rids}));

  if (defined($args->{cdata})) {
    ##-- concatenated GU-matrix: data
    $args->{gudata} = $cd->matrixCat($cdata,$data);

    ##-- concatenated GU-matrix: mask
    $args->{cmask} = $cdata->isgood() if (!defined($args->{cmask}));
    $args->{gumask} = $cd->matrixCat(@$args{qw(cmask mask)});

    ##-- concatenated GU-matrix: ids
    $args->{gucids} = $args->{cids};
    $args->{gurids_rows} = $args->{rids} + $cdata->dim(1);
    $args->{gurids_cids} = zeroes(long, $args->{cids}->dim(0)+$args->{rids}->dim(0))-1;
    $args->{gurids_cids}->index($args->{gurids_rows}) .= $args->{rids}->sequence;
  } else {
    ##-- shared GU-matrix
    $args->{gudata} = $data;
    $args->{gumask} = $args->{mask};
    $args->{gucids} = $args->{cids};
    $args->{gurids_rows} = $args->{rids};
    $args->{gurids_cids} = zeroes(long,$data->dim(1))-1;
    $args->{gurids_cids}->index($args->{gurids_rows}) .= $args->{rids}->sequence;
  }

  return $args;
}

##======================================================================
## Utils: comparison-index generation

##--------------------------------------------------------------
## ($i1,$i2)     = $cd->crossproduct($n,$m); ##-- list context
## pdl(2,$ncmps) = $cd->crossproduct($n,$m); ##-- scalar context; returned pdl is as for whichND()
##  + index cross-product ($n x $m)
sub crossproduct {
  my ($cd,$n,$m) = @_;
  $m = $n if (!defined($m));
  my $x = xvals(long,$n,$m)->flat->cat(yvals(long,$n,$m)->flat);
  return $x->xchg(0,1) if (!wantarray);
  return $x->dog;
}

##--------------------------------------------------------------
## ($i1,$i2)     = $cd->cmp_pairs($n) ##-- list context
## pdl(2,$ncmps) = $cd->cmp_pairs($n) ##-- scalar context; returned pdl is as for whichND()
##  + returns all index pairs ($i1,$i2) s.t. 0 <= $i1 < $i2 < $n
##  + stupid-but-easy version using sequence(), less-than, and whichND()
BEGIN { *cmp_pairs = \&cmp_pairs_v1; }
sub cmp_pairs_v1 {
  my ($cd,$n)   = @_;
  my $ncmps     = ($n/2)*($n-1);
  my $cmp0_vals = sequence(long,$n);#->reshape($ncmps);
  my $cmp0_runl = sequence(long,$n)->slice("-1:0");#->reshape($ncmps);
  my ($cmp0,$cmp0_lens);
  $cmp0_runl->rld($cmp0_vals, $cmp0     =zeroes(long,$ncmps));
  $cmp0_runl->rld($cmp0_runl, $cmp0_lens=zeroes(long,$ncmps));
  my $cmp1 = 1 + $cmp0 + ($cmp0->sequence->slice("-1:0") % $cmp0_lens);
  return wantarray ? ($cmp0,$cmp1) : $cmp0->cat($cmp1)->xchg(0,1);
}

##--------------------------------------------------------------
## ($i1,$i2)     = $cd->cmp_pairs($n) ##-- list context
## pdl(2,$ncmps) = $cd->cmp_pairs($n) ##-- scalar context; returned pdl is as for whichND()
##  + returns all index pairs ($i1,$i2) s.t. 0 <= $i1 < $i2 < $n
##  + stupid-but-easy version using sequence(), less-than, and whichND()
sub cmp_pairs_v0 {
  my ($cd,$n)  = @_;
  my $cmp_wnd  = (sequence(long,$n) < sequence(long,1,$n))->whichND();
  return wantarray ? ($cmp_wnd->xchg(0,1)->dog) : $cmp_wnd;
}



##======================================================================
## API: Low-Level

##--------------------------------------------------------------
## $cmpvec = $cd->compare(%args)
##  + %args:
##     data   => $data,    ##-- dbl ($d,$n)  : $d=N_features, $n=N_data                  [REQUIRED]
##     mask   => $mask,    ##-- int ($d,$n)  : $d=N_features, $n=N_data                  [default=$data->isgood()]
##     rows1  => $rows1,   ##-- int ($ncmps) : [$i] -> $data1_rowid_for_cmp_i            [REQUIRED]
##     rows2  => $rows2,   ##-- int ($ncmps) : [$i] -> $data2_rowid_for_cmp_i            [REQUIRED]
##     weight => $weight,  ##-- dbl ($d)     : feature-weight mask (weights distances)   [default=ones()]
##  [o]cmpvec => $cmpvec,  ##-- dbl ($ncmps) : output pdl [optional]
sub compare {
  my ($cd,%args) = @_;
  croak(ref($cd)."::compare(): not yet implemented!");
}

##======================================================================
## Data-mangling utilities (funcs)

##--------------------------------------------------------------
## $mat12 = $cd->matrixCat($mat1,$mat2)
##  + equivalent to $mat1->glue(1,$mat2)
##  + Signature:
##         $mat1 (m,n1)
##         $mat2 (m,n2)
##      [o]$mat12(m,n1+n2)
sub matrixCat {
  shift if (UNIVERSAL::isa($_[0], __PACKAGE__));
  return $_[0]->glue(1,$_[1]);
}

##======================================================================
## Utilities (for child classes)

##--------------------------------------------------------------
## $bool = $cd->compare_check(\%args)
##  + %args: as for compare()
##  + sanity check for %args
sub compare_check {
  my ($cd,$args) = @_;
  my $rc=1;
  my ($data,$rows1,$rows2) = @$args{qw(data rows1 rows2)};
  if (!defined($data))  { carp(ref($cd)."::compare_check(): no 'data' matrix specified!"); $rc=0; }
  if (!defined($rows1)) { carp(ref($cd)."::compare_check(): no 'rows1' vector specified!"); $rc=0; }
  if (!defined($rows2)) { carp(ref($cd)."::compare_check(): no 'rows2' vector specified!"); $rc=0; }

  my ($d,$n,$nr) = ($data->dim(0),$data->dim(1),$rows1->nelem);

  if ($nr != $rows2->nelem) {
    ##-- check: ncmps($rows1) == ncmps($rows2)
    carp(ref($cd)."::compare_check(): dimension mismatch: "
	 ."(rows1->nelem==$nr) != (rows2->nelem==".$rows2->nelem.")"
	);
    $rc=0;
  }

  if (defined($args->{mask}) && ($args->{mask}->dim(0) != $d || $args->{mask}->dim(1) != $n)) {
    ##-- check: dims($mask) == dims($data)
    carp(ref($cd)."::compare_check(): dimension mismatch: "
	 ."(mask->dims==(".join(',',$args->{mask}->dims)."))"
	 ." != "
	 ."(data->dims==".join(',',$data->dims)."))"
	);
    $rc=0;
    }

  if (defined($args->{weight}) && $args->{weight}->dim(0) != $d)
    {
      ##-- check: d($weight) == d($data)
      carp(ref($cd)."::compare_check(): dimension mismatch: "
	   ."(weight->dim(0)==".$args->{weight}->dim(0).")"
	   ." != "
	   ."(data->dim(0)==$d)"
	  );
      $rc=0;
    }

  my ($min,$max);
  ($min,$max) = $rows1->minmax();
  if ($min < 0  || $max >= $n) {
    ##-- check: index range for {rows1}
    carp(ref($cd)."::compare_check(): index out of range in 'rows1': "
	 ."(rows1 c= [$min..$max]) !c= (data_rows = [0..".($n-1)."]"
	);
    $rc=0;
  }

  ($min,$max) = $rows2->minmax();
  if ($min < 0  || $max >= $n) {
    ##-- check: index range for {rows2}
    carp(ref($cd)."::compare_check(): index out of range in 'rows2': "
	 ."(rows2 c= [$min..$max]) !c= (data_rows = [0..".($args->{data}->dim(1)-1)."]"
	);
    $rc=0;
  }

  return $rc;
}

##--------------------------------------------------------------
## undef = $cd->compare_defaults(\%args)
##  + %args: as for compare()
##  + instantiates default 'mask' and 'weight' keys in \%args
##  + assumes that \%args is 'sane', e.g. $cd->compare_check(\%args) returned true
sub compare_defaults {
  my ($cd,$args) = @_;
  $args->{mask}   = $args->{data}->isgood->long if (!defined($args->{mask}));
  $args->{weight} = ones(double,$args->{data}->dim(0)) if (!defined($args->{weight}));
  return;
}

##--------------------------------------------------------------
## $cmpvec = $cd->compare_cmpvec(\%args)
##  + %args: as for compare()
##  + returns output pdl $args->{cmpvec}, creating it if necessary
##  + assumes that \%args is 'sane', e.g. $cd->compare_check(\%args) returned true
sub compare_cmpvec {
  my ($cd,$args) = @_;
  return $args->{cmpvec} if (defined($args->{cmpvec}));
  return $args->{cmpvec} = zeroes(double,$args->{rows1}->nelem);
}

##--------------------------------------------------------------
## $cmpvec = $cd->compare_set_cmpvec($cmpvec_arg,$cmpvec_vals)
##  + %args: as for compare()
##  + returns output pdl $cmpvec_arg, setting it to $cmpvec_vals if undefined
sub compare_set_cmpvec {
  #my ($cd,$arg,$vals) = @_;
  return $_[2] if (!defined($_[1]) || $_[1]->isnull);
  $_[1] .= $_[2];
  return $_[1];
}


1; ##-- be happy
