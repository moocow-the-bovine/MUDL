##
## File: MUDL::Cluster::Distance.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: generic clustering (row-row) and (cluster-row) distance functions
##======================================================================

package MUDL::Cluster::Distance;
use PDL;
use MUDL::Object;
use MUDL::CmdUtils qw();
#use MUDL::Cluster::Distance::Builtin;
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
##  + for builtin methods (see MUDL::Cluster::Distance::Builtin)
##     #distFlag => $distFlag,  # for PDL::Cluster::distancematrix(), PDL::Cluster::clusterdistancematrix(), ???
##     #linkFlag => $linkFlag,  # for PDL::Cluster::distancematrix(), PDL::Cluster::clusterdistancematrix(), ???
sub new {
  my ($that,%args) = @_;

  ##-- optional class argument: dispatch
  if (!ref($that) && defined($args{class})) {
    $that = $args{class};
    my @alias_args = qw();
    while (ref($that) && ref($that) eq 'ARRAY') {
      ($that,@alias_args) = @$that;
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

## [proposal, v2]:
##  + returns distance matrix between each pair of cluster-id values in ($cids1,$cids2)
##  + %args
##     data   => $data,     ##-- dbl ($d,$n)     : (joined): $d=N_feat, $n=N_elts            [REQUIRED]
##     cids1  => $cids1,    ##-- int ($nce)      : dim=0 cluster-ids by rowid, 0<=$cid1<$k1  [default: sequence($n)]
##     cids2  => $cids2,    ##-- int ($nce)      : dim=1 cluster-ids by rowid, 0<=$cid2<$k2  [default: sequence($n)]
##     mask   => $mask,     ##-- int ($d,$n)     : boolean mask for $data                    [default=$data->isgood]
##     weight => $weight,   ##-- dbl ($d)        : feature-wise distance weights             [default=ones($d)]
##  [o]cdmat  => $cdmat,    ##-- dbl ($k1,$k2)   : output matrix                             [optional]

## [proposal, v1]:
##  + returns distance matrix between each cluster-id listed in $cids() and each row in $data()
##  + %args
##     data   => $data,     ##-- dbl ($d,$nde)   : $d=N_features, $nde=N_data_elts           [REQUIRED]
##     cdata  => $cdata,    ##-- dbl ($d,$nce)   : $d=N_features, $nce=N_clustered_elts      [REQUIRED]
##     cids   => $cids,     ##-- int ($nce)      : cluster-ids by $cdata row-id, 0<=$cid<$k  [default: sequence($nce)]
##     mask   => $mask,     ##-- int ($d,$nde)   : boolean mask for $data                    [default=$data->isgood]
##     cmask  => $cmask,    ##-- int ($d,$nce)   : boolean mask for $cdata                   [default=$cdata->isgood]
##     weight => $weight,   ##-- dbl ($d)        : feature-weight mask (weights distances)   [default=ones($d)]
##  [o]cdmat  => $cdmat,    ##-- dbl ($k,$nde)   : output matrix                             [optional]

sub clusterDistanceMatrix {
  my ($cd,%args) = @_;
  croak(ref($cd)."::clusterDistanceMatrix(): not yet implemented!");
}

##======================================================================
## Utils: for high-level API

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
##     #data1  => $data1,   ##-- pdl($d,$n1) : $d=N_features, $n1=N_data1                [REQUIRED]
##     #data2  => $data2,   ##-- pdl($d,$n1) : $d=N_features, $n1=N_data2                [REQUIRED]
##     #mask1  => $mask1,   ##-- pdl($d,$n1) : "feature-is-good" boolean mask for $data1 [default=ones()]
##     #mask2  => $mask2,   ##-- pdl($d,$n1) : "feature-is-good" boolean mask for $data2 [default=ones()]
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
