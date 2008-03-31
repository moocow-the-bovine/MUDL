##
## File: MUDL::Cluster::Distance::Builtin.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: cluster distance functions: PDL::Cluster built-in functions
##======================================================================

package MUDL::Cluster::Distance::Builtin;
use PDL;
use PDL::Cluster;
use MUDL::Object;
#use MUDL::Cluster::Distance; ##-- this package gets read by MUDL::Cluster::Distance
use MUDL::Cluster::LinkMethod; ##-- this package gets read by MUDL::Cluster::Distance (?)
use Carp;

use strict;

our @ISA = qw(MUDL::Cluster::Distance MUDL::Cluster::LinkMethod);


##======================================================================
## Constructor

## $cm = CLASS->new(%args);
##  + basic %args (see MUDL::Cluster::Distance)
##     class   => $className,   # string: class-name or MUDL::Cluster::Distance:: suffix; overrides $distFlag
##  + new %args for MUDL::Cluster::Distance::Builtin
##     distFlag => $distFlag,    # string: distance-flag for builtin PDL::Cluster distance function
##     linkFlag => $linkFlag,    # string: link-method-flag for builtin PDL::Cluster::clusterdistance() function
##                               #  + overrides $cd->{cdLinkFlag}
##     tcLinkFlag=>$tcLinkFlag,  # string: link-method-flag for PDL::Cluster::treecluster()
##                               #  + default: guess equivalent from $cd->{linkFlag}
##     distName => $distName,    # optional
##     linkName => $linkName,    # optional
sub new {
  my ($that,%args) = @_;
  my $cd = $that->SUPER::new(
			     distFlag=>'c',             ##-- default: Pearson's correlation coefficient
			     linkFlag=>'v',             ##-- default: pairwise-average link
			     %args,
			    );
  $cd->{cdLinkFlag} = $cd->{linkFlag}; ##-- override
  $cd->{distName}   = "(builtin:".(defined($cd->{distFlag}) ? $cd->{distFlag} : '?').")" if (!defined($cd->{distName}));
  $cd->{linkName}   = "(builtin:".(defined($cd->{linkFlag}) ? $cd->{linkFlag} : '?').")" if (!defined($cd->{linkName}));
  return $cd;
}


##======================================================================
## API: Distance, LinkMethod: linker access

## $clm = $cd->linker()
##  + just returns the literal $cd object for builtin distance+link functions
sub linker { return $_[0]; }

## $flag = $clm->cdLinkFlag()
##  + returns equivalent "link-method" flag for PDL::Cluster::clusterdistance()
sub cdLinkFlag {
  croak(ref($_[0])."::cdLinkFlag(): no {linkFlag} key defined!") if (!defined($_[0]{linkFlag}));
  return $_[0]{linkFlag};
}

## $flag = $clm->tcLinkFlag()
##  + returns equivalent "link-method" flag for PDL::Cluster::treecluster()
sub tcLinkFlag {
  my $clm = shift;
  return $clm->{tcLinkFlag} if (defined($clm->{tcLinkFlag})); ##-- cached or overridden {tcLinkFlag}

  # LABEL           : clusterdistance()  ~ treecluster()
  # arithmetic_mean : cd=a               ~ tc=f           (?)
  # cluster_median  : cd=m               ~ tc=c           (?)
  # min             : cd=s               ~ tc=s
  # max             : cd=x               ~ tc=m
  # pairwise-avg    : cd=v               ~ tc=a
  ##
  my $lf = $clm->{linkFlag};
  croak(ref($clm)."::tcLinkFlag(): no {linkFlag} key defined!") if (!defined($lf));
  $lf = substr($lf,0,1);

  my ($tclf);
  if    ($lf eq 'a') { $tclf='f'; } ##-- arithmetic mean: cd='a', tc='f' (?)
  elsif ($lf eq 'm') { $tclf='c'; } ##-- cluster median:  cd='m', tc='c' (?)
  elsif ($lf eq 's') { $tclf='s'; } ##-- minimum link:    cd='s', tc='s'
  elsif ($lf eq 'x') { $tclf='m'; } ##-- maximum link:    cd='x', tc='m'
  elsif ($lf eq 'v') { $tclf='a'; } ##-- pairwise-avg:    cd='v', tc='a'
  else {
    croak(ref($clm)."::tcLinkFlag(): can't translate linkFlag='$clm->{linkFlag}' to treecluster() link flag!");
  }

  return $clm->{tcLinkFlag}=$tclf;
}



##======================================================================
## API: Distance: High-level

##--------------------------------------------------------------
## $dmat = $cd->distanceMatrix(%args)
##  + %args:
##     data   => $data,   ##-- pdl($d,$n) : $d=N_features, $n=N_data
##     mask   => $mask,   ##-- pdl($d,$n) : "feature-is-good" boolean mask [default=$data->isgood()]
##     weight => $weight, ##-- pdl($d)    : feature-weight mask            [default=ones()]
##  [o]dmat  => $dmat,   ##-- pdl($n,$n)  : output matrix [optional]
##  + default implementation calls $cd->compare()
sub distanceMatrix {
  my ($cd,%args) = @_;
  croak(ref($cd)."::distanceMatrix(): cowardly refusing undefined 'data' matrix") if (!defined($args{data}));
  $cd->compare_defaults(\%args); ##-- default 'mask', 'weight'

  ##-- dispatch to PDL::Cluster::distancematrix()
  my $n    = $args{data}->dim(1);
  my $dmat = $args{dmat};
  $dmat = zeroes(double, $n,$n) if (!defined($dmat));
  PDL::Cluster::distancematrix(@args{qw(data mask weight)}, $dmat, $cd->{distFlag});
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

  ##-- get defaults (gudata,...)
  $cd->cdm_defaults(\%args);
  my ($gucids,$gurids) = @args{'gucids','gurids'};

  ##-- encode cluster-membership
  my ($clens,$cvals,$crows) = clusterenc($gucids);
  my ($rlens,$rvals,$rrows) = clusterenc($gurids);
  my ($cdmat_raw);
  clusterdistancematrixenc(@args{qw(gudata gumask weight)},
			   $clens,$crows,   $rlens,$rrows,
			   $cdmat_raw=zeroes(double,$clens->dim(0),$rlens->dim(0)),
			   $cd->{distFlag}, $cd->cdLinkFlag,
			  );

  ##-- we might need to do some index-twiddling
  if ( ($cvals!=$cvals->sequence)->any || ($rvals!=$rvals->sequence)->any ) {
   my $cdmat0  = $cdmat_raw;
   my ($k,$nr) = @args{qw(k nr)};
   $k          = $cvals->max+1 if (!defined($k));
   $nr         = $rvals->max+1 if (!defined($nr));
   $cdmat_raw  = pdl(double,"inf")->slice("*$k,*$nr")->make_physical(); ##-- "missing" distances are infinite
   $cdmat_raw->dice_axis(0,$cvals)->dice_axis(1,$rvals) .= $cdmat0;
  }

  ##-- return
  return $cdmat_raw if (!defined($args{cdmat}));

  $args{cdmat} .= $cdmat_raw;
  return $args{cdmat};
}

##======================================================================
## API: Distance: Low-Level

##--------------------------------------------------------------
## $cmpvec = $cd->compare(%args)
##  + %args:
##     #data1  => $data1,   ##-- pdl($d,$n1) : $d=N_features, $n1=N_data1                [REQUIRED]
##     #data2  => $data2,   ##-- pdl($d,$n2) : $d=N_features, $n1=N_data2                [REQUIRED]
##     #mask1  => $mask1,   ##-- pdl($d,$n1) : "feature-is-good" boolean mask for $data1 [default=ones()]
##     #mask2  => $mask2,   ##-- pdl($d,$n2) : "feature-is-good" boolean mask for $data2 [default=ones()]
##     data   => $data,    ##-- pdl($d,$n)  : $d=N_features, $n=N_data                  [REQUIRED]
##     rows1  => $rows1,   ##-- pdl($ncmps) : [$i] -> $data1_rowid_for_cmp_i            [REQUIRED]
##     rows2  => $rows2,   ##-- pdl($ncmps) : [$i] -> $data2_rowid_for_cmp_i            [REQUIRED]
##     mask   => $mask,    ##-- pdl($d,$n)  : "feature-is-good" boolean mask for $data1 [default=$data->isgood()]
##     weight => $weight,  ##-- pdl($d)     : feature-weight mask (weights distances)   [default=ones()]
##  [o]cmpvec => $cmpvec,  ##-- pdl($ncmps) : output pdl [optional]
sub compare {
  my ($cd,%args) = @_;
  $cd->compare_check(\%args) or croak(ref($cd)."::compare(): cowardly refusing to process inconsistent request");
  $cd->compare_defaults(\%args); ##-- ensure mask, weight

  ##-- forwards-compatibility hack: roll data, mask into a Grand Unified Data Matrix
  my $cmpvec = $cd->compare_cmpvec(\%args);
  PDL::Cluster::rowdistances(@args{qw(data mask weight rows1 rows2)}, $cmpvec, $cd->{distFlag});
  return $cmpvec;
}

##======================================================================
## API: LinkMethod: Low-Level

##--------------------------------------------------------------
## ($lwhich,$lcmps) = $clm->compare_link(%args)
##  + cluster-row and cluster-cluster linkage utility
##  + %args
##     which   => $whichX, ##-- int (2,$ncmps) : link keys (cluster-ids) as for indexND [REQUIRED]
##     cmps    => $cmps,   ##-- dbl ($ncmps)   : row-row distances                      [REQUIRED]
##  [o]lwhich  => $lwhich, ##-- int (2,$k*$n)  : unique link keys                       [default=new]
##  [o]lcmps   => $lcmp,   ##-- dbl ($k*$n)    : link-distances for unique link keys    [default=new]
sub compare_link {
  my ($clm,%args) = @_;
  croak(ref($clm)."::compare_link(): not yet implemented!");
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
