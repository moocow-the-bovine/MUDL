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
use Carp;

use strict;

our @ISA = qw(MUDL::Cluster::Distance);


##======================================================================
## Constructor

## $cm = MUDL::Cluster::Distance::Builtin->new(%args);
##  + basic %args:
##     distFlag => $distFlag,    # string: distance-flag for builtin PDL::Cluster distance function
##     distName => $distName,    # optional
##     #class   => $className,   # string: class-name or MUDL::Cluster::Distance:: suffix; overrides $distFlag
sub new {
  my ($that,%args) = @_;
  return $that->SUPER::new(distName=>'(builtin)',distFlag=>'e',%args); ##-- defaults
}


##======================================================================
## API: High-level

##--------------------------------------------------------------
## $dmat = $cd->distanceMatrix(%args)
##  + %args:
##     data   => $data,   ##-- pdl($d,$n) : $d=N_features, $n=N_data
##     mask   => $mask,   ##-- pdl($d,$n) : "feature-is-good" boolean mask [default=ones()]
##     weight => $weight, ##-- pdl($d)    : feature-weight mask (weights distances)
##  [o]dmat  => $dmat,   ##-- pdl($n,$n) : output matrix [optional]
##  + default implementation calls $cd->compare()
sub distanceMatrix {
  my ($cd,%args) = @_;
  croak(ref($cd)."::distanceMatrix(): cowardly refusing undefined 'data' matrix") if (!defined($args{data}));

  ##-- dispatch to PDL::Cluster::distancematrix()
  my $n    = $args{data}->dim(1);
  my $dmat = $args{dmat};
  $dmat = zeroes(double, $n,$n) if (!defined($dmat));
  PDL::Cluster::distancematrix(@args{qw(data mask weight)}, $dmat, $cd->{distFlag});
  return $dmat;
}

##======================================================================
## API: Low-Level

##--------------------------------------------------------------
## $cmpvec = $cd->compare(%args)
##  + %args:
##     data1  => $data1,   ##-- pdl($d,$n1) : $d=N_features, $n1=N_data1                [REQUIRED]
##     data2  => $data2,   ##-- pdl($d,$n1) : $d=N_features, $n1=N_data2                [REQUIRED]
##     rows1  => $rows1,   ##-- pdl($ncmps) : [$i] -> $data1_rowid_for_cmp_i            [REQUIRED]
##     rows2  => $rows2,   ##-- pdl($ncmps) : [$i] -> $data2_rowid_for_cmp_i            [REQUIRED]
##     mask1  => $mask1,   ##-- pdl($d,$n1) : "feature-is-good" boolean mask for $data1 [default=ones()]
##     mask2  => $mask2,   ##-- pdl($d,$n1) : "feature-is-good" boolean mask for $data2 [default=ones()]
##     weight => $weight,  ##-- pdl($d)     : feature-weight mask (weights distances)   [default=ones()]
##  [o]cmpvec => $cmpvec,  ##-- pdl($ncmps) : output pdl [optional]
sub compare {
  my ($cd,%args) = @_;
  $cd->compare_check(\%args) or croak(ref($cd)."::compare(): cowardly refusing to process inconsistent request");
  $cd->compare_defaults(\%args); ##-- ensure mask*, weight

  ##-- forwards-compatibility hack: roll data, mask into a Grand Unified Data Matrix
  my ($d,$n1,$n2) = ($args{data1}->dims,$args{data2}->dim(1));
  my $gudata = $args{data1}->glue(1,$args{data2});
  my $gumask = $args{mask1}->glue(1,$args{mask2});
  my $guwt   = $args{weight};
  my $cmpvec = $cd->compare_cmpvec(\%args);
  PDL::Cluster::rowdistances($gudata,$gumask,$guwt, $args{rows1}, $args{rows2}+$n1, $cmpvec, $cd->{distFlag});
  return $cmpvec;
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
