##
## File: MUDL::Cluster::LinkMethod.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: generic clustering (cluster-row) distance linkage
##======================================================================

package MUDL::Cluster::LinkMethod;
use PDL;
use MUDL::Object;
use MUDL::CmdUtils qw();
use Carp;

use strict;

our @ISA = qw(MUDL::Object);

## %LINK_ALIAS = ($alias => [$CLASS_OR_SUFFIX, %class_new_args], ...)
##  + maps builtin suffixes to constructor arguments
our (%LINK_ALIAS);
BEGIN {
  %LINK_ALIAS =
    (
     ##-- PDL::Cluster::clusterdistance() built-in linkage methods
     'a' => ['Builtin', linkFlag=>'a', linkName=>'mean'],
     'm' => ['Builtin', linkFlag=>'m', linkName=>'median'],
     's' => ['Builtin', linkFlag=>'s', linkName=>'minimum'],
     'x' => ['Builtin', linkFlag=>'s', linkName=>'maximum'],
     'x' => ['Builtin', linkFlag=>'s', linkName=>'average'],
     ##
     ##-- PDL::Cluster:treecluster() built-in linkage methods (for treecluster())
     #'s' => ['Builtin', linkFlag=>'s', linkName=>'minimum'],
     #'m' => ['Builtin', linkFlag=>'m', linkName=>'maximum'],
     #'a' => ['Builtin', linkFlag=>'a', linkName=>'average'],
     #'c' => ['Builtin', linkFlag=>'a', linkName=>'centroid'],
    );
}

##======================================================================
## Generic constructor

## $lm = MUDL::Cluster::LinkMethod->new(%args);
##  + basic %args:
##     class    => $className,  # string: class-name or -alias or MUDL::Cluster::LinkMethod:: suffix
##     ...
sub new {
  my ($that,%args) = @_;

  ##-- optional class argument: dispatch
  if (!ref($that) && defined($args{class})) {
    $that = $args{class};
    my @alias_args = qw();
    while (ref($that) && ref($that) eq 'ARRAY') {
      ($that,@alias_args) = @$that;
      $that = $LINK_ALIAS{$that} if (defined($LINK_ALIAS{$that}));
      %args = (@alias_args,%args);
    }
    delete($args{class});
    $that = "MUDL::Cluster::LinkMethod::$that" if ($that !~ /::/);
    MUDL::CmdUtils::loadModule($that);
    return $that->new(%args);
  }

  return $that->SUPER::new(%args);
}


##======================================================================
## API: High-level

##--------------------------------------------------------------
## $cdmat = compare_link(%args)
##  + cluster-row distances
##  + %args
##     data   => $data,   ##-- dbl ($d,$n) : $d=N_features, $n=N_data                [REQUIRED]
##     cids   => $cids,   ##-- int ($nc)  : cluster-ids by row-id     [REQUIRED]
##     rids   => $rids,   ##-- int ($nr)  : row-ids
##     mask   => $mask,   ##-- int ($d,$n) : "feature-is-good" boolean mask          [default=$data->isgood()]
##     weight => $weight, ##-- dbl ($d)    : feature-weight mask (weights distances) [default=ones($d)]


#	 ($clens,$cvals,$crows)=clusterenc($cids);
#	 clusterdistancematrixenc($data,$mask,$wt, $clens,$crows, $rids->ones,$rids,
#				  $cdmat_enc=zeroes(double,$k,$n),
#				  $distFlag,$methodFlag);


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
