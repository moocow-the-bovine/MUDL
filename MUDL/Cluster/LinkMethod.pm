##
## File: MUDL::Cluster::LinkMethod.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL: generic clustering (cluster-row & cluster-cluster) distance linkage, native perl
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
     ##
     ##-- Native linkage methods: aliases
     'min' => ['Minimum'],
     'max' => ['Maximum'],
     'avg' => ['GroupAverage'],
     'Min' => ['Minimum'],
     'Max' => ['Maximum'],
     'Avg' => ['GroupAverage'],
     'Average' => ['GroupAverage'],
     ##
     ##-- PDL::Cluster::clusterdistance() built-in linkage methods
     #'s' => ['Builtin', linkFlag=>'s', linkName=>'minimum'],
     #'x' => ['Builtin', linkFlag=>'s', linkName=>'maximum'],
     #'v' => ['Builtin', linkFlag=>'s', linkName=>'average'],
     #'a' => ['Builtin', linkFlag=>'a', linkName=>'mean'],
     #'m' => ['Builtin', linkFlag=>'m', linkName=>'median'],
     ###
     ###-- PDL::Cluster:treecluster() built-in linkage methods (for treecluster())
     ##'s' => ['Builtin', linkFlag=>'s', linkName=>'minimum'],
     ##'m' => ['Builtin', linkFlag=>'m', linkName=>'maximum'],
     ##'a' => ['Builtin', linkFlag=>'a', linkName=>'average'],
     ##'c' => ['Builtin', linkFlag=>'a', linkName=>'centroid'],
    );
}

##======================================================================
## Generic constructor

## $clm = MUDL::Cluster::LinkMethod->new(%args);
##  + basic %args:
##     class      => $className,  # string: class-name or -alias or MUDL::Cluster::LinkMethod:: suffix
##     tcLinkFlag => $tcFlag,     # string: equivalent 'method' flag for PDL::Cluster::treecluster()
##     cdLinkFlag => $cdFlag,     # string: equivalent 'method' flag for PDL::Cluster::clusterdistance() & friends
sub new {
  my ($that,%args) = @_;

  ##-- optional class argument: dispatch
  if (!ref($that) && defined($args{class})) {
    $that = $args{class};
    my @alias_args = qw();
    while ( (ref($that) && ref($that) eq 'ARRAY') || exists($LINK_ALIAS{$that}) ) {
      ($that,@alias_args) = @$that if (ref($that) && ref($that) eq 'ARRAY');
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
## API: Labelling

## $str = $clm->linkName()
##  + returns {linkName} key if defined, otherwise ref($clm)
sub linkName {
  my $clm = shift;
  return defined($clm->{linkName}) ? $clm->{linkName} : ref($clm);
}

##======================================================================
## API: PDL::Cluster equivalence (for treecluster() hacks)

## $flag = $clm->cdLinkFlag()
##  + returns $clm->{cdLinkFlag} if defined
##  + returns equivalent "link-method" flag for PDL::Cluster::clusterdistance()
##  + croak()s if no equivalent link method exists [default]
sub cdLinkFlag {
  my $clm   = shift;
  return $clm->{cdLinkFlag} if (defined($clm->{cdLinkFlag}));
  croak(ref($clm)."::cdLinkFlag(): no equivalent built-in link method known!");
}

## $flag = $cd->tcLinkFlag()
##  + returns equivalent "link-method" flag for PDL::Cluster::treecluster(), PDL::Cluster::treeclusterd()
##  + returns $cd->{tcLinkFlag} if defined
##  + croak()s if no equivalent link method exists [default]
sub tcLinkFlag {
  my $clm = shift;
  return $clm->{tcLinkFlag} if (defined($clm->{tcLinkFlag}));
  croak(ref($clm)."::tcLinkFlag(): no equivalent built-in link method known!");
}



##======================================================================
## API: Low-level

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


##======================================================================
## Utilities: Low-level

## $bool = $clm->compare_link_check(\%args)
##  + checks sanity of \%args for $clm->compare_link()
sub compare_link_check {
  my ($clm,$args) = @_;
  my $rc=1;

  if (!defined($args->{which})) {
    ##-- check: $which
    carp(ref($clm)."::compare_link_check(): no 'which' parameter specified");
    $rc=0;
  }

  if (!defined($args->{cmps})) {
    ##-- check: $cmps
    carp(ref($clm)."::compare_link_check(): no 'cmps' parameter specified");
    $rc=0;
  }

  if ($args->{which}->dim(1) != $args->{cmps}->dim(0)) {
    ##-- check: ncmps($which) == ncmps($cmps)
    carp(ref($clm)."::compare_link_check(): dimension mismatch in 'ncmps': "
	 ."ncmps(which)=".$args->{which}->dim(1)
	 ." != "
	 ."ncmps(cmps)=".$args->{cmps}->dim(0)
	);
    $rc=0;
  }

  return $rc;
}


## ($lwhich_ret,$lcmps_ret) = $clm->compare_link_set($lwhich,$lcmps,\%args)
##  + returns ($lwhich,$lcmps), setting keys in \%args if required
##  + %args are as for $clm->compare()
sub compare_link_set {
  my ($clm,$lwhich,$lcmps,$args) = @_;
  if (defined($args->{lwhich})) { $lwhich = ($args->{lwhich} .= $lwhich); }
  if (defined($args->{lcmps}))  { $lcmps  = ($args->{lcmps}  .= $lcmps);  }
  return ($lwhich,$lcmps);
}


1; ##-- be happy
