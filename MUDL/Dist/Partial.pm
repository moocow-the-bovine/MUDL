#-*- Mode: Perl -*-

## File: MUDL::Dist::Partial.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: partial distributions
##======================================================================

package MUDL::Dist::Partial;
use MUDL::Dist;
use Carp;

our @ISA = qw(MUDL::Object);

## object structure:
##   + size  : number of possible events
##   + zmass : total mass alotted to zero-count events ("missing mass")
##   + nzero : number of zero-probability entries
sub new {
  my $that = shift;
  return bless {
		nz=>MUDL::Dist->new(),
		size=>0,
		zmass=>0,
		nzero=>0,
		@_
	       }, ref($that)||$that;
}

##======================================================================
## Access
##======================================================================

## \%hash = $d->f()
## $freq  = $d->f($event)
##  + alias: p(), nz()
*p = \&f;
*nz = \&f;
sub f {
  if (@_ == 2) {
    return $_[0]->{nz}{$_[1]};
  }
  return $_[0]->{nz}{$_[1]} = $_[2];
}

## $d = $d->clear()
sub clear {
  my $d = shift;
  $d->{nz}->clear;
  $d->{size} = 0;
  $d->{zmass} = 0;
  $d->{nzero} = 0;
  return $d;
}

## @events = $d->events()

## $size = $d->size()
## $size = $d->size($size)
##  + should only be called in first form after all events have been added
sub size {
  my $d = shift;
  if (@_) {
    return $d->{size} = shift;
  }
  return $d->{size} ? $d->{size} : $d->{size}=$d->getSize;
}

## $size = $d->getSize()
##  + recomputes {size} member
sub getSize { return scalar(keys(%{$_[0]->{nz}})); }

## $nzero = $d->nzero()
##  + should only be called in first form after all non-zero events have been added
*nZero = \&nzero;
sub nzero { $_[0]{nzero} ? $_[0]{nzero} : $_[0]->getNzero; }

## $nZero = $d->getNZero()
##  + recomputes {nzero} member
*getnzero = *getNzero = \&getNZero;
sub getNZero {
  my $d = shift;
  return $d->{nzero} = $d->size - scalar(grep {$_ != 0} values(%{$d->{nz}}));
}

##======================================================================
## Smoothing & Utilities
##======================================================================

## $total = $d->total()
##   + includes "missing mass"
sub total {
  my $d = shift;
  my $nztotal = $d->{nz}->total;
  return $nztotal + $nztotal*$d->{zmass};
}

## $zmass = $d->missingMass()
##  + get total missing mass
*zmass = \&missingMass;
sub missingMass { return $_[0]->{zmass}; }

## $d = $d->normalize
## $d = $d->normlaize($total);
sub normalize {
  my ($d,$total) = @_;
  $total = $d->total if (!$total);
  $d->{nz}->normalize($total);
  return $d;
}

## $nnz = $d->nNonZero
sub nNonZero { return $_[0]->size - $_[0]->nZero; }

## $zmass1 = $d->zeroCount()
##  + returns quantity of missing mass alotted to any single zero-count event
##  + uses $d->size()
sub zeroCount { return $_[0]->{zmass} / $_[0]->nZero; }

## $zmass = $dist->smoothGTLogLin()
##  + Good-Turing smoothing, log-linear
##  + sets key 'zmass' for missing mass
sub smoothGTLogLin { return $_[0]{zmass} = $_[0]{nz}->smoothGTLogLin; }

##======================================================================
## Metrics, etc.
##======================================================================

## $mean = $d->mean()
## $mean = $d->mean($total)
## $mean = $d->mean($total,$nvalues)
sub mean {
  my $d = shift;
  my $total = shift || $d->total;
  my $size = shift || $d->size;
  $total += $d->zmass;
  return $d->{nz}->mean($total,$size);
}

## $H = $d->entropy()
## $H = $d->entropy($total)
*H = \&entropy;
sub entropy {
  my $d = shift;
  my $total = shift||$d->total;
  my $H = $d->{nz}->entropy($total);
  my $nz = $d->nZero;
  if ($d->{zmass} && $nz) {
    #$H += $nz * ($d->{zmass}/$nz * log($total/($d->{zmass}/$nz))/log(2));
    $H += $d->{zmass} * log($total/($d->{zmass}/$nz))/log(2) if ($d->{zmass} && $nz);
  }
  return $H;
}


##======================================================================
## I/O: Native
##======================================================================

## $bool = $obj->saveNativeFh($fh,%args)
##   + zmass, N are not saved in native format
sub saveNativeFh {
  my $d = shift;
  return $d if ($d->{nz}->saveNativeFh(@_));
  return undef;
}

## $obj = $class_or_obj->loadNativeFh($fh,%args)
##  + LOSSY
sub loadNative {
  my $d = shift;
  $d = $d->new() if (!ref($d));
  return $d if ($d->{nz}->loadNativeFh(@_));
  return undef;
}


##======================================================================
## AUTOLOAD: pass to 'nz' member
##======================================================================

##-- ... but don't try to autoload DESTROY ...
sub DESTROY {}

our $AUTOLOAD;
sub AUTOLOAD {
  my $d = shift;
  return undef if (!defined($d) || !defined($d->{nz}));
  (my $name = $AUTOLOAD) =~ s/.*:://; ##-- strip qualification
  my ($sub);
  if (!($sub=$d->{nz}->can($name))) {
    croak( __PACKAGE__ , "::$name() not defined in AUTOLOAD.\n");
  }
  return &$sub($d->{nz},@_);
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
