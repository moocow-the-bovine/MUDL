#-*- Mode: Perl -*-

## File: MUDL::Dist::Nary.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: distributions with complex events
##======================================================================

package MUDL::Dist::Nary;
use MUDL::Dist::Partial;
use Carp;

our @ISA = qw(MUDL::Dist::Partial);

## object structure:
##   + size  : number of possible events
##   + sizes : array of sizes (per slot)
##   + zmass : total mass alotted to zero-count events ("missing mass")
##   + nzero : number of zero-probability entries
##   + sep   : field-separator string (default="\t")
sub new {
  my $that = shift;
  my $self = $that->SUPER::new(sep=>"\t", @_);
  $self->{sizes} = [] if (!$self->{sizes});
  return $self;
}

##======================================================================
## Access
##======================================================================

##----------------------------------------------
## Projection

## $subdist = $d->projectN(@indices)
##   + returns a MUDL::Dist::Nary over @indices
*project = \&projectN;
sub projectN {
  my ($d,@indices) = @_;
  my $sub = ref($d)->new(sep=>$d->{sep});
  my ($key,$f);
  while (($key,$f)=each(%{$d->{nz}})) {
    $sub->{nz}{join($sub->{sep}, (split(/(?:\Q$d->{sep}\E)+/, $key))[@indices])} += $f;
  }
  return $sub;
}

## $subdist = $d->project1($index)
##   + returns a MUDL::Dist for $index
sub project1 {
  my ($d,$i) = @_;
  my $sub = MUDL::Dist->new();
  my ($key,$f);
  while (($key,$f)=each(%{$d->{nz}})) {
    $sub->{(split(/(?:\Q$d->{sep}\E)+/, $key))[$i]} += $f;
  }
  return $sub;
}

##----------------------------------------------
## Selection

## $d2 = $d->select($coderef_or_str)
##  + selects all events for which &$coderef_or_str($key,$val) evaluates to true
##  + $codestr can use variables $key,$val and/or parameters @_[0,1]
sub select {
  my ($d,$code) = @_;
  $code = eval qq(sub { $code }) if (!ref($code));
  my $d2 = ref($d)->new();
  $d2->{sep} = $d->{sep};
  my ($key,$val);
  while (($key,$val)=each(%{$d->{nz}})) {
    $d2->{nz}{$key} = $val if (&$code($key,$val));
  }
  return $d2;
}


##----------------------------------------------
## clear
sub clear {
  @{$_[0]{sizes}} = qw();
  return $_[0]->SUPER::clear();
}

##----------------------------------------------
## size

## \@sizes = $d->sizes()
##   + returns size of each slot
sub sizes {
  return @{$_[0]{sizes}} ? $_[0]{sizes} : $_[0]->{sizes} = $_[0]->getSizes;
}

## \@sizes = $d->getSizes()
##   + recomputes size per slot
sub getSizes {
  my $d = shift;
  my @sets = qw();
  my ($k,@fields,$i);
  foreach $k (keys(%{$d->{nz}})) {
    @fields = split(/(?:\Q$d->{sep}\E)+/, $k);
    for ($i=0; $i < @fields; $i++) {
      $sets[$i] = {} if (!$sets[$i]);
      $sets[$i]{$fields[$i]} = undef;
    }
  }
  @{$d->{sizes}} = map { scalar(keys(%$_)) } @sets;
  return $d->{sizes};
}

## $size = $d->getSize();
sub getSize {
  my $d = shift;
  my $prod = 1;
  $prod *= $_ foreach (@{$d->{sizes}});
  return $prod;
}


##----------------------------------------------
## conditionalization

##-- implement as another class

## $cnd = $nd->conditionalizeN(\@given_fields)
##   + converts $nd->{nz} to { $event => P($event|@given_fields), ... }
sub conditionalize {
  my ($d,$gf) = @_;
  my $gd = $d->projectN(@$gf);
  my ($k,$gk);
  foreach $k (keys(%{$d->{nz}})) {
    @fields = split(/(?:\Q$d->{sep}\E)+/, $k);
    $gk = join($d->{sep}, @fields[@$gf]);
    $d->{nz}{$k} /= $gd->{nz}{$gk};
  }
  return $d;
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
