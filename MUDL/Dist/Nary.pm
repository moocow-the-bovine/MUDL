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
##   + zmass : total mass alotted to zero-count events ("missing mass")
##   + nzero : number of zero-probability entries
##   + sep   : field-separator string (default="\t")
sub new {
  my $that = shift;
  my $self = $that->SUPER::new(sep=>"\t", @_);
  #bless $self->{nz}, 'MUDL::Dist::Nary::NZ';
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
## size
sub getSize {
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
  my $prod = 1;
  $prod *= scalar(keys(%$_)) foreach (@sets);
  return $prod;
}


##----------------------------------------------
## conditionalization

##-- implement as another class

## $cnd = $nd->conditionalizeN(\@target_fields, \@given_fields)
##   + converts $nd->{nz} to { "@target_fields\t@given_fields" => P(@target_fields|@given_fields), ... }


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
