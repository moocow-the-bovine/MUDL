#-*- Mode: Perl -*-

## File: MUDL::Dist::Enum.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: distributions: enumerated
##======================================================================

########################################################################
# MUDL::Dist::Enum
########################################################################
package MUDL::Dist::Enum;
use MUDL::Dist;
use MUDL::Enum;
our @ISA = qw(MUDL::Dist);

##======================================================================
## Converters

## $de = $class_or_obj->fromDist($dist,$enum);
sub fromDist {
  my ($de,$d,$e) = @_;
  $de = $de->new if (!ref($de));
  my ($k,$v);
  while (($k,$v)=each(%$d)) {
    $de->{$e->addSymbol($k)} = $v;
  }
  return $de;
}


########################################################################
# MUDL::Dist::Partial::Enum
########################################################################
package MUDL::Dist::Partial::Enum;
use MUDL::Dist::Partial;
use MUDL::Enum;
use Carp;

our @ISA = qw(MUDL::Dist::Partial);

##======================================================================
## Constructor
sub new {
  return $_[0]->SUPER::new(enum=>MUDL::Enum->new(), @_[1..$#_]);
}

##======================================================================
## Converters

## $de = $class_or_obj->fromDist($partialDist,$enum);
sub fromDist {
  my ($de,$d,$e) = @_;
  $de = $de->new if (!ref($de));
  $e = $de->{enum} if (!$e);
  my ($k,$v);
  while (($k,$v)=each(%{$d->{nz}})) {
    $de->{nz}{$e->addSymbol($k)} = $v;
  }
  @$de{qw(size zmass nzero)} = @$d{qw(size zmass nzero)};
  return $de;
}

########################################################################
# MUDL::Dist::Nary::Enum
########################################################################
package MUDL::Dist::Nary::Enum;
use MUDL::Dist::Nary;
use MUDL::Enum;
use Carp;

our @ISA = qw(MUDL::Dist::Nary);

##======================================================================
## Constructor
sub new {
  return $_[0]->SUPER::new(enums=>[], @_[1..$#_]);
}

##======================================================================
## Converters

## $de = $class_or_obj->fromDist($nAryDist,\@enums);
sub fromDist {
  my ($de,$d,$enums) = @_;
  $de = $de->new if (!ref($de));
  $enums = $de->{enums} if (!$enums || !@$enums);
  $de->{enums} = $enums if (!$de->{enums} || !@{$de->{enums}});
  $de->{sep} = $d->{sep};
  my ($k,$v,$e,@fields,$i);
  while (($k,$v)=each(%{$d->{nz}})) {
    @fields = split(/(?:\Q$d->{sep}\E)+/, $k);
    foreach $i (0..$#fields) {
      $enums->[$i] = MUDL::Enum->new() if (!$enums->[$i]);
      $fields[$i] = $enums->[$i]->addSymbol($fields[$i]);
    }
    $de->{nz}{join($de->{sep}, @fields)} = $v;
  }
  @$de{qw(sep size zmass nzero)} = @$d{qw(sep size zmass nzero)};
  return $de;
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
