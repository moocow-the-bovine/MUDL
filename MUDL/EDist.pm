#-*- Mode: Perl -*-

## File: MUDL::EDist.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: distributions: enumerated
##======================================================================

########################################################################
# MUDL::EDist
########################################################################
package MUDL::EDist;
use MUDL::Dist;
use MUDL::Enum;
use MUDL::PDL;
use PDL;
our @ISA = qw(MUDL::Dist::Partial);

##======================================================================
## EDist: new
sub new {
  return bless {
		#nz => $_[0]->SUPER::new(),
		nz => MUDL::Dist->new(),
		enum => MUDL::Enum->new,
		@_[1..$#_]
	       }, ref($_[0])||$_[0];
}

##======================================================================
## EDist: Access

## $enum = $ed->enum()
## $enum = $ed->enum($enum);
sub enum { return $_[1] ? $_[0]{enum}=$_[1] : $_[0]{enum}; }

## $dist = $ed->dist()
## $dist = $ed->dist($dist);
sub dist { return $_[1] ? $_[0]{nz}=$_[1] : $_[0]{nz}; }

##======================================================================
## EDist: Converters: Dist (plain)

## $d = $de->toDist();
## $d = $de->toDist($d);
sub toDist {
  my ($de,$d) = @_;
  $d = MUDL::Dist->new() if (!$d);
  @$d{map { $de->{enum}->symbol($_) } keys(%{$de->{nz}})} = values(%{$de->{nz}});
  return $d;
}

##======================================================================
## EDist: Converters: PDL

## $de = $class_or_obj->fromPDLnz($pdl)
sub fromPDLnz {
  my ($de,$pdl) = @_;
  $de = $de->new() if (!ref($de));
  my $which = $pdl->which;
  @{$de->{nz}}{$which->list} = $pdl->dice($which)->list;
  return $de;
}

## $de = $class_or_obj->fromPDL($pdl)
sub fromPDL {
  my ($de,$pdl) = @_;
  $de = $de->new() if (!ref($de));
  @{$de->{nz}}{0..($pdl->nelem-1)} = $pdl->list;
  return $de;
}

## $pdl = $de->toPDL()
sub toPDL {
  my $de = shift;
  my $pdl = MUDL::PDL->new(zeroes( pdl([keys %{$de->{nz}}])->max + 1));
  $pdl->set($_, $de->{nz}{$_}) foreach (keys(%{$de->{nz}}));
  return $pdl;
}

########################################################################
# MUDL::EDist::Partial
########################################################################
package MUDL::EDist::Partial;
use MUDL::Dist::Partial;
use MUDL::Enum;
use PDL;
use Carp;

our @ISA = qw(MUDL::Dist::Partial); # MUDL::EDist

##======================================================================
## EDist::Partial : Constructor
sub new {
  return $_[0]->SUPER::new(
			   enum=>MUDL::Enum->new(),
			   nz=>MUDL::EDist->new(),
			   @_[1..$#_]
			  );
}

##======================================================================
## EDist::Partial : Converters: Dist::Partial

## $d = $de->toDist();
## $d = $de->toDist($d);
sub toDist {
  my ($de,$d) = @_;
  #$d = $de->MUDL::EDist::toDist($d);
  $d = MUDL::Dist::Partial->new() if (!defined($d));
  @{$d->{nz}}{map { $de->{enum}->symbol($_) } keys(%{$de->{nz}})} = values(%{$de->{nz}});
  @$d{qw(size zmass nzero nfields)} = @$de{qw(size zmass nzero nfields)};
  return $d;
}

##======================================================================
## EDist::Partial : Converters: PDL

## $de = $class_or_obj->fromPDLnz($pdl)
##   + ignore zeroes
sub fromPDLnz {
  my ($de,$pdl) = @_;
  $de = $de->new() if (!ref($de));
  my $which = $pdl->which;
  @{$de->{nz}}{$which->list} = $pdl->dice($which)->list;
  return $de;
}

## $de = $class_or_obj->fromPDL($pdl)
##   + ignore zeroes
sub fromPDL {
  my ($de,$pdl) = @_;
  $de = $de->new() if (!ref($de));
  @{$de->{nz}}{0..($pdl->nelem-1)} = $pdl->list;
  return $de;
}

## $pdl = $de->toPDL()
sub toPDL {
  my $de = shift;
  #my $pdl = zeroes( pdl([keys %$de])->max + 1 );
  my $pdl = MUDL::PDL->new(zeroes scalar(@{$de->{enum}{id2sym}}));
  $pdl->set($_, $de->{nz}{$_}) foreach (keys(%{$de->{nz}}));
  return $pdl;
}


########################################################################
# MUDL::EDist::Nary
########################################################################
package MUDL::EDist::Nary;
use MUDL::Dist::Nary;
use MUDL::Enum::Nary;
use PDL;
use Carp;

our @ISA = qw(MUDL::EDist::Partial MUDL::Dist::Nary);

##======================================================================
## Constructor
sub new {
  return $_[0]->MUDL::Dist::Nary::new(
				      enum=>MUDL::Enum::Nary->new(@_[1..$#_]),
				      @_[1..$#_]
				     );
}

##======================================================================
## Converters: Dist (plain)

## $d = $de->toDist();
## $d = $de->toDist($d);
sub toDist {
  my ($de,$d) = @_;

  #$d = MUDL::Dist::Nary->new() if (!$d);
  #@$d{qw(sep size zmass nzero)} = @$ed{qw(sep size zmass nzero)};
  #my $enum = $de->{enum};
  #@{ $d->{nz} }{
  #  map {
  #    join($d->{sep}, $enum->symbol($de->split($_)))
  #  } keys(%{$de->{nz}})}
  #  = values(%{$de->{nz}});

  $d = MUDL::Dist::Nary->new(sep=>$de->{sep}) if (!$d);
  $de->MUDL::EDist::Partial::toDist($d);

  return $d;
}


##======================================================================
## Converters: PDL

## $de = $class_or_obj->fromPDLnz($pdl)
sub fromPDLnz {
  my ($de,$pdl) = @_;
  $de = $de->new() if (!ref($de));
  my @w = $pdl->whichND->list;
  my (@i);
  while (@i=splice(@w,0,$pdl->ndims)) {
    @{$de->{nz}}{join($de->{sep}, @i)} = $pdl->at(@i);
  }
  return $de;
}

## $de = $class_or_obj->fromPDL($pdl)
##  -- TODO!
sub fromPDL {
  my ($de,$pdl) = @_;
  $de = $de->new() if (!ref($de));
  my @w = $pdl->whichND->list;
  my (@i);
  while (@i=splice(@w,0,$pdl->ndims)) {
    @{$de->{nz}}{join($de->{sep}, @i)} = $pdl->at(@i);
  }
  return $de;
}

## $pdl = $de->toPDL()
sub toPDL {
  my $de = shift;
  my $pdl = MUDL::PDL->new(zeroes( map { scalar(@{$_->{id2sym}}) } @{$de->{enum}{enums}} ));
  $de->{nfields} = $pdl->ndims;
  foreach (keys(%{$de->{nz}})) {
    $pdl->set($de->split($_), $de->{nz}{$_});
  }
  return $pdl;
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
