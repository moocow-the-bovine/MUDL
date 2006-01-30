#-*- Mode: CPerl -*-

## File: MUDL::Gfsm::ALphabet
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Gfsm::Alphabet wrapper
##======================================================================

package MUDL::Gfsm::Alphabet;
use Gfsm;
use MUDL::Object;

use Carp qw(carp croak);

use strict;
our @ISA = qw(Gfsm::Alphabet MUDL::Object);

##======================================================================
## Constructors

## $abet = $class_or_obj->new(%args)
##  + inherited from Gfsm::Alphabet


##======================================================================
## I/O: Native
##======================================================================

##--------------------------------------------------------------
## I/O: Native: Save

## $str = $obj->saveNativeString(@args)
sub saveNativeString {
  my $abet = shift;
  return join('', map { ($abet->find_key($_), "\t", $_, "\n") } @{$abet->labels});
}

## $obj = $obj->saveNativeFh($fh,%args)
*saveNativeFh = \&Gfsm::Automaton::print_att;


##--------------------------------------------------------------
## I/O: Native: Load

## $obj = $class_or_obj->loadNativeString($str,%args)
sub loadNativeString {
  my ($abet,$str) = @_;
  $abet = $abet->new if (!ref($abet));
  my ($line,$id,$key);
  foreach $line (split(/\n+/, $str)) {
    next if (!defined($line) || $line =~ /^\s*$/);
    ($key,$id) = split(/\s+/,$line);
    $abet->insert($key,$id);
  }
  return $abet;
}

## $obj = $class_or_obj->loadNativeFh($fh,%args)
*loadNativeFH = \&Gfsm::Automaton::compile;


##======================================================================
## I/O: Binary (override Storable routines: use libgfsm binary format)
##======================================================================

## default: uses Storable interface


##======================================================================
## I/O: XML
##======================================================================

##  - NYI

##======================================================================
## Connverters: MUDL::Enum
##======================================================================

## $abet = $class_or_obj->fromEnum($mudl_enum)
sub fromEnum {
  my ($abet,$enum) = @_;
  my ($sym,$id);
  while (($sym,$id)=each(%{$enum->{sym2id}})) {
    croak(ref($abet)."::fromEnum(): Error: label overflow!\n") if ($id >= $Gfsm::noLabel);
    $abet->insert($sym,$id);
  }
  return $abet;
}

## $enum = $obj->toEnum()
## $enum = $obj->toEnum($mudl_enum)
sub toEnum {
  my ($abet,$enum) = @_;
  require MUDL::Enum;
  $enum = MUDL::Enum->new() if (!$enum);
  $abet->toHash($enum->{sym2id});
  $abet->toArray($enum->{id2sym});
  return $enum;
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
