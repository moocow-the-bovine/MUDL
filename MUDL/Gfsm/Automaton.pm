#-*- Mode: CPerl -*-

## File: MUDL::Gfsm::Automaton
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Gfsm::Automaton wrapper
##======================================================================

package MUDL::Gfsm::Automaton;
use Gfsm;
use MUDL::Object;

use strict;
our @ISA = qw(Gfsm::Automaton MUDL::Object);

##======================================================================
## Constructors

## $fsm = $class_or_obj->new(%args)
##  + inherited from Gfsm::Automaton


##======================================================================
## I/O: Native
##======================================================================

##--------------------------------------------------------------
## I/O: Native: Save

## $str = $obj->saveNativeString(@args)
sub saveNativeString {
  my $fsm = shift;
  my $str = '';
  return $fsm->print_att_string($str) ? $str : undef;
}

## $obj = $obj->saveNativeFh($fh,%args)
*saveNativeFh = \&Gfsm::Automaton::print_att;


##--------------------------------------------------------------
## I/O: Native: Load

## $obj = $class_or_obj->loadNativeString($str,%args)
*loadNativeString = \&Gfsm::Automaton::compile_string;

## $obj = $class_or_obj->loadNativeFh($fh,%args)
*loadNativeFH = \&Gfsm::Automaton::compile;


##======================================================================
## I/O: Binary (override Storable routines: use libgfsm binary format)
##======================================================================

##--------------------------------------------------------------
## I/O: Binary: Save

## $str = $obj->saveBinString(@args)
sub saveBinString {
  my $fsm = shift;
  my $str = '';
  return $fsm->save_string($str) ? $str : undef;
}

## $bool = $obj->saveBinFile($filename,%args)
##  %args : zlevel=>$zlevel
sub saveBinFile {
  my ($fsm,$filename,%args) = @_;
  return $fsm->save($filename,$args{zlevel});
}

## $bool = $obj->saveBinFh($fh,@args)
*saveBinFh = \&saveBinFile;

##--------------------------------------------------------------
## I/O: Binary: Load

## $obj_or_undef = $class_or_obj->loadBinString($str,@args)
sub loadBinString {
  my $obj = shift;
  $obj = $obj->new() if (!ref($obj));
  return $obj->load_string(shift) ? $obj : undef;
}

## $obj_or_undef = $class_or_obj->loadBinFile($filename,@args)
sub loadBinFile {
  my ($obj,$file) = @_;
  $obj = $obj->new() if (!ref($obj));
  return $obj->load($file) ? $obj : undef;
}

## $obj_or_undef = $class_or_obj->loadBinFh($fh,@args)
*loadBinFh = \&loadBinFile;


##======================================================================
## I/O: XML
##======================================================================

##  - NYI


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
