#-*- Mode: Perl -*-

## File: MUDL::Set.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: sets
##======================================================================

package MUDL::Set;
use MUDL::Object qw(dummy);
use IO::File;
use Carp;

our @ISA = qw(MUDL::Object);

##======================================================================
## New

## $set = Set->new(@elts)
sub new {
  return bless { (map { $_=>$_ } @_[1..$#_]) }, ref($_[0])||$_[0];
}

##======================================================================
## Accessors

## @elts = $set->elements
*members = \&elements;
sub elements { return values(%{$_[0]}); }

## $size = $set->size
sub size { return scalar(values(%{$_[0]})); }

##======================================================================
## Operations

##-- not yet implemented
*union = dummy('union');
*intersection = dummy('intersection');
*difference = dummy('difference');


##======================================================================
## I/O: XML
##======================================================================

## $node = $set->entry2XMLNode($key,$val)
sub entry2XMLNode {
  my $node = XML::LibXML::Element->new('value');
  $node->appendChild(MUDL::Object::saveXMLNode($_[2]));
  return $node;
}

## undef = $set->XMLNode2Entry($node)
sub XMLNode2Entry {
  my ($s,$node) = @_;
  my $val = MUDL::Object->loadXMLNode($node->firstChild);
  $s->{$val} = $val;
}

##======================================================================
## I/O: Native
##======================================================================

## $bool = $obj->saveNativeFh($fh)
sub saveNativeFh {
  my ($s,$fh) = @_;
  foreach (values(%$s)) {
    $fh->print($_, "\n");
  }
  return $s;
}

## $bool = $obj->loadNativeFh($fh)
sub loadNativeFh {
  my ($s,$fh) = @_;
  $s = $s->new() if (!ref($s));
  my ($val);
  while (defined($val=$fh->getline)) {
    chomp($val);
    $s->{$val} = $val;
  }
  return $s;
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
