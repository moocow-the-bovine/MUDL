#-*- Mode: Perl -*-

## File: MUDL::Enum.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: enumerations
##======================================================================

package MUDL::Enum;
use MUDL::XML;
use IO::File;
use Carp;

our @ISA = qw(MUDL::XML::Object);

our $VERSION = 0.01;
our $NoLabel = -1;

sub new {
  my ($that,%args) = @_;
  return bless { sym2id=>{}, id2sym=>[], %args}, ref($that)||$that;
}

##======================================================================
## Accessors

## $id = $e->index($symbol)
*id = \&index;
*sym2id = \&index;
sub index { return $_[0]->{sym2id}{$_[1]}; }

## $sym = $e->symbol($index)
*sym = \&symbol;
*id2sym = \&symbol;
sub symbol { return $_[1] == $NoLabel ? undef : $_[0]->{id2sym}[$_[1]]; }

## @syms = $e->symbols
sub symbols { return @{$_[0]->{id2sym}}; }

## %indices = $e->indices()
*indexes = \&indices;
sub indices { return values(%{$_[0]->{sym2id}}); }

##======================================================================
## Manipulators

## $id = $e->addSymbol($sym)
*add = \&addSymbol;
sub addSymbol {
  my $id = $_[0]->{sym2id}{$_[1]};
  return $id if (defined($id));
  $id = $_[0]->{sym2id}{$_[1]} = scalar @{$_[0]->{id2sym}};
  push(@{$_[0]->{id2sym}}, $_[1]);
  return $id;
}

## $id = $e->addIndexedSymbol($sym,$id)
sub addIndexedSymbol {
  my ($e,$sym,$id) = @_;
  my ($oldid);
  if (defined($oldid=$e->{sym2id}{$sym})) {
    $e->{id2sym}[$oldid] = undef;
  }
  $e->{sym2id}{$sym} = $newid;
  $e->{id2sym}[$id]  = $sym;
  return $id;
}

## undef = $e->clear
sub clear {
  my $e = shift;
  @{$e->{id2sym}} = qw();
  %{$e->{sym2id}} = qw();
  return $e;
}

##======================================================================
## I/O : AT&T

# $e = $e->saveATT($file_or_fh)
sub saveATT {
  my ($e,$file) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  croak( __PACKAGE__ , "::saveATT(): open failed for '$file': $!") if (!$fh);

  my ($lab,$sym);
  my $id2sym = $e->{id2sym};
  for ($lab=0; $lab < scalar(@$id2sym); $lab++) {
    next if (!defined($sym=$id2sym->[$lab]));
    $fh->print($sym, "\t", $lab, "\n");
  }

  $fh->close() if (ref($file));
  return $e;
}

# $e = $e->loadATT($file_or_fh)
sub loadATT {
  my ($e,$file) = @_;
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  croak( __PACKAGE__ , "::loadATT(): open failed for '$file': $!") if (!$fh);

  my ($lab,$sym,$line);
  while (defined($line=<$fh>)) {
    chomp $line;
    next if ($line eq '');
    if ($line !~ /^(.*\S)\s+(\d+)$/) {
      warn( __PACKAGE__ , "::loadATT(): parse error in file '$file' at line ", $fh->input_line_number);
      next;
    }
    ($sym,$lab) = ($1,$2);
    $e->{sym2id}{$sym} = $lab;
    $e->{id2sym}[$lab] = $sym;
  }

  $fh->close() if (ref($file));
  return $e;
}


##======================================================================
## I/O : XML

## $node = $e->saveXMLNode()
sub saveXMLNode {
  my $e = shift;
  (my $nodename = ref($d)) =~ s/::/./g;
  my $node = XML::LibXML::Element->new($nodename);

  my ($lab,$sym,$inode);
  my $id2sym = $e->{id2sym};
  for ($lab=0; $lab < scalar(@$id2sym); $lab++) {
    next if (!defined($sym=$id2sym->[$lab]));
    $node->appendChild($inode=XML::LibXML::Element->new('symbol'));
    $inode->setAttribute('id', $lab);
    if (ref($sym)) {
      $inode->appendText($sym);
    } else {
      $inode->appendChild($sym->saveXMLNode);
    }
  }

  return $node;
}

## $e = $e->loadXMLNode($node)
sub loadXMLNode {
  my ($e,$node) = @_;
  (my $nodename = ref($e)) =~ s/::/./g;
  carp( __PACKAGE__ , "::loadXMLNode() expected '$nodename' element, got '", $node->nodeName, "'\n")
    if ($node->nodeName ne $nodename);

  my ($symnode,$lab,$sym);
  foreach $symnode ($node->getChildrenByTagName('symbol')) {
    $lab = $symnode->getAttribute('id');
    if ($symnode->hasChildNodes) {
      $sym = MUDL::XML::Object->newFromXMLNode($enode->firstChild);
    } else {
      $sym = $symnode->textContent;
    }
    $e->{sym2id}{$sym} = $lab;
    $e->{id2sym}[$lab] = $sym;
  }

  return $e;
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
