#-*- Mode: Perl -*-

## File: MUDL::Enum.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: enumerations
##======================================================================

package MUDL::Enum;
use MUDL::Object;
use IO::File;
use Carp;

our @ISA = qw(MUDL::Object);

our $NoLabel = -1;

##======================================================================
## Constructor
sub new {
  my ($that,%args) = @_;
  return bless { sym2id=>{}, id2sym=>[], %args}, ref($that)||$that;
}


##======================================================================
## Accessors

## $size = $e->size
sub size { return scalar(@{$_[0]{id2sym}}); }

## $id = $e->index($symbol)
*id = *sym2id = \&index;
sub index { return $_[0]->{sym2id}{$_[1]}; }

## $sym = $e->symbol($index)
*sym = *id2sym = \&symbol;
sub symbol { return $_[1] == $NoLabel ? undef : $_[0]->{id2sym}[$_[1]]; }

## @syms = $e->allSymbols
sub allSymbols { return @{$_[0]->{id2sym}}; }

## @indices = $e->allIndices()
sub allIndices { return values(%{$_[0]->{sym2id}}); }

##======================================================================
## Manipulators: General

## undef = $e->clear
sub clear {
  my $e = shift;
  @{$e->{id2sym}} = qw();
  %{$e->{sym2id}} = qw();
  return $e;
}

## $e = $e->compactFast()
## $e = $e->compactFast(\@changed)
##   + renumbers elements, possibly keeping track of changes
sub compact {
  my ($e,$changed) = @_;
  my $id2s = $e->{id2sym};
  my $s2id = $e->{sym2id};
  my $offset = 0;
  my ($i,$s);
  foreach $i (0..$#$id2s) {
    if (!defined($s=$id2s->[$i])) {
      ++$offset;
      next;
    }
    $s2id->{$s} -= $offset;
    @$id2s[$i-$offset,$i] = @$id2s[$i,$i-$offset];
    $changed->[$i] = $i-$offset if ($changed);
  }
  return $e;
}

##======================================================================
## Manipulators: Elements

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

## $old_id = $e->removeSymbol($sym)
*rmSymbol = *rmsym = \&removeSymbol;
sub removeSymbol {
  my ($e,$sym) = @_;
  my $i = $e->{sym2id}{$sym};
  delete($e->{sym2id}{$sym});
  delete($e->{id2sym}[$i]) if (defined($i));
  return $i;
}

## $old_sym = $e->removeIndex($i)
*rmIndex = *rmindex = *rmid = *rmId = *removeId = \&removeIndex;
sub removeIndex {
  my ($e,$i) = @_;
  my $s = $e->{id2sym}[$i];
  delete($e->{id2sym}[$i]);
  delete($e->{sym2id}{$s}) if (defined($s));
  return $s;

}

##======================================================================
## Conversion: Encoding
use Encode;

## $enum = $enum->recodeAll($fromenc, $toenc)
##  + decodes all strings to $encoding
##  + not helpful
sub recodeAll {
  my ($enum,$from,$to) = @_;

  %{$enum->{sym2id}} = qw();
  foreach $i (0..$#{$enum->{id2sym}}) {
    next if (!defined($enum->{id2sym}[$i]));
    Encode::from_to($enum->{id2sym}[$i], $from, $to);
    $enum->{sym2id}{$enum->{id2sym}[$i]} = $i;
  }
  return $enum;
}

## $enum = $enum->encode($dst_encoding)
##  + wrapper for Encode::encode: convert from perl internal to $dst_encoding
##  + helpful!
sub encodeAll {
  my ($enum,$dst) = @_;

  %{$enum->{sym2id}} = qw();
  foreach $i (0..$#{$enum->{id2sym}}) {
    next if (!defined($enum->{id2sym}[$i]));
    $enum->{id2sym}[$i] = Encode::encode($dst, $enum->{id2sym}[$i]);
    $enum->{sym2id}{$enum->{id2sym}[$i]} = $i;
  }
  return $enum;
}

## $enum = $enum->decodeAll($src_encoding)
##   + wrapper for Encode::decode: convert from $src_encoding to perl-internal
##   + not helpful
sub decodeAll {
  my ($enum,$src) = @_;

  %{$enum->{sym2id}} = qw();
  foreach $i (0..$#{$enum->{id2sym}}) {
    next if (!defined($enum->{id2sym}[$i]));
    $enum->{id2sym}[$i] = Encode::decode($src,$enum->{id2sym}[$i]);
    $enum->{sym2id}{$enum->{id2sym}[$i]} = $i;
  }
  return $enum;
}


##======================================================================
## I/O : AT&T / Native

# $e = $e->saveNative($file_or_fh)
*saveATT = *saveNative = *saveNativeFh = \&saveNativeFile;
sub saveNativeFile {
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
*loadATT = *loadNative = *loadNativeFh = \&loadNativeFile;
sub loadNativeFile {
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
  (my $nodename = ref($e)) =~ s/::/./g;
  my $node = XML::LibXML::Element->new($nodename);
  $node->setAttribute('MUDL.type','HASH');

  my ($lab,$sym,$inode);
  my $id2sym = $e->{id2sym};
  for ($lab=0; $lab < scalar(@$id2sym); $lab++) {
    next if (!defined($sym=$id2sym->[$lab]));
    $node->appendChild($inode=XML::LibXML::Element->new('symbol'));
    $inode->setAttribute('id', $lab);
    if (ref($sym)) {
      $inode->appendChild($sym->saveXMLNode);
    } else {
      $inode->appendText($sym);
    }
  }
  return $node;
}

## $e = $e->loadXML($node)
sub loadXMLNode {
  my ($e,$node) = @_;
  $e = $e->new() if (!ref($e));
  (my $nodename = ref($e)) =~ s/::/./g;
  carp( ref($e) , "::loadXMLNode() expected '$nodename' element, got '", $node->nodeName, "'\n")
    if ($node->nodeName ne $nodename);

  my ($symnode,$lab,$sym);
  foreach $symnode ($node->getChildrenByTagName('symbol')) {
    $lab = $symnode->getAttribute('id');
    if ($symnode->hasChildNodes) {
      $sym = MUDL::Object->loadXMLNode($symnode->firstChild);
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
