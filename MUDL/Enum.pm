##-*- Mode: CPerl -*-

## File: MUDL::Enum.pm
## Author: Bryan Jurish <moocow@cpan.org>
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

## $e = $e->compact()
## $e = $e->compact(\@changed)
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
  splice(@$id2s, ($#$id2s-$offset+1), $offset);
  #splice(@$id2s, -$offset, $#$id2s-$offset+1);

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
  $e->{sym2id}{$sym} = $id;
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
## Manipulators: union, difference

## $enum1 = $enum1->addEnum($enum2)
##  + adds all symbols from $enum2 to $enum1
##  + does not preserve mapping of $enum2
*merge = *union = \&addEnum;
sub addEnum {
  my ($e1,$e2) = @_;
  #$e1->addSymbol($_) foreach (@{$e2->{id2sym}});
  ##--
  my $oldsize = scalar(@{$e1->{id2sym}});
  push(@{$e1->{id2sym}}, grep {!exists($e1->{sym2id}{$_})} @{$e2->{id2sym}});
  $e1->{sym2id}{$e1->{id2sym}[$_]} = $_ foreach ($oldsize..$#{$e1->{id2sym}});
  return $e1;
}

## $enum1 = $enum1->removeEnum($enum2)
##  + removes all symbols in $enum2 from $enum1
##  + preserves mapping of $enum1
*difference = \&removeEnum;
sub removeEnum {
  my ($e1,$e2) = @_;
  $e1->removeSymbol($_) foreach (@{$e2->{id2sym}});
  return $e1;
}

##======================================================================
## Conversion: translator pdls


## $pdl = $enumFrom->xlatePdlTo($enumTo,%args)
##  + create a translation pdl from $enumFrom ids to $enumTo ids
##  + output pdl is 1d, and has length $enumFrom->size()
##  + %args:
##     badval=>$badval,  ##-- value to use for "missing" keys (keys in $enumFrom & not in $enumTo)
##                       ##   + default: $enumTo->size+1
##     badstr=>$badstr,  ##-- string to lookup in $enumTo used for "missing" keys
##                       ##   + only used for $badval if badval is unset
##                       ##   + always used for unallocated ids in $enumFrom
##                       ##   + if it doesn't exist in $enumTo, $badval will be used (via $PDL::undefval)
##                       ##   + default: ''
##     pdl   =>$pdl,     ##-- output pdl
sub xlatePdlTo {
  require PDL;
  my ($efrom,$eto,%args) = @_;

  ##-- get bad value
  my $badval = $args{badval};
  if (!defined($badval)) {
    $badval = $eto->{sym2id}{$args{badstr}} if (defined($args{badstr}));
    $badval = $eto->size+1 if (!defined($badval));
  }

  ##-- get output pdl
  my $undefval   = $PDL::undefval;
  $PDL::undefval = $badval;
  my $pdl        = PDL->pdl(PDL::long(), [
					  @{$eto->{sym2id}}{
					    map {
					      defined($_) ? $_
						: (defined($args{badstr}) ? $args{badstr} : '')
					    } @{$efrom->{id2sym}}
					  }
					 ]);
  $PDL::undefval = $undefval;

  ##-- return
  if (defined($args{pdl})) {
    $args{pdl} .= $pdl;
    return $args{pdl};
  }

  return $pdl;
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
## I/O : Binary: hooks

## ($serialized_string, @other_refs) = STORABLE_freeze($obj,$cloning_flag)
sub STORABLE_freeze {
  my ($obj,$cloning) = @_;
  return ('', [
	       map {
		 ($_ eq 'sym2id' ? qw()
		  : ($_ eq 'id2sym' && UNIVERSAL::isa(tied(@{$obj->{$_}}), 'DiaColloDB::EnumFile::TiedArray')
		     ? ($_=>${tied(@{$obj->{$_}})}->toArray) ##-- hack for enums via DiaColloDB::EnumFile
		     : ($_=>$obj->{$_})))
		} keys(%$obj)
	       ]);
}

## $obj = STORABLE_thaw($obj, $cloning_flag, $serialized_string, @other_refs)
sub STORABLE_thaw {
  my ($obj,$cloning,$str,$ar) = @_;
  if (!defined($str) || $str eq '') {
    ##-- backwards-compatibility
    %$obj = @$ar;
    #return $obj if (ref($obj) ne __PACKAGE__); ##-- hack
    $obj->{sym2id} = {} if (!defined($obj->{sym2id}));
    @{$obj->{sym2id}}{grep {defined($_)} @{$obj->{id2sym}}}
      = grep {defined($obj->{id2sym}[$_])} (0..$#{$obj->{id2sym}});
  }
  return $obj;
}


##======================================================================
## I/O : AT&T / Native

# $e = $e->saveNative($file_or_fh,%opts)
#  + %opts
#    invert => $bool,  ##-- save (ID,SYM) lines rather than (SYM,ID); default=0
#    utf8   => $boo,   ##-- save in utf8 mode
*saveATT = *saveNative = *saveNativeFh = \&saveNativeFile;
sub saveNativeFile {
  my ($e,$file,%opts) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  croak( __PACKAGE__ , "::saveATT(): open failed for '$file': $!") if (!$fh);
  $fh->binmode(':utf8') if ($opts{utf8});
  $fh->binmode($_) foreach ($fh->can('binmode') && $opts{iolayers} ? @{$opts{iolayers}} : qw());

  my ($lab,$sym);
  my $id2sym = $e->{id2sym};
  my $invert = $opts{invert};
  for ($lab=0; $lab < scalar(@$id2sym); $lab++) {
    next if (!defined($sym=$id2sym->[$lab]));
    $fh->print(($invert ? ($lab, "\t", $sym) : ($sym, "\t", $lab)), "\n");
  }

  $fh->close() if (ref($file));
  return $e;
}

# $e = $e->loadNative($file_or_fh)
#  + %opts
#    invert => $bool,  ##-- save (ID,SYM) lines rather than (SYM,ID); default=0
#    utf8   => $boo,   ##-- save in utf8 mode (default=1)
*loadATT = *loadNative = *loadNativeFh = \&loadNativeFile;
sub loadNativeFile {
  my ($e,$file,%opts) = @_;
  $e     = $e->new() if (!ref($e));
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  croak( __PACKAGE__ , "::loadNative(): open failed for '$file': $!") if (!$fh);
  $fh->binmode(':utf8') if ($opts{utf8});
  $fh->binmode($_) foreach ($fh->can('binmode') && $opts{iolayers} ? @{$opts{iolayers}} : qw());

  my $invert = $opts{invert};
  my ($lab,$sym,$line);
  while (defined($line=<$fh>)) {
    chomp $line;
    next if ($line eq '');
    if (!$invert && $line =~ /^(.*\S)\s+(\d+)$/) {
      ($sym,$lab) = ($1,$2);
    }
    elsif ($invert) {
      ($lab,$sym) = split(' ',$line,2);
      $sym //= '';
    }
    else {
      warn( __PACKAGE__ , "::loadNativeFile(): parse error in file '$file' at line ", $fh->input_line_number);
      next;
    }
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


##======================================================================
## I/O : raw (via DiaColloDB::EnumFile)

## $enum = $enum->saveRawFiles($base)
##  + saves enum data as DiaColloDB::EnumFile
sub saveRawFiles {
  my ($enum,$base) = @_;
  require DiaColloDB::EnumFile;
  my $denum = (UNIVERSAL::isa(${tied(@{$enum->{id2sym}})//\undef}, 'DiaColloDB::EnumFile')
	       ? ${tied(@{$enum->{id2sym}})}
	       : DiaColloDB::EnumFile->new->fromArray($enum->{id2sym}))
    or confess(__PACKAGE__, "::saveRawFiles(): could not create temporary DiaColloDB::EnumFile for '$base.*'");
  $denum->save($base)
    or confess(__PACKAGE__, "::saveRawFiles(): failed to save temporary DiaColloDB::EnumFile to '$base.*': $!");
  return $enum;
}

## $enum = $CLASS_OR_OBJECT->loadRawFiles($base,%opts)
##  + %opts:
##     mmap => $bool,  # mmap files?
##     ...             # passed to DiaColloDB::EnumFile->tiepair()
sub loadRawFiles {
  my ($enum,$base,%opts) = @_;

  require DiaColloDB::EnumFile;
  require DiaColloDB::EnumFile::MMap;
  require DiaColloDB::EnumFile::Tied;
  my $class = $opts{class} // ('DiaColloDB::EnumFile' . ($opts{mmap} ? '::MMap' : ''));
  delete @opts{qw(class mmap)};
  my ($id2sym,$sym2id) = $class->tiepair(%opts,base=>$base)
    or confess(__PACKAGE__, "::loadRawFiles(): $class->tiepair() failed for '$base.*'");

  $enum = $enum->new() if (!ref($enum));
  @$enum{qw(id2sym sym2id)} = ($id2sym,$sym2id);

  return $enum;
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004-2015, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
