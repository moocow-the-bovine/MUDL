#-*- Mode: Perl -*-

## File: MUDL::Object.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: base class
##======================================================================

package MUDL::Object;
use MUDL::XML;

use Storable;
use IO::File;
use IO::Scalar;
use Carp;

##======================================================================
## Exports
our @ISA = qw(Exporter);
our @EXPORT = qw();
our %EXPORT_TAGS =
  (
   parser => [qw($XMLPARSER)],
   utils  => [qw(newFromXMLNode dummy)],
  );
$EXPORT_TAGS{all} = [map { @$_ } values(%EXPORT_TAGS)];
our @EXPORT_OK = @{$EXPORT_TAGS{all}};

##======================================================================
## GLOBALS
our $XMLPARSER = MUDL::XML::Parser->new();

##======================================================================
## Generic Constructor
## $obj = $class->new(@args)
##  + object structure= hash { %args }
##  + calls $obj->init(@args) on empty hash obj
sub new {
  my $that = shift;
  my $self = bless {}, ref($that)||$that;
  $self->init(@_) if (@_);
  return $self;
}

##======================================================================
## Dummy sub
## \&dummy_sub = dummy($name)
sub dummy {
  my $name = shift;
  return sub { confess( ref($_[0])||$_[0] , "::${name}() not implemented"); };
}

##======================================================================
## Generic initializer
##  + object structure= hash { \%args }
##  + just takes over \%args
sub init {
  my ($obj,%args) = @_;
  @$obj{keys(%args)} = values(%args);
}

##======================================================================
## Deep copy
##  $copy = copy($obj)
*clone = \&copy;
sub copy { return Storable::dclone($_[0]); }


########################################################################
## class MUDL::Array
########################################################################
package MUDL::Array;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor

## $obj = $class_or_obj->new(@args);
sub new {
  my $that = shift;
  my $self = bless [], ref($that)||$that;
  $self->init(@_) if (@_);
  return $self;
}

##======================================================================
## Generic initializer
##  + object structure= array \@args
##  + just adopts \@args
sub init { @{$_[0]}[1..$#_] = @_[1..$#_]; }

##======================================================================
## Accessors / Manipulators

## undef = $obj->clear();
sub clear { @{$_[0]} = qw(); return $_[0]; }


########################################################################
## class MUDL::Array
########################################################################
package MUDL::Scalar;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor

## $obj = $class_or_obj->new(@args);
sub new {
  my $that = shift;
  my $val = undef;
  my $self = bless \$val, ref($that)||$that;
  $self->init(@_) if (@_);
  return $self;
}

##======================================================================
## Generic initializer
##  + object structure = \$scalar
##  + just sets value to first scalar
sub init { ${$_[0]} = $_[1]; }

##======================================================================
## Accessors / Manipulators

## undef = $obj->clear();
sub clear { ${$_[0]} = undef; }


########################################################################
## I/O
########################################################################
package MUDL::Object;

##======================================================================
## I/O: XML: save
##======================================================================

## $node = $obj->saveXMLNode(%args)
##  + return a new node representing the object
##  + default implementation
##  + hash-entries are converted with $obj->entry2XMLNode($key,$val) if present
##  + list-entries are converted with $obj->entry2XMLNode($index,$val) if present
sub saveXMLNode {
  #confess( ref($_[0]) , "::saveXMLNode(): dummy method called.\n");
  my $obj = shift;
  my $node = undef;
  my @queue = ($obj,\$node,undef);
  my ($src,$srcref,$noder,$mom,$nodename,$savesub, $k,$v,$enode);

  while (($src,$noder,$mom)=splice(@queue,0,3)) {

    ##-- nonref --> textnode
    if (!($nodename=ref($src))) {
      $$noder = XML::LibXML::Text->new(defined($src) ? $src : '');
      next;
    }

    ##-- subcall
    $savesub = UNIVERSAL::can($src,'saveXMLNode');
    if ($savesub && $savesub ne UNIVERSAL::can( __PACKAGE__ , 'saveXMLNode')) {
      $$noder = $src->saveXMLNode(@_);
      next;
    }

    ##-- ref --> element
    $nodename =~ s/::/./g;
    $$noder = XML::LibXML::Element->new($nodename);

    ##-- HASH refs
    if (UNIVERSAL::isa($src,'HASH')) {
      $$noder->setAttribute('MUDL.type','HASH');
      if ($savesub=UNIVERSAL::can($src,'entry2XMLNode')) {
	while (($k,$v)=each(%$src)) {
	  $$noder->appendChild(&$savesub($src,$k,$v));
	}
      } else {
	while (($k,$v)=each(%$src)) {
	  my $vr = undef;
	  $enode=XML::LibXML::Element->new('value');
	  $enode->setAttribute('key', $k);
	  $$noder->appendChild($enode);
	  push(@queue, $v, \$vr, $enode);
	}
      }
    }
    ##-- ARRAY refs
    elsif (UNIVERSAL::isa($src,'ARRAY')) {
      $$noder->setAttribute('MUDL.type','ARRAY');
      if ($savesub=UNIVERSAL::can($src,'entry2XMLNode')) {
	foreach $k (0..$#$src) {
	  next if (!exists($src->[$k]));
	  $$noder->appendChild(&$savesub($src,$k,$src->[$k]));
	}
      } else {
	foreach $k (0..$#$src) {
	  next if (!exists($src->[$k]));
	  my $vr = undef;
	  $enode=XML::LibXML::Element->new('value');
	  $enode->setAttribute('idx', $k);
	  $$noder->appendChild($enode);
	  push(@queue, $src->[$k], \$vr, $enode);
	}
      }
    }
    ##-- PDLs
    elsif (UNIVERSAL::isa($src,'PDL')) {
      $$noder->setAttribute('MUDL.type', 'PDL');
      $$noder->setAttribute('MUDL.dims', join(' ', $src->dims));
      my ($i,$n);
      for ($i=0; $i < $src->nelem; $i++) {
	$n = XML::LibXML::Element->new('value');
	$n->setAttribute('i',$i);
	$n->setAttribute('v',$src->flat->at($i));
	$$noder->appendChild($n);
      }
    }
    ##-- SCALAR refs
    elsif (UNIVERSAL::isa($src,'SCALAR')) {
      $$noder->setAttribute('MUDL.type','SCALAR');
      next if (!defined($$src));
      my $vr = undef;
      push(@queue, $$src, \$vr, $$noder);
    }
    ##-- REF refs
    elsif (UNIVERSAL::isa($src,'REF')) {
      $$noder->setAttribute('MUDL.type','REF');
      next if (!defined($$src));
      my $vr = undef;
      push(@queue, $$src, \$vr, $$noder);
    }
    else {
      ##-- bad data
      confess( __PACKAGE__ , "::saveXMLNode(): cannot save value '$src'!");
      undef $mom;
    }
  }
  continue {
    $mom->appendChild($$noder) if ($mom);
  }


  return $node;
}

## $str = $obj->saveXMLString(%args)
##  + returns object as XML string (NOT a doc string, just raw node)
##  + known %args : format=>$flevel, doencoding=>$bool
sub saveXMLString {
  my ($obj,%args) = @_;
  %args = (format=>0, doencoding=>0, %args);
  my $node = $obj->saveXMLNode(%args);
  return $node->toString($args{format}, $args{doencoding});
}

## $doc = $obj->saveXMLDoc(%args)
##  + return a new XML::LibXML::Document representing the object
##  + known %args : xmlencoding, xmlversion
sub saveXMLDoc {
  my $o = shift;
  my $doc = MUDL::XML::Document->new(@_);
  my $node = $o->saveXMLNode(@_);
  $doc->setDocumentElement($node);
  return $doc;
}

## $doc = $obj->saveXMLFile($file,%args)
##  + create XML document, save it to $file_or_fh, return document
##  + known %args : xmlencoding, xmlversion, compress=>$zlevel, format=>$flevel
sub saveXMLFile {
  my ($obj,$file,%args) = @_;
  %args = (
	   compress=>undef,
	   format=>0,
	   %args
	  );
  my $doc = $obj->saveXMLDoc(%args);
  $doc->setCompression($args{compress}) if (defined($args{compress}));
  $doc->toFile($file, $args{format}) || return undef;
  return $doc;
}

## $doc = $obj->saveXMLFh($fh,%args)
##  + create XML document, save it to $fh, return document
##  + known %args : xmlencoding, xmlversion, compress=>$zlevel, format=>$flevel
sub saveXMLFh {
  my ($obj,$fh,%args) = @_;
  %args = (compress=>undef, format=>0, %args);
  my $doc = $obj->saveXMLDoc(%args);
  $doc->setCompression($args{compress}) if (defined($args{compress}));
  $doc->toFH($fh, $args{format}) || return undef;
  return $doc;
}



##======================================================================
## I/O: XML: load
##======================================================================

## $obj_or_undef = $class_or_obj->loadXMLNode($node,@args)
##  + should load object from $node
##  + default implementation should work basically correctly
##  + hash entries are loaded with $obj->XMLNode2Entry($node) if present
##  + array entries are loaded with $obj->XMLNode2Entry($node) if present
sub loadXMLNode {
  #my ($obj,$node) = splice(@_,0,2);
  #confess( ref($obj) , "::loadXMLNode(): dummy method called.\n");

  my ($obj,$node) = splice(@_,0,2);
  my @queue = (\$obj,$node);

  my ($objr, $loadsub, $otyp, $ntyp, $nodename, $k, $enode);
  while (($objr,$node)=splice(@queue,0,2)) {

    ##-- text node --> scalar string
    if ($node->nodeName eq 'text') {
      $$objr = $node->nodeValue;
      next;
    }

    ##-- non-text node --> reference
    $otyp = $nodename = $node->nodeName;
    $otyp =~ s/\./::/g;
    if (ref($$objr)) {
      carp( __PACKAGE__ , "::loadXMLNode() expected '", ref($$objr), "' element, got '", $nodename, "'\n")
	if (ref($$objr) ne $otyp);
    } else {
      $$objr = $otyp->new();
    }

    ##-- subcall
    $loadsub = UNIVERSAL::can($$objr, 'loadXMLNode');
    if ($loadsub && $loadsub ne UNIVERSAL::can( __PACKAGE__ , 'loadXMLNode')) {
      $$objr = $$objr->loadXMLNode(@_);
      next;
    }

    $ntyp = $node->getAttribute('MUDL.type');
    $ntyp = '' if (!defined($ntyp));

    ##-- HASH refs
    if ($ntyp eq 'HASH' || UNIVERSAL::isa($$objr, 'HASH')) {
      if ($loadsub=UNIVERSAL::can($$objr, 'XMLNode2Entry')) {
	foreach $enode ($node->childNodes) {
	  &$loadsub($$objr,$enode);
	}
      } else {
	foreach $enode ($node->getChildrenByTagName('value')) {
	  $k = $enode->getAttribute('key');
	  push(@queue, \$$objr->{$k}, $enode->firstChild);  ##-- firstChild(): dangerous
	}
      }
    }
    ##-- ARRAY refs
    elsif ($ntyp eq 'ARRAY' || UNIVERSAL::isa($$objr, 'ARRAY')) {
      if ($loadsub=UNIVERSAL::can($$objr, 'XMLNode2Entry')) {
	foreach $enode ($node->childNodes) {
	  &$loadsub($$objr,$enode);
	}
      } else {
	foreach $enode ($node->getChildrenByTagName('value')) {
	  $k = $enode->getAttribute('idx');
	  push(@queue, \$$objr->[$k], $enode->firstChild);  ##-- firstChild(): dangerous
	}
      }
    }
    ##-- PDLs
    elsif ($ntyp eq 'PDL' || UNIVERSAL::isa($$objr, 'PDL')) {
      $$objr->reshape(split(/\s+/, $node->getAttribute('MUDL.dims') || '1'));
      foreach $enode ($node->childNodes) {
	$$objr->flat->set($enode->getAttribute('i'), $enode->getAttribute('v'));
      }
    }
    ##-- SCALAR refs
    elsif ($ntyp eq 'SCALAR' || UNIVERSAL::isa($$objr, 'SCALAR')) {
      push(@queue, $$objr, $node->firstChild); ##-- firstChild(): dangerous
    }
    ##-- REF refs
    elsif ($ntyp eq 'REF' || UNIVERSAL::isa($$objr, 'REF')) {
      push(@queue, $$objr, $node->firstChild); ##-- firstChild(): dangerous
    }
    else {
      ##-- bad data
      confess( __PACKAGE__ , "::loadXMLNode(): cannot load data for '$$objr'!");
    }
  }

  return $obj;
}

## $obj_or_undef = $obj->loadXMLDoc($doc,@args)
##  + @args are passed to loadXMLNode
sub loadXMLDoc {
  my ($obj,$doc) = splice(@_,0,2);
  return $obj->loadXMLNode($doc->documentElement,@_);
}

## $obj_or_undef = $obj->loadXMLString($str,@args)
##  + calls loadXMLDoc
sub loadXMLString {
  my ($obj,$str) = splice(@_,0,2);
  my $doc = $XMLPARSER->parse_string($str)
    or confess( __PACKAGE__ , "::loadXMLString() failed: $!");
  return $obj->loadXMLDoc($doc,@_);
}

## $obj_or_undef = $obj->loadXMLFile($filename,@args)
##  + calls loadXMLDoc
sub loadXMLFile {
  my ($obj,$file) = splice(@_,0,2);
  my $doc = $XMLPARSER->parse_file($file)
    or confess( __PACKAGE__ , "::loadXMLFile() failed for '$file': $!");
  return $obj->loadXMLDoc($doc,@_);
}

## $obj_or_undef = $obj->loadXMLFh($fh,@args)
##  + calls loadXMLDoc
sub loadXMLFh {
  my ($obj,$fh) = splice(@_,0,2);
  my $doc = $XMLPARSER->parse_fh($fh)
    or confess( __PACKAGE__ , "::loadXMLFh() failed for fh '$fh': $!");
  return $obj->loadXMLDoc($doc,@_);
}

##======================================================================
## I/O: XML: new
##======================================================================

## $obj = $class->newFromXMLNode($node)
sub newFromXMLNode {
  my ($that,$node) = splice(@_,0,2);
  (my $class = $node->nodeName) =~ s/\./::/g;
  return $node->textContent if ($class eq 'text');
  return $class->new()->loadXMLNode($node);
}

## $obj = $class->newFromXMLDoc($doc)
sub newFromXMLDoc {
  my ($that,$doc) = splice(@_,0,2);
  return newFromXMLNode($that,$doc->documentElement,@_);
}

## $obj = $class->newFromXMLFile($filename)
sub newFromXMLFile {
  my ($that,$file) = splice(@_,0,2);
  my $doc = $XMLPARSER->parse_file($file)
    or confess( __PACKAGE__ , "::newFromXMLFile() failed for '$file': $!");
  return newFromXMLDoc($that,$doc,@_);
}

## $obj = $class->newFromXMLFh($fh)
sub newFromXMLFh {
  my ($that,$fh) = splice(@_,0,2);
  my $doc = $XMLPARSER->parse_fh($fh)
    or confess( __PACKAGE__ , "::newfromXMLFh() failed for '$fh': $!");
  return newFromXMLDoc($that,$doc,@_);
}


##======================================================================
## I/O: Binary: Save
##======================================================================

## $str = $obj->saveBinString(@args)
sub saveBinString {
  my $obj = shift;
  my $ref = ref($obj) ? $obj : \$obj;
  return Storable::freeze($ref);
}

## $bool = $obj->saveBinFile($filename,@args)
##   + calls saveBinFh($fh)
sub saveBinFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::saveBinFile(): open failed for '$file': $!");
    return undef;
  }
  my $rc = saveBinFh($obj,$fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $bool = $obj->saveBinFh($fh,@args)
sub saveBinFh {
  my ($obj,$fh) = splice(@_,0,2);
  my $ref = ref($obj) ? $obj : \$obj;
  if (!Storable::store_fd($ref, $fh)) {
    confess( __PACKAGE__ , "::saveBinFh(): Storable::store_fd() failed.\n");
    return undef;
  }
  return 1;
}

##======================================================================
## I/O: Binary: Load
##======================================================================

## $obj_or_undef = $obj->loadBinString($str,@args)
sub loadBinString {
  my $str = shift;
  return Storable::thaw($str);
}

## $obj_or_undef = $obj->loadBinFile($filename,@args)
##   + calls loadBinFh($fh)
sub loadBinFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::loadBinFile(): open failed for '$file': $!");
    return undef;
  }
  my $rc = loadBinFh($obj,$fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $obj_or_undef = $obj->loadBinFh($fh,@args)
sub loadBinFh {
  my ($obj,$fh) = splice(@_,0,2);
  if (!defined($obj=Storable::fd_retrieve($fh))) {
    confess( __PACKAGE__ , "::loadBinFh(): Storable::fd_retrieve() failed.\n");
    return undef;
  }
  return $obj;
}


##======================================================================
## I/O: Native: save
##======================================================================

## $str = $obj->saveNativeString(@args)
##  + calls saveNativeFh()
sub saveNativeString {
  my $obj = shift;
  my $str = '';
  my $fh = IO::Scalar->new(\$str);
  if (!$fh) {
    confess( __PACKAGE__ , "::saveNativeString(): open failed: $!");
    return undef;
  }
  #binmode($fh,':utf8');

  $obj->saveNativeFh($fh,@_);
  $fh->close();
  return $str;
}

## $bool = $obj->saveNativeFile($file,@args)
##  + calls saveNativeFh()
sub saveNativeFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::saveNativeFile(): open failed for '$file': $!");
    return undef;
  }
  binmode($fh,':utf8');

  my $rc = $obj->saveNativeFh($fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $bool = $obj->saveNativeFh($fh,@args)
##  + dummy
*saveNativeFh = MUDL::Object::dummy('saveNativeFh');
#sub saveNativeFh {
#  my ($obj,$fh) = splice(@_,0,2);
#  $fh->print($obj->saveNativeStr(@_));
#  return 1;
#}

##======================================================================
## I/O: Native: load
##======================================================================

## $obj = $obj->loadNativeString($str,@args)
sub loadNativeString {
  my ($obj,$str) = splice(@_,0,2);
  my $fh = IO::Scalar->new(\$str);
  if (!$fh) {
    confess( __PACKAGE__ , "::loadNativeString(): open failed: $!");
    return undef;
  }
  binmode($fh,':utf8');

  my $rc = $obj->loadNativeFh($fh,@_);
  $fh->close();
  return $rc;
}

## $obj = $obj->loadNativeFile($file,@args)
##  + calls saveNativeFh()
sub loadNativeFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::loadNativeFile(): open failed for '$file': $!");
    return undef;
  }
  binmode($fh,':utf8');

  my $rc = $obj->loadNativeFh($fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $obj = $obj->loadNativeFh($fh,@args)
##  + calls loadNativeStr()
*loadNativeFh = MUDL::Object::dummy('loadNativeFh');
#sub loadNativeFh {
#  my ($obj,$fh) = splice(@_,0,2);
#  my $rs = IO::Handle::input_record_separator();
#  IO::Handle::input_record_separator(undef);
#  my $rc = $obj->loadNativeStr($fh->getline, @_);
#  IO::Handle::input_record_separator($rs);
#}


##======================================================================
## I/O: Generic: Load
##======================================================================

## $class_or_obj->newFromFile($filename,%args)
##  + creates a new object, loads a file
##  + known args:
##     mode => 'xml' | 'bin' | 'native'
sub loadFile {
  my ($obj,$file,%args) = @_; 
  #$obj = $obj->new(%args) if (!ref($obj));
  my $mode = defined($args{mode}) ? $args{mode} : '';
  if ($mode eq 'xml' || $file =~ /\.xml$/) {
    return $obj->loadXMLFile($file,%args)
      or confess( __PACKAGE__ , "::newFromFile() failed for XML file '$file': $!");
  }
  elsif ($mode eq 'bin' || $file =~ /(?:\.bin|\.sto)$/) {
    return $obj->loadBinFile($file,%args)
      or confess( __PACKAGE__ , "::newFromFile() failed for bin file '$file': $!");
  }
  else {
    return $obj->loadNativeFile($file,%args)
      or confess( __PACKAGE__ , "::newFromFile() failed for native file '$file': $!");
  }
}


##======================================================================
## I/O: Generic: Save
##======================================================================

## $class_or_obj->saveFile($filename,%args)
##  + creates a new object, loads a file
##  + known args:
##     mode => 'xml' | 'bin' | 'native'
sub saveFile {
  my ($obj,$file,%args) = @_;

  my $mode = $args{mode};
  if ($file =~ /^([^:]+):(.*)/) {
    $file = $2;
    $mode = $1;
  }
  elsif (!defined($mode)) {
    $mode = '';
  }

  if ($mode eq 'xml' || $file =~ /\.xml$/) {
    return $obj->saveXMLFile($file,%args)
      or confess( __PACKAGE__ , "::saveFile() failed for XML file '$file': $!");
  }
  elsif ($mode eq 'bin' || $file =~ /(?:\.bin|\.sto)$/) {
    return $obj->saveBinFile($file,%args)
      or confess( __PACKAGE__ , "::saveFile() failed for binary file '$file': $!");
  }
  else {
    return $obj->saveNativeFile($file,%args)
      or confess( __PACKAGE__ , "::saveFile() failed for native file '$file': $!");
  }
  return $obj;
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
