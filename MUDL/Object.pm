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
##  + hash-entries are converted with $obj->hashEntry2XMLNode($key,$val) if present
##  + list-entries are converted with $obj->listEntry2XMLNode($index,$val) if present
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
      $savesub = UNIVERSAL::can($src,'hashEntry2XMLNode');
      if ($savesub && $savesub ne UNIVERSAL::can(__PACKAGE__, 'hashEntry2XMLNode')) {
	while (($k,$v)=each(%$src)) {
	  $enode = &$savesub($src,$k,$v);
	  $$noder->appendChild($enode) if (defined($enode));
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
      $savesub = UNIVERSAL::can($src,'listEntry2XMLNode');
      if ($savesub && $savesub ne UNIVERSAL::can(__PACKAGE__, 'listEntry2XMLNode')) {
	foreach $k (0..$#$src) {
	  next if (!exists($src->[$k]));
	  $enode = &$savesub($src,$k,$src->[$k]);
	  $$noder->appendChild($enode) if (defined($enode));
	}
      } else {
	foreach $k (0..$#$src) {
	  next if (!exists($src->[$k]) || !defined($src->[$k]));
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

## $node = $obj->hashEntry2XMLNode($key,$value)
sub hashEntry2XMLNode {
  my $enode = XML::LibXML::Element->new('value');
  $enode->setAttribute('key', $_[1]);
  $enode->appendChild(__PACKAGE__->saveXMLNode($_[2]));
  return $enode;
}
## $node = $obj->listEntry2XMLNode()
sub listEntry2XMLNode {
  my $enode = XML::LibXML::Element->new('value');
  $enode->setAttribute('idx', $_[1]);
  $enode->appendChild(__PACKAGE__->saveXMLNode($_[2]));
  return $enode;
}


## $str = $obj->saveXMLString(%args)
##  + returns object as XML string (NOT a doc string, just raw node)
##  + known %args : format=>$flevel, doencoding=>$bool
sub saveXMLString {
  my ($obj,%args) = @_;
  %args = (format=>0, doencoding=>0, %args);
  my $node = ref($obj) ? $obj->saveXMLNode(%args) : saveXMLNode($obj,%args);
  return $node->toString($args{format}, $args{doencoding});
}

## $doc = $obj->saveXMLDoc(%args)
##  + return a new XML::LibXML::Document representing the object
##  + known %args : xmlencoding, xmlversion
sub saveXMLDoc {
  my $o = shift;
  my $doc = MUDL::XML::Document->new(@_);
  my $node = ref($o) ? $o->saveXMLNode(@_) : saveXMLNode($o,@_);
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
##  + hash entries are loaded with $obj->XMLNode2HashEntry($node) if present
##  + array entries are loaded with $obj->XMLNode2ListEntry($node) if present
sub loadXMLNode {
  #my ($obj,$node) = splice(@_,0,2);
  #confess( ref($obj) , "::loadXMLNode(): dummy method called.\n");

  my ($obj,$node) = splice(@_,0,2);
  my @queue = (\$obj,$node);

  my ($objr, $loadsub, $otyp, $ntyp, $nodename, $k, $enode);
  while (($objr,$node)=splice(@queue,0,2)) {

    ##-- undef node --> undef
    if (!defined($node)) {
      $$objr = undef;
      next;
    }

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
      if ($otyp eq 'HASH') { $$objr = {}; }
      elsif ($otyp eq 'ARRAY') { $$objr = []; }
      elsif ($otyp eq 'REF') { my $objrr; $$objr = \$objrr; }
      else {
	$$objr = $otyp->new();
      }
    }

    ##-- subcall
    $loadsub = UNIVERSAL::can($$objr, 'loadXMLNode');
    if ($loadsub && $loadsub ne UNIVERSAL::can( __PACKAGE__ , 'loadXMLNode')) {
      $$objr = $$objr->loadXMLNode($node,@_);
      next;
    }

    $ntyp = $node->getAttribute('MUDL.type');
    $ntyp = '' if (!defined($ntyp));

    ##-- HASH refs
    if ($ntyp eq 'HASH' || UNIVERSAL::isa($$objr, 'HASH')) {
      $loadsub=UNIVERSAL::can($$objr, 'XMLNode2HashEntry');
      if ($loadsub && $loadsub ne UNIVERSAL::can( __PACKAGE__ , 'XMLNode2HashEntry')) {
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
      $loadsub=UNIVERSAL::can($$objr, 'XMLNode2ListEntry');
      if ($loadsub && $loadsub ne UNIVERSAL::can( __PACKAGE__ , 'XMLNode2ListEntry')) {
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

## undef = $obj->XMLNode2HashEntry($xmlnode)
sub XMLNode2HashEntry {
  $_[0]->{$_[1]->getAttribute('key')} = __PACKAGE__->loadXMLNode($_[1]->firstChild);
}
## undef = $obj->XMLNode2ListEntry($xmlnode)
sub XMLNode2ListEntry {
  $_[0]->[$_[1]->getAttribute('idx')] = __PACKAGE__->loadXMLNode($_[1]->firstChild);
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
  my $str = $_[1];
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
  #binmode($fh,':utf8');

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
## I/O: Generic: Modes
##======================================================================

## \%ioModes = $class_or_obj->ioModes()
##   + %ioModes = { $modeString => \%modeInfo, ... , 'DEFAULT'=>\%defaultModeInfo }
##   + %modeInfo = { loadFh=>$loadFh_coderef_or_name, saveFh=>$saveFh_coderef_or_name }
##   + subclasses may redefine this, but use of 'registerIOMode()' is reccommended
sub ioModes {
  return {
	  'native'  => __PACKAGE__->ioModeHash('Native'),
	  'xml'     => __PACKAGE__->ioModeHash('XML'),
	  'bin'     => __PACKAGE__->ioModeHash('Bin'),
	  ##--
	  'DEFAULT' => __PACKAGE__->ioModeHash('Native'),
	 };

}

## \%modeHash = $class_or_obj->ioModeHash($filenameInfix)
##  + populates a mode hash for $filenameInfix:
##    (load|save)${filenameInfix}(String|File|Fh)
sub ioModeHash {
  my $infix = $_[1];
  return
    {
     loadFh=>"load${infix}Fh",
     saveFh=>"save${infix}Fh",
     loadString=>"load${infix}String",
     saveString=>"save${infix}String",
     loadFile=>"load${infix}File",
     saveFile=>"save${infix}File",
    };
}

## undef = $class_or_obj->registerIOMode($modeString,\%modeInfo)
##  + redefines 'sub ${class}::ioModes() {...}'
sub registerIOMode {
  my ($that,$modeString,$modeInfo) = @_;
  my $modes = {};
  %$modes = %{$that->ioModes()} if (UNIVERSAL::can($that,'ioModes'));
  $modes->{$modeString} = $modeInfo;
  *{(ref($that)||$that)."::ioModes"} = sub { return $modes; }
}


## \@fileSuffixModes = $class_or_obj->fileSuffixModes()
##   + @fileSuffixModes = ( {regex=>$fileRegex, mode=>$ioModeStr}, ... )
##   + subclasses *may* redefine this method directly, but use of
##     'registerFileSuffix()' is highly reccommended
sub fileSuffixModes {
  return [
	  {regex=>qr/\.bin$/i,    mode=>'bin'},
	  {regex=>qr/\.xml$/i,    mode=>'xml'},
	  {regex=>qr/\.native$/i, mode=>'native'},
	 ];
}

## undef = $class_or_obj->registerFileSuffix($suffix,$modeString)
##  + redefines 'sub ${class}::fileSuffixModes() {...}'
sub registerFileSuffix {
  my ($that,$suffix,$modeString) = @_;
  my $suffixes = [];
  @$suffixes = @{$that->fileSuffixModes()} if (UNIVERSAL::can($that,'fileSuffixModes'));
  unshift(@$suffixes, {regex=>qr/${suffix}$/i, mode=>$modeString});
  *{(ref($that)||$that)."::fileSuffixModes"} = sub { return $suffixes; }
}


##======================================================================
## I/O: Generic: Load
##======================================================================

## $obj = $obj->loadGenericString($str,%args)
*loadString = \&loadGenericString;
sub loadGenericString {
  my ($that,$str,%args) = @_;

  my $mode = (defined($args{mode}) ? $args{mode} : 'DEFAULT');
  delete($args{mode});

  ##-- first try string method directly
  if (defined($sub=$that->ioModes->{$mode}{loadString})
      &&
      (ref($sub) || defined($sub=UNIVERSAL::can($that,$sub))))
    {
      return $sub->($that,$str,%args);
    }

  ##-- fallback to Fh method
  if (!defined($sub=$that->ioModes->{$mode}{loadFh})
      ||
      (!ref($sub) && !defined($sub=UNIVERSAL::can($that,$sub))))
    {
      confess((ref($that)||$that), "::loadGenericString(): could not find load sub for mode '$mode': $!");
      return undef;
    }

  my $fh = IO::Scalar->new(\$str);
  if (!$fh) {
    confess((ref($that)||$that) , "::loadGenericString(): open failed for string: $!");
    return undef;
  }
  my $rc = $sub->($that, $fh, %args);
  $fh->close();
  return $rc;
}

## $obj = $obj->loadGenericFile($filename,@args)
*loadFile = \&loadGenericFile;
sub loadGenericFile {
  my ($that,$filename,%args) = @_;
  my $mode = $args{mode};
  delete($args{mode});

  ##-- guess mode from filename suffix
  if (!defined($mode)) {
    my $suffs = $that->fileSuffixModes();
    foreach $suffSpec (@$suffs) {
      if ($filename =~ $suffSpec->{regex}) {
	$mode = $suffSpec->{mode};
	last;
      }
    }
  }
  $mode = 'DEFAULT' if (!defined($mode));

  ##-- first try file method directly
  if (defined($sub=$that->ioModes->{$mode}{loadFile})
      &&
      (ref($sub) || defined($sub=UNIVERSAL::can($that,$sub))))
    {
      return $sub->($that,$filename,%args);
    }

  ##-- get load sub: fallback to fh method
  if (!defined($sub=$that->ioModes->{$mode}{loadFh})
      ||
      (!ref($sub) && !defined($sub=UNIVERSAL::can($that,$sub))))
    {
      confess((ref($that)||$that), "::loadGenericFile(): could not find load sub for mode '$mode': $!");
      return undef;
    }

  ##-- get fh
  my $fh = IO::File->new("<$filename");
  if (!$fh) {
    confess((ref($that)||$that) , "::loadGenericFile(): open failed for '$filename': $!");
    return undef;
  }
  binmode($fh,':utf8');

  my $rc = $sub->($that, $fh, %args);
  $fh->close();
  return $rc;
}


## $obj = $obj->loadGenericFh($fh,%args)
*loadFh = \&loadGenericFh;
sub loadGenericFh {
  my ($that,$fh,%args) = @_;
  my $mode = defined($args{mode}) ? $args{mode} : 'DEFAULT';
  delete($args{mode});

  ##-- get load sub: fh method only
  if (!defined($sub=$that->ioModes->{$mode}{loadFh})
      ||
      (!ref($sub) && !defined($sub=UNIVERSAL::can($that,$sub))))
    {
      confess((ref($that)||$that), "::loadGenericFh(): could not find load sub for mode '$mode': $!");
      return undef;
    }

  my $rc = $sub->($that, $fh, %args);
  $fh->close();
  return $rc;
}




##======================================================================
## I/O: Generic: Save
##======================================================================

## $string = $obj->saveGenericString(%args)
*saveString = \&saveGenericString;
sub saveGenericString {
  my ($that,%args) = @_;

  my $mode = (defined($args{mode}) ? $args{mode} : 'DEFAULT');
  delete($args{mode});

  ##-- first try string method directly
  if (defined($sub=$that->ioModes->{$mode}{saveString})
      &&
      (ref($sub) || defined($sub=UNIVERSAL::can($that,$sub))))
    {
      return $sub->($that,%args);
    }

  ##-- get save sub: fallback to Fh method
  if (!defined($sub=$that->ioModes->{$mode}{saveFh})
      ||
      (!ref($sub) && !defined($sub=UNIVERSAL::can($that,$sub))))
    {
      confess((ref($that)||$that), "::saveGenericString(): could not find save sub for mode '$mode': $!");
      return undef;
    }

  my $str = '';
  my $fh = IO::Scalar->new(\$str);
  if (!$fh) {
    confess((ref($that)||$that) , "::saveGenericString(): open failed for string: $!");
    return undef;
  }
  my $rc = $sub->($that, $fh, %args);
  $fh->close();
  return $str;
}

## $bool = $obj->saveGenericFile($filename,%args)
*saveFile = \&saveGenericFile;
sub saveGenericFile {
  my ($that,$filename,%args) = @_;
  my $mode = $args{mode};
  delete($args{mode});

  ##-- guess mode from filename suffix
  if (!defined($mode)) {
    my $suffs = $that->fileSuffixModes();
    foreach $suffSpec (@$suffs) {
      if ($filename =~ $suffSpec->{regex}) {
	$mode = $suffSpec->{mode};
	last;
      }
    }
  }
  $mode = 'DEFAULT' if (!defined($mode));

  ##-- get save sub: first try file method directly
  if (defined($sub=$that->ioModes->{$mode}{saveFile})
      &&
      (ref($sub) || defined($sub=UNIVERSAL::can($that,$sub))))
    {
      return $sub->($that,$filename,%args);
    }


  ##-- get save sub: fallback to fh method
  if (!defined($sub=$that->ioModes->{$mode}{saveFh})
      ||
      (!ref($sub) && !defined($sub=UNIVERSAL::can($that,$sub))))
    {
      confess((ref($that)||$that), "::saveGenericFile(): could not find load sub for mode '$mode': $!");
      return undef;
    }

  ##-- get fh
  my $fh = IO::File->new(">$filename");
  if (!$fh) {
    confess((ref($that)||$that) , "::saveGenericFile(): open failed for '$filename': $!");
    return undef;
  }
  binmode($fh,':utf8');

  my $rc = $sub->($that, $fh, %args);
  $fh->close();
  return $rc;
}


## $obj = $obj->saveGenericFh($fh,%args)
*saveFh = \&saveGenericFh;
sub saveGenericFh {
  my ($that,$fh,%args) = @_;
  my $mode = defined($args{mode}) ? $args{mode} : 'DEFAULT';
  delete($args{mode});

  ##-- get load sub
  if (!defined($sub=$that->ioModes->{$mode}{saveFh})
      ||
      (!ref($sub) && !defined($sub=UNIVERSAL::can($that,$sub))))
    {
      confess((ref($that)||$that), "::saveGenericFh(): could not find save sub for mode '$mode': $!");
      return undef;
    }

  my $rc = $sub->($that, $fh, %args);
  $fh->close();
  return $rc;
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
