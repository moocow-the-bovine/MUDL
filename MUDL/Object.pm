#-*- Mode: CPerl -*-

## File: MUDL::Object.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: base class
##======================================================================

package MUDL::Object;
use MUDL::XML;      ##-- for XML I/O
use IO::File;
use IO::Scalar;     ##-- for string I/O (occasional goofiness)
use Storable;       ##-- for binary I/O
use Data::Dumper;   ##-- for perl-code I/O
use PerlIO::gzip;   ##-- for :gzip I/O layer: useful for text-based I/O modes
use Compress::Zlib; ##-- for zlib compression of binary files (:gzip I/O layer fails for these)

## PDL::IO::Storable
##  + very tricky: can cause errors "%Config::Config is read-only"
##    when 'require'-ing PDL::Core::Dev.
##  + especially hairy when used in conjunction with Inline::Pdlpp
##  + it seems that this module must load AFTER everything else
##    PDL-related
##
##use PDL::IO::Storable; ##-- WARNING Will Robinson DANGER DANGER
##

##-- alt: ignore PDL::IO::Storable; use tempfile + copy hack instead
##   + problem: converting old files (ack!)
#use File::Copy qw();
#use File::Temp qw(tempfile);
#our $TMPDIR = (defined($ENV{TMPDIR}) ? $ENV{TMPDIR} : '.');

use Carp;


##======================================================================
## Exports
our @ISA = qw(Exporter);
our @EXPORT = qw();
our %EXPORT_TAGS =
  (
   parser => [qw($XMLPARSER)],
   utils  => [qw(newFromXMLNode dummy)],
   #const  => [qw($TMPDIR)],
  );
$EXPORT_TAGS{all} = [map { @$_ } values(%EXPORT_TAGS)];
our @EXPORT_OK = @{$EXPORT_TAGS{all}};

##======================================================================
## GLOBALS
our $XMLPARSER = MUDL::XML::Parser->new();

#our $DEFAULT_ZBIN_ZLEVEL = 9; ##-- default compression level for compressed binary files (maximum)
our $DEFAULT_ZBIN_ZLEVEL = 3; ##-- default compression level for compressed binary files (sane default)

our %DEFAULT_GZBIN_ARGS =
  (
   -Level    => 3,            ##-- default compression level for compressed binary files (sane default)
   TextFlag  => 0,
   BinModeIn => 1,
   Strict    => 1,
  );
our $DEFAULT_GZIP_CMD   = 'gzip -c';
our $DEFAULT_GUNZIP_CMD = 'gunzip -c';

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

## $copy = copy($obj)
##  + chokes on embedded PDLs
*clone = \&copy;
sub copy { return Storable::dclone($_[0]); }

## $copy = safeCopy($obj)
##  + useful for runtime cloning, since copy() aka clone() chokes on PDLs
*safeClone = \&safeCopy;
sub safeCopy { return loadBinString((ref($_[0])||$_[0]), saveBinString($_[0])); }

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
## class MUDL::Scalar
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
  binmode($fh,$_) foreach ($args{iolayers} ? @{$args{iolayers}} : qw());
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
## I/O: PDL: Hacks
##======================================================================

## ($hdr,$tmpfilename) = pdl2tmp($pdl)
##   + generates PDL header and a temporary data file
sub pdl2tmp {
  my $pdl = shift;

  require PDL;
  require PDL::IO::FastRaw;
  require PDL::IO::FlexRaw;
  require File::Temp;
  require File::Copy;

  my $TMPDIR = (defined($ENV{TMPDIR}) ? $ENV{TMPDIR} : '.');

  my ($tmpfh,$tmpfilename) = File::Temp::tempfile('tmpXXXXX', DIR=>$TMPDIR, SUFFIX=>'.pdl');
  my $hdr = writeflex($tmpfh, $pdl);
  $tmpfh->close;

  return ($hdr,$tempfilename);
}

## ($hdr,$string) = pdl2string($pdl)
sub pdl2string {
  my $pdl = shift;
  my ($hdr,$tmpfilename) = pdl2tmp($pdl);
  my $fh = IO::File->new("<$tmpfilename")
    or confess(__PACKAGE__, "::pdl2string(): could not open temp file '$tmpfilename': $!");
  binmode($fh);

  local $/ = undef;
  my $str = <$fh>;
  $fh->close();
  return ($hdr,$str);
}

##======================================================================
## I/O: Binary: Hooks
##======================================================================

## $ref = $obj->saveBinRef(@args)
#sub saveBinRef { return $_[0]; }

## $obj = $class_or_obj->loadBinRef($ref)
#sub loadBinRef { return bless($_[1], ref($_[0])||$_[0]); }

##======================================================================
## I/O: Binary: Save
##======================================================================

## $str = $obj->saveBinString(%args)
sub saveBinString {
  my $obj = shift;
  my $ref = ref($obj) ? $obj : \$obj;
  $ref = $obj->saveBinRef(@_) if (UNIVERSAL::can($obj,'saveBinRef'));
  require PDL::IO::Storable if (defined($PDL::VERSION)); ##-- HACK
  return Storable::freeze($ref);
}

## $bool = $obj->saveBinFile($filename,%args)
##   + calls saveBinFh($fh)
sub saveBinFile {
  my ($obj,$file,%args) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::saveBinFile(): open failed for '$file': $!");
    return undef;
  }
  binmode($fh,$_) foreach ($args{iolayers} ? @{$args{iolayers}} : qw());
  my $rc  = $obj->saveBinFh($fh,%args);
  $fh->close() if (!ref($file));
  return $rc;
}

## $bool = $obj->saveBinFh($fh,%args)
sub saveBinFh {
  my ($obj,$fh,%args) = @_;
  require PDL::IO::Storable if (defined($PDL::VERSION)); ##-- HACK

  my $ref = ref($obj) ? $obj : \$obj;
  $ref = $obj->saveBinRef(%args) if (UNIVERSAL::can($obj,'saveBinRef'));

  ##-- I/O layers
  my @iolayers = $args{iolayers} ? @{$args{iolayers}} : qw();
  binmode($fh);
  binmode($fh,$_) foreach (grep { $_ !~ /^:gzip/ } @iolayers); ##-- HACK: ignore ':gzip' layers!

  ##-- Hack: freeze & gzip (maybe)
  if (grep { $_ =~ /^:gzip/ } @iolayers) {
    my ($frozen);
    if (!defined($frozen=Storable::freeze($ref))) {
      confess( __PACKAGE__ , "::saveBinFh(): Storable::freeze() failed.\n");
      return undef;
    }
    $fh->print($frozen);
  }
  elsif (!Storable::store_fd($ref, $fh)) {
    confess( __PACKAGE__ , "::saveBinFh(): Storable::store_fd() failed.\n");
    return undef;
  }

  return 1;
}

##======================================================================
## I/O: Binary: Load
##======================================================================

## $obj_or_undef = $class_or_obj->loadBinString($str,@args)
sub loadBinString {
  my $str = $_[1];
  require PDL::IO::Storable if (defined($PDL::VERSION)); ##-- HACK
  my $ref = Storable::thaw($str);
  return UNIVERSAL::can($_[0],'loadBinRef') ? $_[0]->loadBinRef($ref) : $ref;
}

## $obj_or_undef = $class_or_obj->loadBinFile($filename,%args)
##   + calls loadBinFh($fh)
sub loadBinFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new("<$file");
   if (!$fh) {
    confess( __PACKAGE__ , "::loadBinFile(): open failed for '$file': $!");
    return undef;
  }

  #my $rc = loadBinFh($obj,$fh,@_);
  my $rc = $obj->loadBinFh($fh,@_);

  $fh->close() if (!ref($file));
  return $rc;
}

## $obj_or_undef = $class_or_obj->loadBinFh($fh,@args)
sub loadBinFh {
  my ($obj,$fh,%args) = @_;
  require PDL::IO::Storable if (defined($PDL::VERSION)); ##-- HACK

  ##-- I/O layers
  my @iolayers = $args{iolayers} ? @{$args{iolayers}} : qw();
  binmode($fh);
  binmode($fh,$_) foreach (@iolayers);

  if (grep { $_ =~ /^:gzip/ } @iolayers) {
    ##-- hack: thaw gzipped binary files
    my $frozen = join('',<$fh>);
    if (!defined($ref=Storable::thaw($frozen))) {
      confess( __PACKAGE__ , "::loadBinFh(): Storable::thaw() failed.\n");
      return undef;
    }
  }
  elsif (!defined($ref=Storable::fd_retrieve($fh))) {
    confess( __PACKAGE__ , "::loadBinFh(): Storable::fd_retrieve() failed.\n");
    return undef;
  }
  eval "require $ref;"; ##-- hack
  return UNIVERSAL::can($ref,'loadBinRef') ? $ref->loadBinRef() : $ref; ##-- BUGGY
}


##======================================================================
## I/O: Compressed Binary: Save
##======================================================================

## $str = $obj->saveZBinString(%args)
##  + calls $obj->saveBinString()
##  + %args:
##     zlevel=>$level
sub saveZBinString {
  my ($obj,%args) = @_;
  return Compress::Zlib::compress($obj->saveBinString(%args),
				  ($args{zlevel} ? $args{zlevel} : $DEFAULT_ZBIN_ZLEVEL));
}

## $bool = $obj->saveZBinFile($filename,%args)
##   + calls $obj->saveZBinFh($fh,%args)
sub saveZBinFile {
  my ($obj,$file,%args) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::saveZBinFile(): open failed for '$file': $!");
    return undef;
  }
  binmode($fh);
  my $rc  = $obj->saveZBinFh($fh,%args);
  $fh->close() if (!ref($file));
  return $rc;
}

## $bool = $obj->saveZBinFh($fh,%args)
##   + calls $obj->saveBinString(%args)
##  + %args:
##     zlevel=>$level
sub saveZBinFh {
  my ($obj,$fh,%args) = @_;
  $fh->print($obj->saveZBinString(%args));
  return 1;
}


##======================================================================
## I/O: Compressed Binary: Load
##======================================================================

## $obj_or_undef = $class_or_obj->loadZBinString($str,%args)
##  + calls $class_or_obj->loadBinString()
sub loadZBinString {
  my $that = shift;
  return $that->loadBinString(Compress::Zlib::uncompress($_[0]), @_[1..$#_]);
}

## $obj_or_undef = $class_or_obj->loadZBinFile($filename,%args)
##   + calls $class_or_obj->loadZBinFh($fh)
sub loadZBinFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new("<$file");
   if (!$fh) {
    confess( __PACKAGE__ , "::loadZBinFile(): open failed for '$file': $!");
    return undef;
  }
  my $rc = $obj->loadZBinFh($fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $obj_or_undef = $class_or_obj->loadZBinFh($fh,%args)
##  + calls $class_or_obj->loadZBinString(%args)
sub loadZBinFh {
  my ($that,$fh,%args) = @_;
  binmode($fh);
  return $that->loadZBinString(join('',<$fh>), %args);
}


##======================================================================
## I/O: Gzip-Compressed Binary: Save
##======================================================================

## $gz = gzipper($output,%gzargs)
##  + %gzargs: any options to IO::Compress::Gzip->new(), incl:
##    Name    => $string,
##    Comment => $comment,
##    Minimal => $bool,
##    -Strategy => $const,
##    ...
##  + method no longer unused
sub gzipper {
  my ($output,%args) = @_;
  require IO::Compress::Gzip;
  return IO::Compress::Gzip->new($output,
				 %DEFAULT_GZBIN_ARGS,
				 %args);
}

## $str = $obj->saveGZBinString(%args)
##  + calls $obj->saveBinString()
##  + %args:
##     gzargs=>\%gzargs, ##-- see also DEFAULT_GZBIN_ARGS
sub saveGZBinString {
  my ($obj,%args) = @_;
  require IO::Compress::Gzip;
  my $gzstr  = '';
  my $rawstr = 'pst0'.$obj->saveBinString(%args);  ##-- HACK: make it look like a Storable file
  IO::Compress::Gzip::gzip(\$rawstr,\$gzstr,
			   %DEFAULT_GZBIN_ARGS,
			   Comment=>ref($obj),
			   (defined($args{gzargs}) ? %{$args{gzargs}} : qw()),
			  );
  return $gzstr;
}

## $bool = $obj->saveGZBinFile($filename,%args)
##   + calls $obj->saveBinFh($fh,%args)
##   + %args:
##       gzargs => \%gzargs, ##-- uses '-Level'
##   + hack: we actually call system gzip here, so that Storable header gets written correctly
sub saveGZBinFile {
  my ($obj,$file,%args) = @_;
  my %gzargs = (%DEFAULT_GZBIN_ARGS, ($args{gzargs} ? %{$args{gzargs}} : qw()));
  my $level  = $gzargs{'-Level'};
  $level     = 1 if (!defined($level));
  my $fh = ref($file) ? $file : IO::File->new("| $DEFAULT_GZIP_CMD -${level} > \"$file\"");
  if (!$fh) {
    confess( __PACKAGE__ , "::saveGZBinFile(): open failed for pipe: |$DEFAULT_GZIP_CMD -${level} >'$file': $!");
    return undef;
  }
  binmode($fh);
  my $rc  = $obj->saveBinFh($fh,%args);
  $fh->close() if (!ref($file));
  return $rc;
}

## $bool = $obj->saveGZBinFh($fh,%args)
##  + calls $obj->saveBinString(%args)
##  + additional %args:
##     gzargs=>\%args_for_gzipper()
sub saveGZBinFh {
  my ($obj,$fh,%args) = @_;
  require IO::Compress::Gzip;
  my $rawstr = 'pst0'.$obj->saveBinString(%args);  ##-- HACK: make it look like a Storable file
  IO::Compress::Gzip::gzip(\$rawstr,$fh,
			   %DEFAULT_GZBIN_ARGS,
			   Comment=>ref($obj),
			   (defined($args{gzargs}) ? %{$args{gzargs}} : qw()),
			  );
  return $IO::Compress::Gzip::GzipError ? 0 : 1;
}

##======================================================================
## I/O: Gzip-Compressed Binary: Load
##======================================================================

## $obj_or_undef = $class_or_obj->loadZBinString($str,%args)
##  + calls $class_or_obj->loadBinString()
##  + NOT compatible with $obj->saveGZBinFile($filename)
sub loadGZBinString {
  my ($that,$gzstr) = @_[0,1];
  require IO::Uncompress::Gunzip;
  my $rawstr = '';
  IO::Uncompress::Gunzip::gunzip(\$gzstr=>\$rawstr);
  $rawstr =~ s/^pst0//; ##-- HACK: detect Storable file pseudo-strings (potentially dangerous!)
  return $that->loadBinString($rawstr, @_[2..$#_]);
}

## $obj_or_undef = $class_or_obj->loadGZBinFile($filename,%args)
##  + calls $class_or_obj->loadBinFh($fh) on a pipe from 'gunzip'
##  + expects that file was saved with $obj->saveGZBinFile($filename)
##  + NOT compatible with $obj->saveGZBinFh($fh)
##  + NOT compatible with $obj->saveGZBinString()
sub loadGZBinFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new("$DEFAULT_GUNZIP_CMD \"$file\" |");
   if (!$fh) {
    confess( __PACKAGE__ , "::loadGZBinFile(): open failed for pipe 'gunzip -c \"$file\"|': $!");
    return undef;
  }
  my $rc = $obj->loadBinFh($fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $obj_or_undef = $class_or_obj->loadZBinFh($fh,%args)
##  + calls $class_or_obj->loadZBinString(%args)
##  + NOT compatible with $obj->saveGZBinFile($filename)
sub loadGZBinFh {
  my ($that,$fh) = @_[0,1];
  require IO::Uncompress::Gunzip;
  binmode($fh);
  my $rawstr = '';
  IO::Uncompress::Gunzip::gunzip($fh=>\$rawstr);
  $rawstr =~ s/^pst0//; ##-- HACK: detect Storable file pseudo-strings (potentially dangerous!)
  return $that->loadBinString($rawstr, @_[2..$#_]);
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
  my ($obj,$file,%args) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::saveNativeFile(): open failed for '$file': $!");
    return undef;
  }
  binmode($fh,$_) foreach (':utf8', $args{iolayers} ? @{$args{iolayers}} : qw());

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
  binmode($fh,$_) foreach (qw(:utf8 :gzip(autopop)));

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
## I/O: Perl: Save
##======================================================================

## $str = $obj->savePerlString(@args)
##  + calls $obj->savePerlFh()
sub savePerlString {
  my $obj = shift;
  my $str = '';
  my $fh = IO::Scalar->new(\$str);
  if (!$fh) {
    confess( __PACKAGE__ , "::savePerlString(): open failed: $!");
    return undef;
  }
  #binmode($fh,':utf8');

  $obj->savePerlFh($fh,@_);
  $fh->close();
  return $str;
}

## $bool = $obj->savePerlFile($file,@args)
##  + calls savePerlFh()
sub savePerlFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::savePerlFile(): open failed for '$file': $!");
    return undef;
  }
  #binmode($fh,':utf8');

  my $rc = $obj->savePerlFh($fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $bool = $obj->savePerlFh($fh,@args)
##  + uses Data::Dumper
sub savePerlFh {
  my ($obj,$fh,%args) = @_;
  my $dumper = Data::Dumper->new([$obj],[qw(obj)]);
  $dumper->Indent(1)->Purity(1)->Terse(1)->Sortkeys(1);
  binmode($fh,$_) foreach ($args{iolayers} ? @{$args{iolayers}} : qw());
  $fh->print($dumper->Dump);
  return 1;
}

##======================================================================
## I/O: Perl: Load
##======================================================================

## $obj = $obj->loadPerlString($str,@args)
sub loadPerlString {
  my ($obj,$str) = splice(@_,0,2);
  my $fh = IO::Scalar->new(\$str);
  if (!$fh) {
    confess( __PACKAGE__ , "::loadPerlString(): open failed: $!");
    return undef;
  }
  #binmode($fh,':utf8');

  my $rc = $obj->loadPerlFh($fh,@_);
  $fh->close();
  return $rc;
}

## $obj = $obj->loadPerlFile($file,@args)
##  + calls loadPerlFh()
sub loadPerlFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::loadPerlFile(): open failed for '$file': $!");
    return undef;
  }
  #binmode($fh,':utf8');

  my $rc = $obj->loadPerlFh($fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $obj = $class_or_obj->loadPerlFh($fh,@args)
##  + calls loadPerlString()
sub loadPerlFh {
  my ($obj,$fh) = @_;
  eval { binmode($fh, ':gzip(autopop)'); };
  eval('{ no strict qw(vars); $obj='.join('',<$fh>).' }');
  if ($@) {
    carp(ref($obj)."::loadPerlFh(): error: $@");
  }
  return $obj;
}

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
	  'zbin'    => __PACKAGE__->ioModeHash('ZBin'),
	  'gzbin'   => __PACKAGE__->ioModeHash('GZBin'),
	  'perl'    => __PACKAGE__->ioModeHash('Perl'),
	  ##--
	  'DEFAULT' => __PACKAGE__->ioModeHash('Native'),
	 };

}

## \%modeHash = $class_or_obj->ioModeHash($fileTypeInfix)
##  + populates a mode hash for $filenameInfix:
##    (load|save)${fileTypeInfix}(String|File|Fh)
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
  eval {
    no warnings 'redefine';
    *{(ref($that)||$that)."::ioModes"} = sub { return $modes; }
  }
}


## \@fileSuffixModes = $class_or_obj->fileSuffixModes()
##   + @fileSuffixModes = ( {regex=>$fileRegex, mode=>$ioModeStr, iolayers=>\@ioLayers}, ... )
##   + subclasses *may* redefine this method directly, but use of
##     'registerFileSuffix()' is highly reccommended
sub fileSuffixModes {
  return [
	  {regex=>qr/\.bin$/i,     mode=>'bin',  iolayers=>[]},
	  {regex=>qr/\.zbin$/i,    mode=>'zbin', iolayers=>[]},

	  ##-- ??
	  {regex=>qr/(?:\.bin\.gz)|(?:.gzbin)$/i, mode=>'gzbin', iolayers=>[]},

	  {regex=>qr/\.xml$/i,     mode=>'xml', iolayers=>[]},
	  {regex=>qr/\.xml\.gz$/i, mode=>'xml', iolayers=>[':gzip']},

	  {regex=>qr/\.native$/i,     mode=>'native', iolayers=>[]},
	  {regex=>qr/\.native\.gz$/i, mode=>'native', iolayers=>[':gzip']},

	  {regex=>qr/\.perl$/i,     mode=>'perl', iolayers=>[]},
	  {regex=>qr/\.perl\.gz$/i, mode=>'perl', iolayers=>[':gzip']},

	  {regex=>qr/\.pl$/i,     mode=>'perl', iolayers=>[]},
	  {regex=>qr/\.pl\.gz$/i, mode=>'perl', iolayers=>[':gzip']},
	 ];
}

## undef = $class_or_obj->registerFileSuffix($suffix,$modeString)
##  + redefines 'sub ${class}::fileSuffixModes() {...}'
sub registerFileSuffix {
  my ($that,$suffix,$modeString) = @_;
  my $suffixes = [];
  @$suffixes = @{$that->fileSuffixModes()} if (UNIVERSAL::can($that,'fileSuffixModes'));
  unshift(@$suffixes, {regex=>qr/${suffix}$/i, mode=>$modeString});
  eval {
    no warnings 'redefine';
    *{(ref($that)||$that)."::fileSuffixModes"} = sub { return $suffixes; }
  }
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
*load = *loadFile = \&loadGenericFile;
sub loadGenericFile {
  my ($that,$filename,%args) = @_;
  my $mode = $args{mode};
  my $iolayers = $args{iolayers};
  delete(@args{qw(mode iolayers)});

  ##-- guess mode from filename suffix
  if (!defined($mode)) {
    my $suffs = $that->fileSuffixModes();
    foreach $suffSpec (@$suffs) {
      if ($filename =~ $suffSpec->{regex}) {
	$mode     = $suffSpec->{mode};
	$iolayers = $suffSpec->{iolayers} if (!$iolayers);
	last;
      }
    }
  }
  $mode     = 'DEFAULT' if (!defined($mode));
  $iolayers = [] if (!$iolayers);

  ##-- first try file method directly (and pass on 'iolayers' arg)
  if (defined($sub=$that->ioModes->{$mode}{loadFile})
      &&
      (ref($sub) || defined($sub=UNIVERSAL::can($that,$sub))))
    {
      return $sub->($that,$filename,iolayers=>$iolayers,%args);
    }

  ##-- get load sub: fallback to fh method, setting I/O layers ourselves
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
  binmode($fh, $_) foreach (':utf8', ':gzip(autopop)', @$iolayers);

  my $rc = $sub->($that, $fh, %args);
  $fh->close();
  return $rc;
}


## $obj = $obj->loadGenericFh($fh,%args)
*loadFh = \&loadGenericFh;
sub loadGenericFh {
  my ($that,$fh,%args) = @_;
  my $mode = defined($args{mode}) ? $args{mode} : 'DEFAULT';
  my $iolayers = $args{iolayers} ? $args{iolayers} : [];
  delete(@args{qw(mode iolayers)});

  ##-- get load sub: fh method only
  if (!defined($sub=$that->ioModes->{$mode}{loadFh})
      ||
      (!ref($sub) && !defined($sub=UNIVERSAL::can($that,$sub))))
    {
      confess((ref($that)||$that), "::loadGenericFh(): could not find load sub for mode '$mode': $!");
      return undef;
    }

  ##-- set I/O layers
  binmode($fh, $_) foreach (':utf8', ':gzip(autopop)', @$iolayers);

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
*save = *saveFile = \&saveGenericFile;
sub saveGenericFile {
  my ($that,$filename,%args) = @_;
  my $mode = $args{mode};
  my $iolayers = $args{iolayers};
  delete(@args{qw(mode iolayers)});

  ##-- guess mode from filename suffix
  if (!defined($mode)) {
    my $suffs = $that->fileSuffixModes();
    foreach $suffSpec (@$suffs) {
      if ($filename =~ $suffSpec->{regex}) {
	$mode     = $suffSpec->{mode};
	$iolayers = $suffSpec->{iolayers} if (!$iolayers);
	last;
      }
    }
  }
  $mode = 'DEFAULT' if (!defined($mode));
  $iolayers = [] if (!$iolayers);

  ##-- get save sub: first try file method directly, passing on {iolayers}
  if (defined($sub=$that->ioModes->{$mode}{saveFile})
      &&
      (ref($sub) || defined($sub=UNIVERSAL::can($that,$sub))))
    {
      return $sub->($that,$filename, iolayers=>$iolayers,%args);
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

  my $rc = $sub->($that, $fh, iolayers=>[':utf8',$iolayers], %args);
  $fh->close();
  return $rc;
}


## $obj = $obj->saveGenericFh($fh,%args)
*saveFh = \&saveGenericFh;
sub saveGenericFh {
  my ($that,$fh,%args) = @_;
  my $mode = defined($args{mode}) ? $args{mode} : 'DEFAULT';
  my $iolayers = $args{iolayers} ? $args{iolayers} : [];
  delete(@args{qw(mode iolayers)});

  ##-- get save sub
  if (!defined($sub=$that->ioModes->{$mode}{saveFh})
      ||
      (!ref($sub) && !defined($sub=UNIVERSAL::can($that,$sub))))
    {
      confess((ref($that)||$that), "::saveGenericFh(): could not find save sub for mode '$mode': $!");
      return undef;
    }

  my $rc = $sub->($that, $fh, iolayers=>$iolayers, %args);
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
