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
use Carp;

our $VERSION = 0.01;

##======================================================================
## Exports
our @ISA = qw(Exporter);
our @EXPORT = qw();
our %EXPORT_TAGS =
  (
   parser => [qw($XMLPARSER)],
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
## Generic initializer
##  + object structure= hash { \%args }
##  + calls init(\%args)
sub init {
  my ($obj,%args) = @_;
  @$obj{keys(%args)} = values(%args);
}

##======================================================================
## Deep copy
##  $copy = copy($obj)
*clone = \&copy;
sub copy { return Storable::dclone(@_); }

##======================================================================
## I/O: XML: save
##======================================================================

## $node = $obj->saveXMLNode(%args)
##  + return a new node representing the object
sub saveXMLNode {
  confess( ref($_[0]) , "::saveXMLNode(): dummy method called.\n");
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

## $doc = $obj->saveXMLFile($fh)
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

## $doc = $obj->saveXMLFh($fh)
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

## $obj_or_undef = $obj->loadXMLNode($node,@args)
##  + should load object from $node
sub loadXMLNode {
  my ($obj,$node) = splice(@_,0,2);
  confess( ref($obj) , "::loadXMLNode(): dummy method called.\n");
}

## $obj_or_undef = $obj->loadXMLDoc($doc,@args)
##  + @args are passed to loadXMLNode
sub loadXMLDoc {
  my ($obj,$doc) = splice(@_,0,2);
  return $obj->loadXMLNode($doc->documentElement,@_);
}

## $obj_or_undef = $obj->loadXMLFile($filename,@args)
sub loadXMLFile {
  my ($obj,$file) = splice(@_,0,2);
  my $doc = $XMLPARSER->parse_file($file)
    or confess( __PACKAGE__ , "::loadXMLFile() failed for '$file': $!");
  return $obj->loadXMLDoc($doc,@_);
}

## $obj_or_undef = $obj->loadXMLFh($fh,@args)
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
  if (!Storable::store_fd($ref)) {
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
  if (!($obj=Storable::fd_retrieve($ref))) {
    confess( __PACKAGE__ , "::loadBinFh(): Storable::retrieve_fd() failed.\n");
    return undef;
  }
  return $obj;
}


##======================================================================
## I/O: Native: save
##======================================================================

## $str = $obj->saveNativeStr(@args)
sub saveNativeString {
  my $obj = shift;
  croak( __PACKAGE__ , "::saveNativeStr(): dummy method called.\n");
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
  my $rc = saveNativeFile($obj,$fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $bool = $obj->saveNativeFh($fh,@args)
##  + calls saveNativeStr()
sub saveNativeFh {
  my ($obj,$fh) = splice(@_,0,2);
  $fh->print($obj->saveNativeStr(@_));
  return 1;
}

##======================================================================
## I/O: Native: load
##======================================================================

## $obj = $obj->loadNativeStr($str,@args)
sub loadNativeString {
  my $obj = shift;
  croak( __PACKAGE__ , "::loadNativeStr(): dummy method called.\n");
}

## $bool = $obj->loadNativeFile($file,@args)
##  + calls saveNativeFh()
sub loadNativeFile {
  my ($obj,$file) = splice(@_,0,2);
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  if (!$fh) {
    confess( __PACKAGE__ , "::loadNativeFile(): open failed for '$file': $!");
    return undef;
  }
  my $rc = loadNativeFile($obj,$fh,@_);
  $fh->close() if (!ref($file));
  return $rc;
}

## $bool = $obj->loadNativeFh($fh,@args)
##  + calls loadNativeStr()
sub loadNativeFh {
  my ($obj,$fh) = splice(@_,0,2);
  my $rs = IO::Handle::input_record_separator();
  IO::Handle::input_record_separator(undef);
  my $rc = $obj->loadNativeStr($fh->getline, @_);
  IO::Handle::input_record_separator($rs);
}


##======================================================================
## I/O: Generic
##======================================================================

## $class_or_obj->newFromFile($filename,%args)
##  + creates a new object, loads a file
##  + known args:
##     mode => 'xml' | 'bin' | 'native'
sub newFromFile {
  my ($that,$file,%args) = @_; 
  my $obj = $that->new(%args);
  my $mode = defined($args{mode}) ? $args{mode} : '';
  if ($mode eq 'xml' || $file =~ /\.xml$/) {
    $obj->loadXMLFile($file,%args)
      or confess( __PACKAGE__ , "::newFromFile() failed for XML file '$file': $!");
  }
  elsif ($mode eq 'bin' || $file =~ /(?:\.bin|\.sto)$/) {
    $obj->loadBinFile($file,%args)
      or confess( __PACKAGE__ , "::newFromFile() failed for bin file '$file': $!");
  }
  else {
    $obj->loadNativeFile($file,%args)
      or confess( __PACKAGE__ , "::newFromFile() failed for native file '$file': $!");
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
