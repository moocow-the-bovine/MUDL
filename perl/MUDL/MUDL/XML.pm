#-*- Mode: Perl -*-

## File: MUDL::XML.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner : XML utils
##======================================================================

package MUDL::XML;
our $VERSION = 0.01;
our @ISA = qw(Exporter);
our %EXPORT_TAGS =
  (
   xpaths => [qw($s_xpath $token_xpath $text_xpath $detail_xpath)],
   styles => [qw(stylesheet_xml2tt stylesheet_xml2norm)],
  );
$EXPORT_TAGS{all} = [map { @$_ } values(%EXPORT_TAGS)];
our @EXPORT_OK = @{$EXPORT_TAGS{all}};
our @EXPORT_DEFAULT = qw();

use XML::LibXML;
use XML::LibXSLT;

##======================================================================
## Constants
##======================================================================
our $s_xpath = '//s';
our $token_xpath = './token[not(@type) or @type=\'word\']';
our $text_xpath = './text[not(@normalized) or @normalized=\'1\']';
our $detail_xpath = './detail';

##======================================================================
## Parser
##======================================================================
package MUDL::XML::Parser;
our @ISA = XML::LibXML;

sub new {
  my ($that,@args) = @_;
  my $parser = bless XML::LibXML->new(@args), ref($that) || $that;

  $parser->validation(0);
  $parser->recover(1);
  $parser->expand_entities(0);
  $parser->keep_blanks(0);
  $parser->pedantic_parser(0);
  $parser->line_numbers(1);
  $parser->load_ext_dtd(0);
  $parser->complete_attributes(0);
  $parser->expand_xinclude(0);

  return $parser;
}


##======================================================================
## Document
##======================================================================
package MUDL::XML::Document;
our @ISA = qw(XML::LibXML::Document);

our $XML_ENCODING = 'UTF-8';
our $XML_VERSION = '1.0';

sub new {
  my ($that,%args) = @_;
  %args = (
	   xmlencoding=>$XML_ENCODING,
	   xmlversion=>$XML_VERSION,
	   %args
	  );
  return bless(XML::LibXML::Document->new($args{xmlversion},$args{xmlencoding}),
	       ref($that)||$that);
}

##======================================================================
## XML I/O
##======================================================================
package MUDL::XML::Object;
use Carp qw(confess);
our @ISA = qw(Exporter);

our %EXPORT_TAGS =
  (
   parser => [qw($XMLPARSER)],
  );
$EXPORT_TAGS{all} = [map { @$_ } values(%EXPORT_TAGS)];
our @EXPORT_OK = @{$EXPORT_TAGS{all}};
our @EXPORT = qw();

$XMLPARSER = MUDL::XML::Parser->new();

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

## $doc = $obj->saveXMLFile($file_or_fh)
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
  if (ref($file)) {
    $doc->toFH($file, $args{format}) || return undef;
  } else {
    $doc->toFile($file, $args{format}) || return undef;
  }
  return $doc;
}

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

## $obj_or_undef = $obj->loadXMLFile($file_or_fh,@args)
sub loadXMLFile {
  my ($obj,$file) = splice(@_,0,2);
  my ($doc);
  if (ref($file)) {
    $doc = $XMLPARSER->parse_fh($file)
      or confess( __PACKAGE__ , "::loadXMLFile() failed for '$file': $!");
  } else {
    $doc = $XMLPARSER->parse_file($file)
      or confess( __PACKAGE__ , "::loadXMLFile() failed for '$file': $!");
  }
  return $obj->loadXMLDoc($doc,@_);
}

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

## $obj = $class->newFromXMLDoc($doc)
sub newFromXMLFile {
  my ($that,$file) = splice(@_,0,2);
  my ($doc);
  if (ref($file)) {
    $doc = $XMLPARSER->parse_fh($file)
      or confess( __PACKAGE__ , "::newfromXMLFile() failed for '$file': $!");
  } else {
    $doc = $XMLPARSER->parse_file($file)
      or confess( __PACKAGE__ , "::newFromXMLFile() failed for '$file': $!");
  }
  return newFromXMLDoc($that,$doc,@_);
}


##======================================================================
## Stylesheets
##======================================================================
package MUDL::XML;
sub stylesheet_xml2norm {
  my %args = @_;
  %args = (
	   s_xpath => $s_xpath,
	   token_xpath => $token_xpath,
	   text_xpath => $text_xpath,
	   detail_xpath => $detail_xpath,
	   %args
	  );
  return
    qq(<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output
    method="xml"
    version="1.0"
    encoding="UTF-8"
    indent="no"
    />

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- options -->
  <xsl:strip-space elements="token"/>
  <xsl:strip-space elements="sentence"/>
  <xsl:strip-space elements="s"/>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- root -->
  <xsl:template match="/">
    <document>
      <xsl:for-each select="$args{s_xpath}">
        <xsl:call-template name="sentence"/>
      </xsl:for-each>
    </document>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- s -->
  <xsl:template name="sentence">
    <s>
      <xsl:for-each select="$args{token_xpath}">
        <xsl:call-template name="token"/>
      </xsl:for-each>
    </s>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- token -->
  <xsl:template name="token">
    <token>
      <text>
        <xsl:for-each select="$args{text_xpath}">
          <xsl:value-of select="."/>
        </xsl:for-each>
      </text>
      <xsl:for-each select="$args{detail_xpath}">
        <xsl:call-template name="token-detail"/>
      </xsl:for-each>
    </token>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- token-detail -->
  <xsl:template name="token-detail">
    <detail>
      <xsl:value-of select="text()"/>
    </detail>
  </xsl:template>

</xsl:stylesheet>
);
}


##----------------------------------------------------------------------
## Stylesheet setup
##----------------------------------------------------------------------
sub stylesheet_xml2tt {
  my %args = @_;
  %args = (
	   s_xpath => $s_xpath,
	   token_xpath => $token_xpath,
	   text_xpath => $text_xpath,
	   detail_xpath => $detail_xpath,
	   %args
	  );
  return
    qq(<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="text"/>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- options -->
  <xsl:strip-space elements="token"/>
  <xsl:strip-space elements="sentence"/>
  <xsl:strip-space elements="s"/>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- body -->
  <xsl:template match="/">
    <xsl:for-each select="$args{s_xpath}">
      <xsl:call-template name="sentence"/>
    </xsl:for-each>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- s -->
  <xsl:template name="sentence">
    <xsl:for-each select="$args{token_xpath}">
      <xsl:call-template name="token"/>
    </xsl:for-each>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- token -->
  <xsl:template name="token">
    <xsl:for-each select="$args{text_xpath}">
      <xsl:call-template name="token-text"/>
    </xsl:for-each>
    <xsl:for-each select="$args{detail_xpath}">
      <xsl:call-template name="token-detail"/>
    </xsl:for-each>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- text -->
  <xsl:template name="token-text">
    <xsl:value-of select="./text()"/>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- detail -->
  <xsl:template name="token-detail">
    <xsl:text>	</xsl:text>
    <xsl:value-of select="./text()"/>
  </xsl:template>

</xsl:stylesheet>
);
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
