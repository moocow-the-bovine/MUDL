#-*- Mode: Perl -*-

## File: MUDL::XML.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner : XML utils
##======================================================================

package MUDL::XML;
use XML::LibXML;
use XML::LibXSLT;

our $VERSION = 0.02;

our @ISA = qw(Exporter);
our %EXPORT_TAGS =
  (
   xpaths => [qw($s_xpath $token_xpath
		 $token_norm_test $token_nonorm_test
		 $text_xpath $test_xpath_nonorm
		 $tag_xpath $detail_xpath)
	     ],
   styles => [qw(stylesheet_xml2tt stylesheet_xml2norm)],
  );
$EXPORT_TAGS{all} = [map { @$_ } values(%EXPORT_TAGS)];
our @EXPORT_OK = @{$EXPORT_TAGS{all}};
our @EXPORT_DEFAULT = qw();

##======================================================================
## Constants
##======================================================================
our $s_xpath = '//s';

#our $token_xpath = './token';
#our $token_xpath   = './token[not(@type) or @type=\'word\']';
#our $token_xpath  = './token[not(@type) or not(@type=\'punct\')]';

our $token_xpath = './token';

## $token_norm_test
##  + XPath predicate matching words to be bashed to '<text normalized="1">' value
our $token_norm_test    = 'not(@type) or @type=\'word\'';

##--
## $token_nonorm_test
##  + XPath predicate matching words to be bashed to '<text normalized="0">' value
#our $token_nonorm_test = '@type and not(@type=\'word\')';   ##-- include all non-word tokens un-normalized
#our $token_nonorm_test = '@type and not(@type,\'punct\')';  ##-- include all non-punct tokens un-normalized
#our $token_nonorm_test = '@type and starts-with(@type,\'card\')';  ##-- include all numerics
our $token_nonorm_test  = 'false()';

our $text_xpath        = './text[not(@normalized) or @normalized=\'1\']';
our $text_xpath_nonorm = './text[not(@normalized) or @normalized=\'0\']';
our $detail_xpath      = './detail';
our $tag_xpath         = './tag';

##======================================================================
## Aliases
##======================================================================

##-- words-only
sub allow_words_only {
  if (!exists($_[0]) || $_[0]) {
    $token_norm_test   = 'not(@type) or @type=\'word\'';
    $token_nonorm_test = 'false()';
  } else {
    allow_nonword_literals(1);
  }
}

##-- normalize numerics as type-labels
sub allow_card_types {
  if ($_[0]) {
    $token_norm_test   = 'not(@type) or @type=\'word\' or starts-with(@type,\'card\')';
    $token_nonorm_test = 'false()';
  } else {
    allow_words_only(1);
  }
}

##-- normalize numerics as literals
sub allow_card_literals {
  if ($_[0]) {
    $token_norm_test   = 'not(@type) or @type=\'word\'';
    $token_nonorm_test = 'starts-with(@type,\'card\')';
  } else {
    allow_words_only(1);
  }
}

##-- normalize non-words as type-labels
sub allow_nonword_types {
  if ($_[0]) {
    $token_norm_test   = 'true()';
    $token_nonorm_test = 'false()';
  } else {
    allow_words_only(1);
  }
}

##-- normalize non-words as literals
sub allow_nonword_literals {
  if ($_[0]) {
    $token_norm_test   = 'not(@type) or @type=\'word\'';
    $token_nonorm_test = 'true()';
  } else {
    allow_words_only(1);
  }
}

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
## Stylesheets
##======================================================================
package MUDL::XML;
sub stylesheet_xml2norm {
  my %args = @_;
  %args = (
	   s_xpath => $s_xpath,
	   token_xpath => $token_xpath,
	   token_norm_test => $token_norm_test,
	   token_nonorm_test => $token_nonorm_test,
	   text_xpath => $text_xpath,
	   text_xpath_nonorm => $text_xpath_nonorm,
	   detail_xpath => $detail_xpath,
	   tag_xpath => $tag_xpath,
	   %args
	  );
  return qq(<?xml version="1.0" encoding="utf-8"?>
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
        <xsl:choose>
          <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ normalized tokens -->
          <xsl:when test="$args{token_norm_test}">
            <xsl:call-template name="token_normalized"/>
          </xsl:when>
          <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ unnormalized tokens -->
          <xsl:when test="$args{token_nonorm_test}">
            <xsl:call-template name="token_non_normalized"/>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </s>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- token (normalized) -->
  <xsl:template name="token_normalized">
    <token>
      <text>
        <xsl:for-each select="$args{text_xpath}">
          <xsl:value-of select="."/>
        </xsl:for-each>
      </text>
      <xsl:for-each select="$args{tag_xpath}">
        <xsl:call-template name="token-tag"/>
      </xsl:for-each>
      <xsl:for-each select="$args{detail_xpath}">
        <xsl:call-template name="token-detail"/>
      </xsl:for-each>
    </token>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- token (non-normalized) -->
  <xsl:template name="token_non_normalized">
    <token>
      <text>
        <xsl:for-each select="$args{text_xpath_nonorm}">
          <xsl:value-of select="."/>
        </xsl:for-each>
      </text>
      <xsl:for-each select="$args{tag_xpath}">
        <xsl:call-template name="token-tag"/>
      </xsl:for-each>
      <xsl:for-each select="$args{detail_xpath}">
        <xsl:call-template name="token-detail"/>
      </xsl:for-each>
    </token>
  </xsl:template>

  <!--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-->
  <!-- token-tag -->
  <xsl:template name="token-tag">
    <tag>
      <xsl:value-of select="text()"/>
    </tag>
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
	   tag_xpath => $tag_xpath,
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
    <xsl:for-each select="$args{tag_xpath}">
      <xsl:call-template name="token-tag"/>
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
  <!-- tag -->
  <xsl:template name="token-tag">
    <xsl:text>	</xsl:text>
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
