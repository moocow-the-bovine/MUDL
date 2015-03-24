##-*- Mode: Perl -*-
##
## File: MUDL::CorpusIO::XML.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpora: I/O:
##    XML corpora via libxml
##======================================================================

package MUDL::CorpusIO::XML;
use MUDL::Object qw(dummy);
use MUDL::CorpusIO;
use MUDL::Token;
use MUDL::Sentence;
use File::Basename;
use IO::File;

use strict;
use Carp;


########################################################################
## I/O : XML : Reader
########################################################################
package MUDL::CorpusReader::XML;
use strict;
use Carp;
use MUDL::XML qw(:xpaths :styles);
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusReader);

our %TokenClasses =
  (
   'raw' => 'MUDL::Token::Raw',
   'tt' => 'MUDL::Token::TT',
   'xml' => 'MUDL::Token::XML',
   'token' => 'MUDL::Token',
   'default' => 'MUDL::Token::XML',
  );

## new(%args)
sub new {
  my ($that,%args) = @_;
  my $self = bless {
		    tokenClass => 'default', ##-- token subclass to generate

		    s_xpath => $s_xpath,
		    token_xpath => $token_xpath,
		    text_xpath => $text_xpath,
		    detail_xpath => $detail_xpath,
		    tag_xpath => $tag_xpath,
		
		    s_elt => 's',
		    token_elt => 'token',
		    text_elt => 'text',
		    detail_elt => 'detail',
		    tag_elt => 'tag',

		    xmlparser => MUDL::XML::Parser->new(),
		    snodes => [], ##-- remaining sentence nodes
		    sentbuf => undef, ##-- sentence buffer for getToken
		    nsents => 0,
		    ntoks => 0,
		    %args,
		   }, ref($that)||$that;

  if (!$self->{stylesheet}) {
    my $style = $self->{style} ? $self->{sytle} : stylesheet_xml2norm(%$self);
    my $xslt  = XML::LibXSLT->new();
    my $sdoc  = $self->{xmlparser}->parse_string($style)
      or croak( __PACKAGE__ , "::new(): could not parse stylesheet document: $!");
    $self->{stylesheet} = $xslt->parse_stylesheet($sdoc)
      or croak( __PACKAGE__ , "::new(): could not parse stylesheet: $!");
  }

  ##-- token class
  if (defined($TokenClasses{"\L$self->{tokenClass}\E"})) {
    $self->{tokenClass} = $TokenClasses{"\L$self->{tokenClass}\E"};
  } else {
    carp( __PACKAGE__ , "::new(): unknown tokenClass '$self->{tokenClass}' -- using default.");
    $self->{tokenClass} = $TokenClasses{default};
  }

  return $self;
}

## $n = nSentences()
*nSents = *nsents = \&nSentences;
sub nSentences { return $_[0]->{nsents}; }

## $n = $cr->nTokens()
*nToks = *ntoks = \&nTokens;
sub nTokens { return $_[0]->{ntoks}; }

## reset()
sub reset {
  $_[0]{nsents} = $_[0]{ntoks} = 0;
  $_[0]{doc} = undef;
}


## $bool = $cr->eof
sub eof { return !$_[0]->{snodes} && !$_[0]->{sentbuf}; }

## undef = $cr->fromString($string)
sub fromString {
  my ($cr,$str) = @_;
  $cr->{doc} = $cr->{xmlparser}->parse_string($str)
    or croak( __PACKAGE__ , "::fromString(): parse failed: $!");
  if ($cr->{stylesheet}) {
    $cr->{doc} = $cr->{stylesheet}->transform($cr->{doc})
      or croak( __PACKAGE__ , "::fromString(): transform failed: $!");
  }
  @{$cr->{snodes}} = $cr->{doc}->documentElement->getChildrenByTagName($cr->{s_elt});
  $cr->{nsents} = scalar(@{$cr->{snodes}});
}

## undef = $cr->fromFile($filename_or_fh);
sub fromFile {
  my ($cr,$file) = @_;
  if (ref($file)) {
    $cr->{doc} = $cr->{xmlparser}->parse_fh($file)
      or croak( __PACKAGE__ , "::fromFile(): parse failed for filehandle: $!");
  } else {
    $cr->{doc} = $cr->{xmlparser}->parse_file($file)
      or croak( __PACKAGE__ , "::fromFile(): parse failed for file '$file': $!");
  }
  if ($cr->{stylesheet}) {
    $cr->{doc} = $cr->{stylesheet}->transform($cr->{doc})
      or croak( __PACKAGE__ , "::fromString(): transform failed: $!");
  }
  $cr->{filename} = $file;
  @{$cr->{snodes}} = $cr->{doc}->documentElement->getChildrenByTagName($cr->{s_elt});
  $cr->{nsents} = scalar(@{$cr->{snodes}});
}

## undef = $cr->fromFh($fh);
sub fromFh {
  my ($cr,$fh) = @_;
  $cr->{doc} = $cr->{xmlparser}->parse_fh($fh)
    or croak( __PACKAGE__ , "::fromFh(): parse failed for filehandle: $!");

  if ($cr->{stylesheet}) {
    $cr->{doc} = $cr->{stylesheet}->transform($cr->{doc})
      or croak( __PACKAGE__ , "::fromString(): transform failed: $!");
  }
  @{$cr->{snodes}} = $cr->{doc}->documentElement->getChildrenByTagName($cr->{s_elt});
  $cr->{nsents} = scalar(@{$cr->{snodes}});
}


## \@sentence = $cr->getSentence();
#select(STDERR); $|=1; select(STDOUT);
sub getSentence {
  my $cr = shift;
  return undef if (!$cr->{doc} || !@{$cr->{snodes}});

  #print STDERR ".";

  if ($cr->{sentbuf}) {
    my $sent = $cr->{sentbuf};
    $cr->{sentbuf} = undef;
    return $sent;
  }

  my $snode = shift(@{$cr->{snodes}});
  my $sent = bless [], 'MUDL::Sentence';
  foreach my $toknode ($snode->getChildrenByTagName($cr->{token_elt})) {
    $cr->{ntoks}++;
    push(@$sent, $cr->{tokenClass}->fromCorpusXMLNode($toknode));
  }
  return $sent;
}

## \%token_or_undef = $cr->getToken();
sub getToken {
  my $cr = shift;
  return undef if (!$cr->{doc} || !@{$cr->{snodes}});

  if ($cr->{sentbuf}) {
    if (@{$cr->{sentbuf}}) {
      $cr->{ntoks}++;
      return shift(@{$cr->{sentbuf}});
    }
  }
  $cr->{sentbuf} = $cr->getSentence;
  return undef;
}


########################################################################
## I/O : XML : Writer
########################################################################
package MUDL::CorpusWriter::XML;
use strict;
use Carp;
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusWriter);

## new(%args)
sub new {
  my ($that,%args) = @_;
  my $self = bless {
		    s_elt => 's',
		    token_elt => 'token',
		    text_elt => 'text',
		    detail_elt => 'detail',

		    attr2elt => {}, ##-- attribute-to-element-name conversion

		    tag_elt => 'tag',
		    root_elt => 'MUDL.Corpus',
		    flush => undef, ##-- flushing sub
		    xmlencoding => 'UTF-8',
		    xmlversion => '1.0',
		    %args,
		   }, ref($that)||$that;

  return $self;
}

## $bool = $cw->flush
sub flush {
  my $cw = shift;
  return $cw->{flush} ? $cw->{flush}->($cw) : undef;
}

## undef = $cw->toString(\$str)
sub toString {
  my ($cw,$sref) = @_;
  my $doc  = $cw->{doc} = XML::LibXML::Document->new($cw->{xmlversion}, $cw->{xmlencoding});
  $doc->setDocumentElement($cw->{root}=XML::LibXML::Element->new($cw->{root_elt}));
  $cw->{flush} =
    sub {
      my $cw = shift;
      if (defined($cw->{doc})) {
	$cw->{doc}->setCompression($cw->{compress}) if (defined($cw->{compress}));
	$$sref = $cw->{doc}->toString($cw->{format});
      }
    };
}

## undef = $cw->toFile($filename)
sub toFile {
  my ($cw,$file) = @_;
  my $doc  = $cw->{doc} = XML::LibXML::Document->new($cw->{xmlversion}, $cw->{xmlencoding});
  $doc->setDocumentElement($cw->{root}=XML::LibXML::Element->new($cw->{root_elt}));
  $cw->{flush} =
    sub {
      my $cw = shift;
      if (defined($cw->{doc})) {
	$cw->{doc}->setCompression($cw->{compress}) if (defined($cw->{compress}));
	$cw->{doc}->toFile($file, $cw->{format} || 0);
      }
    };
}

## undef = $cw->toFh($fh)
sub toFh {
  my ($cw,$file) = @_;
  my $doc  = $cw->{doc} = XML::LibXML::Document->new($cw->{xmlversion}, $cw->{xmlencoding});
  $doc->setDocumentElement($cw->{root}=XML::LibXML::Element->new($cw->{root_elt}));
  $cw->{flush} =
    sub {
      my $cw = shift;
      if (defined($cw->{doc})) {
	$cw->{doc}->setCompression($cw->{compress}) if (defined($cw->{compress}));
	$cw->{doc}->toFH($file, $cw->{format});
      }
    };
}

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$sent) = @_;
  return undef if (!$cw->{doc});

  my ($snode,$tok);
  $cw->{root}->appendChild($snode=XML::LibXML::Element->new($cw->{s_elt}));
  foreach $tok (@$sent) {
    $snode->appendChild($tok->toCorpusXMLNode());
  }
}
sub putSentenceOld {
  my ($cw,$sent) = @_;
  return undef if (!$cw->{doc});

  my ($snode,$tok,$toknode,$tag, $akey,$aval,$a_elt,$anode);
  $cw->{root}->appendChild($snode=XML::LibXML::Element->new($cw->{s_elt}));
  foreach $tok (@$sent) {
    $snode->appendChild($toknode = XML::LibXML::Element->new($cw->{token_elt}));
    if (ref($tok)) {
      $toknode->appendTextChild($cw->{text_elt}, $tok->text);
      $toknode->appendTextChild($cw->{tag_elt}, $tag) if (defined($tag=$tok->tag));
      foreach $akey ($tok->attributeNames) {
	$a_elt = $cw->{attr2elt}{$akey};
	$a_elt = $cw->{detail_elt} if (!defined($a_elt));
	$anode = XML::LibXML::Element->new($a_elt);
	$anode->setAttribute('key',$akey);
	$anode->appendText($tok->attribute($akey));
	$toknode->appendChild($anode);
      }
    } else {
      $toknode->appendTextChild($cw->{text_elt}, $tok);
    }
  }
}

## undef = $cw->putToken($text_or_hashref)
#sub putToken

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
