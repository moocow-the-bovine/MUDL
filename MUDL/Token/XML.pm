##-*- Mode: CPerl -*-
##
## File: MUDL::Token::XML.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: tokens: XML-based
##  + OBSOLETE!
##======================================================================

########################################################################
## class MUDL::Token::XML
##  + node-based token class for XML files
########################################################################
package MUDL::Token::XML;
use MUDL::Token::Base;
use XML::LibXML;
use Carp;
use utf8;
our @ISA = qw(MUDL::Token::Base XML::LibXML::Element);

##-- new
sub new {
  my $that = shift;
  my $tok = bless XML::LibXML::Element->new('token'), ref($that)||$that;
  my ($k,$v);
  while (($k,$v)=splice(@_,0,2)) {
    $tok->appendTextChild($k,$v) if (defined($v));
  }
  return $tok;
}

##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text {
  my $tok = shift;
  if (@_) {
    $tok->removeChild($_) foreach ($tok->getChildrenByTagName('text'));
    $tok->appendTextChild('text',$_[1]);
    return $_[1];
  }
  my $old = ($tok->getChildrenByTagName('text'))[0];
  return $old ? $old->textContent : '';
}

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag {
  my $tok = shift;
  if (@_) {
    $tok->removeChild($_) foreach ($tok->getChildrenByTagName('tag'));
    $tok->appendTextChild('tag',$_[1]);
    return $_[1];
  }
  my $old = ($tok->getChildrenByTagName('tag'))[0];
  return $old ? $old->textContent : '';
}

## @attributeNames = $tok->attributeNames()
sub attributeNames { return map { $_->nodeName } $_[0]->childNodes; }

## %attributes = $tok->attributes
sub attributes {
  return map { $_ => $_[0]->attribute($_) } $_[0]->attributeNames;
}


## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  my ($tok,$name) = splice(@_,0,2);
  if ($name eq 'text')   { return $tok->text(@_); }
  elsif ($name eq 'tag') { return $tok->tag(@_); }

  my $old = ($tok->getChildrenByTagName('detail'))[$_[0]];
  if (@_) {
    if ($old) {
      my $new = $old->cloneNode(0);
      $tok->replaceChild($new, $old);
      $new->appendText($_[1]);
    }
    else {
      $tok->appendTextChild('detail', $_[1]);
    }
    return $_[1];
  }

  return $old ? $old->textContent : undef;
}


##======================================================================
## I/O: Native

## $str = $tok->saveNativeStr()
##  + only saves 'text', 'tag', and numerically keyed attributes
*saveTTString = \&saveNativeString;
sub saveNativeString {
  return join("\t",
	      (grep {
		defined($_)
	      }
	       $_[0]->text,
	       $_[0]->tag,
	       (map { $_->textContent } $_[0]->getChildrenByTagName('detail'))),
	     )."\n";
}

# $tok = $tok->loadNativeStr($str)
*loadTTString = \&loadNativeString;
sub loadNativeString {
  my ($tok,$str) = @_;
  $tok = $tok->new() if (!ref($tok));
  my @fields = split(/\s*\t+\s*/,$str);
  $tok->text($fields[0]) if (@fields > 0);
  $tok->tag($fields[1]) if (@fields > 1);
  $tok->appendTextChild('detail', $_) foreach (@fields[2..$#fields]);
  return $tok;
}

##======================================================================
## I/O: XML

## $node = $token->toCorpusXMLNode()
sub toCorpusXMLNode { return $_[0]; }

## $tok = $tok->fromCorpusXMLNode($node)
sub fromCorpusXMLNode { return bless($_[1], $_[0]||ref($_[0])); }

1;
