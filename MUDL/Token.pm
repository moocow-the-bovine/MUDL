##-*- Mode: CPerl -*-

## File: MUDL::Token.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: tokens
##======================================================================

########################################################################
## class MUDL::Token
##  + general hash-based token class
########################################################################

package MUDL::Token;
use MUDL::Object;
use Carp;
use utf8;
our @ISA = qw(MUDL::Object);

##======================================================================
## Default object structure
##  + hashref { text=>$text, tag=>$tag, %attributes }

sub new { return bless { @_[1..$#_] }, ref($_[0])||$_[0]; }

##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text { return (exists($_[1]) ? $_[0]{text}=$_[1] : $_[0]{text}); }

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag { return (exists($_[1]) ? $_[0]{tag}=$_[1] : $_[0]{tag}); }

## @attributeNames = $tok->attributeNames()
sub attributeNames { return grep { $_ ne 'text' && $_ ne 'tag' } keys(%{$_[0]}); }

## %attributes = $tok->attributes
sub attributes { return map { $_=>$_[0]{$_} } $_[0]->attributeNames; }

## @attributeValues = $tok->attributeValues
sub attributeValues { return map { $_[0]{$_} } $_[0]->attributeNames; }

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return ($#_ > 1
	  ? $_[0]{$_[1]}=$_[2]
	  : $_[0]{$_[1]});
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
	       @{$_[0]}{qw(text tag)},
	       (map { $_[0]{$_} } (sort { $a <=> $b } grep { /^\d+$/ } keys(%{$_[0]}))))
	     )."\n";
}

# $tok = $tok->loadNativeStr($str)
*loadTTString = \&loadNativeString;
sub loadNativeString {
  my ($tok,$str) = @_;
  $tok = $tok->new() if (!ref($tok));
  my @fields = split(/\s*\t+\s*/,$str);
  @$tok{qw(text tag),0..($#fields-2)} = @fields;
  return $tok;
}


##======================================================================
## I/O: LOB

## $str = $tok->saveLobStrint()
##  + only saves 'text', 'tag', and numerically keyed attributes
sub saveLobString {
  return join("_",
	      (grep {
		defined($_)
	      }
	       @{$_[0]}{qw(text tag)},
	       (map { $_[0]{$_} } (sort { $a <=> $b } grep { /^\d+$/ } keys(%{$_[0]}))))
	     );
}

## $tok = $tok->loadLobString($str)
sub loadLobString {
  my ($tok,$str) = @_;
  $tok = $tok->new() if (!ref($tok));
  my @fields = split(/\_/,$str);
  @$tok{qw(text tag),0..($#fields-2)} = @fields;
  return $tok;
}


##======================================================================
## I/O: XML

## $node = $token->toCorpusXMLNode()
sub toCorpusXMLNode {
  my $node = XML::LibXML::Element->new('token');
  $node->appendTextChild('text',$_[0]->text);
  $node->appendTextChild('tag',$_[0]->tag);
  my ($a,$anode);
  foreach $a ($_[0]->attributeNames) {
    $anode = XML::LibXML::Element->new('detail');
    $anode->setAttribute('key',$a);
    $anode->appendText($_[0]->attribute($a));
    $node->appendChild($anode);
  }
  return $node;
}

## $tok = $tok->fromCorpusXMLNode($node)
sub fromCorpusXMLNode {
  my ($tok,$node) = @_;
  $tok = $tok->new if (!ref($tok));
  $tok->text(join('', map { $_->textContent } $node->getChildrenByTagName('text')));
  $tok->tag(join('', map { $_->textContent } $node->getChildrenByTagName('tag')));
  my $akey=0;
  foreach my $anode ($node->getChildrenByTagName('detail')) {
    $tok->attribute($akey++,$anode->textContent);
  }
  return $tok;
}


########################################################################
## class MUDL::Token::TT
##  + array-based token class for TT files
########################################################################
package MUDL::Token::TT;
use Carp;
use utf8;
our @ISA = qw(MUDL::Array MUDL::Token);

##======================================================================
## Default object structure
##  + arrayref [$text,$tag,@details]

##-- init(%args)
sub init {
  my ($tok,%args) = @_;
  @$tok = @args{qw(text tag), sort { $a<=>$b } grep { /^\d+$/ } keys(%args)};
  return $tok;
}

##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text { return ($#_ > 0 ? $_[0][0]=$_[1] : $_[0][0]); }

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag { return ($#_ > 0 ? $_[0][1]=$_[1] : $_[0][1]); }

## @attributeNames = $tok->attributeNames()
sub attributeNames { return (0..$#{$_[0]}-2); }

## %attributes = $tok->attributes
sub attributes { return map { $_=>$_[0][$_+2] } 0..$#{$_[0]}-2; }

## @attributeValues = $tok->attributeValues
sub attributeValues { return map { $_[0][$_+2] } 0..$#{$_[0]}-2; }

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  
  return ($_[1] eq 'text'
	  ? $_[0]->text($#_ > 1 ? $_[2] : qw())
	  : ($_[1] eq 'tag'
	     ? $_[0]->tag($#_ > 1 ? $_[2] : qw())
	     : ($#_ > 1
		? $_[0][int($_[1])+2]=$_[2]
		: $_[0][int($_[1])+2])));
}

##======================================================================
## I/O: Native

## $str = $tok->saveNativeStr()
##  + only saves 'text', 'tag', and numerically keyed attributes
*saveTTString = \&saveNativeString;
sub saveNativeString { return join("\t", @{$_[0]})."\n"; }

# $tok = $tok->loadNativeStr($str)
*loadTTString = \&loadNativeString;
sub loadNativeString {
  $_[0] = $_[0]->new if (!ref($_[0]));
  @{$_[0]} = split(/\s*[\t\r\n]+\s*/, $_[1]);
  return $_[0];
}

##======================================================================
## I/O: LOB

## $str = $tok->saveLobString()
##  + only saves 'text', 'tag', and numerically keyed attributes
sub saveLobString { return join('_', @{$_[0]}); }

# $tok = $tok->loadLobString($str)
sub loadLobString {
  $_[0] = $_[0]->new if (!ref($_[0]));
  @{$_[0]} = grep { defined($_) } split(/_/, $_[1]);
  return $_[0];
}

##======================================================================
## I/O: XML

## $node = $token->toCorpusXMLNode()
sub toCorpusXMLNode {
  my $node = XML::LibXML::Element->new('token');
  $node->appendTextChild('text',$_[0][0]);
  $node->appendTextChild('tag',$_[0][1]) if (defined($_[0][1]));
  $node->appendTextChild('detail',$_) foreach (@{$_[0]}[2..$#{$_[0]}]);
  return $node;
}

## $tok = $tok->fromCorpusXMLNode($node)
sub fromCorpusXMLNode {
  my ($tok,$node) = @_;
  $tok = $tok->new if (!ref($tok));
  @$tok = qw();
  $tok->[0] = join('', map { $_->textContent } $node->getChildrenByTagName('text'));
  $tok->[1] = join('', map { $_->textContent } $node->getChildrenByTagName('tag'));
  push(@$tok, $_->textContent) foreach ($node->getChildrenByTagName('detail'));
  return $tok;
}

########################################################################
## class MUDL::Token::XML
##  + node-based token class for XML files
########################################################################
package MUDL::Token::XML;
use XML::LibXML;
use Carp;
use utf8;
our @ISA = qw(MUDL::Token XML::LibXML::Element);

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
## I/O: Lob

## $str = $tok->saveLobString()
##  + only saves 'text', 'tag', and numerically keyed attributes
sub saveLobString {
  return join('_',
	      (grep {
		defined($_)
	      }
	       $_[0]->text,
	       $_[0]->tag,
	       (map { $_->textContent } $_[0]->getChildrenByTagName('detail'))),
	     );
}

# $tok = $tok->loadLobString($str)
sub loadLobString {
  my ($tok,$str) = @_;
  $tok = $tok->new() if (!ref($tok));
  my @fields = grep { defined($_) } split(/_/,$str);
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



########################################################################
## class MUDL::Token::Raw
##  + scalar-based token class for raw-text tokens
########################################################################
package MUDL::Token::Raw;
use Carp;
use utf8;
our @ISA = qw(MUDL::Scalar MUDL::Token);

##======================================================================
## Default object structure
##  + scalar-ref \$text

##-- init(%args)
sub init {
  my ($tok,%args) = @_;
  $$tok = $args{text};
  return $tok;
}

##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text { return ($#_ > 0 ? ${$_[0]}=$_[1] : ${$_[0]}); }

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag { return undef; }

## @attributeNames = $tok->attributeNames()
sub attributeNames { return qw(); }

## %attributes = $tok->attributes()
sub attributes { return qw(); }

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return ($_[1] eq 'text'
	  ? $_[0]->text($_[2])
	  : undef);
}

##======================================================================
## I/O: Native

## $str = $tok->saveNativeStr()
##  + only saves 'text', 'tag', and numerically keyed attributes
*saveTTString = \&saveNativeString;
sub saveNativeString { return ${$_[0]}."\n"; }

# $tok = $tok->loadNativeStr($str)
*loadTTString = \&loadNativeString;
sub loadNativeString {
  $_[0] = $_[0]->new if (!ref($_[0]));
  (${$_[0]} = $_[1]) =~ s/^([^\t\r\n]*\S)\s*[\t\r\n]/$1/;
  return $_[0];
}

##======================================================================
## I/O: Lob

## $str = $tok->saveLobString()
##  + only saves 'text', 'tag', and numerically keyed attributes
sub saveLobeString { return ${$_[0]}; }

# $tok = $tok->loadLobString($str)
sub loadLobString {
  $_[0] = $_[0]->new if (!ref($_[0]));
  (${$_[0]} = $_[1]) =~ s/^([^\s\_]*\S)[\s\_]/$1/;
  return $_[0];
}


##======================================================================
## I/O: XML

## $node = $token->toCorpusXMLNode()
sub toCorpusXMLNode {
  my $node = XML::LibXML::Element->new('token');
  $node->appendTextChild('text',${$_[0]});
  return $node;
}

## $tok = $tok->fromCorpusXMLNode($node)
sub fromCorpusXMLNode {
  my ($tok,$node) = @_;
  $tok = $tok->new if (!ref($tok));
  $$tok = join('', map { $_->textContent } $node->getChildrenByTagName('text'));
  return $tok;
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
