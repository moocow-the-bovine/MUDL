##-*- Mode: CPerl -*-
##
## File: MUDL::Token::Raw.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: tokens (raw)
##======================================================================

########################################################################
## class MUDL::Token::Raw
##  + scalar-based token class for raw-text tokens
########################################################################
package MUDL::Token::Raw;
use MUDL::Object;
use MUDL::Token::Base;
use Carp;
use utf8;
our @ISA = qw(MUDL::Scalar MUDL::Token::Base);

##======================================================================
## Default object structure
##  + scalar-ref \$text

## $tok = $class->new(text=>$text, tag=>$tag, %attributes);
##  + object structure: hashref { text=>$text, tag=>$tag, %attributes }
sub new {
  my ($tok,%args) = @_;
  my $text = $args{text};
  return bless \$text, ref($tok)||$tok;
}

##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text { return ($#_ > 0 ? ${$_[0]}=$_[1] : ${$_[0]}); }

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag { return undef; }

## $n = $tok->nfields
sub nfields { return 1; }

##======================================================================
## I/O: Native

## $str = $tok->saveNativeStr()
##  + only saves 'text', 'tag', and numerically keyed attributes
#*saveTTString = \&saveNativeString;
sub saveNativeString { return ${$_[0]}."\n"; }

# $tok = $tok->loadNativeStr($str)
#*loadTTString = \&loadNativeString;
sub loadNativeString {
  my $tok = shift;
  if (!ref($tok)) {
    my $str = '';
    $tok = bless(\$str, $tok);
  }
  ($$tok = $_[1]) =~ s/^([^\t\r\n]*\S)\s*[\t\r\n]/$1/;
  return $tok;
}

##======================================================================
## I/O: Lob

## $str = $tok->saveSepString($sep)
sub saveSepString { return ${$_[0]}; }

# $tok = $tok->loadSepString($sep)
#(inherited)


##======================================================================
## I/O: XML

## $node = $token->toCorpusXMLNode()
#(inherited)

## $tok = $tok->fromCorpusXMLNode($node)
#(inherited)


1;
