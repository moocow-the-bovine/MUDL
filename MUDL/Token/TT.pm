##-*- Mode: CPerl -*-
##
## File: MUDL::Token::TT.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: tokens: array-based (TT)
##======================================================================

########################################################################
## class MUDL::Token::TT
##  + array-based token class for TT files
########################################################################
package MUDL::Token::TT;
use MUDL::Token::Base;
use Carp;
use utf8;
use strict;
our @ISA = qw(MUDL::Array MUDL::Token::Base);

##======================================================================
## Constructors

## $tok = $class_or_obj->new(text=>$text, tag=>$tag, %attributes);
##  + object structure: hashref { text=>$text, tag=>$tag, %attributes }
##  + REQUIRED
##  + arrayref [$text,$tag,@details]
sub new {
  my $that = shift;
  my $tok  = bless [], ref($that)||$that;
  my ($key,$val);
  while (scalar(@_)) {
    ($key,$val)=splice(@_,0,2);
    $key = ($key eq 'text' ? 0
	     : ($key eq 'tag' ? 1
		: ($key =~ /^\d+$/ ? $key : undef)));
    $tok->[$key] = $key if (defined($key));
  }
  return $tok;
}

## $tok = $class_or_obj->newFromToken($srcTok,@args_to_new)
##  + no @args_to_new are accepted
##  + OPTIONAL: general typecasting method, always copies
sub newFromToken {
  return bless([@{$_[1]}], ref($_[0])||$_[0]) if (ref($_[1]) eq (ref($_[0])||$_[0]));
  return bless([@{$_[1]->asArray}], ref($_[0])||$_[0]);
}

## $tok_or_srcTok = $class->fromToken($srcTok)
## $tok_or_newTok = $tok->fromToken($srcTok)
##  + general typecasting method, may just return $srcTok
##  + OPTIONAL: default dispatches to newFromToken()
sub fromToken {
  return $_[1] if (!ref($_[0]) && ref($_[1]) && ref($_[1])eq $_[0]);
  return $_[0]->newFromToken($_[1]);
}

## $tok_or_newTok =   $tok->fromHash(\%srcHash)
##  + general constructor; \%srcHash has structure of args to 'new()' method
##  + OPTIONAL: default just calls new()
sub fromHash {
  my ($tok,$h) = @_;
  my ($key,$val);
  while (($key,$val)=each(%$h)) {
    $key = ($key eq 'text' ? 0
	     : ($key eq 'tag' ? 1
		: ($key =~ /^\d+$/ ? $key : undef)));
    $tok->[$key] = $key if (defined($key));
  }
  return $tok;
}

## $tok_or_newTok =   $tok->fromArray(\@srcArray)
## $tok_or_newTok =   $tok->fromArray(\@srcArray,\@srcAttributeNames)
##  + general constructor
##    - \@srcArray has structure of a TT token: [ $val_attr1, ..., $val_attrN ]
##    - if \@srcAttributeNames is unspecified, it defaults to ['text','tag',0..$#$srcArray]
##  -> ::TT implementation doesn't support @srcAttributeNames
sub fromArray {
  if (ref($_[0])) {
    @{$_[0]} = @{$_[1]};
    return $_[0];
  }
  return bless [@{$_[1]}], $_[0];
}

##  %tokHash = $tok->asHash #-- list context
## \%tokHash = $tok->asHash #-- scalar context, may be dangerous to change
sub asHash {
  return (wantarray
	  ? (text=>$_[0][0], tag=>$_[0][1], map { $_=>$_[0][$_+2] } 0..$#{$_[0]}-2)
	  : {text=>$_[0][0], tag=>$_[0][1], map { $_=>$_[0][$_+2] } 0..$#{$_[0]}-2});
}

##  @tokArray = $tok->asArray()            #-- list context
## \@tokArray = $tok->asArray()            #-- scalar context
##  @tokArray = $tok->asArray(\@attrNames) #-- list context, given names
## \@tokArray = $tok->asArray(\@attrNames) #-- scalar context, given names
##  + if @attrNames is unspecified, it defaults to all numeric attribute names
##  + like @ary[0,1,@attrNames]=($tok->text, $tok->tag, @attrNames)
##  --> :TT version doesn't support @attrNames !
sub asArray {
  return wantarray ? @{$_[0]} : $_[0];
	#  ? (@{ $_[0] }[ 0, 1, ($#_ > 1 ? @_[1..$#_] : (2..$#{$_[0]})) ])
	#  : [@{ $_[0] }[ 0, 1, ($#_ > 1 ? @_[1..$#_] : (2..$#{$_[0]})) ]]);
}


##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text { return ($#_ > 0 ? $_[0][0]=$_[1] : $_[0][0]); }

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag { return ($#_ > 0 ? $_[0][1]=$_[1] : $_[0][1]); }

## $n = $tok->nfields()
sub nfields { return scalar(@{$_[0]}); }

## @attributeNames = $tok->attributeNames()
sub attributeNames { return (0..$#{$_[0]}-2); }

## @attributeValues = $tok->attributeValues
sub attributeValues { return map { $_[0][$_+2] } 0..$#{$_[0]}-2; }

## %attributes = $tok->attributes
sub attributes { return map { $_=>$_[0][$_+2] } 0..$#{$_[0]}-2; }

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
#*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
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
#*saveTTString = \&saveNativeString;
sub saveNativeString { return join("\t", grep {defined($_)} @{$_[0]})."\n"; }

# $tok = $tok->loadNativeStr($str)
#*loadTTString = \&loadNativeString;
sub loadNativeString {
  $_[0] = bless([], ref($_[0])||$_[0]) if (!ref($_[0]));
  @{$_[0]} = split(/\s*[\t\r\n]+\s*/, $_[1]);
  return $_[0];
}

##======================================================================
## I/O: Separated (LOB|Brown)

## $str = $tok->saveSepString($sep)
## $str = $tok->saveSepString($sep,\@attrNames)
#(inherited)

## $tok = $tok->loadSepString($sepre,$str)
## $tok = $tok->loadSepString($sepre,$str,\@attrNames)
#(inherited)

##======================================================================
## I/O: XML

## $node = $token->toCorpusXMLNode()
#(inherited)

## $tok = $tok->fromCorpusXMLNode($node)
#(inherited)

##======================================================================
## Sentences
##======================================================================
package MUDL::Sentence::TT;
use MUDL::Sentence;
our @ISA = qw(MUDL::Sentence);


## $stolen_sent = $class_or_obj->stealSentence($sent)
##  + convert $sent to MUDL::Sentence::TT & return converted rep
sub stealSentence {
  my $s = $_[1];
  my $tts = bless((tied(@$s) ? [@$s] : $s), 'MUDL::Sentence::TT');
  foreach (@$tts) {
    next if (ref($_) eq 'MUDL::Token::TT');
    $_ = bless scalar($_->asArray), 'MUDL::Token::TT';
  }
  return $tts;
}

1;
