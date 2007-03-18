##-*- Mode: CPerl -*-
##
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
use MUDL::Token::Base; ##-- required API
use MUDL::Object;
use Carp;
use utf8;
use strict;
our @ISA = qw(MUDL::Token::Base);

##======================================================================
## Constructors

## $tok = $class->new(text=>$text, tag=>$tag, %attributes);
##  + object structure: hashref { text=>$text, tag=>$tag, %attributes }
sub new { return bless { @_[1..$#_] }, ref($_[0])||$_[0]; }

## $tok = $class_or_obj->newFromToken($srcTok,@args_to_new)
##  + general typecasting method
sub newFromToken {
  return bless({%{$_[1]},@_[2..$#_]},ref($_[0])||$_[0]) if (ref($_[1]) eq (ref($_[0])||$_[0]));
  return $_[0]->new($_[1]->asHash,@_[2..$#_]);
}

## $tok = $class_or_obj->fromToken($srcTok)
##  + general typecasting method, may just return $srcTok
#inherited

## $tok_or_newTok =   $tok->fromHash(\%srcHash)
##  + general constructor; \%srcHash has structure of args to 'new()' method
##  + OPTIONAL: default just calls new()
sub fromHash {
  if (ref($_[0])) {
    ##-- just adopt hash values
    %{$_[0]} = %{$_[1]};
    return $_[0];
  }
  return bless { @_[1..$#_] }, $_[0];
}

## $tok_or_newTok =   $tok->fromArray(\@srcArray)
## $tok_or_newTok =   $tok->fromArray(\@srcArray,\@srcAttributeNames)
##  + general constructor
##    - \@srcArray has structure of a TT token: [ $val_attr1, ..., $val_attrN ]
##    - if \@srcAttributeNames is unspecified, it defaults to ['text','tag',0..$#$srcArray]
##  + OPTIONAL: default calls fromHash()
#(inherited)

##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text { return (exists($_[1]) ? $_[0]{text}=$_[1] : $_[0]{text}); }

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag { return (exists($_[1]) ? $_[0]{tag}=$_[1] : $_[0]{tag}); }

## $n = $tok->nfields()
sub nfields { return scalar(keys(%{$_[0]})); }

## @attributeNames = $tok->attributeNames()
our %skipAttributeNames = (text=>undef,tag=>undef);
sub attributeNames { return grep { !exists($skipAttributeNames{$_}) } keys(%{$_[0]}); }

## @attributeValues = $tok->attributeValues
sub attributeValues { return map { $_[0]{$_} } grep { !exists($skipAttributeNames{$_}) } keys(%{$_[0]}); }

## %attributes = $tok->attributes
sub attributes { return map { $_=>$_[0]{$_} } grep { !exists($skipAttributeNames{$_}) } keys(%{$_[0]}); }

##  %tokHash = $tok->asHash #-- list context
## \%tokHash = $tok->asHash #-- scalar context, may be dangerous to change
##  + like (text=>$tok->text, tag=>$tok->tag, $tok->attributes());
sub asHash { return wantarray ? %{$_[0]} : $_[0]; }

##  @tokArray = $tok->asArray()            #-- list context
##  @tokArray = $tok->asArray(\@attrNames) #-- list context, given names
## \@tokArray = $tok->asArray()            #-- scalar context
## \@tokArray = $tok->asArray(\@attrNames) #-- scalar context, given names
##  + if @attrNames is unspecified, it defaults to all numeric attribute names
##  + like @ary[0,1,@attrNames]=($tok->text, $tok->tag, @attrNames)
##  + OPTIONAL: default calls asHash()
sub asArray {
  my ($tok,$attrNames) = @_;
  $attrNames = ['text','tag',grep {defined($_) && $_ =~ /^\d+$/} keys(%$tok)] if (!defined($attrNames));
  my $ary = [];
  @$ary[map {defined($attrNames->[$_]) ? $_ : qw()} (0..$#$attrNames)] = @$tok{@$attrNames};
  return wantarray ? @$ary : $ary;
}

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
#*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return ($#_ > 1
	  ? $_[0]{$_[1]}=$_[2]
	  : $_[0]{$_[1]});
}

##======================================================================
## I/O: Native (TT)

## $str = $tok->saveNativeStr()
## $str = $tok->saveNativeStr(\@attributeNames)
##  + OPTIONAL: default calls $tok->asArray(\@attrNames)
#*saveTTString = \&saveNativeString;
#inherited

## $tok_or_newtok = $tok->loadNativeStr($str)
## $tok_or_newtok = $tok->loadNativeStr($str,\@attrNames)
##  + OPTIONAL: default calls $tok->fromArray(\@attrNames)
#*loadTTString = \&loadNativeString;
#inherited


##======================================================================
## I/O: LOB | Brown

## $str = $tok->saveSepString($sep)
## $str = $tok->saveSepString($sep,\@attrNames)
##  + OPTIONAL: default calls $_[0]->asArray(\@attrNames)
#inherited

## $tok = $tok->loadSepString($sepre,$str)
## $tok = $tok->loadSepString($sepre,$str,\@attrNames)
##  + OPTIONAL: default calls $_[0]->fromArray(\@attrNames)
#inherited

##======================================================================
## I/O: XML

## $node = $token->toCorpusXMLNode()
#inherited

## $tok = $tok->fromCorpusXMLNode($node)
#inherited



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
