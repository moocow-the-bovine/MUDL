##-*- Mode: CPerl -*-
##
## File: MUDL::Token::Base.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: tokens: abstract API
##======================================================================

########################################################################
## class MUDL::Token::Base
##  + catch-all methods & required API
######################################################################
package MUDL::Token::Base;
use MUDL::Object qw(dummy);
use Carp;
use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructors

## $tok = $class_or_obj->new(text=>$text, tag=>$tag, %attributes);
##  + object structure: hashref { text=>$text, tag=>$tag, %attributes }
##  + REQUIRED
*new = dummy('new');

## $tok = $class_or_obj->newFromToken($srcTok,@args_to_new)
##  + OPTIONAL: general typecasting method, always copies
sub newFromToken { return $_[0]->new($_[1]->asHash,@_[2..$#_]); }

## $tok_or_srcTok = $class->fromToken($srcTok)
## $tok_or_newTok = $tok->fromToken($srcTok)
##  + general typecasting method, may just return $srcTok
##  + OPTIONAL: default dispatches to newFromToken()
sub fromToken {
  return $_[1] if (!ref($_[0]) && ref($_[1]) && $_[0] eq ref($_[1]));
  return $_[0]->newFromToken($_[1]);
}

## $tok_or_newTok =   $tok->fromHash(\%srcHash)
##  + general constructor; \%srcHash has structure of args to 'new()' method
##  + OPTIONAL: default just calls new()
sub fromHash { return $_[0]->new(%{$_[1]}); }

## $tok_or_newTok =   $tok->fromArray(\@srcArray)
## $tok_or_newTok =   $tok->fromArray(\@srcArray,\@srcAttributeNames)
##  + general constructor
##    - \@srcArray has structure of a TT token: [ $val_attr1, ..., $val_attrN ]
##    - if \@srcAttributeNames is unspecified, it defaults to ['text','tag',0..$#$srcArray]
##  + OPTIONAL: default calls fromHash()
sub fromArray {
  my $anames = $_[2];
  $anames = ['text','tag',0..$#{$_[1]}] if (!defined($anames));
  return $_[0]->fromHash({
			  map {
			    ($anames->[$_]=>$_[1][$_])
			  } grep {defined($anames->[$_])} (0..$#$anames)
			 });
}

##  %tokHash = $tok->asHash #-- list context
## \%tokHash = $tok->asHash #-- scalar context, may be dangerous to change
##  + OPTIONAL: default is (text=>$tok->text, tag=>$tok->tag, $tok->attributes());
sub asHash {
  return (wantarray
	  ? (text=>$_[0]->text, tag=>$_[0]->tag, $_[0]->attributes)
	  : {text=>$_[0]->text, tag=>$_[0]->tag, $_[0]->attributes});
}

##  @tokArray = $tok->asArray()            #-- list context
## \@tokArray = $tok->asArray()            #-- scalar context
##  @tokArray = $tok->asArray(\@attrNames) #-- list context, given names
## \@tokArray = $tok->asArray(\@attrNames) #-- scalar context, given names
##  + if @attrNames is unspecified, it defaults to all numeric attribute names
##  + like @ary[0,1,@attrNames]=($tok->text, $tok->tag, @attrNames)
##  + OPTIONAL: default calls asHash()
sub asArray {
  my $attrNames = $_[1];
  my $tokh   = $_[0]->asHash;
  $attrNames = ['text','tag',grep {defined($_) && $_ =~ /^\d+$/} keys(%$tokh)] if (!defined($attrNames));
  my $ary = [];
  @$ary[map {defined($attrNames->[$_]) ? $_ : qw()} (0..$#$attrNames)] = @$tokh{@$attrNames};
  return wantarray ? @$ary : $ary;
}


##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
##  + REQUIRED
*text = dummy('text');

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
##  + REQUIRED
*tag = dummy('tag');

## $n = $tok->nfields()
##  + SHOULD include 'text', 'tag'
##  + OPTIONAL: default calls scalar( @{$tok->asArray()} )
sub nfields { return scalar(@{$_[0]->asArray}); }

## @attributeNames = $tok->attributeNames()
##  + should NOT include 'text', 'tag'
##  + OPTIONAL: default calls attributes()
sub attributeNames {
  my %attrs = $_[0]->attributes();
  return keys(%attrs);
}

## @attributeValues = $tok->attributeValues()
##  + should NOT include values for 'text', 'tag'
##  + OPTIONAL: default calls attributes()
sub attributeValues {
  my %attrs = $_[0]->attributes();
  return values(%attrs);
}

## %attributes = $tok->attributes()
##  + should NOT include entries for 'text', 'tag'
##  + only required if attributes other than 'text','tag' are to be supported
sub attributes { return qw(); }

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
##  + SHOULD accept attributes 'text', 'tag'
#*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return $_[0]->text(@_[2..$#_]) if ($_[1] eq 'text');
  return $_[0]->tag(@_[2..$#_])  if ($_[1] eq 'tag');
  confess(__PACKAGE__,
	  "::attribute(): fallback method can't handle arbitrary attributes",
	  " for object of type ", ref($_[0]));
  return undef;
}

##======================================================================
## I/O: Native (TT)

## $str = $tok->saveNativeStr()
## $str = $tok->saveNativeStr(\@attributeNames)
##  + OPTIONAL: default calls $tok->asArray(\@attrNames)
#*saveTTString = \&saveNativeString;
sub saveNativeString {
  return join("\t", map {defined($_) ? $_ : ''} @{$_[0]->asArray(@_[1..$#_])})."\n";
}

## $tok_or_newtok = $tok->loadNativeStr($str)
## $tok_or_newtok = $tok->loadNativeStr($str,\@attrNames)
##  + OPTIONAL: default calls $tok->fromArray(\@attrNames)
#*loadTTString = \&loadNativeString;
sub loadNativeString {
  return $_[0]->fromArray([split(/\s*\t+\s*/,$_[1])],@_[2..$#_]);
}


##======================================================================
## I/O: LOB | Brown

## $str = $tok->saveSepString($sep)
## $str = $tok->saveSepString($sep,\@attrNames)
##  + OPTIONAL: default calls $_[0]->asArray(\@attrNames)
sub saveSepString {
  return join((defined($_[1]) ? $_[1] : '/'),
	      map {defined($_) ? $_ : ''} @{$_[0]->asArray(@_[2..$#_])});
}
sub saveBrownString { $_[0]->saveSepString('/',@_[1..$#_]); }
sub saveLobString   { $_[0]->saveSepString('_',@_[1..$#_]); }

## $tok = $tok->loadSepString($sepre,$str)
## $tok = $tok->loadSepString($sepre,$str,\@attrNames)
##  + OPTIONAL: default calls $_[0]->fromArray(\@attrNames)
sub loadSepString {
  return $_[0]->fromArray([split($_[1],$_[2])],@_[3..$#_]);
}
sub loadBrownString { $_[0]->loadSepString('/', @_[1..$#_]); }
sub loadLobString   { $_[0]->loadSepString('_', @_[1..$#_]); }


##======================================================================
## I/O: XML

## $node = $token->toCorpusXMLNode()
##  + OPTIONAL: default calls $tok->asHash()
sub toCorpusXMLNode {
  my $node = XML::LibXML::Element->new('token');
  my $tokh = $_[0]->asHash();
  my ($a,$anode);
  foreach $a (keys(%$tokh)) {
    if ($a eq 'text' || $a eq 'tag') {
      $node->appendTextChild($a,$tokh->{$a});
      next;
    }
    $anode = XML::LibXML::Element->new('detail');
    $anode->setAttribute('key',$a);
    $anode->appendText($tokh->{a});
    $node->appendChild($anode);
  }
  return $node;
}

## $tok = $tok->fromCorpusXMLNode($node)
##  + OPTIONAL: default calls $tok->new() or $tok->fromHash()
sub fromCorpusXMLNode {
  my ($tok,$node) = @_;
  my $tokh = {
	      text=>join('', map { $_->textContent } $node->getChildrenByTagName('text')),
	      tag =>join('', map { $_->textContent } $node->getChildrenByTagName('tag')),
	     };
  my $akey=0;
  my ($anode);
  foreach $anode ($node->getChildrenByTagName('detail')) {
    $tokh->{$akey++} = $anode->textContent;
  }
  return ref($tok) ? $tok->fromHash($tokh) : $tok->new(%$tokh);
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
