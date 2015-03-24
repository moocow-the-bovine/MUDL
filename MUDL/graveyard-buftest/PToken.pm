##-*- Mode: CPerl -*-

## File: MUDL::PToken.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: tokens (packed)
##======================================================================

########################################################################
## class MUDL::PToken
##  + general hash-based enumerating token class
##  + this package makes heavy use of global variables: you have been warned
########################################################################

package MUDL::PToken;
use MUDL::Token;
use MUDL::Enum;
use Carp;
use utf8;
our @ISA = qw(MUDL::Scalar MUDL::Token);

use strict;

##======================================================================
## Globals

## $MUDL::PToken::ENUMS  = [ $attrEnum_id0, ..., $attrEnum_idN ]
##  + s.t. $attrEnum = $ENUMS[$attrId]
our $ENUMS = undef;

## $MUDL::PToken::NAMES = [ $attrName_id0, ..., $attrName_idN ]
##  + s.t. $attrName = $ENUMS[$attrId]
our $NAMES = undef;

## $MUDL::PToken::IDS = { $attrName=>$attrId, ... }
our $IDS = undef;

## $MUDL::PToken::STACK = [ $ENUMS,$NAMES,$IDS, ... ]
##  + for more or less safe attribute-decoding methods
our $STACK = [];

##======================================================================
## Client Class Methods

## ($ENUMS,$NAMES,$IDS) = $class_or_obj->pushAttributes($ENUMS,$NAMES)
## ($ENUMS,$NAMES,$IDS) = $class_or_obj->pushAttributes($ENUMS,$NAMES,$IDS)
sub pushAttributes {
  $ENUMS=$_[1];
  $NAMES=$_[2];
  $IDS  =(defined($_[3]) ? $_[3] : {map {($NAMES->[$_]=>$_)} (0..$#$NAMES)});
  push(@$STACK, $ENUMS,$NAMES,$IDS);
  return ($ENUMS,$NAMES,$IDS);
}

## ($ENUMS,$NAMES,$IDS) = $class_or_obj->pushEnums(\%name2enum)
## ($ENUMS,$NAMES,$IDS) = $class_or_obj->pushEnums(\%name2enum,\@names)
##   + alternate form of pushAttributes()
our ($pushe_name2e,$pushe_names);
sub pushEnums {
  ($pushe_name2e,$pushe_names) = @_[1,2];
  if (!defined($pushe_names)) {
    $pushe_names = [
		    (defined($pushe_name2e->{text}) ? 'text' : qw()),
		    (defined($pushe_name2e->{tag})  ? 'tag'  : qw()),
		    sort grep { $_ ne 'text' && $_ ne 'tag' } keys(%$pushe_name2e)
		   ];
  }
  return $_[0]->pushAttributes([@$pushe_name2e{@$pushe_names}], $pushe_names);
}

## ($NAMES,$ENUMS,$IDS) = $class_or_obj->popAttributes()
sub popAttributes { return ($NAMES,$ENUMS,$IDS)=splice(@$STACK,0,3); }


##======================================================================
## Constructor
## Structure:
##  + scalar: pack("N".scalar(@$NAMES), @value_ids)
##  + for string-oriented methods, values are encoded/decoded by package GLOBALS ($NAMES,$ENUMS,$IDS),
##    which are assumed to be DEFINED and CONSISTENT

## $tok = $class->new(text=>$text, tag=>$tag, %attributes);
##   + the package-globals $ENUMS and $NAMES should be DEFINED and CONSISTENT!
our ($new_that,%new_args);
sub new {
  confess(__PACKAGE__, "::new(): \$ENUMS and \$NAMES must be defined (and consistent)!")
    if (!defined($ENUMS) || !defined($NAMES));
  ($new_that,%new_args) = @_;
  my $pstr = pack('N*',
		  map { defined($_) ? $_ : -1 }
		  map { $ENUMS->[$_]{sym2id}{$new_args{$NAMES->[$_]}} } (0..$#$NAMES)
		 );
  return bless \$pstr, ref($new_that)||$new_that;
}

## $ptok = $class_or_obj->newFomToken($tok, %argsToNew)
##  + alwys creates a new PToken
sub newFromToken { return $_[0]->fromToken(@_[1..$#_]); }

## $ptok = $class_or_obj->fromToken($tok,%argsToNew)
##  + just returns $tok if it's compatible
our ($ptok);
sub fromToken {
  $ptok = $_[0];
  if (!ref($ptok)) {
    return $_[1] if (UNIVERSAL::isa($_[1],$ptok));
    my $pstr='';
    $ptok = bless \$pstr, $ptok;
  }
  if (UNIVERSAL::isa($_[1],$ptok)) {
    $$ptok=${$_[1]};
    return $ptok;
  }
  ##
  ##-- general case: class or object method called on any MUDL::Token source object: typecast
  confess(__PACKAGE__, "::fromToken(): \$ENUMS,\$NAMES,and \$IDS must be defined (and consistent)!")
    if (!defined($ENUMS) || !defined($NAMES) || !defined($IDS));
  my $srcHash = $_[1]->asHash;
  $$ptok = pack('N*',
		map { defined($_) ? $_ : -1 }
		map { $ENUMS->[$_]{sym2id}{$srcHash->{$NAMES->[$_]}} } (0..$#$NAMES)
	       );
  return $ptok;
}


##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text {
  return $ENUMS->[$IDS->{text}]{id2sym}[ vec(${$_[0]},$IDS->{text},32) ] if ($#_ < 1);
  vec(${$_[0]},$IDS->{text},32) = $ENUMS->[$IDS->{text}]{sym2id}{$_[1]};
  return $_[1];
}

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag {
  return $ENUMS->[$IDS->{tag}]{id2sym}[ vec(${$_[0]},$IDS->{tag},32) ] if ($#_ < 1);
  vec(${$_[0]},$IDS->{tag},32) = $ENUMS->[$IDS->{tag}]{sym2id}{$_[1]};
  return $_[1];
}

## @attributeNames = $tok->attributeNames()
our %skipAttributeNames = map { ($_=>undef) } qw(text tag);
sub attributeNames { return grep { defined($_) && !exists($skipAttributeNames{$_}) } @$NAMES; }

## @attributeValues = $tok->attributeValues
sub attributeValues {
  return
    map { $ENUMS->[$_]{id2sym}[vec(${$_[0]},$_,32)] }
    grep { defined($NAMES->[$_]) && !exists($skipAttributeNames{$NAMES->[$_]}) } (0..$#$NAMES)
}

## %attributes = $tok->attributes
sub attributes {
  return
    map { ($NAMES->[$_]=>$ENUMS->[$_]{id2sym}[vec(${$_[0]},$_,32)]) }
    grep { defined($NAMES->[$_]) && !exists($skipAttributeNames{$NAMES->[$_]}) } (0..$#$NAMES)
}

##  %tokHash = $tok->asHash #-- list context
## \%tokHash = $tok->asHash #-- scalar context, may be dangerous to change
##  + like (text=>$tok->text, tag=>$tok->tag, $tok->attributes());
sub asHash {
  return (wantarray
	  ? (map { ($NAMES->[$_]=>$ENUMS->[$_]{id2sym}[vec(${$_[0]},$_,32)]) }
	     grep {defined($NAMES->[$_])} (0..$#$NAMES)
	    )
	  : {map { ($NAMES->[$_]=>$ENUMS->[$_]{id2sym}[vec(${$_[0]},$_,32)]) }
	     grep {defined($NAMES->[$_])} (0..$#$NAMES)
	    });
}

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return $ENUMS->[$IDS->{$_[1]}]{id2sym}[ vec(${$_[0]},$IDS->{$_[1]},32) ] if ($#_ < 1);
  vec(${$_[0]},$IDS->{$_[1]},32) = $ENUMS->[$IDS->{$_[1]}]{sym2id}{$_[2]};
  return $_[2];
}

##======================================================================
## I/O: Native

## $str = $tok->saveNativeStr()
##  + only saves 'text', 'tag', and numerically keyed attributes
*saveTTString = \&saveNativeString;
sub saveNativeString {
  return join("\t",
	      map { $ENUMS->[$_]{id2sym}[ vec(${$_[0]},$_,32) ] } (0..$#$ENUMS)
	     )."\n";
}

# $tok = $tok->loadNativeStr($str)
*loadTTString = \&loadNativeString;
our ($load_tok,$load_str,@load_fields);
sub loadNativeString {
  ($load_tok,$load_str) = @_;
  if (!ref($load_tok)) {
    my $str = '';
    $load_tok = bless \$str, $load_tok;
  }
  @load_fields = split(/\s*[\t\r\n]+\s*/,$load_str);
  $$load_tok = pack('N*', map { $ENUMS->[$_]{sym2id}{$load_fields[$_]} } (0..$#load_fields));
  return $load_tok;
}


##======================================================================
## I/O: LOB | Brown
# not implemented


##======================================================================
## I/O: XML
# not implemented


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
