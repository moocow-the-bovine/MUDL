#-*- Mode: Perl -*-

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

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return (exists($_[2])
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


########################################################################
## class MUDL::TTToken
##  + array-based token class for TT files
########################################################################
package MUDL::TTToken;
use Carp;
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
sub text { return (exists($_[1]) ? $_[0][0]=$_[1] : $_[0][0]); }

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag { return (exists($_[1]) ? $_[0][1]=$_[1] : $_[0][1]); }

## @attributeNames = $tok->attributeNames()
sub attributeNames { return (0..$#{$_[0]}-2); }

## %attributes = $tok->attributes
sub attributes { return map { $_-2=>$_[0][$_] } 0..$#{$_[0]}-2; }

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return ($_[1] eq 'text'
	  ? $_[0]->text($_[2])
	  : ($_[1] eq 'tag'
	     ? $_[0]->tag($_[2])
	     : (exists($_[2])
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


########################################################################
## class MUDL::RawToken
##  + scalar-based token class for raw-text tokens
########################################################################
package MUDL::RawToken;
use Carp;
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
sub text { return (exists($_[1]) ? ${$_[0]}=$_[1] : ${$_[0]}); }

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
