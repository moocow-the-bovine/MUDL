##-*- Mode: CPerl -*-

## File: MUDL::EToken.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: tokens (enumerating)
##======================================================================

########################################################################
## class MUDL::EToken
##  + general hash-based enumerating token class
########################################################################

package MUDL::EToken;
use MUDL::Token;
use MUDL::Enum;
use Carp;
use utf8;
our @ISA = qw(MUDL::Token);

##======================================================================
## Default object structure
##  + hashref { _enums=>\%attr2enum, text=>$text_id, tag=>$tag_id, %attributes2ids }

## $tok = $class->new(_enums=>\%attr2enum, text=>$text, tag=>$tag, %attributes);
##   + _enums is REQUIRED
#sub new { return bless { @_[1..$#_] }, ref($_[0])||$_[0]; }
sub new {
  my ($that,%args) = @_;
  return bless {
		_enums=>$args{_enums},
		map { ($_=>$args{_enums}{$_}->addSymbol($args{$_})) } grep { $_ ne '_enums' } keys(%args)
	       }, ref($that)||$that;
}

## $etok = $class_or_obj->newFomToken($tok, %argsToNew)
##  + alwys creates a new EToken
sub newFromToken { return $_[0]->fromToken(@_[1..$#_]); }

## $etok = $class_or_obj->fromToken($tok,%argsToNew)
##  + just returns $tok if its compatible
sub fromToken {
  my ($etok,$tok,%args) = @_;
  if (!ref($etok)) {
    if (UNIVERSAL::isa($tok,__PACKAGE__)) {
      ##-- special case: class method called on compatible EToken object as source: NO COPY!
      return $tok
	if ( (!defined($args{_enums}) && defined($tok->{_enums}))
	     ||
	     ( defined($args{_enums}  && defined($tok->{_enums}) && $args{_enums} eq $tok->{_enums})) );
    }
    $etok = $etok->new(%args);
  }
  if (UNIVERSAL::isa($tok,__PACKAGE__)) {
    ##-- special case: object method called on compatible EToken object as source: just copy
    @$etok{keys %$tok} = values(%$tok)
      if ( (!defined($etok->{_enums}) && !defined($tok->{_enums}))
	   || (defined($etok->{_enums}) && defined($tok->{_enums}) && $etok->{_enums} eq $tok->{_enums}));
  } else {
    ##-- general case: class or object method called on any MUDL::Token source object: general typecast
    $etok->{_enums}={} if (!defined($etok->{_enums}));
    my $srcHash = $tok->asHash;
    my ($key,$val,$enum,$vali);
    while (($key,$val)=each(%$srcHash)) {
      next if (!defined($val));
      $enum=$etok->{_enums}{$key} = MUDL::Enum->new() if (!defined($enum=$etok->{_enums}{$key}));
      ##
      #$etok->{$key} = $etok->{_enums}{$key}->addSymbol($val);
      if (!defined($vali=$enum->{sym2id}{$val})) {
	$vali = $enum->{sym2id}{$val} = scalar(@{$enum->{id2sym}});
	push(@{$enum->{id2sym}},$val);
      }
      $etok->{$key} = $vali;
    }
  }
  return $etok;
}


##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text {
  return defined($_[0]{text}) ? $_[0]{_enums}{text}{id2sym}[ $_[0]{text} ] : undef if ($#_ < 1);
  #$_[0]{text} = $_[0]{_enums}{text}->addSymbol($_[1]);
  #return $_[1];
  ##--
  my $enum = $_[0]{_enums}{text};
  my ($id);
  if (!defined($id=$enum->{sym2id}{$_[1]})) {
    $id = $enum->{sym2id}{$_[1]} = scalar(@{$enum->{id2sym}});
    push(@{$enum->{id2sym}},$_[1]);
  }
  $_[0]{text}=$id;
  return $_[1];
}

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag {
  return defined($_[0]{tag}) ? $_[0]{_enums}{tag}{id2sym}[ $_[0]{tag} ] : undef if ($#_ < 1);
  #$_[0]{tag} = $_[0]{enums}{tag}->addSymbol($_[1]);
  #return $_[1];
  ##--
  my $enum = $_[0]{_enums}{tag};
  my ($id);
  if (!defined($id=$enum->{sym2id}{$_[1]})) {
    $id = $enum->{sym2id}{$_[1]} = scalar(@{$enum->{id2sym}});
    push(@{$enum->{id2sym}},$_[1]);
  }
  $_[0]{tag}=$id;
  return $_[1];
}

## @attributeNames = $tok->attributeNames()
our %skipAttributeNames = map { ($_=>undef) } qw(text tag _enums);
sub attributeNames { return grep { !exists($skipAttributeNames{$_}) } keys(%{$_[0]}); }

## @attributeValues = $tok->attributeValues
sub attributeValues {
  return
    map { $_[0]{_enums}{$_}{id2sym}[ $_[0]{$_} ] }
    grep { !exists($skipAttributeNames{$_}) } keys(%{$_[0]});
}

## %attributes = $tok->attributes
sub attributes {
  return
    map { $_=>$_[0]{_enums}{$_}{id2sym}[ $_[0]{$_} ] } 
    grep { !exists($skipAttributeNames{$_}) } keys(%{$_[0]});
}

##  %tokHash = $tok->asHash #-- list context
## \%tokHash = $tok->asHash #-- scalar context, may be dangerous to change
##  + like (text=>$tok->text, tag=>$tok->tag, $tok->attributes());
sub asHash {
  return (wantarray
	  ? (map { ($_=>$_[0]{_enums}{$_}{id2sym}[ $_[0]{$_} ]) } grep {$_ ne '_enums'} keys(%{$_[0]}))
	  : {map { ($_=>$_[0]{_enums}{$_}{id2sym}[ $_[0]{$_} ]) } grep {$_ ne '_enums'} keys(%{$_[0]})});
}

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return defined($_[0]{$_[1]}) ? $_[0]{_enums}{$_[1]}{id2sym}[ $_[0]{$_[1]} ] : undef if ($#_ <= 1);
  #$_[0]{$_[1]} = $_[0]{_enums}{$_[1]}->addSymbol($_[2]);
  #return $_[2];
  ##--
  my $enum = $_[0]{_enums}{$_[1]};
  my ($id);
  if (!defined($id=$enum->{sym2id}{$_[2]})) {
    $id = $enum->{sym2id}{$_[2]} = scalar(@{$enum->{id2sym}});
    push(@{$enum->{id2sym}},$_[2]);
  }
  $_[0]{$_[1]}=$id;
  return $_[2];
}

##======================================================================
## I/O: Native

## $str = $tok->saveNativeStr()
##  + only saves 'text', 'tag', and numerically keyed attributes
*saveTTString = \&saveNativeString;
sub saveNativeString {
  return join("\t",
	      map {
		(defined($_[0]{$_})
		 ? $_[0]{_enums}{$_}{id2sym}[ $_[0]{$_} ]
		 : qw())
	      }
	      (qw(text tag), sort { $a <=> $b } grep { /^\d+$/ } keys(%{$_[0]}))
	     )."\n";
}

# $tok = $tok->loadNativeStr($str)
*loadTTString = \&loadNativeString;
sub loadNativeString {
  my ($tok,$str) = @_;
  $tok = $tok->new() if (!ref($tok));
  my @fields = split(/\s*[\t\r\n]+\s*/,$str);
  foreach (qw(text tag), 0..($#fields-2)) {
    $tok->{_enums}[$_] = MUDL::Enum->new if (!defined($tok->{_enums}{$_}))
  }
  $tok->{text} = $tok->{_enums}{text}->addSymbol(shift(@fields)) if (@fields);
  $tok->{tag}  = $tok->{_enums}{tag} ->addSymbol(shift(@fields)) if (@fields);
  @$tok{0..$#fields} = map { $tok->{_enums}{$_}->addSymbol($fields[$_]) } 0..$#fields;
  return $tok;
}


##======================================================================
## I/O: LOB | Brown
# not implemented


##======================================================================
## I/O: XML
# not implemented


########################################################################
## class MUDL::EToken::TT
##  + array-based enumerated token class
########################################################################

package MUDL::EToken::TT;
use MUDL::Enum;
use Carp;
use utf8;
our @ISA = qw(MUDL::Token::TT MUDL::EToken);

##======================================================================
## Default object structure
##  + arrayref [ \@enums, $text, $tag, @details ]

## $tok = $class->new(text=>$text, tag=>$tag, _enums=>\@enums, %attributes);
sub new {
  my ($that,%args) = @_;
  my $tok = bless [
		   _enums=>$args{_enums},
		   (defined($args{text}) ? $args{_enums}{text}->addSymbol($args{text}) : undef),
		   (defined($args{tag})  ? $args{_enums}{tag} ->addSymbol($args{tag})  : undef),
		  ], ref($that)||$that;
  $tok->[$_+3] = $tok->[0][$_+2]{sym2id}{$args{$_}} foreach (sort { $a<=>$b } grep { /^\d+$/ } keys(%args));
  return $tok;
}


## $etok = $class_or_obj->newFomToken($tok, %argsToNew)
##  + always creates a new token
# (inherited)

## $etok = $class_or_obj->fromToken($tok,%args)
sub fromToken {
  my ($etok,$tok,%args) = @_;
  if (!ref($etok)) {
    if (UNIVERSAL::isa($tok,__PACKAGE__)) {
      ##-- special case: class method called on compatible EToken::TT source object: NO COPY!
      return $tok
	if ( (!defined($args{_enums}) && defined($tok->[0]))
	     ||
	     ( defined($args{_enums}  && defined($tok->[0]) && $args{_enums} eq $tok->[0])) );
    }
    $etok = $etok->new(%args); ##-- need a new token
  }
  if (UNIVERSAL::isa($tok,__PACKAGE__)) {
    ##-- special case: object method called on compatible EToken::TT source object: just copy
    @$etok[1..$#$tok]=@$tok[1..$#$tok]
      if ( (!defined($etok->[0]) && !defined($tok->[0]))
	   || (defined($etok->[0]) && defined($tok->[0]) && $etok->[0] eq $tok->[0]));
  } else {
    ##-- general case: class or object method called on any MUDL::Token source object: general typecast
    $etok->[0]=[] if (!defined($etok->[0]));
    my $srcArray = $tok->asArray;
    my ($i,$enum,$val,$vali);
    foreach $i (0..$#$srcArray) {
      next if (!defined($val=$srcArray->[$i]));
      $enum = $etok->[0][$i] = MUDL::Enum->new() if (!defined($enum=$etok->[0][$i]));
      ##
      #$etok->[$i+1]  = $etok->[0][$i]->addSymbol($val);
      if (!defined($vali=$enum->{sym2id}{$val})) {
	$vali = $enum->{sym2id}{$val} = scalar(@{$enum->{id2sym}});
	push(@{$enum->{id2sym}},$val);
      }
      $etok->[$i+1] = $vali;
    }
  }
  return $etok;
}


##======================================================================
## Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text {
  return defined($_[0][1]) ? $_[0][0][0]{id2sym}[$_[0][1]] : undef if ($#_ < 1);
  #$_[0][1] = $_[0][0][0]->addSymbol($_[1]);
  #return $_[1]
  ##--
  my ($id);
  if (!defined($id=$_[0][0][0]{sym2id}{$_[1]})) {
    $id = $_[0][0][0]{sym2id}{$_[1]} = scalar(@{$_[0][0][0]{id2sym}});
    push(@{$_[0][0][0]{id2sym}},$id);
  }
  $_[0][1] = $id;
  return $_[1];
}

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag {
  return defined($_[0][2]) ? $_[0][0][1]{id2sym}[ $_[0][2] ] : undef if ($#_ < 1);
  #$_[0][2] = $_[0][0][1]->addSymbol($_[1]);
  #return $_[1]
  ##--
  my ($id);
  if (!defined($id=$_[0][0][1]{sym2id}{$_[1]})) {
    $id = $_[0][0][1]{sym2id}{$_[1]} = scalar(@{$_[0][0][1]{id2sym}});
    push(@{$_[0][0][1]{id2sym}},$id);
  }
  $_[0][2] = $id;
  return $_[1];
}

## %attributes = $tok->attributes
sub attributes {
  return map { ($_=>$_[0][0][$_+2]{id2sym}[$_[0][$_+3]]) } 0..($#{$_[0]}-3);
}

## @attributeNames = $tok->attributeNames()
sub attributeNames { return 0..($#{$_[0]}-3); }

## @attributeValues = $tok->attributeValues
sub attributeValues {
  return map { $_[0][0][$_+2]{id2sym}[$_[0][$_+3]] } 0..($#{$_[0]}-3);
}

##  %tokHash = $tok->asHash #-- list context
## \%tokHash = $tok->asHash #-- scalar context, may be dangerous to change
##  + like (text=>$tok->text, tag=>$tok->tag, $tok->attributes());
sub asHash {
  my $h = {};
  $h->{text} = $_[0][0][0]{id2sym}[ $_[0][1] ] if (defined($_[0][1]));
  $h->{tag}  = $_[0][0][1]{id2sym}[ $_[0][2] ] if (defined($_[0][2]));
  @$h{0..($#{$_[0]}-3)} = (map { defined($_[0][$_+1]) ? $_[0][0][$_]{id2sym}[ $_[0][$_+1] ] : undef }
			  3..($#{$_[0]-1}));
  return wantarray ? %$h : $h;
}

## @tokArray = $tok->asArray(\@attrNames) #-- list context
## %tokArray = $tok->asArray(\@attrNames) #-- scalar context
##  + if @attrNames is unspecified, it defaults to all numeric attribute names
##  + like @ary[0,1,@attrNames]=($tok->text, $tok->tag, @attrNames)
sub asArray {
  return (wantarray
	  ? (map { $_[0][0][$_]{id2sym}[ $_[0][$_+1] ] } (0..$#{$_[0]-1}))
	  : [map { $_[0][0][$_]{id2sym}[ $_[0][$_+1] ] } (0..$#{$_[0]-1})]);
}


## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return $_[0]->text($#_ > 1 ? $_[2] : qw()) if ($_[1] eq 'text');
  return $_[0]->tag ($#_ > 1 ? $_[2] : qw()) if ($_[1] eq 'tag');
  my $i = int($_[1])+2;
  return $_[0][0][$i]{id2sym}[ $_[0][$i+1] ] if ($#_ <= 1);
  #$_[0][$i+1]=$_[0][0][$i]->addSymbol($_[2]);
  #return $_[2];
  ##--
  my ($id);
  if (!defined($id=$_[0][0][$i]{sym2id}{$_[2]})) {
    $id = $_[0][0][$i]{sym2id}{$_[2]} = scalar(@{$_[0][0][$i]{id2sym}});
    push(@{$_[0][0][$i]{id2sym}},$id);
  }
  $_[0][$i+1]=$id;
  return $_[2];
}

##======================================================================
## I/O: Native

## $str = $tok->saveNativeStr()
##  + only saves 'text', 'tag', and numerically keyed attributes
*saveTTString = \&saveNativeString;
sub saveNativeString {
  return join("\t",
	      map {
		$_[0][0][$_]{id2sym}[ $_[0][$_+1] ]
	      } 0..($#{$_[0]}-1))."\n"
}

# $tok = $tok->loadNativeStr($str)
*loadTTString = \&loadNativeString;
sub loadNativeString {
  #my ($tok,$str) = @_;
  return $_[0]->fromToken(MUDL::Token::TT->loadNativeString($_[1]));
}

##======================================================================
## I/O: LOB

# (not implemented)

##======================================================================
## I/O: XML

# (not implemented)



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
