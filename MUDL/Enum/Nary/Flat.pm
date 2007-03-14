#-*- Mode: CPerl -*-

## File: MUDL::Enum::Nary::Flat.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: flat enumeration of n-ary data
##======================================================================

package MUDL::Enum::Nary::Flat;
use MUDL::Object;
use MUDL::Enum;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $ne = MUDL::Enum::Nary->new(%args);
##   + %args:
##        enum    => $raw_enum,    # raw enumeration
##        sep     => $sep,         # default="\t"
##        nfields => $n,           # default=0 (arbitrary)
##        badsym  => $bad_symbol,  # default='__BAD__'
##        badidx  => $bad_index,   # default='-1'
sub new {
  my ($that,%args) = @_;
  return $that->SUPER::new(enum=>MUDL::Enum->new(),
			   sep=>"\t",
			   nfields=>0,
			   badsym=>'__BAD__',
			   badidx=>-1,
			   %args);
}

##======================================================================
## Access: General

## $size = $e->size
sub size { return scalar(@{$_[0]{enum}{id2sym}}); }

## @joined_syms = $e->allSymbols
sub allSymbols { return @{$_[0]->{enum}{id2sym}}; }

## @indices = $e->allIndices()
sub allIndices { return values(%{$_[0]->{enum}{sym2id}}); }

## @split_syms = $e->allComponents
sub allComponents { return map { $_[0]->split($_) } @{$_[0]->{enum}{id2sym}}; }

##======================================================================
## Access: Element-wise

## $id = $e->index($symbol)
*id = *sym2id = \&index;
sub index { return $_[0]->{enum}{sym2id}{$_[1]}; }

## $sym = $e->symbol($index)
*sym = *id2sym = \&symbol;
sub symbol { return $_[1] == $_[0]{badidx} ? undef : $_[0]->{enum}{id2sym}[$_[1]]; }


##======================================================================
## Access: List-wise

## $index = $e->lindex(@symbols)
sub lindex {
  return $_[0]->{enum}{sym2id}{join($_[0]{sep}, @_[1..$#_])};
}

## @symbols = $e->split($joined_symbol)
sub split {
  return ($_[0]{nfields}
	  ? CORE::split(/(?:\Q$_[0]->{sep}\E)+/, $_[1], $_[0]{nfields})
	  : CORE::split(/(?:\Q$_[0]->{sep}\E)+/, $_[1]));
}

## $symstr = $e->join(@symbols)
sub join {
  return CORE::join($_[0]{sep}, @_[1..$#_]);
}


## @symbols = $e->lsymbols($index)
sub lsymbols {
  my $syms = $_[1]==$_[0]{badidx} ? undef : $_[0]->{enum}{id2sym}[$_[1]];
  return defined($syms) ? $_[0]->split($syms) : qw();
}

##======================================================================
## Manipulators: General

## undef = $e->clear()
sub clear { $_[0]->{enum}->clear; }

## $e = $e->compact()
##   + renumbers elements, possibly keeping track of changes
##   + TODO: allow user to keep track of changes here
sub compact { $_[0]->{enum}->compact; }


##======================================================================
## Manuipulators: element-wise

## $id = $e->addSymbol($joined_symbol)
*add = \&addSymbol;
sub addSymbol { return $_[0]{enum}->addSymbol(@_[1..$#_]); }

## $id = $e->addIndexedSymbol($joined_symbol, $id)
sub addIndexedSymbol { return $_[0]{enum}->addIndexedSymbol(@_[1..$#_]); }

## $old_id = $e->removeSymbol($joined_symbol)
*rmSymbol = *rmsym = \&removeSymbol;
sub removeSymbol { return $_[0]{enum}->removeSymbol(@_[1..$#_]); }

## $old_sym = $e->removeIndex($i)
sub removeIndex { return $_[0]{enum}->removeIndex(@_[1..$#_]); }


##======================================================================
## Manuipulators: list-wise

## $id = $e->addSymbols(@syms_or_undef)
*adds = \&addSymbols;
sub addSymbols {
  return $_[0]{enum}->addSymbol(CORE::join($_[0]{sep}, @_[1..$#_]));
}

## $id = $e->addIndexedSymbols(@syms,$id)
sub addIndexedSymbols {
  return $_[0]{enum}->addIndexedSymbol(CORE::join($_[0]{sep}, @_[1..($#_-1)]), $_[$#_]);
}

## $old_id = $e->removeSymbols(@syms_or_undef)
*rmSymbols = *rmsyms = \&removeSymbols;
sub removeSymbols {
  return $_[0]{enum}->removeSymbol(CORE::join($_[0]{sep}, @_[1..$#_]));
}


##======================================================================
## I/O : Native

## $e = $e->saveNative($file_or_fh)
sub saveNativeFh {
  my ($e,$fh) = @_;
  $e->{enum}->saveNativeFh($fh);
  return $e;
}

# $e = $e->loadNative($file_or_fh)
sub loadNativeFh {
  my ($e,$fh) = @_;
  $e = $e->new() if (!ref($e));
  $e->{enum} = $e->{enum}->loadNativeFh($fh);
  return $e;
}


##----------------------------------------------------------------------
## AUTOLOAD: pass to {enum}

##-- don't autoload DESTROY
sub DESTROY { ; }

our $AUTOLOAD;
sub AUTOLOAD {
  my $ef = shift;
  return undef if (!defined($ef));
  (my $name = $AUTOLOAD) =~ s/.*:://; ##-- strip qualification

  my ($sub);
  if ($sub=UNIVERSAL::can($ef->{enum}, $name)) {
    return $sub->($ef->{enum},@_);
  }
  croak( ref($ef) , "::$name() not defined in ", __PACKAGE__ , "::AUTOLOAD.\n");
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
