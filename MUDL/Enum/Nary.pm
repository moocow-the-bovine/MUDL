#-*- Mode: Perl -*-

## File: MUDL::Enum::Nary.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: n-ary Enums
##======================================================================

package MUDL::Enum::Nary;
use MUDL::Object;
use MUDL::Enum;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $ne = MUDL::Enum::Nary->new(%args);
##   + %args:
##        enums   => \@enums,
##        sep     => $sep,         # default="\t"
##        nfields => $n,           # default=0 (arbitrary)
##        badsym  => $bad_symbol,  # default='__BAD__'
##        badidx  => $bad_index,   # default='-1'
sub new {
  my ($that,%args) = @_;
  return $_[0]->SUPER::new(enums=>[],
			   sep=>"\t",
			   nfields=>0,
			   badsym=>'__BAD__',
			   badidx=>-1,
			   @_[1..$#_]);
}

##======================================================================
## Access: List-wise

## @ids = $e->indices(@symbols)
*indexes = *ids = *syms2ids = \&indices;
sub indices {
  map {
    (defined($_[$_]) && defined($_[0]{enums}[$_-1])
     ? $_[0]{enums}[$_-1]->index($_[$_])
     : undef)
  } (1..$#_);
}

## @syms = $e->symbols(@indices)
*syms = *ids2syms = \&symbols;
sub symbols {
  map {
    (defined($_[$_]) && defined($_[0]{enums}[$_-1])
     ? $_[0]{enums}[$_-1]->symbol($_[$_])
     : undef)
  } (1..$#_);
}

##======================================================================
## String Utilties

## @vals = $e->split($str)
sub split {
  return ($_[0]{nfields}
	  ? CORE::split(/(?:\Q$_[0]->{sep}\E)+/, $_[1], $_[0]{nfields})
	  : CORE::split(/(?:\Q$_[0]->{sep}\E)+/, $_[1]));
}


##======================================================================
## Access: String-wise

## @ids = $e->index($symbol_str)
*id = *sym2id = \&index;
sub index {
  join($_[0]{sep},
       map {
	 defined($_) ? $_ : $_[0]{badsym}
       } $_[0]->indices($_[0]->split($_[1])));
}

## @syms = $e->symbols($index_str)
*sym = *id2sym = \&symbol;
sub symbol {
  join($_[0]{sep},
       map {
	 defined($_) ? $_ : $_[0]{badidx}
       } $_[0]->symbols($_[0]->split($_[1])));
}

##======================================================================
## Manipulators: General

## undef = $e->clear()
sub clear {
  @{$_[0]{enums}} = qw();
}

## undef = $e->clearEnums()
sub clearEnums {
  $_->clear foreach (grep { defined($_) } @{$_[0]{enums}});
}

## $e = $e->compact()
##   + renumbers elements, possibly keeping track of changes
##   + TODO: allow user to keep track of changes here
sub compact {
  $_->compact foreach (grep { defined($_) } @{$_[0]});
  return $_[0];
}

##======================================================================
## Manipulators: List-Wise

## @ids = $e->addSymbols(@syms_or_undef)
*adds = \&addSymbols;
sub addSymbols {
  map {
    (defined($_[$_])
     ? (defined($_[0]{enums}[$_-1])
	? $_[0]{enums}[$_-1]->addSymbol($_[$_])
	: ($_[0]{enums}[$_-1]=MUDL::Enum->new())->addSymbol($_[$_]))
     : undef)
  } (1..$#_);
}

## @ids = $e->addIndexedSymbols([$sym,$id],...)
sub addIndexedSymbols {
  map {
    (defined($_[$_])
     ? (defined($_[0]{enums}[$_-1])
	? $_[0]{enums}[$_-1]->addIndexedSymbol(@{$_[$_]})
	: ($_[0]{enums}[$_-1]=MUDL::Enum->new())->addIndexedSymbol(@{$_[$_]}))
     : undef)
  } (1..$#_);
}

## @old_ids = $e->removeSymbols(@syms_or_undef)
*rmSymbols = *rmsyms = \&removeSymbols;
sub removeSymbols {
  map {
    (defined($_[$_]) && defined($_[0][$_-1])
     ? $_[0][$_-1]->removeSymbol($_[$_])
     : undef)
  } (1..$#_);
}

## @old_syms = $e->removeIndices(@indices_or_undef)
*rmIndices = *rmIndexes = *rmindices = *rmindexes = *rmids = *rmIds = *removeIds = \&removeIndices;
sub removeIndices {
  map {
    (defined($_[$_]) && defined($_[0]{enums}[$_-1])
     ? $_[0]{enums}[$_-1]->removeIndex($_[$_])
     : undef)
  } (1..$#_);
}


##======================================================================
## Manipulators: String-Wise

## $id_str = $e->addSymbol($sym_str)
*add = \&addSymbol;
sub addSymbol {
  join($_[0]{sep}, $_[0]->addSymbols($_[0]->split($_[1])));
}


##======================================================================
## I/O : Native

# $e = $e->saveNative($file_or_fh)
sub saveNativeFh {
  my ($e,$fh) = @_;
  my ($i,$lab,$sym,$id2sym);
  for ($i=0; $i < @$e; $i++) {
    next if (!defined($e->[$i]));
    $id2sym = $e->[$i]{id2sym};
    for ($lab=0; $lab < @$id2sym; $lab++) {
      next if (!defined($sym=$id2sym->[$lab]));
      $fh->print($i, "\t", $sym, "\t", $lab, "\n");
    }
    $fh->print("\n");
  }
  return $e;
}

# $e = $e->loadNative($file_or_fh)
sub loadNativeFh {
  my ($e,$fh) = @_;
  my ($i,$lab,$sym,$line);
  while (defined($line=<$fh>)) {
    chomp $line;
    next if ($line eq '');
    if ($line !~ /^(\d+)\s+(.*\S)\s+(\d+)$/) {
      warn( __PACKAGE__ , "::loadATT(): parse error at line ", $fh->input_line_number);
      next;
    }
    ($i,$sym,$lab) = ($1,$2,$3);
    $e->[$i] = MUDL::Enum->new() if (!defined($e->[$i]));
    $e->[$i]{sym2id}{$sym} = $lab;
    $e->[$i]{id2sym}[$lab] = $sym;
  }
  return $e;
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
