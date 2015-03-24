##-*- Mode: CPerl -*-

## File: MUDL::Trie::Freq.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: trie: frequency tries
##======================================================================

package MUDL::Trie::Freq;
use MUDL::Trie::Base;

use Carp qw(carp croak);
use strict;
our @ISA = qw(MUDL::Trie::Base);

##======================================================================
## Constructors

## $obj = $class_or_obj->new(%args)
##
## + user args (inherited from Trie::Base)
##   cw       => $char_width, ##-- character width (default=1)
##   reversed => $bool,       ##-- true iff trie is reversed (suffix acceptor)
##   indexDepth => $bool,     ##-- index state depth on finish()?
##   indexWidth => $bool,     ##-- index state out degree (width) on finish()?
##   indexStrings => $bool,   ##-- index full prefixes on finish()?
##
## + data (new in Trie::Freq): post-finish()
##   q2f   => \@q2f,          ##-- state frequency: [$qid] => $qid_freq,
##
## + data (inherited from Trie::Base)
##   goto  => \@delta,        ##-- [$qid]{$sym} => $qid_to      s.t. $qid --$sym--> $qid_to
##   rgoto => \@rdelta,       ##-- [$qid_to]    => "$qid $sym"  s.t. $qid --$sym--> $qid_to
##   final => \%final,        ##-- {$qid} => undef,             s.t. $qid \in F
##   nq    => $n_states,      ##-- number of states (>= 1)
##   chars => \%chars,        ##-- pseudo-set: $sym => undef
##   depth => \@q2depth,      ##-- [$qid]       => $depth
##
sub new {
  my ($that,%args) = @_;
  return $that->SUPER::new(
			   q2f=>[],
			   %args
			  );
}

## $trie = $trie->clear()
sub clear {
  my $trie = shift;
  @{$trie->{q2f}} = qw();
  return $trie->SUPER::clear();
}

##======================================================================
## Methods: Manipulation
##======================================================================

## $qid  = $trie->addArray(\@chars)  ##-- scalar context
## @qids = $trie->addArray(\@chars)  ##-- array context
##  + adds path for $string; returns ID
*add = \&addString;
sub addString {
  my ($trie,$str,$freq) = @_;
  my @path = $trie->SUPER::addString($str);
  $trie->{q2f}[$path[$#path]] += defined($freq) ? $freq : 1;
  return wantarray ? @path : $path[$#path];
}

## $qid  = $trie->addArray(\@chars)  ##-- scalar context
## @qids = $trie->addArray(\@chars)  ##-- array context
##  + adds path for \@chars, returns id
sub addArray {
  my ($trie,$chars,$freq) = @_;
  my @path = $trie->SUPER::addArray($chars);
  $trie->{q2f}[$path[$#path]] += defined($freq) ? $freq : 1;
  return wantarray ? @path : $path[$#path];
}

##==============================================================================
## Methods: Manipulation: final index
##==============================================================================

## $trie = $trie->finish(%args)
##  + sets prefix frequencies from full-word frequencies
##  + performs indexing if requested
sub finish {
  my $trie = shift;
  $trie->SUPER::finish(@_); ##-- inherited method

  my $q2f = $trie->{q2f};
  my ($qfinal,$f,$qa,$q,$a);
  foreach $qfinal (keys(%{$trie->{final}})) {
    $f = $q2f->[$qfinal];
    for ($qa=$trie->{rgoto}[$qfinal]; defined($qa); $qa=$trie->{rgoto}[$q]) {
      ($q,$a) = CORE::split(/ /, $qa, 2);
      $q2f->[$q] += $f;
    }
  }
  return $trie;
}

##==============================================================================
## Methods: Lookup: String -> Id
##==============================================================================

## $qid_or_undef = $trie->string2id($str)
##  + inherited from Trie::Base

## $qid_or_undef = $trie->array2id(\@chars)
##  + inherited from Trie::Base

##==============================================================================
## Methods: Lookup: Id -> String
##==============================================================================

## $str = $trie->id2string($qid)
##  + inherited from Trie::Base

## \@chars = $trie->id2array($qid)
##  + inherited from Trie::Base

##==============================================================================
## Methods: Lookup: Id -> Path
##==============================================================================

## \@qids = $trie->id2path($qid)
##  + inherited from Trie::Base

## \@states = $trie->string2path($str)
##  + inherited from Trie::Base

## \@states = $trie->array2path(\@syms)
##  + inherited from Trie::Base

##==============================================================================
## Methods: Lookup: (String | Array | Id) -> Frequency
##==============================================================================

## $f = $trie->id2f($qid)
sub id2f {
  my ($f);
  return 0 if (!defined($_[1]) || !defined($f=$_[0]{q2f}[$_[1]]));
  return $f;
}

## $f = $trie->string2f($string)
*str2f = \&string2f;
sub string2f { return $_[0]->id2f($_[0]->string2id($_[1])); }

## $f = $trie->array2f(\@chars)
sub array2f { return $_[0]->id2f($_[0]->array2id($_[1])); }


##======================================================================
## I/O: Native
##======================================================================

##-- not implemented

##======================================================================
## I/O: XML
##======================================================================

##-- not implemented



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
