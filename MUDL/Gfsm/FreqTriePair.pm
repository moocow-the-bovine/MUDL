##-*- Mode: CPerl -*-

## File: MUDL::Gfsm::FreqTriePair.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: prefix-, suffix-trie pair
##======================================================================

package MUDL::Gfsm::FreqTriePair;
use MUDL::Gfsm::FreqTrie qw(:all);

use Carp qw(carp croak);
use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructors

## $obj = $class_or_obj->new(%args)
##   + args:
##       abet=>$abet,                 ##-- alphabet, shared by pta and sta
##
##       ##-- add_path() flags, shared by {pta} and {sta}
##       add_to_arcs => $bool,        ##-- add_path(): arcs? (default=1)
##       add_to_state_final => $bool, ##-- add_path(): final-weight for all visited states? (default=0)
##       add_to_path_final => $bool,  ##-- add_path(): final-weight for path-final state?   (default=1)
##
##   + data:
##       pta=>$pta,                   ##-- a MUDL::Gfsm::FreqTrie
##       sta=>$sta,                   ##-- a MUDL::Gfsm::FreqTrie
sub new {
  my ($that,%args) = @_;
  my $tp = bless {
		  ##-- add_path() flags:
		  add_to_arcs=>1,
		  add_to_state_final=>0,
		  add_to_path_final=>1,

		  ##-- user args
		  %args,
		 }, ref($that)||$that;

  ##-- alphabet
  $tp->{abet} = MUDL::Gfsm::Alphabet->new if (!$tp->{abet});

  ##-- pta, sta
  my %subargs = map { $_=>$tp->{$_} }
    qw(add_to_arcs add_to_state_final add_to_path_final);

  $tp->{pta} = MUDL::Gfsm::FreqTrie->new(abet=>$tp->{abet},reversed=>0) if (!$tp->{pta});
  $tp->{sta} = MUDL::Gfsm::FreqTrie->new(abet=>$tp->{abet},reversed=>0) if (!$tp->{sta});
  $tp->{pta}{keys %subargs} = values(%subargs);
  $tp->{sta}{keys %subargs} = values(%subargs);

  ##-- structure stuff
  $tp->{pta}->ensureEpsilon;

  return $tp;
}

## undef = $tp->ensureEpsilon()
sub ensureEpsilon { $_[0]{pta}->ensureEpsilon; }

## $tp = $tp->clear()
sub clear {
  my $tp = shift;
  $tp->{pta}->clear;
  $tp->{sta}->clear;
  $tp->ensureEpsilon;
  return $tp;
}

##======================================================================
## Methods: Manipulation
##======================================================================

## $tp = $tp->addPathLabels(\@labels)
## $tp = $tp->addPathLabels(\@labels,$freq)
##  + adds $freq to path \@labels
##  + $freq defaults to 1
##  + implicitly reverses \@labels if $tp->{reversed} is true
sub addPathLabels {
  my ($tp,$labs,$freq) = @_;
  $freq = 1 if (!defined($freq));

  my @add_path_args = @$tp{qw(add_to_arcs add_to_state_final add_to_path_final)};
  my $qids_pta = $tp->{pta}{fsm}->add_path($labs,           [], $freq, @add_path_args);
  my $qids_sta = $tp->{sta}{fsm}->add_path([reverse @$labs],[], $freq, @add_path_args);

  ##-- save equivalence relation
  my ($i, $qpta, $qsta);
  foreach $i (0..$#{$qids_pta}) {
    $qpta = $qids_pta->[$i]
    $qsta = $qids_sta->[$#$qids_sta-$i];
    
  }

  return $tp;
}

## $tp = $tp->addPathStrings(\@strings_or_tokens)
## $tp = $tp->addPathStrings(\@strings_or_tokens,$freq)
##  + ensures that all text strings in \@strings_or_tokens are mapped to labels
##  + calls addPathLabels(\@labels)
*addPathTokens = *addPathSentence = \&addPathStrings;
sub addPathStrings {
  #my ($tp,$strings,$freq) = @_;
  #return $tp->addPathLabels($tp->strings2labels($strings,1), $freq);
  return $_[0]->addPathLabels($_[0]->strings2labels($_[1],1), $_[2]);
}

## $tp = $tp->addPathChars($word)
## $tp = $tp->addPathChars($word,$freq)
##  + ensures that all characters in $word are mapped to labels
##  + calls addPathLabels(\@labels)
sub addPathChars {
  #my ($tp,$word,$freq) = @_;
  #return $tp->addPathLabels($tp->chars2labels($word,1), $freq);
  return $_[0]->addPathLabels($_[0]->chars2labels($_[1],1), $_[2]);
}

## $tp = $tp->addPathVector($vec)
## $tp = $tp->addPathVector($vec,$freq)
##  + calls addPathLabels(\@labels)
sub addPathVector {
  #my ($tp,$vec,$freq) = @_;
  #return $tp->addPathLabels($tp->vector2labels($vec), $freq);
  return $_[0]->addPathLabels($_[0]->vector2labels($_[1]), $_[2]);
}

##======================================================================
## Methods: Lookup
##======================================================================


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

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
