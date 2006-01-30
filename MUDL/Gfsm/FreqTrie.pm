##-*- Mode: CPerl -*-

## File: MUDL::Gfsm::FreqTrie
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: frequency tries using libgfsm
##======================================================================

package MUDL::Gfsm::FreqTrie;
use MUDL::Gfsm::Automaton;
use MUDL::Gfsm::Alphabet;
use MUDL::Object;

use Carp qw(carp croak);

use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constants & class methods

## $bool = $class_or_obj->reverseDefault()
##  + true if this class handles suffixes and not prefixes
sub reverseDefault { return 0; }

## $str = $class_or_obj->epsilonString()
##  + default epsilon string
sub epsilonString { return '<epsilon>'; }

##======================================================================
## Constructors

## $obj = $class_or_obj->new(%args)
##   + args:
##       fsm=>$mudl_gfsm_automaton,
##       abet=>$mudl_gfsm_alphabet,
##       reversed=>$bool
sub new {
  my ($that,%args) = @_;
  my $trie = bless {
		fsm=>MUDL::Gfsm::Automaton->newTrie(0, Gfsm::SRTReal), ##-- acceptor with Real semiring
		abet=>MUDL::Gfsm::Alphabet->new(),
		reversed=>$that->reverseDefault(),
		%args,
	       }, ref($that)||$that;

  ##-- structure stuff
  $trie->ensureEpsilon;

  return $trie;
}

## $trie = $trie->clear()
sub clear {
  my $trie = shift;
  $trie->{fsm}->clear;
  $trie->{abet}->clear;
  $trie->ensureEpsilon;
  return $trie;
}

##======================================================================
## Methods: Lookup : Vectors <-> Labels <-> Strings
##======================================================================

##--------------------------------------------------------------
## Methods: Lookup : Labels -> Vectors

## $vec = $trie->labels2vector(\@labs)
sub labels2vector { return pack('S*',@{$_[1]}); }

## $labs = $trie->vector2labels($vec)
sub vector2labels { return [unpack('S*',$_[1])]; }

##--------------------------------------------------------------
## Methods: Lookup : Strings -> Labels


## $labels = $trie->strings2labels(\@strings_or_tokens)
## $labels = $trie->strings2labels(\@strings_or_tokens,$autocreate)
##   + ensures labels exist for each text string in @strings_or_tokens
sub strings2labels {
  my ($trie,$strings,$autocreate) = @_;
  my $abet = $trie->{abet};
  my $labs = ($autocreate
	      ? [ map { $abet->get_label  (ref($_) ? $_->text : $_) } @$strings ]
	      : [ map { $abet->find_label (ref($_) ? $_->text : $_) } @$strings ]);

  ##-- sanity check
  croak(ref($trie),"::strings2labels(): Error: label overflow ")
    if ($autocreate && $abet->size >= $Gfsm::noLabel);

  return $labs;
}

## $vec = $trie->strings2vector(\@strings_or_tokens)
## $vec = $trie->strings2vector(\@strings_or_tokens,$autocreate)
sub strings2vector { return $_[0]->labels2vector($_[0]->strings2labels(@_[1..$#_])); }


## $labels = $trie->chars2labels($word_or_token)
## $labels = $trie->chars2labels($word_or_token,$autocreate)
##   + ensures labels exist for each text character in $word_or_token
sub chars2labels {
  my ($trie,$word,$autocreate) = @_;
  my $abet = $trie->{abet};
  my $labs = ($autocreate
	      ? [ map { $abet->get_label ($_) } split(//, ref($word) ? $word->text : $word) ]
	      : [ map { $abet->find_label($_) } split(//, ref($word) ? $word->text : $word) ]);

  ##-- sanity check
  croak(ref($trie),"::strings2labels(): Error: label overflow ")
    if ($autocreate && $abet->size >= $Gfsm::noLabel);

  return $labs;
}

## $vec = $trie->chars2vector($word_or_token)
## $vec = $trie->chars2vector($word_or_token,$autocreate)
sub chars2vector { return $_[0]->labels2vector($_[0]->chars2labels(@_[1..$#_])); }

##--------------------------------------------------------------
## Methods: Lookup : Labels -> Strings

## $strings = $trie->labels2strings(\@labels)
sub labels2strings {
  my ($trie,$labels) = @_;
  my $abet = $trie->{abet};
  return [ map { $abet->find_key($_) } @$labels ];
}

## $strings = $trie->vector2strings($vec)
sub vector2strings { return $_[0]->labels2strings($_[0]->vector2labels($_[1])); }


## $word = $trie->labels2chars(\@labels)
##   + ensures labels exist for each text character in $word_or_token
##   + uses key($Gfsm::epsilon) for undefined labels
sub labels2chars {
  my ($trie,$labels) = @_;
  my $abet = $trie->{abet};
  return join('',
	      map {
		(defined($_) && $_ != $Gfsm::noLabel
		 ? $abet->find_key($_)
		 : $abet->find_key($Gfsm::epsilon))
	      } @$labels);
}

## $chars = $trie->vector2chars($vec)
sub vector2chars { return $_[0]->labels2chars($_[0]->vector2labels($_[1])); }


##======================================================================
## Methods: Lookup : Frequency
##======================================================================

## $freq = $trie->getFreqLabels(\@labels);
##  + gets stored frequency for \@labels
##  + implicitly reverses \@labels if $trie->{reversed} is true
sub getFreqLabels {
  my ($trie,$labs) = @_;
  return 0 if (grep { !defined($_) || $_ == $Gfsm::noLabel } @$labs);
  my $qid = $trie->{fsm}->add_paths(($trie->{reversed} ? [reverse(@$labs)] : $labs), [], 0);
  return 0 if ($qid==$Gfsm::noState);
  return $trie->{fsm}->final_weight($qid);
}

## $freq = $trie->getFreqStrings(\@strings);
##  + gets stored frequency for \@strings
sub getFreqStrings {
  #my ($trie,$strings) = @_;
  return $_[0]->getFreqLabels($_[0]->strings2labels($_[1],0));
}

## $freq = $trie->getFreqChars($word);
##  + gets stored frequency for $word
sub getFreqChars {
  #my ($trie,$word) = @_;
  return $_[0]->getFreqLabels($_[0]->chars2labels($_[1],0));
}


##======================================================================
## Methods: Manipulation
##======================================================================

## undef = $trie->ensureEpsilon()
##  + ensures that $Gfsm::epsilon maps to $trie->epsilonString()
sub ensureEpsilon {
  my $trie = shift;
  $trie->{abet}->insert($trie->epsilonString, $Gfsm::epsilon);
}

## $trie = $trie->addPathLabels(\@labels)
## $trie = $trie->addPathLabels(\@labels,$freq)
##  + adds $freq to path \@labels
##  + $freq defaults to 1
##  + implicitly reverses \@labels if $trie->{reversed} is true
sub addPathLabels {
  my ($trie,$labs,$freq) = @_;
  $freq = 1 if (!defined($freq));
  $trie->{fsm}->add_paths(($trie->{reversed} ? [reverse(@$labs)] : $labs), [], 1);
  return $trie;
}

## $trie = $trie->addPathStrings(\@strings_or_tokens)
## $trie = $trie->addPathStrings(\@strings_or_tokens,$freq)
##  + ensures that all text strings in \@strings_or_tokens are mapped to labels
##  + calls addPathLabels(\@labels)
*addPathTokens = *addPathSentence = \&addPathStrings;
sub addPathStrings {
  #my ($trie,$strings,$freq) = @_;
  #return $trie->addPathLabels($trie->strings2labels($strings,1), $freq);
  return $_[0]->addPathLabels($_[0]->strings2labels($_[1],1), $_[2]);
}

## $trie = $trie->addPathChars($word)
## $trie = $trie->addPathChars($word,$freq)
##  + ensures that all characters in $word are mapped to labels
##  + calls addPathLabels(\@labels)
sub addPathChars {
  #my ($trie,$word,$freq) = @_;
  #return $trie->addPathLabels($trie->chars2labels($word,1), $freq);
  return $_[0]->addPathLabels($_[0]->chars2labels($_[1],1), $_[2]);
}

##======================================================================
## Methods: Traversal
##======================================================================


## undef = $trie->traverse(\&sub)
##  + calls &sub(\@prefix,$qid) for each state in trie
##  + stops if the call to &sub() returns a false value
sub traverse {
  my ($trie,$sub) = @_;
  my $fsm = $trie->{fsm};
  my $abet = $trie->{abet};
  my @queue = ([],$fsm->root);
  my ($prefix,$qid,$ai);
  while (@queue) {
    ($prefix,$qid) = splice(@queue,0,2);
    last if (!&$sub($prefix,$qid));
    for ($ai = Gfsm::ArcIter->new($fsm,$qid); $ai->ok; $ai->next) {
      push(@queue, [@$prefix,$ai->lower], $ai->target);
    }
  }
}

##======================================================================
## Methods: Conversion
##======================================================================

## $hash = $trie->toHash()
## $hash = $trie->toHash(\%h,%opts)
##  %opts: sep=>$symbol_separator_char
*asHash = \&toHash;
sub toHash {
  my ($trie,$h,%opts) = @_;
  $h = {} if (!$h);
  my $sep = defined($opts{sep}) ? $opts{sep} : ' ';
  my $fsm = $trie->{fsm};
  #my ($prefix,$qid);
 $trie->traverse(sub {
		   #($prefix,$qid)=@_;
		   $h->{ join($sep, @{$trie->labels2strings($_[0])}) } = $fsm->final_weight($_[1]);
		   1;
		 });
  return $h;
}

## $hash = $trie->toCharHash()
## $hash = $trie->toCharHash(\%h)
*asCharHash = \&toCharHash;
sub toCharHash {
  my ($trie,$h) = @_;
  return $trie->toHash($h,sep=>'');
}

## $hash = $trie->toVectorHash()
## $hash = $trie->toVectorHash(\%h)
*asVectorHash = *asVecHash = *toVecHash = \&toVectorHash;
sub toVectorHash {
  my ($trie,$h) = @_;
  $h = {} if (!$h);
  my $fsm = $trie->{fsm};
  #my ($prefix,$qid);
  $trie->traverse(sub {
		    #($prefix,$qid)=@_;
		    $h->{$trie->labels2vector($_[0])} = $fsm->final_weight($_[1]);
		    1;
		  });
  return $h;
}

##======================================================================
## I/O: Native
##======================================================================

##-- not implemented

##======================================================================
## I/O: XML
##======================================================================

##-- not implemented

##======================================================================
## Methods: Viewing
##======================================================================

## undef = $trie->viewps(%opts)
##  + view FSM
sub viewps {
  my $trie = shift;
  return $trie->{fsm}->viewps(lower=>$trie->{abet},@_);
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
