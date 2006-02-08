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
##       reversed=>$bool,             ##-- whether to implicitly reverse path-addresses (arrays,vectors,etc.)
##
##       ##-- add_paths flags:
##       add_to_arcs => $bool,        ##-- add_paths(): arcs? (default=1)
##       add_to_state_final => $bool, ##-- add_paths(): final-weight for all visited states? (default=0)
##       add_to_path_final => $bool,  ##-- add_paths(): final-weight for path-final state?   (default=1)
sub new {
  my ($that,%args) = @_;
  my $trie = bless {
		    ##-- guts
		    fsm=>MUDL::Gfsm::Automaton->newTrie(0, Gfsm::SRTReal), ##-- acceptor with Real semiring
		    abet=>MUDL::Gfsm::Alphabet->new(),
		    #rfsm=>undef, ##-- reverse-lookup fsm
		    reversed=>$that->reverseDefault(),

		    ##-- add_paths() flags:
		    add_to_arcs=>1,
		    add_to_state_final=>0,
		    add_to_path_final=>1,

		    ##-- user args
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
  delete($trie->{rfsm});
  $trie->ensureEpsilon;
  return $trie;
}

##======================================================================
## Methods: trie reversal
##======================================================================

## $trie = $trie->_reverse()
##  + reverses contents of trie: PTA to STA (destructive)
sub _reverse {
  my $trie = shift;
  my $fsm = $trie->{fsm};
  $fsm->_reverse;
  $fsm->_rmepsilon;
  $fsm->_determinize;
  $fsm->_connect;
  $fsm->renumber_states;
  return $trie;
}

## $rtrie = $trie->reverseTrie()
##  + returns copy of trie, reversed: PTA to STA (destructive)
*sta2pta = *pta2sta = \&reverseTrie;
sub reverseTrie {
  my $trie = shift;
  my $rtrie = bless { %$trie }, ref($trie);
  $rtrie->{abet} = $trie->{abet}->clone;
  $rtrie->{fsm}  = $trie->{fsm}->clone;
  $rtrie->{reversed} = !$trie->{reversed};
  $rtrie->_reverse;
  return $rtrie;
}


##======================================================================
## Methods: reverse-indexing
##======================================================================

## $rfsm = $trie->reverseLookupFsm()
##  + returns a Gfsm::Automaton suitable for reverse-lookup (state-id => path)
##  + only call this once you have added all data to the trie
sub reverseLookupFsm {
  my $trie = shift;
  return $trie->{fsm}->reverse;
}

##======================================================================
## Methods: Lookup : Suffixes
##======================================================================

## \@suffix_label_vectors = $trie->suffixesState($qid)
sub suffixesState {
  my ($trie,$qid) = @_;
  return qw() if (!defined($qid));
  my $root_tmp = $trie->{fsm}->root;
  $trie->{fsm}->root($qid);
  my @suffixes = map { $_->{lo} } @{$trie->{fsm}->paths};
  $trie->{fsm}->root($root_tmp);
  return \@suffixes;
}

## \@suffix_label_vectors = $trie->suffixesLabels(\@labels)
sub suffixesLabels {
  my ($trie,$labs) = @_;
  return $trie->suffixesState($trie->getStateLabels($labs));
}

## \@suffix_label_vectors = $trie->suffixesStrings(\@strings)
sub suffixesStrings {
  my ($trie,$strs) = @_;
  return $trie->suffixesState($trie->getStateStrings($strs));
}

## \@suffix_label_vectors = $trie->suffixesChars($chars)
sub suffixesChars {
  my ($trie,$chars) = @_;
  return $trie->suffixesState($trie->getStateChars($chars));
}

## \@suffix_label_vectors = $trie->suffixesVector($vec)
sub suffixesVector {
  my ($trie,$vec) = @_;
  return $trie->suffixesVectors($trie->getStateVector($vec));
}


##======================================================================
## Methods: Lookup : Vectors <-> Labels <-> Strings
##======================================================================

##--------------------------------------------------------------
## Methods: Lookup : Labels -> Vectors

## $vec = $trie->labels2vector(\@labs)
##   + no implicit reversal
sub labels2vector { return pack('S*',@{$_[1]}); }

## $labs = $trie->vector2labels($vec)
##   + no implicit reversal
sub vector2labels { return [unpack('S*',$_[1])]; }

##--------------------------------------------------------------
## Methods: Lookup : Strings -> Labels

## $labels = $trie->strings2labels(\@strings_or_tokens)
## $labels = $trie->strings2labels(\@strings_or_tokens,$autocreate)
##   + ensures labels exist for each text string in @strings_or_tokens
##   + no implicit reversal
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
##   + no implicit reversal
sub strings2vector { return $_[0]->labels2vector($_[0]->strings2labels(@_[1..$#_])); }


## $labels = $trie->chars2labels($word_or_token)
## $labels = $trie->chars2labels($word_or_token,$autocreate)
##   + ensures labels exist for each text character in $word_or_token
##   + no implicit reversal
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
## Methods: Lookup : State-ID
##======================================================================

## $qid = $trie->getStateLabels(\@labels);
##  + gets state-id for address \@labels
##  + implicitly reverses \@labels if $trie->{reversed} is true
##  + returns undef if no state is defined
sub getStateLabels {
  my ($trie,$labs) = @_;
  return undef if (grep { !defined($_) || $_ == $Gfsm::noLabel } @$labs);

  my $fsm = $trie->{fsm};
  my $qid = $fsm->root;
  return undef if ($qid == $Gfsm::noState);

  my ($lab);
  foreach $lab ($trie->{reversed} ? reverse(@$labs) : @$labs) {
    $qid = $fsm->find_arc_lower($qid,$lab);
    return undef if ($qid == $Gfsm::noState);
  }
  return $qid;
}

## $qid = $trie->getStateStrings(\@strings);
##  + gets state-id for address \@strings
sub getStateStrings {
  #my ($trie,$strings) = @_;
  return $_[0]->getStateLabels($_[0]->strings2labels($_[1],0));
}

## $qid = $trie->getStateChars($word);
##  + gets state-id for $word
sub getStateChars {
  #my ($trie,$word) = @_;
  return $_[0]->getStateLabels($_[0]->chars2labels($_[1],0));
}

## $qid = $trie->getStateVector($vector);
##  + gets state-id for $vector
sub getStateVector {
  #my ($trie,$vec) = @_;
  return $_[0]->getStateLabels($_[0]->vector2labels($_[1]));
}


##======================================================================
## Methods: Lookup : Frequency
##======================================================================

## $freq = $trie->getFreqState($id_or_undef)
##  + returns 0 if $qid is invalid
sub getFreqState {
  my ($trie,$qid) = @_;
  return defined($qid) && $qid < $trie->{fsm}->n_states ? $trie->{fsm}->final_weight($qid) : 0;
}

## $freq = $trie->getFreqLabels(\@labels);
##  + gets stored frequency for \@labels
##  + implicitly reverses \@labels if $trie->{reversed} is true
sub getFreqLabels {
  return $_[0]->getFreqState($_[0]->getStateLabels($_[1]));
}

## $freq = $trie->getFreqStrings(\@strings);
##  + gets stored frequency for \@strings
sub getFreqStrings {
  return $_[0]->getFreqState($_[0]->getStateStrings($_[1]));
}

## $freq = $trie->getFreqChars($word);
##  + gets stored frequency for $word
sub getFreqChars {
  return $_[0]->getFreqState($_[0]->getStateChars($_[1]));
}

## $freq = $trie->getFreqVector($vec);
##  + gets stored frequency for vector $vec
sub getFreqVector {
  return $_[0]->getFreqState($_[0]->getStateVector($_[1]));
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
  $trie->{fsm}->add_paths(
			  ($trie->{reversed} ? [reverse(@$labs)] : $labs),
			  [],
			  1,
			  ($trie->{add_to_arcs} ? 1 : 0),
			  ($trie->{add_to_state_final} ? 1 : 0),
			  ($trie->{add_to_path_final} ? 1 : 0),
			 );
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

## $trie = $trie->addPathVector($vec)
## $trie = $trie->addPathVector($vec,$freq)
##  + calls addPathLabels(\@labels)
sub addPathVector {
  #my ($trie,$vec,$freq) = @_;
  #return $trie->addPathLabels($trie->vector2labels($vec), $freq);
  return $_[0]->addPathLabels($_[0]->vector2labels($_[1]), $_[2]);
}

##======================================================================
## Methods: Traversal
##======================================================================

## Return conventions for traversal subs

## $TRAVERSE_STOP
##  + immediately cease traversal
our $TRAVERSE_STOP = 0;

## $TRAVERSE_IGNORE
##  + continue inspecting queued states, but do not enqueue successors
##    of current state for inspection
our $TRAVERSE_IGNORE = 1;

## $TRAVERSE_CONTINUE
##  + continue inspecting queued states, enqueing successors of current state
##  + default
our $TRAVERSE_CONTINUE = 2;

## undef = $trie->traverse(\&sub)
## undef = $trie->traverse(\&sub,$qid_initial)
##  + $qid_initial defaults to $trie->root
##  + calls &sub(\@prefix,$qid) for each state in trie
##  + return conventions for \&sub 
##    - one of $TRAVERSE_STOP,$TRAVERSE_IGNORE, or $TRAVERSE_CONTINUE
sub traverse {
  my ($trie,$sub,$qid) = @_;
  my $fsm = $trie->{fsm};
  my $abet = $trie->{abet};
  my @queue = ([], (defined($qid) ? $qid : $fsm->root));
  my ($prefix,$ai,$rc);
  while (@queue) {
    ($prefix,$qid) = splice(@queue,0,2);
    $rc = &$sub($prefix,$qid);
    last if (!defined($rc) || $rc == $TRAVERSE_STOP   || $rc eq 'stop');
    next if ( defined($rc) && $rc == $TRAVERSE_IGNORE || $rc eq 'ignore');
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
		   return $TRAVERSE_CONTINUE;
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
		   return $TRAVERSE_CONTINUE;
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
