##-*- Mode: CPerl -*-

## File: MUDL::Gfsm::FreqTrie
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: frequency tries using libgfsm
##
##  --> OBSOLETE: use MUDL::Trie::Freq instead <--
##
##======================================================================

package MUDL::Gfsm::FreqTrie;
use MUDL::Gfsm::Automaton;
use MUDL::Gfsm::Alphabet;
use MUDL::Object;

use Encode qw(encode decode);
use Carp qw(carp croak);

use strict;
our @ISA = qw(Exporter MUDL::Object);

our %EXPORT_TAGS =
  (
   vecutils => [qw(labels2vector vector2labels)],
  );
$EXPORT_TAGS{all} = [map { @$_ } values(%EXPORT_TAGS)];
our @EXPORT_OK = @{$EXPORT_TAGS{all}};
our @EXPORT = qw();

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
##       ##-- add_path() flags:
##       add_to_arcs => $bool,        ##-- add_path(): arcs? (default=1)
##       add_to_state_final => $bool, ##-- add_path(): final-weight for all visited states? (default=0)
##       add_to_path_final => $bool,  ##-- add_path(): final-weight for path-final state?   (default=1)
##
##       ##-- for character extraction
##       encoding=>$encoding_or_undef, ##-- encoding to use for labels2chars() and labels2strings()
sub new {
  my ($that,%args) = @_;
  my $trie = bless {
		    ##-- guts
		    reversed=>$that->reverseDefault(),

		    ##-- add_path() flags:
		    add_to_arcs=>1,
		    add_to_state_final=>0,
		    add_to_path_final=>1,

		    ##-- encoding
		    encoding=>undef,

		    ##-- user args
		    %args,
		   }, ref($that)||$that;

  ##-- fsm
  $trie->{fsm} = MUDL::Gfsm::Automaton->newTrie(0, Gfsm::SRTReal) ##-- acceptor with Real semiring
    if (!$trie->{fsm});

  ##-- rfsm
  #$trie->{rfsm} = $trie->{fsm}->reverse
  #  if (!$trie->{rfsm});

  ##-- alphabet
  $trie->{abet} = MUDL::Gfsm::Alphabet->new()
    if (!$trie->{abet});

  ##-- ensure that epsilon is defined
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
## Methods: reverse indexing, reverse lookup
##======================================================================

## $rfsm = $trie->reverseIndex()
##  + returns a Gfsm::Automaton suitable for reverse-lookup (state-id => path)
##  + only call this once you have added all data to the trie
##  + populates $trie->{rfsm}
sub reverseIndex {
  my $trie = shift;
  return $trie->{rfsm} = $trie->{fsm}->reverse;
}

## \@label_array = $trie->stateLabels($qid)
##   + requires $trie->{rfsm}
sub stateLabels {
  my ($trie,$qid) = @_;
  return undef if (!defined($qid) || $qid == $Gfsm::noState);
  my ($rfsm,$rfsm_root_tmp,$labels);
  $rfsm = $trie->{rfsm};
  $rfsm_root_tmp = $rfsm->root;
  $rfsm->root($qid);
  $labels = $rfsm->paths->[0]{lo};
  @$labels = reverse(@$labels) if (!$trie->{reversed});
  $rfsm->root($rfsm_root_tmp);
  return $labels;
}

## \@string_array = $trie->stateStrings($qid)
sub stateStrings { return $_[0]->labels2strings($_[0]->stateLabels($_[1])); }

## $chars = $trie->stateChars($qid)
sub stateChars { return $_[0]->labels2chars($_[0]->stateLabels($_[1])); }

## $vec = $trie->stateVector($qid)
sub stateVector { return labels2vector($_[0]->stateLabels($_[1])); }


##======================================================================
## Methods: Lookup : Suffixes
##======================================================================

## \@suffix_label_arrays = $trie->suffixesState($qid)
sub suffixesState {
  my ($trie,$qid) = @_;
  return qw() if (!defined($qid));
  my $root_tmp = $trie->{fsm}->root;
  $trie->{fsm}->root($qid);
  my @suffixes = map { $_->{lo} } @{$trie->{fsm}->paths};
  if ($trie->{reversed}) { @$_ = reverse(@$_) foreach (@suffixes); }
  $trie->{fsm}->root($root_tmp);
  return \@suffixes;
}

## \@suffix_label_arrays = $trie->suffixesLabels(\@labels)
sub suffixesLabels { return $_[0]->suffixesState($_[0]->getStateLabels($_[1])); }

## \@suffix_string_arrays = $trie->suffixesStrings(\@strings)
sub suffixesStrings {
  my $suffs = $_[0]->suffixesState($_[0]->getStateStrings($_[1]));
  $_ = $_[0]->labels2string($_) foreach (@$suffs);
  return $suffs;

}

## \@suffix_words = $trie->suffixesChars($chars)
sub suffixesChars {
  my $suffs = $_[0]->suffixesState($_[0]->getStateChars($_[1]));
  $_ = $_[0]->labels2chars($_) foreach (@$suffs);
  return $suffs;
}

## \@suffix_vectors = $trie->suffixesVector($vec)
sub suffixesVector {
  my $suffs = $_[0]->suffixesState($_[0]->getStateVector($_[1]));
  $_ = labels2vector($_) foreach (@$suffs);
  return $suffs;
}


##======================================================================
## Methods: Lookup : Vectors <-> Labels <-> Strings
##======================================================================

##--------------------------------------------------------------
## Methods: Lookup : Labels -> Vectors

## $vec = $trie->labels2vector(\@labs)
##   + no implicit reversal
sub labels2vector { return pack('S*',@{$_[0]}); }

## $labs = $trie->vector2labels($vec)
##   + no implicit reversal
sub vector2labels { return [unpack('S*',$_[0])]; }

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
sub strings2vector { return labels2vector($_[0]->strings2labels(@_[1..$#_])); }


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
sub chars2vector { return labels2vector($_[0]->chars2labels(@_[1..$#_])); }

##--------------------------------------------------------------
## Methods: Lookup : Labels -> Strings

## $strings = $trie->labels2strings(\@labels)
sub labels2strings {
  my ($trie,$labels) = @_;
  my $abet = $trie->{abet};
  return [
	  (defined($trie->{encoding})
	   ? (map { decode($trie->{encoding},$abet->find_key($_)) } @$labels)
	   : (map { $abet->find_key($_) } @$labels))
	 ];
}

## $strings = $trie->vector2strings($vec)
sub vector2strings { return $_[0]->labels2strings(vector2labels($_[1])); }


## $word = $trie->labels2chars(\@labels)
##   + ensures labels exist for each text character in $word_or_token
##   + uses key($Gfsm::epsilon) for undefined labels
sub labels2chars {
  my ($trie,$labels) = @_;
  my $abet = $trie->{abet};
  return (defined($trie->{encoding})
	  ? join('',
		 map {
		   (defined($_) && $_ != $Gfsm::noLabel
		    ? decode($trie->{encoding},$abet->find_key($_))
		    : decode($trie->{encoding},$abet->find_key($Gfsm::epsilon)))
		 } @$labels)
	  : join('',
		 map {
		   (defined($_) && $_ != $Gfsm::noLabel
		    ? $abet->find_key($_)
		    : $abet->find_key($Gfsm::epsilon))
		 } @$labels)
	 );
}

## $chars = $trie->vector2chars($vec)
sub vector2chars { return $_[0]->labels2chars(vector2labels($_[1])); }

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
  my ($qid,$lo_i) = $trie->{fsm}->find_prefix(($trie->{reversed} ? [reverse @$labs] : $labs), []);
  return $lo_i == @$labs ? $qid : undef;
}

## $qid = $trie->getStateStrings(\@strings);
##  + gets state-id for address \@strings
sub getStateStrings {
  return $_[0]->getStateLabels($_[0]->strings2labels($_[1],0));
}

## $qid = $trie->getStateChars($word);
##  + gets state-id for $word
sub getStateChars {
  return $_[0]->getStateLabels($_[0]->chars2labels($_[1],0));
}

## $qid = $trie->getStateVector($vector);
##  + gets state-id for $vector
sub getStateVector {
  return $_[0]->getStateLabels(vector2labels($_[1]));
}

##======================================================================
## Methods: Lookup : State-Path
##======================================================================

## \@qids = $trie->getStatePathLabels(\@labels);
##  + gets state-id path for longest known prefix of address \@labels
##  + implicitly reverses \@labels if $trie->{reversed} is true
##  + returns undef if no state is defined
sub getStatePathLabels {
  my ($trie,$labs) = @_;
  return undef if (grep { !defined($_) || $_ == $Gfsm::noLabel } @$labs);
  return $trie->{fsm}->find_prefix_states(($trie->{reversed} ? [reverse @$labs] : $labs), []);
}

## \@qids = $trie->getStatePathStrings(\@strings);
##  + gets state-id path for longest known prefix of \@strings
sub getStatePathStrings {
  return $_[0]->getStatePathLabels($_[0]->strings2labels($_[1],0));
}

## \@qids = $trie->getStatePathChars($word);
##  + gets state-id path for longest known prefix of $word
sub getStatePathChars {
  return $_[0]->getStatePathLabels($_[0]->chars2labels($_[1],0));
}

## \@qids = $trie->getStatePathVector($vector);
##  + gets state-id path for longest known prefix of $vector
sub getStatePathVector {
  return $_[0]->getStatePathLabels(vector2labels($_[1]));
}


##======================================================================
## Methods: Lookup : Frequency
##======================================================================

##-- gone

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
  $trie->{fsm}->add_path(
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
  return $_[0]->addPathLabels($_[0]->strings2labels($_[1],1), $_[2]);
}

## $trie = $trie->addPathChars($word)
## $trie = $trie->addPathChars($word,$freq)
##  + ensures that all characters in $word are mapped to labels
##  + calls addPathLabels(\@labels)
sub addPathChars {
  return $_[0]->addPathLabels($_[0]->chars2labels($_[1],1), $_[2]);
}

## $trie = $trie->addPathVector($vec)
## $trie = $trie->addPathVector($vec,$freq)
##  + calls addPathLabels(\@labels)
sub addPathVector {
  return $_[0]->addPathLabels(vector2labels($_[1]), $_[2]);
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

## \%hash = $trie->asStaeIdHash()
## \%hash = $trie->asStateIdHash(\%h,%opts)
##  %opts: sep=>$symbol_separator_char
*asStateIdHash = \&toStateIdHash;
sub toStateIdHash {
  my ($trie,$h,%opts) = @_;
  $h = {} if (!$h);
  my $sep = defined($opts{sep}) ? $opts{sep} : ' ';
  my $fsm = $trie->{fsm};
  #my ($prefix,$qid);
 $trie->traverse(sub {
		   #($prefix,$qid)=@_;
		   $h->{ join($sep, @{$trie->labels2strings($_[0])}) } = $_[1];
		   return $TRAVERSE_CONTINUE;
		 });
  return $h;
}

## $hash = $trie->toCharStateIdHash()
## $hash = $trie->toCharStateIdHash(\%h)
*asCharStateIdHash = \&toCharStateIdHash;
sub toCharStateIdHash {
  my ($trie,$h) = @_;
  return $trie->toStateIdHash($h,sep=>'');
}

## $hash = $trie->toVectorStateIdHash()
## $hash = $trie->toVectorStateIdHash(\%h)
*asVectorStateIdHash = *asVecStateIdHash = *toVecStateIdHash = \&toVectorStateIdHash;
sub toVectorStateIdHash {
  my ($trie,$h) = @_;
  $h = {} if (!$h);
  my $fsm = $trie->{fsm};
  #my ($prefix,$qid);
  $trie->traverse(sub {
		    #($prefix,$qid)=@_;
		    $h->{labels2vector($_[0])} = $_[1];
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
