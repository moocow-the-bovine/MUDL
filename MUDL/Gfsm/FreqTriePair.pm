##-*- Mode: CPerl -*-

## File: MUDL::Gfsm::FreqTriePair.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: (prefix-, suffix-) trie pair
##    using Gfsm
##
##  --> OBSOLETE: use MUDL::Trie::FreqPair instead <--
##
##======================================================================

package MUDL::Gfsm::FreqTriePair;
use MUDL::Gfsm::FreqTrie qw(:all);

use Carp qw(carp croak);
use strict;
our @ISA = qw(MUDL::Gfsm::FreqTrie);

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
##       ##-- encoding
##       encoding=>$encoding,         ##-- for labels2chars(), labels2strings()
##
##   + data:
##       pta=>$pta,                   ##-- a MUDL::Gfsm::FreqTrie
##       sta=>$sta,                   ##-- a MUDL::Gfsm::FreqTrie
##
##       ##-- Equivalence maps: 2^Q_{PTA} <-> 2^Q_{STA}
##       pairs=>\%pairs,            ##-- (pack('LL',$qpta,$qsta)=>undef,...)
##       ##
##       ##-- Using pack() + unpack()
##       #p2s=>\@p2s,                ##-- [$qid_pta] => pack('L*', sort {$a<=>$b} @q_sta)
##       #s2p=>\@s2p,                ##-- [$qid_sta] => pack('L*', sort {$a<=>$b} @q_pta)
##       ##
##       ##-- Using split() + join()
##       ##   ~ use this if pack+unpack doesn't work (had some weird 512-item limit at one point)
##       #p2s=>\@p2s,                ##-- [$qid_pta] => join(' ', sort {$a<=>$b} @q_sta)
##       #s2p=>\@s2p,                ##-- [$qid_sta] => join(' ', sort {$a<=>$b} @q_pta)
sub new {
  my ($that,%args) = @_;
  my $tp = bless {
		  ##-- add_path() flags:
		  add_to_arcs=>1,
		  add_to_state_final=>0,
		  add_to_path_final=>1,
		  encoding=>undef,

		  ##-- relation data
		  pairs=>{},
		  p2depth=>[],
		  s2depth=>[],
		  p2s=>[],
		  s2p=>[],

		  ##-- user args
		  %args,
		 }, ref($that)||$that;

  ##-- alphabet
  $tp->{abet} = MUDL::Gfsm::Alphabet->new if (!$tp->{abet});

  ##-- pta, sta
  my %subargs = map { $_=>$tp->{$_} }
    qw(add_to_arcs add_to_state_final add_to_path_final encoding);

  $tp->{pta} = MUDL::Gfsm::FreqTrie->new(abet=>$tp->{abet},reversed=>0) if (!$tp->{pta});
  $tp->{sta} = MUDL::Gfsm::FreqTrie->new(abet=>$tp->{abet},reversed=>1) if (!$tp->{sta});
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
## Methods: Indexing
##======================================================================

## $tp = $tp->reverseIndex()
##  + creates reverse-lookup FSMs in $tp->{pta}, $tp->{sta}
sub reverseIndex {
  my $tp = shift;
  $tp->{pta}->reverseIndex;
  $tp->{sta}->reverseIndex;
  return $tp;
}

## $tp = $tp->index()
##  + constructs id-relations $tp->{p2s}, $tp->{s2p} from $tp->{pairs}
*index = \&index_packed;
sub index_packed {
  my $tp = shift;

  my $pairs = $tp->{pairs};
  my $p2s = $tp->{p2s};
  my $s2p = $tp->{s2p};

  ##-- build maps
  @$p2s = qw();
  @$s2p = qw();
  my ($pair,$qpta,$qsta);

  foreach $pair
    (sort keys %$pairs)
    #(     keys %$pairs)
  {
    ($qpta,$qsta) = unpack('LL', $pair);
    $p2s->[$qpta] .= pack('L',$qsta);
    $s2p->[$qsta] .= pack('L',$qpta);
  }

  ##-- sort map values
  $_ = pack('L*', sort {$a<=>$b} unpack('L*',$_)) foreach (@$p2s);
  $_ = pack('L*', sort {$a<=>$b} unpack('L*',$_)) foreach (@$s2p);

  ##-- sort tries
  $tp->{pta}{fsm}->arcsort(Gfsm::ASMLower);
  $tp->{sta}{fsm}->arcsort(Gfsm::ASMLower);

  return $tp;
}

## $tp = $tp->index_joined()
##  + version of index() using join() and split() : OBSOLETE
sub index_joined {
  my $tp = shift;

  my $pairs = $tp->{pairs};
  my $p2s = $tp->{p2s};
  my $s2p = $tp->{s2p};

  ##-- build maps
  @$p2s = qw();
  @$s2p = qw();
  my ($pair,$qpta,$qsta);

  foreach $pair
    (sort keys %$pairs)
    #(     keys %$pairs)
  {
    ($qpta,$qsta) = unpack('LL', $pair);
    $p2s->[$qpta] .= $qsta.' ';
    $s2p->[$qsta] .= $qpta.' ';
  }

  ##-- sort map values
  $_ = join(' ', sort {$a<=>$b} split(/ /,$_)) foreach (@$p2s);
  $_ = join(' ', sort {$a<=>$b} split(/ /,$_)) foreach (@$s2p);

  ##-- sort tries
  $tp->{pta}{fsm}->arcsort(Gfsm::ASMLower);
  $tp->{sta}{fsm}->arcsort(Gfsm::ASMLower);

  return $tp;
}

##======================================================================
## Methods: Manipulation
##======================================================================

## $tp = $tp->addPathLabels(\@labels)
## $tp = $tp->addPathLabels(\@labels,$freq)
##  + adds $freq to path \@labels
##  + $freq defaults to 1
sub addPathLabels {
  my ($tp,$labs,$freq) = @_;
  $freq = 1 if (!defined($freq));

  my @add_path_args = @$tp{qw(add_to_arcs add_to_state_final add_to_path_final)};
  my $qids_pta = $tp->{pta}{fsm}->add_path_states($labs,            [], $freq, @add_path_args);
  my $qids_sta = $tp->{sta}{fsm}->add_path_states([reverse @$labs], [], $freq, @add_path_args);

  ##-- save equivalence relation & depth
  foreach (0..$#{$qids_pta}) {
    $tp->{pairs}{pack('LL', $qids_pta->[$_], $qids_sta->[$#$qids_sta-$_])} = undef;
    $tp->{p2depth}[$qids_pta->[$_]]
      = $tp->{s2depth}[$qids_sta->[$#$qids_sta-$_]]
	= $_;
  }

  return $tp;
}

## $tp = $tp->addPath{Strings|Chars|Vector}(...)
##  + inherited from MUDL::Gfsm::FreqTrie

##======================================================================
## Methods: Lookup: PTA<->STA Equivalence
##======================================================================

## @sta_ids = $tp->pta2sta($pta_id)
##   + requires: indexed pairs
sub pta2sta { return unpack('L*', $_[0]{p2s}[$_[1]]); }

## @pta_ids = $tp->sta2pta($sta_id)
##   + requires: indexed pairs
sub sta2pta { return unpack('L*', $_[0]{s2p}[$_[1]]); }

## $depth = $tp->ptadepth($pta_id)
sub ptadepth { return $_[0]{p2depth}[$_[1]]; }

## $depth = $tp->stadepth($sta_id)
sub stadepth { return $_[0]{s2depth}[$_[1]]; }

##======================================================================
## Methods: Lookup: Suffixes, Prefixes
##======================================================================

## \@suffixes_THING = $trie->suffixesTHING($THING_pta)
sub suffixesState   { return $_[0]{pta}->suffixesState($_[1]); }
sub suffixesLabels  { return $_[0]{pta}->suffixesLabels($_[1]); }
sub suffixesStrings { return $_[0]{pta}->suffixesStrings($_[1]); }
sub suffixesChars   { return $_[0]{pta}->suffixesChars($_[1]); }
sub suffixesVector  { return $_[0]{pta}->suffixesVector($_[1]); }

## \@prefixes_THING = $trie->prefixesTHING($THING_sta)
sub prefixesState   { return $_[0]{sta}->suffixesState($_[1]); }
sub prefixesLabels  { return $_[0]{sta}->suffixesLabels($_[1]); }
sub prefixesStrings { return $_[0]{sta}->suffixesStrings($_[1]); }
sub prefixesChars   { return $_[0]{sta}->suffixesChars($_[1]); }
sub prefixesVector  { return $_[0]{sta}->suffixesVector($_[1]); }

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
