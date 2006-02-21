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
##   + data:
##       pta=>$pta,                   ##-- a MUDL::Gfsm::FreqTrie
##       sta=>$sta,                   ##-- a MUDL::Gfsm::FreqTrie
##
##       pairs=>\%pairs,              ##-- (pack('L2',$qpta,$qsta)=>undef,...)
##
##       ##--------------------------
##       ## Equivalence maps: 2^Q_{PTA} <-> 2^Q_{STA}
##       ##  + we *should* use an id-relation for this, but it's too expensive to construct
##             and to store
##       ##  + otherwise, we should use pack(), but pack-template '*' is limited to 512 items (ack)
##       ##  + so, we use split() and join() instead...
##       p2s=>\@p2s,                ##-- [$qid_pta] => join(' ', sort {$a<=>$b} @q_sta)
##       s2p=>\@s2p,                ##-- [$qid_sta] => join(' ', sort {$a<=>$b} @q_pta)
sub new {
  my ($that,%args) = @_;
  my $tp = bless {
		  ##-- add_path() flags:
		  add_to_arcs=>1,
		  add_to_state_final=>0,
		  add_to_path_final=>1,

		  ##-- relation data
		  pairs=>{},
		  p2s=>[],
		  s2p=>[],

		  ##-- user args
		  %args,
		 }, ref($that)||$that;

  ##-- alphabet
  $tp->{abet} = MUDL::Gfsm::Alphabet->new if (!$tp->{abet});

  ##-- pta, sta
  my %subargs = map { $_=>$tp->{$_} }
    qw(add_to_arcs add_to_state_final add_to_path_final);

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
## Methods: Manipulation
##======================================================================

## $tp = $tp->index()
##  + constructs id-relations $tp->{p2s}, $tp->{s2p} from $tp->{pairs}
sub index {
  my $tp = shift;

  my $pairs = $tp->{pairs};
  my $p2s = $tp->{p2s};
  my $s2p = $tp->{s2p};

  ##-- build maps
  @$p2s = qw();
  @$s2p = qw();
  my ($pair,$qpta,$qsta);
  foreach $pair (sort keys %$pairs) {
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

  ##-- save equivalence relation
  my ($i);
  foreach $i (0..$#{$qids_pta}) {
    $tp->{pairs}{pack('LL', $qids_pta->[$i], $qids_sta->[$#$qids_sta-$i])} = undef;
  }

  return $tp;
}

## $tp = $tp->addPath{Strings|Chars|Vector}(...)
##  + inherited from MUDL::Gfsm::FreqTrie

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
