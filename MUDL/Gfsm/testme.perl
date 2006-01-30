#!/usr/bin/perl -wd

use lib qw(../..);
use Gfsm;
use MUDL::Gfsm::Automaton;
use MUDL::Gfsm::Alphabet;
use MUDL::Gfsm::FreqTrie;

##--------------------------------------------------
BEGIN {
package EAutomaton;
our @ISA = qw(MUDL::Object);

sub new {
  my ($that,%args) = @_;
  return bless {
		fsm=>MUDL::Gfsm::Automaton->new(),
		%args
	       }, ref($that)||$that;
}
}

##--------------------------------------------------
## dummy
package main;
sub dummy {
  foreach $i (0..10) {
    print "--dummy[$i]--\n";
  }
}
dummy;
