##-*- Mode: CPerl -*-

## File: MUDL::Gfsm::StateIdRelation.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: prefix-, suffix-trie pair
##======================================================================

package MUDL::Gfsm::StateIdRelation;
use MUDL::Gfsm::Automaton;

use Carp qw(carp croak);
use strict;
our @ISA = qw(MUDL::Gfsm::Automaton);

##======================================================================
## Constructors
## $obj = $class_or_obj->new(%args)
sub new {
  my $that = shift;
  return bless MUDL::Gfsm::Automaton->newTrie(@_), ref($that)||$that;
}

## undef = $idr->gc()
##  + garbage collection
sub gc { $_[0]->_connect; }

## undef = $idr->sweep()
##  + collects garbage and renumbers states
##  + may invalidate old pair-ids
sub sweep {
  $_[0]->_connect;
  $_[0]->renumber_states;
}

## $idr_inverted = $idr->invert()
##   + $idr_inverted contains ($qid2,$qid1) iff $idr contains ($qid1,$qid2)
sub invert {
  my $idr  = shift;
  my $idr2 = $idr->shadow;
  foreach (@{$idr->paths}) {
    $idr2->add_path($_->{hi},$_->{lo}, 0.0, 0,0,1);
  }
  return $idr2;
}


##======================================================================
## Methods: Manipulation
##======================================================================

## $pair_id = $idr->insert($qid1,$qid2)
sub insert {
  my ($idr,$qid1,$qid2) = @_;
  return $idr->add_path([unpack('S2',pack('L',$qid1))],
			[unpack('S2',pack('L',$qid2))],
			0.0, 0,0,1);
}

## $old_pair_id_or_undef = $idr->remove($qid1,$qid2)
##   + doesn't really remove anything, just marks the state as non-final
sub remove {
  my ($idr,$qid1,$qid2) = @_;
  my $pid = $idr->find($qid1,$qid2);
  $idr->is_final($pid,0) if (defined($pid));
  return $pid;
}

##======================================================================
## Methods: Properties
##======================================================================

## $npairs = $idr->size()
sub size { return $_[0]->n_final_states; }


##======================================================================
## Methods: Lookup
##======================================================================

## $prefix_id = $idr->find1($qid1)
sub find1 {
  my ($idr,$qid1) = @_;
  my @pdata = $idr->find_prefix([unpack('S2',pack('L',$qid1))], []);
  return $pdata[1]==2 ? $pdata[0] : undef;
}

## $pair_id = $idr->find($qid1,$qid2)
sub find {
  my ($idr,$qid1,$qid2) = @_;
  my @pdata = $idr->find_prefix([unpack('S2',pack('L',$qid1))], [unpack('S2',pack('L',$qid2))]);
  return $pdata[1]==2 && $pdata[2]==2 && $idr->is_final($pdata[0]) ? $pdata[0] : undef;
}

## $bool = $idr->contains($qid1,$qid2)
sub contains { return defined($_[0]->find(@_[1,2])); }


##  @qid2s          = $idr->values($qid1); ##-- array context
## \@qid2s_or_undef = $idr->values($qid1); ##-- scalar context
sub values {
  my ($idr,$qid1) = @_;
  my @pdata = $idr->find_prefix([unpack('S2',pack('L',$qid1))], []);
  return qw() if ($pdata[1] != 2);
  my $root_tmp = $idr->root;
  $idr->root($pdata[0]);
  my @qid2s = map { unpack('L',pack('S2',@{$_->{hi}})) } @{$idr->paths};
  $idr->root($root_tmp);
  return wantarray ? @qid2s : \@qid2s;
}

##  @pairs = $idr->pairs()  ##-- array context
## \@pairs = $idr->pairs(); ##-- scalar context
##   + returns @pairs = ($id1_1,$id2_1, ..., $id1_N,$id2_N)
sub pairs {
  my $idr  = shift;
  my @pairs =
    map {
      (unpack('L',pack('S2',@{$_->{lo}})), unpack('L',pack('S2',@{$_->{hi}})))
    } @{$idr->paths};
  return wantarray ? @pairs : \@pairs;
}



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
