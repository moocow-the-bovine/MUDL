##-*- Mode: CPerl -*-

## File: MUDL::Trie::Base.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: trie: base class
##======================================================================

package MUDL::Trie::Base;
use MUDL::Enum;
use MUDL::Object;

use Carp qw(carp croak);
use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructors

## $obj = $class_or_obj->new(%args)
##
## + user args:
##   cw    => $char_width,    ##-- character width (default=1)
##   reversed=>$bool,         ##-- true if this is a reversed trie (suffix acceptor)
##
## + data:
##   chars => \%chars,        ##-- pseudo-set: $sym => undef
##   nq    => $n_states,      ##-- number of states (>= 1)
##   goto  => \@delta,        ##-- [$qid]{$sym} => $qid_to      s.t. $qid --$sym--> $qid_to
##   rgoto => \@rdelta,       ##-- [$qid_to]    => "$qid $sym"  s.t. $qid --$sym--> $qid_to
##   final => \%final,        ##-- {$qid}       => undef        s.t. $qid \in F
##   depth => \@q2depth,      ##-- [$qid]       => $depth


##
sub new {
  my ($that,%args) = @_;
  my $trie = bless {
		    reversed=>0,
		    cw=>1,
		    goto=>[],
		    rgoto=>[],
		    nq=>1,
		    chars=>{},
		    final=>{},
		    depth=>[],

		    ##-- data (not required)
		    #q2f=>[],

		    ##-- user args
		    %args,
		   }, ref($that)||$that;

  return $trie;
}

## $trie = $trie->clear()
sub clear {
  my $trie = shift;
  $trie->{nq} = 1;
  @{$trie->{goto}} = @{$trie->{rgoto}} = qw();
  %{$trie->{chars}} = undef;
  %{$trie->{final}} = qw();
  @{$trie->{depth}} = qw();
  return $trie;
}

##======================================================================
## Methods: Manipulation
##======================================================================

## $qid = $trie->addString($string)
##  + adds path for $string; returns ID
*add = \&addString;
sub addString {
  my ($trie,$str) = @_;

  my ($i,$q,$a,$qnext);
  for ($q=0,$i=0; $i < length($str); $q=$qnext, $i+=$trie->{cw}) {
    $a = ($trie->{reversed}
	  ? substr($str, length($str)-$i-1, $trie->{cw})
	  : substr($str, $i,                $trie->{cw}));
    if (!defined($qnext=$trie->{goto}[$q]{$a})) {
      $qnext = $trie->{goto}[$q]{$a} = $trie->{nq}++;
      $trie->{rgoto}[$qnext] = $q.' '.$a;
      $trie->{chars}{$a} = undef;
      $trie->{depth}[$qnext] = $i/$trie->{cw};
    }
  }
  $trie->{final}{$q} = undef;
  return $q;
}

## $qid = $trie->addArray(\@chars)
##  + adds path for \@chars, returns id
sub addArray {
  my ($trie,$chars) = @_;

  my ($i,$a,$qnext);
  my $q=0;
  foreach $i (0..$#$chars) {
    $a = $trie->{reversed} ? $chars->[$#$chars-$i] : $chars->[$i];
    if (!defined($qnext=$trie->{goto}[$q]{$a})) {
      $qnext = $trie->{goto}[$q]{$a} = $trie->{nq}++;
      $trie->{rgoto}[$qnext] = $q.' '.$a;
      $trie->{depth}[$qnext] = $i;
    }
  }
  @{$trie->{chars}}{@$chars} = undef;
  $trie->{final}{$q} = undef;
  return $q;
}

##==============================================================================
## Methods: Lookup: String -> Id
##==============================================================================

## $qid_or_undef = $trie->string2id($str)
*str2id = \&string2id;
sub string2id {
  my ($trie,$str) = @_;
  my ($q,$i);
  for ($q=0,$i=0; defined($q) && $i < length($str); $i+=$trie->{cw}) {
    $q = $trie->{goto}[$q]{$trie->{reversed}
			   ? substr($str,length($str)-$i-1,$trie->{cw})
			   : substr($str,$i,$trie->{cw})
			  };
  }
  return $q;
}

## $qid_or_undef = $trie->array2id(\@chars)
sub array2id {
  my ($trie,$ary) = @_;
  my $q = 0;
  my ($a);
  foreach $a ($trie->{reversed} ? reverse(@$ary) : @$ary) {
    $q = $trie->{goto}[$q]{$a};
    return undef if (!defined($q));
  }
  return $q;
}

##==============================================================================
## Methods: Lookup: Id -> String
##==============================================================================

## $str = $trie->id2string($qid)
*id2str = \&id2string;
sub id2string {
  return join('', @{$_[0]->id2array($_[1])});
}

## \@chars = $trie->id2array($qid)
sub id2array {
  my ($trie,$q) = @_;
  my @syms = qw();
  my ($qa,$a);
  while (defined($q) && $q > 0 && defined($qa=$trie->{rgoto}[$q])) {
    ($q,$a) = CORE::split(/ /, $qa, 2);
    push(@syms, $a);
  }
  return $trie->{reversed} ? \@syms : [reverse(@syms)];
}

##==============================================================================
## Methods: Lookup: Id -> Path
##==============================================================================

## \@qids = $trie->id2path($qid)
sub id2path {
  my ($trie,$q) = @_;
  my @path = ($q);
  my ($qa,$a);
  while (defined($q) && $q > 0 && defined($qa=$trie->{rgoto}[$q])) {
    ($q,$a) = CORE::split(/ /, $qa, 2);
    push(@path, $q);
  }
  return [reverse(@path)];
}

## \@states = $trie->string2path($str)
*str2path = \&string2path;
sub string2path {
  my ($trie,$str) = @_;
  my ($i,$q,@path);
  for ($q=0,$i=0; defined($q) && $i < length($str); $i+=$trie->{cw}) {
    push(@path,$q);
    $q = $trie->{goto}[$q]{$trie->{reversed}
			   ? substr($str,length($str)-$i-1,$trie->{cw})
			   : substr($str,$i,$trie->{cw})
			  };
  }
  push(@path,$q) if (defined($q));
  return \@path;
}

## \@states = $trie->array2path(\@syms)
sub array2path {
  my ($trie,$ary) = @_;
  my ($i,$q,@path);
  for ($q=0,$i=0; defined($q) && $i <= $#$ary; $i++) {
    push(@path,$q);
    $q = $trie->{goto}[$q]{$trie->{reversed}
			   ? $ary->[$#$ary-$i]
			   : $ary->[$i]
			  };
  }
  push(@path,$q) if (defined($q));
  return \@path;
}

##======================================================================
## Conversion: Gfsm
##======================================================================

## $gfsmAlphabet = $trie->gfsmAlphabet(%args)
##  + %args:
##     labels=>$alphabet,
##     epsilon=>$eps_str,
sub gfsmAlphabet {
  require Gfsm;
  my ($trie,%args) = @_;
  my $labs = defined($args{labels}) ? $args{labels} : Gfsm::Alphabet->new();
  my $eps  = defined($args{epsilon}) ? $args{epsilon} : '<epsilon>';
  $labs->insert($eps);
  $labs->insert($_) foreach (sort(keys(%{$trie->{chars}})));
  return $labs;
}

## $gfsmTrie = $trie->gfsmTrie(%args)
##  + %args:
##     labels=>$alphabet, #-- gfsm alphabet (default=temporary)
##     fsm=>$fsm,       #-- destination fsm (default=new)
##     dosort=>$bool,   #-- sort arcs? (default=1)
sub gfsmAutomaton {
  require Gfsm;
  my ($trie,%args) = @_;
  my $labs = defined($args{labels}) ? $args{labels} : $trie->gfsmAlphabet(%args);
  my $fsm   = defined($args{fsm})  ? $args{fsm}  : Gfsm::Automaton->newTrie();
  $fsm->is_transducer(0);
  $fsm->is_weighted(0);
  $fsm->root(0);
  my ($q,$qah,$a,$alab,$qto);
  foreach $q (0..($trie->{nq}-1)) {
    if (defined($qah = $trie->{goto}[$q])) {
      while (($a,$qto)=each(%$qah)) {
	$alab = $labs->get_label($a);
	$fsm->add_arc($q,$qto, $alab,$alab, 0);
      }
    }
    $fsm->is_final($q,1) if (exists($trie->{final}{$q}));
  }
  $fsm->arcsort(Gfsm::ASMLower()) if ($args{dosort} || !exists($args{dosort}));
  return $fsm;
}

##==============================================================================
## Methods: Debug: View
##==============================================================================

## undef = $trie->viewps(%options)
sub viewps {
  my ($trie,%args) = @_;
  my $ilabs = exists($args{labels}) ? exists($args{labels}) : $trie->gfsmAlphabet(%args);
  my $fsm   = $args{fsm}            ? $args{fsm}            : $trie->gfsmAutomaton(labels=>$ilabs);
  $fsm->viewps(labels=>$ilabs,%args); #states=>$qlabs
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
