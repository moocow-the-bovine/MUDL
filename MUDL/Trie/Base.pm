##-*- Mode: CPerl -*-

## File: MUDL::Trie::Base.pm
## Author: Bryan Jurish <moocow@cpan.org>
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
##   indexDepth => $bool,     ##-- index state depth on finish()?
##   indexWidth => $bool,     ##-- index state out degree (width) on finish()?
##   indexStrings => $bool,   ##-- index full prefixes on finish()?
##
## + data:
##   chars => \%chars,        ##-- pseudo-set: $sym => undef
##   nq    => $n_states,      ##-- number of states (>= 1)
##   goto  => \@delta,        ##-- [$qid]{$sym} => $qid_to      s.t. $qid --$sym--> $qid_to
##   rgoto => \@rdelta,       ##-- [$qid_to]    => "$qid $sym"  s.t. $qid --$sym--> $qid_to
##   final => \%final,        ##-- {$qid}       => undef        s.t. $qid \in F
##
## + index data (post finish())
##   depth => \@q2depth,      ##-- [$qid] => $depth
##   width => \@q2width,      ##-- [$qid] => out_degree($qid)
##   str2id => \%str2id,    ##-- {$prefix_string} => $qid,
##   id2str => \@id2str,    ##-- [$qid]           => $prefix_string,
##
sub new {
  my ($that,%args) = @_;
  my $trie = bless {
		    ##-- User args
		    cw=>1,
		    reversed=>0,
		    indexDepth=>0,
		    indexWidth=>0,
		    indexStrings=>0,

		    ##-- Data
		    goto=>[],
		    rgoto=>[],
		    nq=>1,
		    chars=>{},
		    final=>{},

		    ##-- Index data
		    #depth=>[],
		    #width=>[],
		    #id2str=>[],
		    #str2id=>{},

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
  delete(@$trie{qw(depth width id2str str2id)});
  return $trie;
}

##======================================================================
## Methods: Manipulation
##======================================================================

## $qid  = $trie->addString($string)     ##-- scalar context
## @qids = $trie->addString($string)     ##-- array context
##  + adds path for $string; returns ID
*add = \&addString;
sub addString {
  my ($trie,$str) = @_;

  my @path = (0);
  my ($i,$q,$a,$qnext);
  for ($q=0,$i=0; $i < length($str); $q=$qnext, $i+=$trie->{cw}) {
    $a = ($trie->{reversed}
	  ? substr($str, length($str)-$i-1, $trie->{cw})
	  : substr($str, $i,                $trie->{cw}));
    if (!defined($qnext=$trie->{goto}[$q]{$a})) {
      $qnext = $trie->{goto}[$q]{$a} = $trie->{nq}++;
      $trie->{rgoto}[$qnext] = $q.' '.$a;
      $trie->{chars}{$a} = undef;
    }
    push(@path,$qnext);
  }
  $trie->{final}{$q} = undef;
  return wantarray ? @path : $q;
}

## $qid  = $trie->addArray(\@chars)  ##-- scalar context
## @qids = $trie->addArray(\@chars)  ##-- array context
##  + adds path for \@chars, returns id
sub addArray {
  my ($trie,$chars) = @_;

  my ($i,$a,$qnext);
  my @path = (0);
  my $q=0;
  foreach $i (0..$#$chars) {
    $a = $trie->{reversed} ? $chars->[$#$chars-$i] : $chars->[$i];
    if (!defined($qnext=$trie->{goto}[$q]{$a})) {
      $qnext = $trie->{goto}[$q]{$a} = $trie->{nq}++;
      $trie->{rgoto}[$qnext] = $q.' '.$a;
    }
    push(@path,$qnext);
  }
  @{$trie->{chars}}{@$chars} = undef;
  $trie->{final}{$q} = undef;
  return wantarray ? @path : $q;
}

##==============================================================================
## Methods: Indexing (finish)
##==============================================================================

## $trie = $trie->finish(%args)
##  + %args: overrides @$trie{keys %args}
##  + creates indices
##  + only call this after all data has been added
sub finish {
  my ($trie,%args) = @_;

  ##-- override trie args
  @$trie{keys(%args)} = values(%args);

  ##-- Do we want to index anything at all?
  my ($indexDepth,$indexWidth,$indexStrings) = @$trie{qw(indexDepth indexWidth indexStrings)};

  ##-- Index: Traverse
  if ($indexDepth || $indexWidth || $indexStrings) {
    my $goto   = $trie->{goto};

    my $widthx = $trie->{width} = $indexWidth ? [] : undef;
    my $depthx = $trie->{depth} = $indexDepth ? [] : undef;
    my $id2str = $trie->{id2str} = $indexStrings ? [] : undef;
    my $str2id = $trie->{str2id} = $indexStrings ? {} : undef;

    my ($qid,$depth,$prefix);
    my @fifo = (0,0,''); ##-- ($qid,$depth,$prefix, ...)
    while (@fifo) {
      ($qid,$depth,$prefix) = splice(@fifo,0,3);
      $widthx->[$qid] = scalar(keys(%{$trie->{goto}[$qid]})) if ($indexWidth);
      $depthx->[$qid] = $depth if ($indexDepth);
      if ($indexStrings) {
	$str2id->{$prefix} = $qid;
	$id2str->[$qid]    = $prefix;
      }
      push(@fifo,
	   map {
	     (
	      $goto->[$qid]{$_},
	      $depth+1,
	      ($trie->{reversed} ? ($_.$prefix) : ($prefix.$_))
	     )
	   } keys(%{$goto->[$qid]})
	  );
    }
  }

  return $trie;
}

##==============================================================================
## Methods: Properties
##==============================================================================

## $depth = $trie->depth($qid)
sub depth {
  return $_[0]{depth}[$_[1]] if ($_[0]{depth}); ##-- use index if available
  my $depth = $#{$_[0]->id2path($_[1])};
  return $depth < 0 ? undef : $depth;
}

## $width = $trie->width($qid);
sub width {
  return $_[0]{qidth}[$_[1]] if ($_[0]{width}); ##-- use index if available
  my $gotoq = $_[0]{goto}[$_[1]];
  return ($gotoq ? scalar(keys(%$gotoq)) : undef);
}

##==============================================================================
## Methods: Lookup: String -> Id
##==============================================================================

## $qid_or_undef = $trie->string2id($str)
*str2id = \&string2id;
sub string2id {
  return $_[0]{str2id}{$_[1]} if ($_[0]{str2id}); ##-- use index if available

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
  return $_[0]{str2id}{join('',@{$_[1]})} if ($_[0]{str2id}); ##-- use index if available
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
  return $_[0]{id2str}[$_[1]] if ($_[0]{id2str}); ##-- check for index
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
## Methods: Lookup: * -> State-Path
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
