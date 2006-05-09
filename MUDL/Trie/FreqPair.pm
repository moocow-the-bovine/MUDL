##-*- Mode: CPerl -*-

## File: MUDL::Trie::FreqPair.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: trie: (prefix-, suffix-) frequency trie pairs
##======================================================================

package MUDL::Trie::FreqPair;
use MUDL::Trie::Freq;

use Carp qw(carp croak);
use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructors

## $obj = $class_or_obj->new(%args)
## + args (shared):
##    cw      => $char_width,   ##-- for string methods
##    chars   => \%chars,       ##-- shared alphabet (pseudo-set)
##    reversed => $bool,        ##-- swap (pta,sta) ?
##    indexDepth => $bool,      ##-- index state depth on finish()?
##    indexWidth => $bool,      ##-- index state out degree (width) on finish()?
##    indexStrings => $bool,    ##-- index full prefixes on finish()?
##    indexSort=>$bool,         ##-- sort equivalence maps by numeric id value?
##
## + data (new in Trie::FreqPair)
##    pta     => $prefix_trie,  ##-- a MUDL::Trie::Freq object
##    sta     => $suffix_trie,  ##-- a MUDL::Trie::Freq object (reversed)
##
##    pspairs => \%pairs,       ##-- pairs of ($prefix_qid, $suffix_qid)
##                              ##   s.t. for some @w[1..$n] \in @paths,
##                              ##        $prefix_id = $pta->string2id(        @w[1..$i]    ) , and
##                              ##        $suffix_id = $sta->string2id(reverse(@w[$i+1..$n])) .
##                              ##   encoded as:
##                              ##        pack('LL', $prefix_id, $suffix_id) => undef
##
## + index data (post finish())
##    p2s     => \@p2s,         ##-- [$prefix_qid] => pack('L*', @equiv_suffix_qids) #-- +sorted if {indexSort}
##    s2p     => \@s2p,         ##-- [$suffix_qid] => pack('L*', @equiv_prefix_qids) #-- +sorted if {indexSort}
##                              ## : a pair ($prefix_qid, $suffix_qid) are equivalent iff:
##                              ##   for some @w[1..$n] \in @paths,
##                              ##        $prefix_id = $pta->string2id(        @w[1..$i]    ) , and
##                              ##        $suffix_id = $sta->string2id(reverse(@w[$i+1..$n])) .
##
sub new {
  my ($that,%args) = @_;

  ##-- shared data
  my $reversed = $args{reversed} ? 1 : 0;
  my @shared_keys = qw(chars cw indexDepth indexWidth indexStrings);
  $args{chars} = {} if (!$args{chars});
  $args{cw}    = 1  if (!$args{cw});
  $args{indexDepth} = 0 if (!$args{indexDepth});
  $args{indexWidth} = 0 if (!$args{indexWidth});
  $args{indexStrings} = 0 if (!$args{indexStrings});

  my %shared_args = map { $_=>$args{$_} } @shared_keys;

  ##-- object
  my $tp = bless {
		  ##-- User args (new)
		  indexSort=>0,

		  ##-- Profiling data
		  pta=>MUDL::Trie::Freq->new(%shared_args, reversed=>$args{reversed}),
		  sta=>MUDL::Trie::Freq->new(%shared_args, reversed=>!$args{reversed}),
		  pspairs=>{},

		  ##-- Index data
		  s2p=>[],
		  p2s=>[],

		  ##-- user & shared args
		  %args,
		 }, ref($that)||$that;

  return $tp;
}

## $tp = $tp->clear()
sub clear {
  my $tp = shift;
  $tp->{pta}->clear();
  $tp->{sta}->clear();
  @{$tp->{s2p}} = @{$tp->{p2s}} = qw();
  %{$tp->{chars}} = qw();
  return $tp;
}

##======================================================================
## Methods: Manipulation
##======================================================================


##--------------------------------------------------------------
## [$qid_prefix,  $qid_suffix]   = $tp->addString($string,$freq)  ##-- scalar context
## (\@path_prefix,\@path_suffix) = $tp->addString($string,$freq)  ##-- array context
##  + adds paths for $string; returns pairs of ids
*add = \&addString;
sub addString {
  my ($tp,$str,$freq) = @_;
  my @pathpta = $tp->{pta}->addString($str,$freq);
  my @pathsta = $tp->{sta}->addString($str,$freq);

  ##-- save state-id pairs
  foreach (0..$#pathpta) {
    $tp->{pspairs}{pack('LL', $pathpta[$_], $pathsta[$#pathsta-$_])} = undef;
  }

  return wantarray ? (\@pathpta,\@pathsta) : [$pathpta[$#pathpta], $pathsta[$#pathsta]];
}

sub addString0 {
  my ($tp,$str,$freq) = @_;

  my ($i,$q,$a,$qnext);

  ##-- Get ids
  my @qp = (0); ##-- prefix path (state-ids)
  my @qs = (0); ##-- suffix path (state-ids)
  my $strlen = length($str);
  for ($i=0; $i < length($str); $i+=$tp->{cw}) {
    ##-- PTA
    $a = substr($str, $i, $tp->{cw});
    $q = $qp[$#qp];
    if (!defined($qnext=$tp->{pta}{goto}[$q]{$a})) {
      $qnext = $tp->{pta}{goto}[$q]{$a} = $tp->{pta}{nq}++;
      $tp->{pta}{rgoto}[$qnext] = $q.' '.$a;

      ##-- Chars
      $tp->{chars}{$a} = undef;
    }
    push(@qp,$qnext);

    ##-- STA
    $a = substr($str, $strlen-$i-1, $tp->{cw});
    $q = $qs[$#qs];
    if (!defined($qnext=$tp->{sta}{goto}[$q]{$a})) {
      $qnext = $tp->{sta}{goto}[$q]{$a} = $tp->{sta}{nq}++;
      $tp->{sta}{rgoto}[$qnext] = $q.' '.$a;
    }
    push(@qs,$qnext);
  }

  ##-- Add pairs
  foreach $i (0..$#qp) {
    $tp->{pspairs}{pack('LL', $qp[$i], $qs[$#qs-$i])} = undef;
  }

  ##-- Mark finals
  $tp->{pta}{final}{$qp[$#qp]} = undef;
  $tp->{sta}{final}{$qs[$#qs]} = undef;

  ##-- Store frequency
  $tp->{pta}{q2f}[$qp[$#qp]] += $freq;
  $tp->{sta}{q2f}[$qs[$#qs]] += $freq;

  ##-- Return
  return ($qp[$#qp], $qs[$#qs]);
}

##--------------------------------------------------------------
## [$qid_prefix,  $qid_suffix]   = $tp->addArray(\@chars,$freq)  ##-- scalar context
## (\@path_prefix,\@path_suffix) = $tp->addArray(\@chars,$freq)  ##-- array context
##  + adds paths for \@chars, returns pair of ids
sub addArray {
  my ($tp,$chars,$freq) = @_;
  my @pathpta = $tp->{pta}->addArray($chars,$freq);
  my @pathsta = $tp->{sta}->addArray($chars,$freq);

  ##-- save state-id pairs
  foreach (0..$#pathpta) {
    $tp->{pspairs}{pack('LL', $pathpta[$_], $pathsta[$#pathsta-$_])} = undef;
  }

  return wantarray ? (\@pathpta,\@pathsta) : [$pathpta[$#pathpta], $pathsta[$#pathsta]];
}

sub addArray0 {
  my ($tp,$chars,$freq) = @_;

  my ($i,$q,$a,$qnext);

  ##-- Get ids
  my @qp = (0); ##-- prefix path (state-ids)
  my @qs = (0); ##-- suffix path (state-ids)
  foreach $i (0..$#$chars) {
    ##-- PTA
    $a = $chars->[$i];
    $q = $qp[$#qp];
    if (!defined($qnext=$tp->{pta}{goto}[$q]{$a})) {
      $qnext = $tp->{pta}{goto}[$q]{$a} = $tp->{pta}{nq}++;
      $tp->{pta}{rgoto}[$qnext] = $q.' '.$a;
    }
    push(@qp,$qnext);

    ##-- STA
    $a = $chars->[$#$chars-$i];
    $q = $qs[$#qs];
    if (!defined($qnext=$tp->{sta}{goto}[$q]{$a})) {
      $qnext = $tp->{sta}{goto}[$q]{$a} = $tp->{sta}{nq}++;
      $tp->{sta}{rgoto}[$qnext] = $q.' '.$a;
    }
    push(@qs,$qnext);
  }

  ##-- Chars
  @{$tp->{chars}}{@$chars} = undef;

  ##-- Add pairs (!)
  foreach $i (0..$#qp) {
    $tp->{pspairs}{pack('LL', $qp[$i], $qs[$#qs-$i])} = undef;
  }

  ##-- Mark finals
  $tp->{pta}{final}{$qp[$#qp]} = undef;
  $tp->{sta}{final}{$qs[$#qs]} = undef;

  ##-- Store frequency
  $tp->{pta}{q2f}[$qp[$#qp]] += $freq;
  $tp->{sta}{q2f}[$qs[$#qs]] += $freq;

  ##-- Return
  return ($qp[$#qp], $qs[$#qs]);
}

##==============================================================================
## Methods: Manipulation: final index
##==============================================================================

## $tp = $tp->finish(%args)
##  + sets prefix frequencies from full-word frequencies in PTA and STA
##  + builds PTA<->STA equivalence maps $tp->{p2s} and $tp->{s2p}
sub finish {
  my ($tp,%args) = @_;
  @$tp{keys %args} = values(%args);

  ##-- Frequency inheritance
  $tp->{pta}->finish(%args);
  $tp->{sta}->finish(%args);

  ##-- Generate equivalence maps
  my $pspairs = $tp->{pspairs};

  my $p2s = $tp->{p2s};
  my $s2p = $tp->{s2p};
  @$p2s = qw();
  @$s2p = qw();

  my ($pair,$qpta,$qsta);
  foreach $pair (keys(%$pspairs)) {
    ($qpta,$qsta) = unpack('LL',$pair);
    $p2s->[$qpta] .= pack('L',$qsta);
    $s2p->[$qsta] .= pack('L',$qpta);
  }

  ##-- Sort equivalence maps?
  if ($tp->{indexSort}) {
    foreach $qpta (0..$#$p2s) {
      $p2s->[$qpta] = pack('L*', sort {$a<=>$b} (unpack('L*', $p2s->[$qpta])));
    }
    foreach $qsta (0..$#$s2p) {
      $s2p->[$qsta] = pack('L*', sort {$a<=>$b} (unpack('L*', $s2p->[$qsta])));
    }
  }

  return $tp;
}


##==============================================================================
## Methods: Lookup: Prefixes, Suffixes
##==============================================================================

## \@suffix_ids = $tp->prefix2suffixes($prefix_qid)
##   + requirs finish()
*p2s = \&prefix2suffixes;
sub prefix2suffixes {
  my ($tp,$qid_pta) = @_;
  return unpack('L*',$tp->{p2s}[$qid_pta]);
}

## \@prefix_ids = $tp->suffix2prefixes($suffix_qid)
##   + requirs finish()
*s2p = \&suffix2prefixes;
sub suffix2prefixes {
  my ($tp,$qid_sta) = @_;
  return unpack('L*',$tp->{s2p}[$qid_sta]);
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
