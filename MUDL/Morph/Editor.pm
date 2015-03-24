##-*- Mode: CPerl -*-

## File: MUDL::Morph::Editor.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: Gtk morphological editors
##======================================================================

package MUDL::Morph::Editor;
use MUDL::Morph::Analyses;
use MUDL::Object;
use MUDL::Corpus::Buffer::Pdl;
use MUDL::Enum;
use MUDL::EDist;
use MUDL::Map;

use PDL;

use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $me = MUDL::Morph::Editor->new(%args)
## + %args, structure:
sub new {
  my $that = shift;
  my $me = $that->SUPER::new
    (
     ##-- data
     wenum=>MUDL::Enum->new(),   ##-- word enum
     wfreq=>MUDL::EDist->new(),  ##-- word-id unigram frequencies
     woccs=>{},                  ##-- maps word ids to occurrence indices, as
                                 ##  $occs = pack('(LS)*', $sentidx,$occidx, ...)
     corpus=>undef, ##-- a MUDL::Corpus::Buffer::Pdl

     segsep=>'.',   ##-- segment-separator character

     ##-- analyses
     analyses=>MUDL::Morph::Analyses->new(), ##-- word-text -> analysis map

     ##-- model
     w2seg=>[],     ##-- word-id -> segmented word text
     morphs=>{},    ##-- pseudo-set of morphs (text)

     ##-- gui
     gui=>undef,

     ##-- User options
     @_,
    );

  ##-- generate corpus
  $me->{corpus} = MUDL::Corpus::Buffer::Pdl->new(txtenum=>$me->{wenum},
						 dobos=>0,
						 doeos=>0,
						 dobash=>0,
						);

  return $me;
}


##======================================================================
## Manipulators
##======================================================================

## $me->clear()
##  + clear all object content
sub clear {
  my $me = shift;
  $me->clearCorpus;
  $me->{wenum}->clear;
  $me->{analyses}->clear;
  @{$me->{w2seg}} = qw();
  %{$me->{morphs}} = qw();
}

##======================================================================
## I/O: Analyses
##======================================================================

##-- ... just use {analyses} element directly

##======================================================================
## I/O: Corpus
##======================================================================

## $me->loadCorpus($file,%args)
##  + load corpus into the object
##  + corpus may NOT be analyzed (token tag=best-lemma, analyses=lemma1, ..., lemmaN)
##  + populates $me->{wenum}, $me->{wfreq}, ...?
sub loadCorpus {
  my ($me,$file,%args) = @_;
  my $cr = MUDL::CorpusIO->fileReader($file);
  my $wenum = $me->{wenum};
  my $wfreq = $me->{wfreq};
  my $csents = $me->{corpus}{sents};
  my $woccs = $me->{woccs};
  my ($sent,$tok,@tokids);
  my $sentid = $#$csents+1;
  while (defined($sent=$cr->getSentence)) {
    @tokids = map { $wenum->addSymbol(ref($_) ? $_->text : $_) } @$sent;
    $wfreq->{$_}++ foreach (@tokids);
    push(@$csents, pdl(long,@tokids));
    $woccs->{$tokids[$_]} .= pack('LS', $sentid, $_) foreach (0..$#tokids);
    ++$sentid;
  }
}

## $me->clearCorpus()
##  + clear corpus-specific object content
sub clearCorpus {
  my $me = shift;
  #$me->{wenum}->clear;
  %{$me->{wfreq}} = qw();
  %{$me->{woccs}} = qw();
  $me->{corpus}->clear;
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
