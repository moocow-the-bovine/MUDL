##-*- Mode: CPerl -*-

## File: MUDL::Morph::Editor.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Gtk morphological editors
##======================================================================

package MUDL::Morph::Editor;
use MUDL::Object;
use MUDL::Corpus::Buffer::Pdl;
use MUDL::Enum;
use MUDL::EDist;

use MUDL::Gfsm::Automaton;
use MUDL::Gfsm::Alphabet;
use PDL;

use utf8;
use Encode;

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

     fst=>MUDL::Gfsm::Automaton->new, ##-- morph fst
     labs=>MUDL::Gfsm::Alphabet->new, ##-- morph labels

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
## I/O: Corpus
##======================================================================

## $me->loadCorpus($file,%args)
##  + load corpus into the object
##  + corpus may be analyzed (token tag=best-lemma, analyses=lemma1, ..., lemmaN)
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

## $me->clearCorpus($file,%args)
##  + clear object contents
sub clearCorpus {
  my $me = shift;
  $me->{wenum}->clear;
  %{$me->{wfreq}} = qw();
  %{$me->{woccs}} = qw();
  $me->{corpus}->clear;
}
