#!/usr/bin/perl -wd

use lib '../..';
use Gfsm;
use MUDL::Morph::Goldsmith;
use MUDL::Dist;
use MUDL::CorpusIO;
use MUDL::CmdUtils;
use PDL;


##--------------------------------------------------
## create a freq trie
BEGIN {
  our (@words);
  @words = (
	    #qw(a as aa aas ab abs abc abcs),
	    (map { 'xxx' . $_ } qw(ed s ion ing)),
	    (map { $_ . 'yyy' } qw(post un)),
	   );
  ##--
  our ($bos,$eos);
  #($bos,$eos) = ('__$','__$');
  #($bos,$eos) = ('#','#');
  #($bos,$eos) = ('','');
  ($bos,$eos) = (undef,undef);
}

sub newmorph {
  $morph = MUDL::Morph::Goldsmith->new(bos=>$bos,eos=>$eos, tsplit=>'', @_);
  $morph->addSentence(\@words);
  $morph->finish;
}

sub newmorph_w {
  $morph = MUDL::Morph::Goldsmith->new(bos=>$bos,eos=>$eos, tsplit=>undef);
  $morph->addSentence(\@words);
  $morph->finish;
}
#newmorph_w;


##--------------------------------------------------
## test: successor frequencies

sub testsf1_0 {
  my %args = @_;
  newmorph();
  $morph->succFreqs(mode=>1,%args);
  our $cuts = $morph->{cuts};

  our $wcuts = {};
  my ($vid,$cutat);
  while (($vid,$cutat)=each(%$cuts)) {
    $labs = $morph->{pta}->vector2labels($morph->{wenum}{id2sym}[$vid]);
    $word=$morph->{pta}->labels2chars($labs);
    $wcuts->{$word} = substr($word,0,$cutat+1).".".substr($word,$cutat+1);
    #$wcuts->{$word} = $cutat;
  }
}
testsf1_0;
#testsf1_0(reversed=>1);

##--------------------------------------------------
## dummy
package main;
foreach $i (0..10) {
  print "--dummy[$i]--\n";
}
