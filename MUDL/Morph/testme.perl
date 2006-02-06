#!/usr/bin/perl -wd

use lib '../..';
use Gfsm;
use MUDL::Morph::Goldsmith;
use MUDL::Dist;
use MUDL::CorpusIO;
use MUDL::CmdUtils;
use PDL;


##--------------------------------------------------
## initialization
BEGIN {
  our (@words);
  @words = (
	    #qw(a as aa aas ab abs abc abcs),
	    ##--
	    (map { 'xxx' . $_ } qw(ed s ion ing)),
	    #(map { $_ . 'yyy' } qw(post un)),
	    ##--
	    #(map { 'zz' . $_ } qw(es s ion ing)),
	    #(map { $_ . 'zz' } qw(post un)),
	   );
  ##--
  our ($bos,$eos);
  #($bos,$eos) = ('__$','__$');
  #($bos,$eos) = ('#','#');
  #($bos,$eos) = ('','');
  ($bos,$eos) = (undef,undef);
}

##--------------------------------------------------
## object creation: dummy
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
## object creation: profile

sub profmorph {
  $morph = MUDL::Morph::Goldsmith->new(bos=>$bos,eos=>$eos,tsplit=>'',@_);
  $cr = MUDL::CorpusIO->fileReader('utest-nl.t');
  $sent = [];
  for ($i=0; defined($sent=$cr->getSentence) && $i <= 10000; $i += $#$sent+1) {
    $morph->addSentence($sent);
  }
  return $morph;
}

sub save_morph {
  my $file = shift;
  $file = 'morph-test.bin' if (!$file);
  $morph->saveFile($file);
}
sub loadmorph { load_morph(@_); }
sub load_morph {
  my $file=shift;
  $file = 'morph-test.bin' if (!$file);
  $morph = MUDL::Morph::Goldsmith->loadFile($file);
}


##--------------------------------------------------
## test: successor frequencies

sub testsf1_0 {
  my %args = @_;
  newmorph(reversed=>$args{reversed}) if (!$morph);
  #newmorph(minStemLen1=>0);
  $morph->succFreqs(mode=>1,%args);
  our $wcuts = $morph->debug_cuts();

  $morph->generateModelFromCuts(%args);
  our $t2f   = $morph->debug_t2f;
  our $f2t   = $morph->debug_f2t;
  our $sig2t = $morph->debug_sig2t;
}
loadmorph;
testsf1_0(reversed=>0);
#testsf1_0(reversed=>1);

##--------------------------------------------------
## dummy
package main;
foreach $i (0..10) {
  print "--dummy[$i]--\n";
}
