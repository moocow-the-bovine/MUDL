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

  ##--
  our $maxtoks = 10000;
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
  for ($i=0; defined($sent=$cr->getSentence) && $i < $maxtoks; $i += @$sent) {
    @$sent = splice(@$sent,0,($maxtoks-$i)) if ($i+@$sent > $maxtoks);
    $morph->addSentence($sent);
  }
  return $morph;
}

sub morphsave {
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

  ##-- generate initial model
  #$morph->generateModelFromCuts(%args);
  #our $model0 = debugModel();

  ##-- filter initial model
  #$morph->filterFirstModel();

  our $model = debugModel();
}
loadmorph;
#profmorph;
testsf1_0(reversed=>0);
#testsf1_0(reversed=>1);

sub debugModel {
  $morph->vmsg(0, "debugModel()\n");

  my $f2t   = $morph->debug_f2t;
  my $t2sig = $morph->debug_t2sig;
  my $sig2t = $morph->debug_sig2t;
  my $sig2r = $morph->debug_sig2robust;

  my $f2t_nn   = { %$f2t };
  my $t2sig_nn = { %$t2sig };
  my $sig2t_nn = { %$sig2t };
  my $sig2r_nn = { %$sig2r };

  delete($f2t_nn->{'NULL'});
  delete(@$t2sig_nn{grep { $t2sig->{$_} eq 'NULL' } keys(%$t2sig)});
  delete($sig2t_nn->{'NULL'});
  delete($sig2r_nn->{'NULL'});

  return {
	  f2t=>$f2t,
	  t2sig=>$t2sig,
	  sig2t=>$sig2t,
	  sig2robust=>$sig2r,
	  ##--
	  f2t_nn=>$f2t_nn,
	  t2sig_nn=>$t2sig_nn,
	  sig2t_nn=>$sig2t_nn,
	  sig2robust_nn=>$sig2r_nn,
	 };
}

##--------------------------------------------------
## dummy
package main;
foreach $i (0..10) {
  print "--dummy[$i]--\n";
}
