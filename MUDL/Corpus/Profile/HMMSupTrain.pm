#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::HMMSupTrain.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile:
##    HMM supervised trainer
##======================================================================

package MUDL::Corpus::Profile::HMMSupTrain;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use MUDL::Lex;
use MUDL::Ngrams;

use strict;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile);

##======================================================================
## $obj = $class_or_obj->new(%args)
##  + %args:
##     utxt => $unknown_txt, ##-- 'unknown' pseudo-token ['@UNKNOWN']
##     fmin => $fmin,        ##-- minimum frequency for estimating unknown probs [default=1], -1 for none, 0 for all
##     bos  => $bos,         ##-- bos psuedo-tag ('__$')
##     eos  => $eos,         ##-- eos psuedo-tag ('__$')
##     lexf => $lex,         ##-- a MUDL::Lex    object : ($word,$tag)  => f($word  @  $tag)
##     arcf => $arc_dist,    ##-- a MUDL::Ngrams object : ($tag1,$tag2) => f($tag1 --> $tag2)
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new(
			       bos=>'__$',
			       eos=>'__$',
			       fmin=>1,
			       utxt=>"\@UNKNOWN",
			       lexf=>MUDL::Lex->new(),
			       arcf=>MUDL::Ngrams->new(nfields=>2),
			       %args
			      );
  return $self;
}

##======================================================================
## Profiling
##======================================================================

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($pr,$s) = @_;

  my $lexnz = $pr->{lexf}{counts};
  my $arcnz = $pr->{arcf}{nz};
  my ($tok, $txt,$tag);
  my $prevtag = $pr->{bos};
  foreach $tok (@$s) {
    $txt = $tok->text;
    $tag = $tok->tag;

    $lexnz->{''}++;
    $lexnz->{$txt}{''}++;
    $lexnz->{$txt}{$tag}++;
    $arcnz->{$prevtag."\t".$tag}++;

    $prevtag = $tag;
  }

  $arcnz->{$prevtag."\t".$pr->{eos}}++ if ($prevtag ne $pr->{bos});

  return $pr;
}

## undef = $profile->finish(%args)
##  + perform any finishing actions
##  + here,
##    - computes lexical probabilities for '@UNKNOWN'
sub finish {
  my $pr = shift;
  my $utxt = $pr->{utxt};
  my $fmin = $pr->{fmin};
  return if (!defined($utxt) || $fmin < 0);

  my $lexnz = $pr->{lexf}{counts};
  my $lexu  = $lexnz->{$utxt};
  $lexu = $lexnz->{$utxt} = {''=>0} if (!$lexu);

  my ($w,$wdist,$t,$fwt);
  while (($w,$wdist)=each(%$lexnz)) {
    next if (($w eq '') || ($fmin > 0 && $wdist->{''} > $fmin));
    while (($t,$fwt)=each(%$wdist)) {
      next if ($t eq '');
      $lexu->{''} += $fwt;
      $lexu->{$t} += $fwt;
    }
  }

  return $pr;
}

##======================================================================
## Conversion: to HMM
##======================================================================

## $hmm = $pr->toHMM(%args)
##  + %args: passed to MUDL::HMM->new(), $hmm->compileDists()
sub toHMM {
  my $pr = shift;

  require MUDL::HMM;
  my $hmm = MUDL::HMM->new(bos=>$pr->{bos},
			   eos=>$pr->{eos},
			   unknown=>$pr->{utxt},
			   @_);
  my ($afd,$pifd,$omegafd) = $hmm->separateArcDist($pr->{arcf});
  my $bfd = $pr->{lexf}->toDist->projectN(1,0);

  return $hmm->compileDists($afd, $bfd, $pifd, $omegafd, @_);
}


##======================================================================
## I/O : .lex
##======================================================================
__PACKAGE__->registerIOMode('lex',{saveFh=>'saveLexFh',loadFh=>'loadLexFh'});
__PACKAGE__->registerFileSuffix('.lex','lex');

## $bool = $obj->saveLexFh($fh,%args)
sub saveLexFh {
  my ($pr,$fh,%args) = @_;
  $pr->{lexf}->saveNativeFh($fh,%args);
  return $pr;
}


## $obj = $class_or_obj->loadLexFh($fh,%args)
sub loadLexFh {
  my ($pr,$fh,%args) = @_;
  $pr = $pr->new(%args) if (!ref($pr));
  return undef if (!($pr->{lexf} = $pr->{lexf}->loadNativeFh($fh,%args)));
  return $pr;
}

##======================================================================
## I/O : .123
##======================================================================
__PACKAGE__->registerIOMode('123',{saveFh=>'save123Fh',loadFh=>'load123Fh'});
__PACKAGE__->registerFileSuffix('.123','123');

## $bool = $obj->save123Fh($fh,%args)
##  %args:
##    verbose => $bool,
sub save123Fh {
  my ($pr,$fh,%args) = @_;
  return $pr->{arcf}->saveNativeFh($fh,%args);
}


## $obj = $class_or_obj->load123Fh($fh,%args)
sub load123Fh {
  my ($pr,$fh,%args) = @_;
  $pr = $pr->new(%args) if (!ref($pr));
  return undef if (!($pr->{arcf} = $pr->{arcf}->loadNativeFh($fh,%args)));
  return $pr;
}


##======================================================================
## I/O : Model (lex & n-grams)
##======================================================================
__PACKAGE__->registerIOMode('model', {saveFile=>'saveModelFile',loadFile=>'loadModelFile'});
__PACKAGE__->registerIOMode('native',{saveFile=>'saveModelFile',loadFile=>'loadModelFile'});
__PACKAGE__->registerFileSuffix('.model','model');


## $obj = $obj->saveModelFile($filename,%args)
##   + saves:
##     * lexicon "${filename}.lex"
##     * n-grams "${filename}.123"
sub saveModelFile {
  my ($pr,$filename,%args) = @_;

  $pr->saveFile("$filename.lex", %args, mode=>'lex')
    or confess(ref($pr),"::saveModelFile(): save failed for lexicon file '$filename.lex': $!");
  $pr->saveFile("$filename.123", %args, mode=>'123')
    or confess(ref($pr),"::saveModelFile(): save failed for n-gram file '$filename.123': $!");

  return $pr;
}


## $obj = $class_or_obj->loadModelFile($filename,%args)
##   + loads:
##     * lexicon "${filename}.lex"
##     * n-grams "${filename}.123"
sub loadModelFile {
  my ($pr,$filename,%args) = @_;

  $pr = $pr->new(%args) if (!ref($pr));

  $pr = $pr->loadFile("$filename.lex", %args, mode=>'lex')
    or confess(ref($pr),"::loadModelFile(): load failed for lexicon file '$filename.lex': $!");
  $pr = $pr->loadFile("$filename.123", %args, mode=>'123')
    or confess(ref($pr),"::loadModelFile(): load failed for n-gram file '$filename.123': $!");

  return $pr;
}



##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract supervised unigrams (token,tag) pairs.\n)
     .qq(Options:\n)
     .qq(  bos  = BOS            [default='__\$']\n)
     .qq(  eos  = EOS            [default='__\$']\n)
     .qq(  utxt = UNKNOWN_TEXT   [default='\@UNKNOWN']\n)
     .qq(  fmin = FREQ_MIN       [default=1]\n)
     .qq(  ... and more\n)
    );
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

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
