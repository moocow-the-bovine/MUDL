#-*- Mode: Perl -*-

## File: MUDL::Unigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: unigram distribution
##======================================================================

package MUDL::Unigrams;
use MUDL::Dist;
use MUDL::XML;
use Carp;
use IO::File;
our @ISA = qw(MUDL::Dist);
our $VERSION = 0.01;

## OBJECT STRUCTURE:
##   + (from Dist): $wd => $count, ...

##======================================================================
## Accessors

## @words = $ug->vocabulary()
*V = *vocab = *Sigma = *sigma = \&vocabulary;
sub vocabulary {
  my $ug = shift;
  return keys(%$ug);
}

## size()
##   + returns number of unigrams
sub size { return scalar($_[0]->sigma); }

##======================================================================
## add unigrams learn from a corpus

## $ug = $ug->addCorpus($corpus,%args)
sub addCorpus {
  my ($ug,$corpus) = @_;
  my ($s,$txt);
  foreach $s (@{$corpus->sentences}) {
    foreach (@$s) {
      $txt = ref($_) ? $_->{text} : $_;
      ++$ug->{$txt};
    }
  }
  return $ug;
}

## $bg = $bg->addReader($reader,%args)
sub addReader {
  my ($ug,$cr) = @_;
  my ($s,$txt);
  while (defined($s=$cr->getSentence)) {
    next if (!@$s);
    foreach (@$s) {
      $txt = ref($_) ? $_->{text} : $_;
      if (!defined($txt)) {
	warn( __PACKAGE__ , "::addReader(): undefined token text!");
	next;
      }
      ++$ug->{$txt};
    }
  }
  return $ug;
}


##======================================================================
## I/O: XML

## (inherited)

##======================================================================
## I/O: TnT
##
## (not yet implemented)

##======================================================================
## Modelling

## $logprob = sentenceProbability(\@sent,%args)
##   + %args : 'unknown'=>$unknown_symbol
*sp = *sP = *sentp = *sentP = *sentProb = *sentprob = *sentenceProb = \&sentenceProbability;
sub sentenceProbability {
  my ($ug,$s,%args) = @_;
  $args{unknown} = '__UNKNOWN__' if (!defined($args{unknown}));
  $ug->{$args{unknown}} = 2**-32 if(!defined($ug->{$args{unknown}}));
  my $p = 0;
  my ($tok,$txt,$ptxt);
  foreach $tok (@$s) {
    $txt = ref($tok) ? $tok->{text} : $tok;
    $txt = $args{unknown} if (!defined($ug->{$txt}));
    $p += log($ug->{$txt});
  }
  return $p;
}

## $logprob = corpusProbability($corpus,%args)
##   + %args : 'unknown'=>$unknown_symbol
*corpusp = *corpusP = *corpusprob = *corpusProb = \&corpusProbability;
sub corpusProbability {
  my ($ug,$corpus,%args) = @_;
  $args{unknown} = '__UNKNOWN__' if (!defined($args{unknown}));
  $ug->{$args{unknown}} = 2**-32 if(!defined($ug->{$args{unknown}}));
  my $p = 0;
  my ($s,$tok,$txt,$ptxt);
  foreach $s (@{$corpus->{sents}}) {
    foreach $tok (@$s) {
      $txt = ref($tok) ? $tok->{text} : $tok;
      $txt = $args{unknown} if (!defined($ug->{$txt}));
      $p += log($ug->{$txt});
    }
  }
  return $p;
}

## $logprob = readerProbability($corpusREader,%args)
##   + %args : 'unknown'=>$unknown_symbol
*readerp = *readerP = *readerrob = *readerProb = \&readerProbability;
sub readerProbability {
  my ($ug,$cr,%args) = @_;
  $args{unknown} = '__UNKNOWN__' if (!defined($args{unknown}));
  $ug->{$args{unknown}} = 2**-32 if(!defined($ug->{$args{unknown}}));
  my $p = 0;
  my ($s,$tok,$txt);
  while (defined($s=$cr->getSentence)) {
    foreach $tok (@$s) {
      $txt = ref($tok) ? $tok->{text} : $tok;
      $txt = $args{unknown} if (!defined($ug->{$txt}));
      $p += log($ug->{$txt});
    }
  }
  return $p;
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
