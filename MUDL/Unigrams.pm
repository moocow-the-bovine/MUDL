#-*- Mode: Perl -*-

## File: MUDL::Unigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: unigram distribution
##======================================================================

package MUDL::Unigrams;
use MUDL::Corpus::Profile;
use MUDL::Corpus::Model;
use MUDL::Dist;
use Carp;
our @ISA = qw(MUDL::Dist MUDL::Corpus::Profile MUDL::Corpus::Model);

## OBJECT STRUCTURE:
##   + (from Dist): $wd => $count, ...

##======================================================================
## Accessors

## @words = $ug->vocabulary()
sub vocabulary {
  my $ug = shift;
  return keys(%$ug);
}

## getSize()
##   + returns number of unigrams
sub getSize { return scalar($_[0]->vocabulary); }

##======================================================================
## Corpus::Profile Methods

## undef = $ug->addSentence(\@sent)
sub addSentence { ++$_[0]->{ref($_) ? $_->text : $_} foreach (@{$_[1]}); }


##======================================================================
## I/O: Native
##
## (not yet implemented)

##======================================================================
## Modelling

## $logprob = sentenceProbability(\@sent,%args)
##   + %args : 'unknown'=>$unknown_symbol
##             'punknown'=>p($unknown_symbol)
sub sentenceProbability {
  my ($ug,$s,%args) = @_;
  $args{unknown} = '__UNKNOWN__' if (!defined($args{unknown}));
  $ug->{$args{unknown}} = 2**-32 if(!defined($ug->{$args{unknown}}));  ##-- HACK
  my $p = 0;
  my ($tok,$txt);
  foreach $tok (@$s) {
    $txt = ref($tok) ? $tok->text : $tok;
    $txt = $args{unknown} if (!defined($ug->{$txt}));
    $p += log($ug->{$txt});
  }
  return $p;
}

## $logprob = readerProbability($corpusREader,%args)
##   + %args : 'unknown'=>$unknown_symbol
*readerp = *readerP = *readerrob = *readerProb = \&readerProbability;
sub readerProbability {
  my ($ug,$cr,%args) = @_;
  $args{unknown} = '__UNKNOWN__' if (!defined($args{unknown}));
  $ug->{$args{unknown}} = 2**-32 if(!defined($ug->{$args{unknown}})); ##-- HACK
  my $p = 0;
  my ($s,$tok,$txt);
  while (defined($s=$cr->getSentence)) {
    foreach $tok (@$s) {
      $txt = ref($tok) ? $tok->text : $tok;
      $txt = $args{unknown} if (!defined($ug->{$txt}));
      $p += log($ug->{$txt});
    }
  }
  return $p;
}

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return qq(Extract token-text unigrams.\n)
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
