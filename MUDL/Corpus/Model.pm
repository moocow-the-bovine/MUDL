#-*- Mode: Perl -*-

## File: MUDL::Corpus::Model.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus models
##======================================================================

package MUDL::Corpus::Model;
use MUDL::Object qw(dummy);
use MUDL::CorpusIO;
use Carp;
our @ISA = qw(MUDL::Object);

##======================================================================
## Corpus::Model: Abstract Methods

## $logp = $profile->corpusProbability($Corpus,@args)
##  + calls readerProbability()
sub corpusProbability {
  return $_[0]->readerProbability(MUDL::CorpusReader::Corpus->new(corpus=>$_[1]));
}

## $logp = $profile->readerProbability($CorpusReader,@args)
##  + returns sum of sentenceProbability($s,@args) for every sentence
sub readerProbability {
  my ($pr,$cr) = splice(@_,0,2);
  my ($s);
  my $sum = 0;
  $sum += $pr->sentenceProbability($s) while (defined($s=$cr->getSentence));
  return $sum;
}

## $logp = $profile->sentenceProbability(\@sentence,@args)
##  + dummy
*addSentence = dummy('sentenceProbability');

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
