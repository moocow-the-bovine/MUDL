#-*- Mode: Perl -*-

## File: MUDL::Corpus::Model.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus models
##======================================================================

package MUDL::Corpus::Model;
use MUDL::Object qw(dummy);
use MUDL::CorpusIO;
use MUDL::LogUtils qw(:all);
use Carp;
our @ISA = qw(MUDL::Object);

##======================================================================
## Corpus::Model: Abstract Methods

## $log_sum_sentprobs = $model->fileProbability($filename,@args)
##  + calls fileReader(), readerProbability()
##  + returns log(sum(p($s))) for each sentence $s in $filename
##  + @args are passed to MUDL::CorpusIO->fileReader()
sub fileProbability {
  return $_[0]->readerProbability(MUDL::CorpusIO->fileReader(@_[1..$#_]));
}

## $log_sum_sentprobs = $model->bufferProbability($buffer,@args)
##  + calls readerProbability()
##  + returns log(sum(p($s))) for each sentence $s in $buffer
##  + @args are passed to $buffer->reader()
sub bufferProbability {
  return $_[0]->readerProbability($_[1]->reader(@_[2..$#_]));
}

## $log_sum_sentprobs = $model->readerProbability($CorpusReader,@args)
##  + returns log(sum(p($s))) for each sentence $s in $buffer
##  + default version calls sentenceProbability($s,@args) for every sentence
##  + @args are passed to $model->sentenceProbability() in default version
sub readerProbability {
  my ($model,$cr) = splice(@_,0,2);
  my ($s);
  my $logp = $LOG_ZERO;
  while (defined($s=$cr->getSentence)) {
    $logp = plogadd($logp, $model->sentenceProbability($s,@_[2..$#_]));
  }
  return $logp;
}

## $log_psent = $model->sentenceProbability(\@sentence,@args)
##  + returns log(p(\@sentence))
##  + dummy method
*sentenceProbability = dummy('sentenceProbability');

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
