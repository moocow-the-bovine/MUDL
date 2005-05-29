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

## return type \%info:
##  + hash ref:
##     logprod => $log_product_of_sentprobs,
##     logsum  => $log_sum_of_sentprobs,
##     nsents  => $number_of_sentences,
##     ...

## \%info = $model->fileProbability($filename,@args)
##  + calls fileReader(), readerProbability()
##  + returns log(sum(p($s))) for each sentence $s in $filename
##  + @args are passed to MUDL::CorpusIO->fileReader()
sub fileProbability {
  return $_[0]->readerProbability(MUDL::CorpusIO->fileReader(@_[1..$#_]));
}

## \%info = $model->bufferProbability($buffer,@args)
##  + calls readerProbability()
##  + returns log(sum(p($s))) for each sentence $s in $buffer
##  + @args are passed to $buffer->reader()
sub bufferProbability {
  return $_[0]->readerProbability($_[1]->reader(@_[2..$#_]));
}

## \%info = $model->readerProbability($CorpusReader,@args)
##  + returns log(sum(p($s))) for each sentence $s in $buffer
##  + default version calls sentenceProbability($s,@args) for every sentence
##  + @args are passed to $model->sentenceProbability() in default version
sub readerProbability {
  my ($model,$cr) = splice(@_,0,2);
  my ($s);
  my $logsum  = $LOG_ZERO;
  my $logprod = $LOG_ONE;
  my $nsents  = 0;
  my ($logp);
  while (defined($s=$cr->getSentence)) {
    $logp     = $model->sentenceProbability($s,@_[2..$#_]);
    $logsum   = plogadd($logp, $logsum);
    $logprod += $logp;
    ++$nsents;
  }
  return {logsum=>$logsum,logprod=>$logprod,nsents=>$nsents};
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
