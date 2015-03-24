#-*- Mode: Perl -*-

## File: MUDL::Corpus::Model.pm
## Author: Bryan Jurish <moocow@cpan.org>
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
##     logprod  => $log_product_of_sentprobs,
##     logsum   => $log_sum_of_sentprobs,
##     logprod  => $log_product_of_sentprobs,
##     #logwsum  => $log_sum_of_wordprobs,
##     nsents   => $number_of_sentences,
##     ntoks    => $number_of_tokens,
##     entropy  => $log_sum_of_p_times_negative_log2_p,
##     #wentropy => $sum_of_log2_wordprob,
##     ...

## \%info = $model->initInfo()
## \%info = $model->initInfo(\%info)
##  + initializes an information hash
sub initInfo {
  my ($model,$info)=@_;
  $info = {} if (!defined($info));

  %$info =
    (
     logsum  => $LOG_ZERO,
     #logwsum => $LOG_ZERO,
     logprod => $LOG_ONE,
     nsents  => 0,
     ntoks   => 0,
     entropy => $LOG_ZERO,
     #wentropy => 0,
    );

  return $info;
}


## \%info = $model->addInfo($log_sentence_probability,$sentence_length,\%info)
##  + adds relevant information to \%info
sub addInfo {
  my ($model,$logp,$slen,$info)=@_;
  #my $logp = $LOG_ONE;
  #foreach (@$logps) {
  #  $logp += $_;
  #  $info->{logwsum}   = plogadd($info->{logwsum}, $_);
  #  $info->{wentropy} += $_ / $LOG_TWO;
  #}
  $info->{logprod} += $logp;
  $info->{logsum}   = plogadd($info->{logsum}, $logp);
  $info->{entropy}  = plogadd($info->{entropy}, $logp + log( -($logp / $LOG_TWO) ));
  $info->{nsents}  ++;
  $info->{ntoks}   += $slen;
  return $info;
}


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
  my ($s,$logp);
  my $info = $model->initInfo();
  while (defined($s=$cr->getSentence)) {
    $logp = $model->sentenceProbability($s,@_[2..$#_]);
    $model->addInfo($logp, scalar(@$s), $info);
  }
  return $info;
}

## $log_psent = $model->sentenceProbability(\@sentence,@args)
##  + returns log(p(\@sentence))
##  + dummy method
*sentenceProbability = dummy('sentenceProbability');

## [$log_p1,...,$log_pN_given_p1_to_Nminus1] = $model->sentenceProbabilities(\@sentence,@args)
##  + returns log(p(\@sentence))
##  + dummy method: OBSOLETER!
#*sentenceProbabilities = dummy('sentenceProbabilities');


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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
