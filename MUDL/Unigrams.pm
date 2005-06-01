##-*- Mode: Perl -*-

## File: MUDL::Unigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: unigram distribution
##======================================================================

package MUDL::Unigrams;
use MUDL::Corpus::Profile;
use MUDL::Corpus::Model;
use MUDL::LogUtils qw(:all);
use MUDL::Dist;
use Carp;
our @ISA = qw(MUDL::Dist MUDL::Corpus::Profile MUDL::Corpus::Model);

our $DEFAULT_ZERO_PROB = 1e-3;
#our $DEFAULT_ZERO_PROB = 1e-4;
#our $DEFAULT_ZERO_PROB = 1e-5;
#our $DEFAULT_ZERO_PROB = 1e-6;
#our $DEFAULT_ZERO_PROB = 1e-7;

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
sub addSentence {
  ++$_[0]->{ref($_) ? $_->text : $_} foreach (@{$_[1]});
}


##======================================================================
## I/O: Native
##
## (inherited from MUDL::Dist)

##======================================================================
## Modelling

## $log_p_sentence = $model->sentenceProbability(\@sent,%args)
##   + %args : 'unknown'=>$unknown_symbol
##             'punknown'=>p($unknown_symbol)
sub sentenceProbability {
  my ($ug,$s,%args) = @_;
  $args{unknown} = '@UNKNOWN' if (!defined($args{unknown}));
  $ug->{$args{unknown}} = $DEFAULT_ZERO_PROB if(!defined($ug->{$args{unknown}}));  ##-- HACK
  my ($tok,$txt,$txtp);
  my $logp = $LOG_ONE;
  foreach $tok (@$s) {
    $txt   = ref($tok) ? $tok->text : $tok;
    $txtp  = $ug->{$args{unknown}} if (!defined($txtp=$ug->{$txt}));
    $txtp  = 0 if (!defined($txtp));
    $logp += log($txtp);
  }
  return $logp;
}

## $log_psent = $model->sentenceProbability(\@sent,%args)
##   + %args : 'unknown'=>$unknown_symbol
##             'punknown'=>p($unknown_symbol)
##   + OBSOLETE!
sub sentenceProbability_old {
  my ($ug,$s,%args) = @_;
  $args{unknown} = '@UNKNOWN' if (!defined($args{unknown}));
  $ug->{$args{unknown}} = $DEFAULT_ZERO_PROB if(!defined($ug->{$args{unknown}}));  ##-- HACK
  my ($tok,$txt,$txtp);
  foreach $tok (@$s) {
    $txt   = ref($tok) ? $tok->text : $tok;
    $txtp  = $ug->{$args{unknown}} if (!defined($txtp=$ug->{$txt}));
    $txtp  = 0 if (!defined($txtp));
    $logp += log($txtp);                     ##-- independent trials: multiply
  }
  return $logp;
}


## \%info = $model->readerProbability($corpusReader,%args)
##   + %args : 'unknown'=>$unknown_symbol (default='@UNKNOWN')
##   + OBSOLETE
#*readerp = *readerP = *readerrob = *readerProb = \&readerProbability;
sub readerProbability_old {
  my ($ug,$cr,%args) = @_;
  $args{unknown} = '@UNKNOWN' if (!defined($args{unknown}));
  $ug->{$args{unknown}} = $DEFAULT_ZERO_PROB if(!defined($ug->{$args{unknown}})); ##-- HACK

  my ($s,$tok,$txt,$txtp);
  my $logps = [];
  while (defined($s=$cr->getSentence)) {
    $logp = $LOG_ONE;
    foreach $tok (@$s) {
      $txt   = ref($tok) ? $tok->text : $tok;
      $txtp  = $ug->{$args{unknown}} if (!defined($txtp=$ug->{$txt}));
      $txtp  = 0 if (!defined($txtp));

      $logp += log($txtp);                     ##-- independent trials: multiply
      $wentropy += $logp;
    }
    $logsum   = plogadd($logsum, $logp);
    $logprod += $logp;
    $entropy += exp($logp) * -($logp/$LOG_TWO);
    ++$nsents;
    $ntoks += scalar(@$s);
  }
  return {
	  logsum=>$logsum,
	  logprod=>$logprod,
	  nsents=>$nsents,
	  ntoks=>$ntoks,
	  entropy=>$entropy,
	  wentropy=>$wentropy
	 };
}

##======================================================================
## Profile: Help

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
