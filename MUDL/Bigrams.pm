##-*- Mode: Perl -*-

## File: MUDL::Bigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: bigram distribution
##======================================================================

package MUDL::Bigrams;
use MUDL::Dist::Nary;
use MUDL::Unigrams;
use MUDL::LogUtils qw(:all);
use Carp;
our @ISA = qw(MUDL::Dist::Nary MUDL::Corpus::Profile MUDL::Corpus::Model);

our $DEFAULT_ZERO_PROB = 1e-5;

## OBJECT STRUCTURE:
##   + (from Dist::Nary): nz=>$dist, size=>$nevts, sizes=>\@sizes, nzero=>$nzero, zmass=>$zmass, sep=>$sep
##   + new:
##      bos=>$bos_marker    # default "__$"
##      eos=>$eos_marker    # default "__$"
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(), ref($that)||$that;
  @$self{qw(bos eos nfields)} = ('__$', '__$', 2);
  return $self;
}

##======================================================================
## Accessors

## @words = $bg->vocabulary()
sub vocabulary {
  my $bg = shift;
  return $bg->unigrams->vocabulary();
}

## size()
##   + returns potential number of bigrams
sub getSize { return scalar($_[0]->vocabulary)**2; }

## $unigrams = $bg->unigrams
sub unigrams {
  return bless $_[0]->project1(0), 'MUDL::Unigrams';
}

##======================================================================
## conditionalize

## $bg = $bg->conditionalize()
## $bg = $bg->conditionalize($unigrams)
## $bg = $bg->conditionalize($unigrams,$totalunigrams)
##   + converts $bg->{nz} to { "$w1$fs$w2" => P($w2|$w1), ... }
sub conditionalize {
  my ($bg,$ug,$total) = @_;
  $ug = $bg->unigrams if (!$ug);
  $total = $ug->total if (!$total);
  my ($k,$f1,$w1,$w2);
  foreach $k (keys(%{$bg->{nz}})) {
    ($w1,$w2) = $bg->split($k);
    $f1 = $ug->{$w1};
    if ($f1) {
      $bg->{nz}{$k} /= $f1;
    } else {
      carp ( __PACKAGE__ , "::conditionalize(): no unigram probability for '$w1': set to zero.\n");
      $bg->{nz}{$k} = 0;
    }
  }
  return $bg;
}

##======================================================================
## metrics, etc

## $H = $bg->conditionalEntropy($unigrams)
## $H = $bg->conditionalEntropy($unigrams,$ugtotal)
## $H = $bg->conditionalEntropy($unigrams,$ugtotal,$bgtotal)
sub conditionalEntropy {
  my ($bg,$ug,$ugtotal,$bgtotal) = @_;
  $ug = $bg->unigrams if (!$ug);
  my $Huni   = $ug->entropy($ugtotal);
  my $Hjoint = $bg->entropy($bgtotal);
  return $Hjoint - $Huni;
}

##======================================================================
## Profiling

## undef = $bg->addReader($reader,%args)
sub addReader {
  my ($bg,$cr) = @_;
  my ($fs,$bos,$eos) = @$bg{qw(sep bos eos)};
  my ($s,$wprev,$txt);
  while (defined($s=$cr->getSentence)) {
    next if (!@$s);
    $wprev = $bos;
    foreach (@$s) {
      $txt = ref($_) ? $_->text : $_;
      if (!defined($txt)) {
	warn( __PACKAGE__ , "::addReader(): undefined token text!");
	next;
      }
      ++$bg->{nz}{$wprev.$fs.$txt};
      $wprev = $txt;
    }
    ++$bg->{nz}{$wprev.$fs.$eos};
  }
  return $bg;
}

## undef = $bg->addSentence($sent,%args)
sub addSentence {
  my ($bg,$s) = @_;
  return if (!@$s);
  my $wprev = $bg->{bos};
  my ($txt);
  foreach (@$s) {
    $txt = ref($_) ? $_->text : $_;
    if (!defined($txt)) {
      warn( __PACKAGE__ , "::addSentence(): undefined token text!");
      next;
    }
    ++$bg->{nz}{$wprev.$bg->{sep}.$txt};
    $wprev = $txt;
  }
  ++$bg->{nz}{$wprev.$bg->{sep}.$bg->{eos}};
  return $bg;
}



##======================================================================
## I/O: XML

##-- inherited (from MUDL::Dist::Nary)

##======================================================================
## I/O: TnT
##
## (not yet implemented -- but see MUDL::Ngrams for an implementation)


##======================================================================
## Modelling

## $log_p_sentence = $model->sentenceProbability(\@sent,%args)
##   + $model should have been conditionalized
##   + %args: zeroCount=>$zeroCount ##-- obsolete
sub sentenceProbability {
  my ($bg,$s,%args) = @_;
  $args{zeroCount} = $bg->zeroCount if (!defined($args{zeroCount}));
  $args{zeroCount} = $DEFAULT_ZERO_PROB if (!$args{zeroCount}); ##-- HACK

  my ($t1) = $bg->{bos};
  my ($pbg,$txt);
  my $logp = $LOG_ONE;
  foreach $tok (@$s) {
    $txt   = ref($tok) ? $tok->text : $tok;
    $pbg   = $bg->{nz}{$t1.$bg->{sep}.$txt};
    $pbg   = $args{zeroCount} if (!$pbg);

    $logp += log($pbg);
    $t1    = $txt;
  }
  ##-- eos
  $pbg   = $bg->{nz}{$t1.$bg->{sep}.$bg->{eos}};
  $pbg   = $args{zeroCount} if (!$pbg);
  $logp += log($pbg);

  return $logp;
}

## $log_psent = $model->sentenceProbability(\@sent,%args)
##   + $model should have been conditionalized
##   + %args: zeroCount=>$zeroCount ##-- obsolete
##   + OBSOLETE
sub sentenceProbability_old {
  my ($bg,$s,%args) = @_;
  $args{zeroCount} = $bg->zeroCount if (!defined($args{zeroCount}));
  $args{zeroCount} = $DEFAULT_ZERO_PROB if (!$args{zeroCount}); ##-- HACK

  my $logp = $LOG_ONE;
  my ($t1) = $bg->{bos};
  my ($pbg,$txt);

  foreach $tok (@$s) {
    $txt   = ref($tok) ? $tok->text : $tok;
    $pbg   = $bg->{nz}{$t1.$bg->{sep}.$txt};
    $pbg   = $args{zeroCount} if (!$pbg);

    $logp += log($pbg);     ##-- independence: multiply
    $t1    = $txt;
  }
  ##-- eos
  $pbg   = $bg->{nz}{$t1.$bg->{sep}.$bg->{eos}};
  $pbg   = $args{zeroCount} if (!$pbg);
  $logp += log($pbg);

  return $logp;
}


## \%info = readerProbability($corpusREader,%args)
##   + $bg should have been conditionalized
##   + %args: zeroCount=>$zeroCount
##   + OBSOLETE
sub readerProbability_old {
  my ($bg,$cr,%args) = @_;
  $args{zeroCount} = $bg->zeroCount if (!defined($args{zeroCount}));
  $args{zeroCount} = $DEFAULT_ZERO_PROB if (!$args{zeroCount}); ##-- HACK

  my $logsum  = $LOG_ZERO;
  my $logprod = $LOG_ONE;
  my ($t1,$logp,$pbg,$txt,$s);
  my $nsents = 0;
  my $ntoks = 0;
  my $entropy = 0;

  while (defined($s=$cr->getSentence)) {
    ++$nsents;
    $ntoks += scalar(@$s);

    $t1 = $bg->{bos};
    $logp = $LOG_ONE;
    foreach $tok (@$s) {
      $txt   = ref($tok) ? $tok->text : $tok;
      $pbg   = $bg->{nz}{$t1.$bg->{sep}.$txt};
      $pbg   = $args{zeroCount} if (!$pbg);

      $logp += log($pbg);
      $t1    = $txt;
    }
    $pbg   = $bg->{nz}{$t1.$bg->{sep}.$bg->{eos}};
    $pbg   = $args{zeroCount} if (!$pbg);
    $logp += log($pbg);

    ##-- sum, prod, entropy
    $logsum   = plogadd($logsum, $logp);
    $logprod += $logp;
    $entropy += exp($logp) * -($logp/$LOG_TWO);
  }

  return {logsum=>$logsum,logprod=>$logprod,nsents=>$nsents,ntoks=>$ntoks,entropy=>$entropy};
}


##======================================================================
## Profile: Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return qq(Extract token-text bigrams.\n)
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
