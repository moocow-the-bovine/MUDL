##-*- Mode: Perl -*-

## File: MUDL::Trigrams.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: trigram distribution
##======================================================================

package MUDL::Trigrams;
use MUDL::Dist::Nary;
use MUDL::Unigrams;
use MUDL::Bigrams;
use MUDL::LogUtils qw(:all);
use Carp;
our @ISA = qw(MUDL::Dist::Nary MUDL::Corpus::Profile MUDL::Corpus::Model);

## OBJECT STRUCTURE:
##   + (from Dist::Nary): nz=>$dist, size=>$nevts, sizes=>\@sizes, nzero=>$nzero, zmass=>$zmass, sep=>$sep
##   + new:
##      bos=>$bos_marker    # default "__$"
##      eos=>$eos_marker    # default "__$"
##   + key structure:
##      "$w1.$sep.$w2.$sep.$w3"
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(
				     bos=>'__$',
				     eos=>'__$',
				     nfields=>3,
				     @_
				    ), ref($that)||$that;
  return $self;
}

##======================================================================
## Accessors

## @words = $tg->vocabulary()
sub vocabulary {
  my $tg = shift;
  return $tg->unigrams->vocabulary();
}

## size()
##   + returns potential number of bigrams
sub getSize { return scalar($_[0]->vocabulary)**3; }

## $unigrams = $tg->unigrams
sub unigrams {
  return bless $_[0]->project1(0), 'MUDL::Unigrams';
}

## $bigrams = $tg->bigrams
sub bigrams {
  return bless $_[0]->projectN(1,2), 'MUDL::Bigrams';
}

##======================================================================
## conditionalize

## $tg = $tg->conditionalize()
## $tg = $tg->conditionalize($bigrams)
## $tg = $tg->conditionalize($bigrams,$bgtotal)
##   + converts $tg->{nz} to { "$w1$fs$w2$fs$w3" => P($w3|$w1,$w2), ... }
sub conditionalize {
  my ($tg,$bg,$total) = @_;
  $bg = $tg->bigrams if (!$bg);
  $total = $bg->total if (!$total);
  my ($k,$f12,$w1,$w2,$w3);
  foreach $k (keys(%{$tg->{nz}})) {
    ($w1,$w2,$w3) = $tg->split($k);
    $f12 = $bg->{nz}{$w1.$bg->{sep}.$w2};
    if ($f12) {
      $tg->{nz}{$k} /= $f12;
    } else {
      carp (ref($tg), "::conditionalize(): no bigram probability for '$w1,$w2': set to zero.\n");
      $tg->{nz}{$k} = 0;
    }
  }
  return $tg;
}

##======================================================================
## metrics, etc

## $H = $tg->conditionalEntropy($bigrams)
## $H = $tg->conditionalEntropy($bigrams,$bgtotal)
## $H = $tg->conditionalEntropy($bigrams,$bgtotal,$tgtotal)
sub conditionalEntropy {
  my ($tg,$bg,$bgtotal,$tgtotal) = @_;
  $bg = $tg->bigrams if (!$bg);
  my $Hbi    = $bg->entropy($bgtotal);
  my $Hjoint = $tg->entropy($tgtotal);
  return $Hjoint - $Hbi;
}

##======================================================================
## Profiling

## undef = $tg->addReader($reader,%args)
sub addReader {
  my ($tg,$cr) = @_;
  my ($fs,$bos,$eos) = @$tg{qw(sep bos eos)};
  my ($s,$wm1,$wm2,$txt);
  while (defined($s=$cr->getSentence)) {
    next if (!@$s);
    $wm1 = $wm2 = $bos;
    foreach (@$s) {
      $txt = ref($_) ? $_->text : $_;
      if (!defined($txt)) {
	warn(ref($tg), "::addReader(): undefined token text!");
	next;
      }
      ++$tg->{nz}{$wm2.$fs.$wm1.$fs.$txt}; #if ($wm2 ne $bos);
      ($wm2,$wm1) = ($wm1,$txt);
    }
    ++$tg->{nz}{$wm2.$fs.$wm1.$fs.$eos};
    ++$tg->{nz}{$wm1.$fs.$eos.$fs.$eos};
  }
  return $tg;
}

## undef = $tg->addSentence($sent,%args)
sub addSentence {
  my ($tg,$s) = @_;
  return if (!@$s);
  my $wm1 = $tg->{bos};
  my $wm2 = $wm1;
  my ($txt);
  foreach (@$s) {
    $txt = ref($_) ? $_->text : $_;
    if (!defined($txt)) {
      warn(ref($tg), "::addSentence(): undefined token text!");
      next;
    }
    ++$tg->{nz}{$wm2.$tg->{sep}.$wm1.$tg->{sep}.$txt}; #if ($wm2 ne $tg->{bos});
    ($wm2,$wm1) = ($wm1,$txt);
  }
  ++$tg->{nz}{$wm2.$tg->{sep}.$wm1.$tg->{sep}.$tg->{eos}};
  return $tg;
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

## $log_psent = $model->sentenceProbability(\@sent,%args)
##   + $model should have been conditionalized
##   + %args: zeroCount=>$zeroCount ##-- obsolete
sub sentenceProbability {
  my ($tg,$s,%args) = @_;
  $args{zeroCount} = $tg->zeroCount if (!defined($args{zeroCount}));

  my $logp = $LOG_ONE;

  my $tm2 = $tg->{bos};
  my $tm1 = $tm2;
  my ($ptg,$txt);

  foreach $tok (@$s) {
    $txt   = ref($tok) ? $tok->text : $tok;
    $ptg   = $tg->{nz}{$tm2.$tg->{sep}.$tm1.$tg->{sep}.$txt};
    $ptg   = $args{zeroCount} if (!$ptg);

    $logp += log($ptg);     ##-- independence: multiply
    ($tm2,$tm1) = ($tm1,$txt);
  }
  ##-- eos
  $ptg   = $tg->{nz}{$tm2.$tg->{sep}.$tm1.$tg->{sep}.$tg->{eos}};
  $ptg   = $args{zeroCount} if (!$pbg);
  $logp += log($ptg);

  return $logp;
}


## $logprob = readerProbability($corpusREader,%args)
##   + $tg should have been conditionalized
##   + %args: zeroCount=>$zeroCount
sub readerProbability {
  my ($tg,$cr,%args) = @_;
  $args{zeroCount} = $tg->zeroCount if (!defined($args{zeroCount}));

  my $logsum = $LOG_ZERO;
  my ($tm1,$tm2,$logp,$ptg,$txt,$s);
  my $fs = $tg->{sep};

  while (defined($s=$cr->getSentence)) {
    $tm1 = $tm2 = $tg->{bos};
    $logp = $LOG_ONE;
    foreach $tok (@$s) {
      $txt   = ref($tok) ? $tok->text : $tok;
      $ptg   = $tg->{nz}{$tm2.$fs.$tm1.$fs.$txt};
      $ptg   = $args{zeroCount} if (!$ptg);
      $logp += log($ptg);
      ($tm2,$tm1) = ($tm1,$txt);
    }
    $ptg   = $tg->{nz}{$tm2.$fs.$tm1.$fs.$tg->{eos}};
    $ptg   = $args{zeroCount} if (!$ptg);
    $logp += log($ptg);

    $logsum = plogadd($logsum, $logp);
  }

  return $logsum;
}


##======================================================================
## Profile: Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return qq(Extract token-text trigrams.\n)
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
