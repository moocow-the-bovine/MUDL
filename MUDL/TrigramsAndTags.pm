##-*- Mode: Perl -*-

## File: MUDL::TrigramsAndTags.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: trigram distribution + w-1 tags
##    - data-wallowing
##======================================================================

package MUDL::TrigramsAndTags;
use MUDL::Dist::Nary;
use MUDL::Unigrams;
use MUDL::Bigrams;
use MUDL::LogUtils qw(:all);
use Carp;
our @ISA = qw(MUDL::Dist::Nary MUDL::Corpus::Profile);

## OBJECT STRUCTURE:
##   + (from Dist::Nary): nz=>$dist, size=>$nevts, sizes=>\@sizes, nzero=>$nzero, zmass=>$zmass, sep=>$sep
##   + new:
##      bos=>$bos_marker    # default "__$"
##      eos=>$eos_marker    # default "__$"
##   + key structure
##      $w1.$sep.$w2.$sep.$w3.$sep.$tag2
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(), ref($that)||$that;
  @$self{qw(bos eos nfields)} = ('__$', '__$', 4);
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
*conditionalize = MUDL::Object::dummy('conditionalize');
sub conditionalizeOld {
  my ($tg,$bg,$total) = @_;
  $bg = $tg->bigrams if (!$bg);
  $total = $bg->total if (!$total);
  my ($k,$f12,$w1,$w2,$w3,$t2);
  foreach $k (keys(%{$tg->{nz}})) {
    ($w1,$w2,$w3,$t2) = $tg->split($k);
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
*conditionalEntropy = MUDL::Object::dummy('conditionalEntropy');
sub conditionalEntropyOld {
  my ($tg,$bg,$bgtotal,$tgtotal) = @_;
  $bg         = $tg->bigrams if (!$bg);
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
  my ($s,$wm1,$wm2,$txt,$tagm1);
  while (defined($s=$cr->getSentence)) {
    next if (!@$s);
    $wm1 = $wm2 = $tagm1 = $bos;
    foreach (@$s) {
      $txt = ref($_) ? $_->text : $_;
      if (!defined($txt)) {
	warn(ref($tg), "::addReader(): undefined token text!");
	next;
      }
      ++$tg->{nz}{$wm2.$fs.$wm1.$fs.$txt.$fs.$tagm1}; #if ($wm2 ne $bos);
      ($wm2,$wm1,$tagm1) = ($wm1,$txt, (ref($_) ? $_->tag : '(NOTAG)'));
    }
    ++$tg->{nz}{$wm2.$fs.$wm1.$fs.$eos.$fs.$tagm1};
  }
  return $tg;
}

## undef = $tg->addSentence($sent,%args)
sub addSentence {
  my ($tg,$s) = @_;
  return if (!@$s);
  my $wm1 = $tg->{bos};
  my $wm2 = $wm1;
  my $tagm1 = $wm1;
  my ($txt);
  foreach (@$s) {
    $txt = ref($_) ? $_->text : $_;
    if (!defined($txt)) {
      warn(ref($tg), "::addSentence(): undefined token text!");
      next;
    }
    ++$tg->{nz}{$wm2.$tg->{sep}.$wm1.$tg->{sep}.$txt.$tg->{sep}.$tagm1}; #if ($wm2 ne $tg->{bos});
    ($wm2,$wm1,$tm1) = ($wm1,$txt, (ref($_) ? $_->tag : '(NOTAG)'));
  }
  ++$tg->{nz}{$wm2.$tg->{sep}.$wm1.$tg->{sep}.$tg->{eos}.$tg->{sep}.$tagm1};
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


##======================================================================
## Profile: Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return qq(Data wallowing: extract token-text trigrams with central tag.\n)
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
