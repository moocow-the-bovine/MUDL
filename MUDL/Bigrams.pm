#-*- Mode: Perl -*-

## File: MUDL::Bigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: bigram distribution
##======================================================================

package MUDL::Bigrams;
use MUDL::Dist;
use MUDL::Unigrams;
use MUDL::XML;
use Carp;
use IO::File;
our @ISA = qw(MUDL::Dist::Partial);
our $VERSION = 0.01;

## OBJECT STRUCTURE:
##   + (from Dist::Partial): nz=>$dist, N=>$nevts, zmass=>$zmass
##   + new:
##      fs=>$fieldSeparator # default "\t"
##      bos=>$bos_marker    # default "__$"
##      eos=>$eos_marker    # default "__$"
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(), ref($that)||$that;
  @$self{qw(fs bos eos)} = ("\t", '__$', '__$');
  return $self;
}

## copy
sub copy {
  my $bg = shift;
  my $bg2 = $bg->SUPER::copy();
  @$bg2{qw(fs bos eos)} = @$bg{qw(fs bos eos)};
  return $bg2;
}

##======================================================================
## Accessors

## @words = $bg->vocabulary()
*V = *vocab = *Sigma = *sigma = \&vocabulary;
sub vocabulary {
  my $bg = shift;
  return $bg->unigrams->vocabulary();
}

## size()
##   + returns potential number of bigrams
sub getSize { return $_[0]->{size} = scalar($_[0]->vocabulary)**2; }

## $unigrams = $bg->unigrams
sub unigrams {
  my $bg = shift;
  my $ug = MUDL::Unigrams->new();
  my $fs = $bg->{fs};
  my ($key,$t,$f,$i);
  while (($key,$f)=each(%{$bg->{nz}})) {
    $i = index($key,$fs,0);
    next if ($i < 0); ##-- no separator: bad data
    $ug->{substr($key,0,$i)} += $f;
    if ($bg->{bos} ne $bg->{eos} && $key =~ /\Q$fs$bg->{eos}\E$/) {
      $ug->{eos} += $f;
    }
  }
  return $ug;
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
    ($w1,$w2) = split($bg->{fs}, $k, 2);
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
sub conditionalEntropy0 {
  my ($bg,$ug,$ugtotal,$bgtotal) = @_;
  $ug = $bg->unigrams if (!$ug);
  $ugtotal = $ug->total if (!$ugtotal);
  $bgtotal = $bg->total if (!$bgtotal);
  my ($k,$p1,$p12,$w1,$w2);
  my $H = 0;
  foreach $k (keys(%{$bg->{nz}})) {
    ($w1,$w2) = split($bg->{fs}, $k, 2);
    #$p1 = $ug->{$w1} / $ugtotal;
    $p2 = $ug->{$w2} / $ugtotal;
    $p12 = $bg->{nz}{$k} / $bgtotal;
    if ($p12 && $p2) {
      $H += $p12 * log($p2/$p12)/log(2);
    } else {
      carp ( __PACKAGE__ , "::conditionalEntropy(): missing data for '$w1,$w2': ignoring.\n");
    }
  }
  my $nz = $bg->nZero;
  $H += $bg->{zmass} * log($bgtotal/($bg->{zmass}/$nz))/log(2) if ($bg->{zmass} && $nz);
  return $H;
}

##======================================================================
## learn from a corpus

## $bg = $bg->addCorpus($corpus,%args)
sub addCorpus {
  my ($bg,$corpus) = @_;
  my ($fs,$bos,$eos) = @$bg{qw(fs bos eos)};
  my ($s,$wprev,$txt);
  foreach $s (@{$corpus->sentences}) {
    $wprev = $bos;
    #++$bg->{nz}{$bos};
    foreach (@$s) {
      $txt = ref($_) ? $_->{text} : $_;
      ++$bg->{nz}{$wprev.$fs.$txt};
      #++$bg->{nz}{$txt};
      $wprev = $txt;
    }
    ++$bg->{nz}{$wprev.$fs.$eos};
    #++$bg->{nz}{$eos};
  }
  return $bg;
}

## $bg = $bg->addReader($reader,%args)
sub addReader {
  my ($bg,$cr) = @_;
  my ($fs,$bos,$eos) = @$bg{qw(fs bos eos)};
  my ($s,$wprev,$txt);
  while (defined($s=$cr->getSentence)) {
    next if (!@$s);

    $wprev = $bos;
    #++$bg->{nz}{$bos};
    foreach (@$s) {
      $txt = ref($_) ? $_->{text} : $_;
      if (!defined($txt)) {
	warn( __PACKAGE__ , "::addReader(): undefined token text!");
	next;
      }
      ++$bg->{nz}{$wprev.$fs.$txt};
      #++$bg->{nz}{$txt};
      $wprev = $txt;
    }
    ++$bg->{nz}{$wprev.$fs.$eos};
    #++$bg->{nz}{$eos};
  }
  return $bg;
}


##======================================================================
## I/O: XML

## $node = $obj->saveXMLNode()
sub saveXMLNode {
  my $obj = shift;
  (my $nodename = ref($obj)) =~ s/::/./g;
  my $node = XML::LibXML::Element->new($nodename);
  $node->appendTextChild('fs',$obj->{fs});
  $node->appendTextChild('bos',$obj->{bos});
  $node->appendTextChild('eos',$obj->{eos});
  $node->appendChild($obj->SUPER::saveXMLNode(@_));
  return $node;
}

## $obj = $obj->loadXMLNode($node)
sub loadXMLNode {
  my ($obj,$node) = @_;
  (my $nodename = ref($obj)) =~ s/::/./g;
  carp( __PACKAGE__ , "::loadXMLNode() expected '$nodename' element, got '", $node->nodeName, "'\n")
    if ($node->nodeName ne $nodename);

  my $subnode;
  $subnode = ($node->getChildrenByTagName('fs'))[0];
  $obj->{fs} = $subnode->textContent;

  $subnode = ($node->getChildrenByTagName('bos'))[0];
  $obj->{bos} = $subnode->textContent;

  $subnode = ($node->getChildrenByTagName('eos'))[0];
  $obj->{eos} = $subnode->textContent;

  $obj->SUPER::loadXMLNode($subnode->nextSibling);
  return $obj;
}

##======================================================================
## I/O: TnT
##
## (not yet implemented)

##======================================================================
## Modelling

## $logprob = $bg->sentenceProbability(\@sent,%args)
##   + $bg should have been conditionalized
##   + %args: zeroCount=>$zeroCount
*sp = *sP = *sentp = *sentP = *sentProb = *sentprob = *sentenceProb = \&sentenceProbability;
sub sentenceProbability {
  my ($bg,$s,%args) = @_;
  $args{zeroCount} = $bg->zeroCount if (!defined($args{zeroCount}));
  my $p = 0;
  my ($t1) = $bg->{bos};
  my ($pbg,$txt);

  foreach $tok (@$s) {
    $txt = ref($tok) ? $tok->{text} : $tok;
    $pbg = $bg->{nz}{$t1.$bg->{fs}.$txt};
    $pbg = $args{zeroCount} if (!$pbg);
    $p += log($pbg);
    $t1 = $txt;
  }
  $pbg = $bg->{nz}{$t1.$bg->{fs}.$bg->{eos}};
  $pbg = $args{zeroCount} if (!$pbg);
  $p += log($pbg);

  return $p;
}

## $logprob = corpusProbability($corpus,%args)
##   + $bg should have been conditionalized
##   + %args: zeroCount=>$zeroCount
*corpusp = *corpusP = *corpusprob = *corpusProb = \&corpusProbability;
sub corpusProbability {
  my ($bg,$corpus,%args) = @_;
  $args{zeroCount} = $bg->zeroCount if (!defined($args{zeroCount}));
  my $p = 0;
  my ($t1,$pbg,$txt,$s);

  foreach $s (@{$corpus->{sents}}) {
    $t1 = $bg->{bos};
    foreach $tok (@$s) {
      $txt = ref($tok) ? $tok->{text} : $tok;
      $pbg = $bg->{nz}{$t1.$bg->{fs}.$txt};
      $pbg = $args{zeroCount} if (!$pbg);
      $p += log($pbg);
      $t1 = $txt;
    }
    $pbg = $bg->{nz}{$t1.$bg->{fs}.$bg->{eos}};
    $pbg = $args{zeroCount} if (!$pbg);
    $p += log($pbg);
  }

  return $p;
}

## $logprob = readerProbability($corpusREader,%args)
##   + $bg should have been conditionalized
##   + %args: zeroCount=>$zeroCount
*readerp = *readerP = *readerrob = *readerProb = \&readerProbability;
sub readerProbability {
  my ($bg,$cr,%args) = @_;
  $args{zeroCount} = $bg->zeroCount if (!defined($args{zeroCount}));
  my $p = 0;
  my ($t1,$pbg,$txt,$s);

  while (defined($s=$cr->getSentence)) {
    $t1 = $bg->{bos};
    foreach $tok (@$s) {
      $txt = ref($tok) ? $tok->{text} : $tok;
      $pbg = $bg->{nz}{$t1.$bg->{fs}.$txt};
      $pbg = $args{zeroCount} if (!$pbg);
      $p += log($pbg);
      $t1 = $txt;
    }
    $pbg = $bg->{nz}{$t1.$bg->{fs}.$bg->{eos}};
    $pbg = $args{zeroCount} if (!$pbg);
    $p += log($pbg);
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
