#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::LRCPMI.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: conditional-weighted L-R mutual information
##======================================================================

package MUDL::Corpus::Profile::LRCPMI;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use PDL;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile::LRBigrams);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
##       left=>$left_bigrams,       ## ($target,$lneighbor)
##       right=>$right_bigrams,     ## ($target,$rneighbor)
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>1,%args);
  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + inherited

## undef = $profile->finish()
sub finish {
  my $pr = shift;
  $pr->dist2mi($pr->{left});
  $pr->dist2mi($pr->{right});
  return $pr;
}

## $midist = $pr->dist2mi($bgdist)
##   + computes pointwise-mi distribution over targets from a bigram distribution
sub dist2mi {
  my ($pr,$dist) = @_;

  $dist->normalize;
  my $Pt = $dist->project1(0);
  my $Pb = $dist->project1(1);

  my $Pmi = MUDL::Dist->new();

  my ($event,$ptb,$t,$b);
  while (($event,$ptb)=each(%{$dist->{nz}})) {
    ($t,$b) = $dist->split($event);
    $pt = $Pt->{$t};
    $pb = $Pb->{$b};
    if ($ptb && $pt && $pb) {
      $Pmi->{$event} = ($ptb/$pt) * log($ptb/($pt*$pb))/log(2);
    } else {
      $Pmi->{$event} = 0;
    }
  }

  return $dist->{nz} = $Pmi;
}

##======================================================================
## Conversion: to PDL

##-- inherited from MUDL:::Corpus::Profile::LR


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract L/R- conditional-weighted MI profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
}

##======================================================================
## I/O: Native
## - (output only!)

## $bool = $obj->saveNativeFh($fh,%args)
sub saveNativeFh {
  my ($obj,$fh) = @_;
  $obj->{left}->toDist->saveNativeFh($fh,@_);
  $fh->print("\n\n\n");
  $obj->{right}->toDist->saveNativeFh($fh,@_);
  return $obj;
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
