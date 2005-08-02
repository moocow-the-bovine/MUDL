#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::LRHB.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R conditional entropy (Bernoulli)
##======================================================================

package MUDL::Corpus::Profile::LRHB2;
use MUDL::Corpus::Profile::LRHB;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile::LRHB);

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
  return $that->SUPER::new(nfields=>1,donorm=>1,norm_min=>0,%args);
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + inherited

##======================================================================
## Conversion: to PDL

##-- inherited from MUDL:::Corpus::Profile::LR
## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl,%args)
##
## $pdl3d = $lr->smoothPdl($pdl3d,%args);


## $pdl3d = $lr->finishPdl($pdl3d,%args);
sub finishPdl {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  my ($P_t, $P_bg, $P_nbg);
  foreach my $z (0,1) {
    $P_bg     = $pdl->slice("($z),,");
    $P_bg    /= $P_bg->sumover->slice("*1,"); ##-- normalize by row

    ##-- zero-check
    $P_bg->inplace->setnantobad->inplace->setbadtoval(0);

    ##-- halve probabilities so that Bernoulli entropy will be monotonic: p'(b|t) = p(b|t)/2
    $P_bg   /= 2;

    ##-- get inverse probabilities: p'(�b|t) = 1 - p(b|t)/2
    $P_nbg    = 1-$P_bg;

    ## $P_bg .= -($P_bg*log($P_bg)/log(2) + $P_nbg*log($P_nbg)/log(2));
    $P_bg *= log($P_bg);
    $P_bg /= pdl(double,2)->log;

    $P_nbg *= log($P_nbg);
    $P_nbg /= pdl(double,2)->log;

    $P_bg  += $P_nbg;
    $P_bg  *= -1;
  }
  $pdl->inplace->setnantobad->inplace->setbadtoval(0); ##-- sanity check

  return $pdl;
}

## undef = $lr->normalizePdl($pdl);
##-- inherited


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right-MI profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
     .qq(  smoothgt=WHICH   [default=0] : one of 'bigrams','pdl',0\n)
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
