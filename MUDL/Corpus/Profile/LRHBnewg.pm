##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRHBnewg.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile:
##    : H_{Bernoulli}(f,n*2)
##======================================================================

package MUDL::Corpus::Profile::LRHBnewg;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile::LRBigrams); #)

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
  return $that->SUPER::new(nfields=>1,
			   donorm=>0,
			   norm_min=>0,
			   %args);
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
  my ($lr,$pdl) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  my ($H_bg, $P_bg, $P_nbg);
  my $log2 = pdl(double,2)->log;
  my $f_tug = $lr->targetUgPdl();
  foreach my $z (0,1) {
    $H_bg = $pdl->slice("($z),,");

    ##-- get conditional bigram probabilities: p(b|t)
    $P_bg  = $H_bg / $f_tug->slice("*1,");
    $P_bg *= 0.5; ##-- halve "success" probabilities to ensure monotonicity

    ##-- get negated probabilities: p(¬b|t) = 1 - p(b|t)
    $P_nbg  = 1.0-$P_bg;
    #$P_nbg *= 0.5; ##-- ... but DON'T halve the negated probabilities!
                    ##   + justification (attempt): half of the events we've observed
                    ##     have been in THE OTHER DIRECTION!

    ##-- get Bernoulli entropy estimates
    # H(b|t) = -p(b|t)*log(p(b|t)) - p(¬b|t)*log(p(¬b|t))
    $P_bg  *= -log($P_bg);
    $P_bg  /= $log2;

    $P_nbg *= -log($P_nbg);
    $P_nbg /= $log2;

    ##-- set entropy
    $H_bg .= $P_bg;
    $H_bg += $P_nbg;
  }
  $pdl->inplace->setnantobad->inplace->setbadtoval(0);

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
    (qq(Extract (local) left- and right- Bernoulli-entropy profile wrt. fixed boundary set.\n)
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
