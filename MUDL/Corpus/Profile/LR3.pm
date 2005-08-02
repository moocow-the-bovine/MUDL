##-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::LR3.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R trigram profiles
##======================================================================

package MUDL::Corpus::Profile::LR3;
use MUDL::Corpus::Profile::LR;
use MUDL::Enum::Nary::Flat;
use PDL;
use MUDL::CmdUtils qw();
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       uprof    => $lrprof,       ##-- MUDL::Corpus::Profile::LR subclass
##                                  ##   : underlying l/r profile over words : REQUIRED
##       targets  => $tg3_enum,     ##-- target TRIGRAMS (flat)
##  + event structure:
##       $lr->{$dir \in qw(left right)} : ($target_elt_id, @nbf_bounds) = $lr->{dir}->split($event);
sub new {
  my ($that,%args) = @_;

  my $targets = $args{targets} || MUDL::Enum::Nary::Flat->new(nfields=>3);
  delete(@args{qw(targets)});

  my $self = $that->SUPER::new(
			       uprof=>undef, ##-- must be instantiated by the user
			       targets=>$targets,
			       norm_min=>0,
			       %args,
			      );

  MUDL::CmdUtils::loadModule(ref($self->{uprof}))
      if (defined($self->{uprof}));

  return $self;
}

##======================================================================
## Shadow

## $lr2 = $lr->shadow(%args)
##  + return new profile of same form
##    - empty {left},{right} distributions
##    - everything else copied
sub shadow {
  my $lr = shift;
  my $uprof  = $lr->{uprof};
  return $lr->copy() if (!defined($uprof));

  MUDL::CmdUtils::loadModule(ref($uprof));

  my $uprof2 = $uprof->shadow();
  delete($lr->{uprof});
  my $lr2 = $lr->copy(uprof=>$uprof2,@_);
  $lr->{uprof}=$uprof;
  return $lr2;
}


##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + dispatch to underlying LR-profile
sub addSentence {
  return $_[0]{uprof}->addSentence($_[1]);
}

## undef = $profile->addBigrams($bigrams)
##  + dispatch to underlying LR-profile
sub addBigrams {
  MUDL::CmdUtils::loadModule(ref($_[0]{uprof}));
  return $_[0]{uprof}->addBigrams($_[1]);
}

## undef = $profile->finish()
##  + dispatch to underlying LR-profile
##  + calls bootstrap()
sub finish {
  my $lr3 = shift;
  MUDL::CmdUtils::loadModule($lr3->{uprof});
  return $lr3->{uprof}->finish(@_);
}


##======================================================================
## Context-Tagger Utilities
##======================================================================

##--------------------------------------------------------------
## $lr3 = $lr3->reset()
##  + clears $lr3->{uprof}, $lr3->{uprof}{targets}
##  + leaved  $lr3->{uprof}{bounds} alone!
sub reset {
  my $lr3 = shift;
  $lr3->{uprof}->reset();
  $lr3->{uprof}{targets}->clear();
  return $lr3;
}

##--------------------------------------------------------------
## $lr3 = $lr3->setTargets($targets3_enum)
##  + computes $targets1, an enum of $targets3 components
##  + adds $targets1 to $uprof->{targets}
##  + to be called after reset()
sub setTargets {
  my ($lr3,$targets3) = @_;
  $lr3->{targets} = $targets3;
  my $targets1 = $lr3->{uprof}{targets};
  $targets1->addSymbol($_) foreach ($targets3->allComponents);
  return $lr3;
}



##======================================================================
## Conversion: to PDL

## $pdl_2d = $lr->toPDL()
## $pdl_2d = $lr->toPDL($pdl_3d)
##   + converts to pdl
##   + notational convention: target trigrams are triples (w_1,w_2,w_3)
##   + returned pdl is of dimensions: ($d,$n), where:
##     - $n == number-of-targets
##     - $d == 4 * $ number-of-bounds  ##-- w_{1,r} . w_{2,l} . w_{2,r} . w_{3,l}
##   + may call the following:
##     - undef   = $lr->normalizePdl($pdl_3d)
##   + $pdl_3d is of dimensions (4, $nbounds, $n) [separated trigram elemtn R- and L-components]
##     - $pdl_3d->slice("0,,") contains data for w_{1,r} [1st trigram element, right-bounds]
##     - $pdl_3d->slice("1,,") contains data for w_{2,l} [2nd trigram element,  left-bounds]
##     - $pdl_3d->slice("2,,") contains data for w_{2,r} [2nd trigram element, right-bounds]
##     - $pdl_3d->slice("3,,") contains data for w_{3,l} [3rd trigram element,  left-bounds]
sub toPDL {
  my ($lr,$pdl) = @_;
  $pdl = $lr->toPDL3d($pdl);
  $pdl->reshape($pdl->dim(0)*$pdl->dim(1), $pdl->dim(2));
  return $pdl;
}

## $pdl_3d = $lr->toPDL3d()
## $pdl_3d = $lr->toPDL3d($pdl_3d)
##   + converts to pdl
##   + returned $pdl_3d is of dimensions: ($z=4,$d,$n), where:
##     - $n == number-of-targets
##     - $d == number-of-bounds           ##-- left-bounds & right-bounds
##   + z-indexing:
##     - $z==0 : data for w_{1,r} [1st trigram element, right-bounds]
##     - $z==1 : data for w_{2,l} [2nd trigram element,  left-bounds]
##     - $z==2 : data for w_{2,r} [2nd trigram element, right-bounds]
##     - $z==3 : data for w_{3,l} [3rd trigram element,  left-bounds]
##   + may call the following:
##     - undef = $lr->normalizePdl($pdl_3d)
##   + default implementation calls $lr->{uprof}->toPDL3d()
sub toPDL3d {
  my ($lr,$pdl) = @_;

  MUDL::CmdUtils::loadModule(ref($lr->{uprof}));

  ##-- enum
  my $eb       = $lr->{uprof}{bounds};
  my $et       = $lr->{uprof}{targets};
  my $et3      = $lr->{targets};
  my $neb      = $eb->size;
  my $net3     = $et3->size;

  ##-- pdl
  $pdl = zeroes(double,1) if (!defined($pdl));
  $pdl->reshape(4, $neb, $net3)
    if ($pdl->ndims < 3 || $pdl->dim(0) < 4 || $pdl->dim(1) < $neb || $pdl->dim(2) < $net3);
  $pdl .= 0;

  ##-- get underlying pdl
  $lr->{uprof}{norm_min} = $lr->{norm_min} if (defined($lr->{norm_min}));
  my $updl = $lr->{uprof}->toPDL3d(undef);

  ##-- instantiate trigram pdl: index pdls
  my $w1ip = zeroes(long, $et3->size);
  my $w2ip = zeroes(long, $et3->size);
  my $w3ip = zeroes(long, $et3->size);
  my ($tgi, $w1i,$w2i,$w3i);
  foreach $tgi (0..($et3->size-1)) {
    ($w1i,$w2i,$w3i) = map { $et->index($_) } $et3->split($et3->symbol($tgi));
    $w1ip->set($tgi,$w1i);
    $w2ip->set($tgi,$w2i);
    $w3ip->set($tgi,$w3i);
  }

  ##-- instantiate trigram pdl: copy data
  $pdl->slice("0,,")   .= $updl->dice_axis(2,$w1ip)->slice("1,,"); ##-- w1,right
  $pdl->slice("1:2,,") .= $updl->dice_axis(2,$w2ip);               ##-- w2,left . w2,right
  $pdl->slice("3,,")   .= $updl->dice_axis(2,$w3ip)->slice("0,,"); ##-- w3,left


  ##-- smoothing
  $lr->smoothPdl($pdl) if ($lr->can('smoothPdl'));

  ##-- data munging
  $lr->finishPdl($pdl) if ($lr->can('finishPdl'));

  ##-- normalization
  $lr->normalizePdl($pdl) if ($lr->can('normalizePdl'));

  return $pdl;
}


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(High-level class for left-/right-trigram profiles using underlying word profiles.\n)
     .qq(Options:\n)
     .qq(  targets=FLAT_ENUM      [default=empty]\n)
     .qq(  uprof  =WORD_PROFILE   [default=none]\n)
    );
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
