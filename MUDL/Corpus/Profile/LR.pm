#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LR.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R profiles
##======================================================================

package MUDL::Corpus::Profile::LR;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use MUDL::EDist;
use MUDL::Object;
use PDL;
use PDL::Fit::Linfit;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile);

#======================================================================
## $lr = $class_or_obj->new(%args)
##  + %args:
##     eos => $eos_str,
##     bos => $bos_str,
##     targets => $targets_enum,
##     bounds => $bounds_enum,
##     donorm => $do_normalize,
##     nfields => $number_of_bound_fields, # ($nbf)
##  + acquired data
##     left => $left_edist,
##     right=> $right_edist,
##  + event structure:
##     $lr->{$dir \in qw(left right)} : ($target_id, @nbf_bounds) = $lr->{dir}->split($event);
sub new {
  my ($that,%args) = @_;

  my $bounds = $args{bounds} || MUDL::Enum->new();
  my $targets = $args{targets} || MUDL::Enum->new();
  my $nfields = $args{nfields} || 1;
  my $enum = $args{enum} || MUDL::Enum::Nary->new(enums=>[$targets,(map { $bounds } (1..$nfields))]);
  delete(@args{qw(bounds targets nfields enum)});

  my $self = $that->SUPER::new
    (eos=>'__$',
     bos=>'__$',
     ##-- targets, bounds
     targets=>$targets,
     bounds=>$bounds,
     nfields=>$nfields,
     enum=>$enum, ##-- nary enum: [targets, bounds, ... , bounds]

     ##-- frequency distributions
     left=>MUDL::EDist::Nary->new(nfields=>($nfields+1), enum=>$enum),
     right=>MUDL::EDist::Nary->new(nfields=>($nfields+1), enum=>$enum),

     donorm=>1, ##-- normalize on pdl-ization ?
     %args);

  return $self;
}

##======================================================================
## Reset

## $lr = $lr->reset()
##   + resets profile distributions
sub reset {
  my $lr = shift;
  $lr->{left}->clear  if (defined($lr->{left}));
  $lr->{right}->clear if (defined($lr->{right}));
  return $lr;
}

##======================================================================
## Shadow

## $lr2 = $lr->shadow(%args)
##  + return new profile of same form
##    - empty {left},{right} distributions
##    - everything else copied
sub shadow {
  my $lr = shift;

  ##-- save temps
  my (%nztmp);
  foreach (qw(left right)) {
    $nztmp{$_} = $lr->{$_}{nz};
    $lr->{$_}{nz} = ref($nztmp{$_})->new();
  }

  ##-- copy
  my $lr2 = $lr->copy(@_);

  ##-- restore temps
  $lr->{$_}{nz} = $nztmp{$_} foreach (qw(left right));

  return $lr2;
}

##======================================================================
## $lr = $lr->setEnums($targets,$bounds)
##   + sets all relevant target- & bound-enums
sub setEnums {
  my ($lr,$tgs,$bds) = @_;
  foreach (@{$lr->{enum}{enums}}) {
    $_ = $bds if ($_ eq $lr->{bounds});
    $_ = $tgs if ($_ eq $lr->{targets});
  }
  $lr->{targets} = $tgs;
  $lr->{bounds}  = $bds;

  return $lr;
}

##======================================================================
## Profiling: API

## undef = $profile->addBuffer($corpus_buffer)
##  + inherited: calls addReader()

## undef = $profile->addReader($corpus_buffer)
##  + inherited: iteratively calls addSentence()

## undef = $profile->addSentence($mudl_sentence)
##  + REQUIRED

## undef = $profile->finish()
##  + optional

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##-- not implemented here!

## undef = $profile->addBigrams($mudl_bigrams)
##-- not implemented here!

## undef = $profile->addPdlBigrams($pdldist_sparse2d)
##-- not implemented here

##======================================================================
## Conversion: to independent PDL
##
## + Function call tree:
##    toPDL($pdl2d)              # top-level: returns pdl(       $d, $n)
##    -> toPDL3d($pdl3d)         # directed:  returns pdl($z, $d/$z, $n) ~ pdl(2, $d/2, $n)
##                               #  + (bad) assumes EDist {left} and {right}
##       -> smoothPdl($pdl3d)    # smooths each direction sub-pdl in $pdl3d
##       -> finishPdl($pdl3d)    # hook for subclasses
##       -> normalizePdl($pdl3d) # normalizes pdl values, only called if ($lr->{donorm})
##
## + Want:
##   - PDL-ization
##     * store underlying LR profiles as PdlDist::Sparse2d
##   - Fast profiling (for Corpus::Buffer::PdlTT)
##   - KEEP backwards-compatibility
##     * add __methods__ to get old {left},{right} EDists
##     * re-implement compatible addSentence() and addBigrams() routines (?)
##   - MetaProfile friendliness (?)
##     * add method somewhere somehow to do EXPECTED profiling
##       ~ e.g. given {phat} and a base profile, convert to cluster-based profile
##       ~ maybe even do this DIRECTLY, to save some disk space & I/O time?
##======================================================================

## $pdl_2d = $lr->toPDL()
## $pdl_2d = $lr->toPDL($pdl_2d)
## $pdl_2d = $lr->toPDL($pdl_2d,%args)
##   + converts to pdl
##   + returned pdl is of dimensions: ($d,$n), where:
##     - $n == number-of-targets
##     - $d == 2 * (number-of-bounds ^ $nfields)   ##-- left-bounds & right-bounds
##   + may call the following:
##     - undef = $lr->finishPdl($pdl_3d)
##     - undef = $lr->normalizePdl($pdl_3d)
##   + $pdl_3d is of dimensions (2, $d/2, $n) [separated R- and L-components]
##   + WARNING:
##     - passing $pdl_2d probably won't work unless it already has correct dimensions!
##   + %args: clobbers %$lr
*toPDLi = \&toPDL;
sub toPDL {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args; ##-- args: clobber
  $pdl = $lr->toPDL3d($pdl);
  $pdl->inplace->reshape($pdl->dim(0)*$pdl->dim(1), $pdl->dim(2)); ##-- not clump, since might be inplace
  #$pdl = $pdl->clump(2);                                          ##-- easier
  return $pdl;
}

## $pdl_3d = $lr->toPDL3d()
## $pdl_3d = $lr->toPDL3d($pdl_3d,%args)
##   + converts to pdl
##   + returned $pdl_3d is of dimensions: (2,$d,$n), where:
##     - $n == number-of-targets
##     - $d == (number-of-bounds ^ $nfields)   ##-- left-bounds & right-bounds
##   + may call the following:
##     - undef = $lr->finishPdl($pdl_3d)
##     - undef = $lr->normalizePdl($pdl_3d)
##   + %args:
##     clobber %$lr
sub toPDL3d {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args; ##-- args: clobber

  ##-- enum
  my $nfields  = $lr->{nfields};
  my ($eb,$et) = @$lr{qw(bounds targets)};
  my $net      = $et->size;
  my $neb      = $eb->size;

  ##-- pdl
  $pdl = zeroes(double,1) if (!defined($pdl));
  $pdl->reshape(2, ($neb**$nfields), $net)
    if ($pdl->ndims < 3 || $pdl->dim(0) < 2 || $pdl->dim(1) < ($neb**$nfields) || $pdl->dim(2) < $net);
  $pdl .= 0;

  ##-- frequency data: left-context
  my ($k,$v,$tid,@bids,$i);
  while (($k,$v)=each(%{$lr->{left}{nz}})) {
    ($tid,@bids) = $lr->{left}->split($k);
    foreach $i (0..$#bids) {
      $pdl->slice('0,' . ($i*$neb + $bids[$i]) . ',' . $tid) += $v;
    }
  }
  ##-- frequency data: right-context
  while (($k,$v)=each(%{$lr->{right}{nz}})) {
    ($tid,@bids) = $lr->{right}->split($k);
    foreach $i (0..$#bids) {
      $pdl->slice('1,' . ($i*$neb + $bids[$i]) . ',' . $tid) += $v
    }
  }

  ##-- smoothing
  $lr->smoothPdl($pdl) if ($lr->can('smoothPdl'));

  ##-- data munging
  $lr->finishPdl($pdl) if ($lr->can('finishPdl'));

  ##-- normalization
  $lr->normalizePdl($pdl) if ($lr->{donorm});

  return $pdl;
}

## $pdl_3d = $lr->smoothPdl($pdl_3d,%args)
##  + smooth a frequency pdl (3d)
##  + relevant flags in $lr:
##     smoothgt          => $which, ##-- perform Good-Turing smoothing here if $which eq 'pdl'
##     norm_zero_f       => $value, ##-- unnormalized zero value
##     norm_zero_zero    => $zero,  ##-- zero value to normalize
sub smoothPdl {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  if ($lr->{smoothgt} && $lr->{smoothgt} eq 'pdl') {
    my ($dpdl, $dnz,$dnzi, $dnr,$dr, $dnrwi, $N);
    my ($r,$nr, $r_hi,$r_lo, $zr);
    my ($logr, $logz, $nrfit,$coeffs);
    my ($S_a,$S_e, $nzrp1,$Enzr,$Enzrp1);
    my ($nrzero,$dim);
    foreach $dim (0..($pdl->dim(0)-1)) {
      ##-- get count-counts
      $dpdl  = $pdl->slice("$dim,:,:");      ##-- direction pdl
      $dnz   = $dpdl->where($dpdl!=0);       ##-- flat non-zero counts
      $dnzi  = $dnz->qsorti;                 ##-- flat non-zero count indices, sorted
      $N     = $dnz->sum;
      ($dnr,$dr) = $dnz->index($dnzi)->rle;  ##-- $dnr(i) = count(f=$dr(i))

      ##-- smear count-counts: Zr ~ E(Nr)
      $dnrwi = $dnr->which;                  ##-- indices of non-zero count-counts
      $nr    = $dnr->index($dnrwi);
      $r     = $dr->index($dnrwi);
      $r_lo  = zeroes(double, $r->dims);
      $r_hi  = zeroes(double, $r->dims);

      $r_lo .= $r->rotate(1);
      $r_lo->set(0,0);

      $r_hi .= $r->rotate(-1);
      $r_hi->set(-1, $r->at(-1)+($r->at(-1)-$r_lo->at(-1)));

      $zr    = pdl(double,2)*$nr;
      $r_hi -= $r_lo;
      $zr   /= $r_hi;

      ##-- smooth: fit
      $logr = $r->log;
      $logz = $zr->log;
      ($nrfit,$coeffs) = linfit1d($logz, cat(ones($logr->nelem), $logr));

      ##-- smooth: assign
      $S_a = $coeffs->slice('(0)')->exp;
      $S_e = $coeffs->slice('(1)');

      ##-- smoothing: denominator: E(N_r) = $S_a * $r**$S_e
      $Enzr = $S_a * $dnz;
      $Enzr->inplace->pow($S_e);

      ##-- smoothing: numerator: E(N_{r+1}): ($S_a * ($r+1)**$S_e)
      #$nzrp1   = $dnz + 1;
      $nzrp1   = $dnz + $dnz->minimum; ##-- HACK
      $Enzrp1  = $nzrp1->pow($S_e);
      $Enzrp1 *= $S_a;

      ##-- smoothing: smoothed values: r* = (r+1) * E(N_{r+1}) / E(N_r)
      $dnz .= $Enzrp1 / $Enzr;
      $dnz *= $nzrp1;

      ##-- smoothing: zero values
      $nrzero = $S_a * $nrfit->slice('(0)') / $N;
      $dpdl->where($dpdl==0) .= $nrzero / ($dpdl->nelem - $dnz->nelem);
    }
  }
  elsif (defined($lr->{norm_zero_f})) {
    ##-- direct value smoothing (used by LRBigrams descendendants with $smoothgt eq 'bigrams'
    my $zeroval = $lr->{norm_zero_zero};
    $zeroval = 0 if (!defined($zeroval));
    $pdl->where($pdl==$zeroval) .= $lr->{norm_zero_f};
  }

  return $pdl;
}


##--------------------------------------------------------------
## PDL-ization: normalization

## $pdl_3d = $lr->normalizePdl($pdl_3d)
##  + normalize a pdl (3d)
##  + relevant flags in $lr:
##     norm_independent => $bool,  ##-- whether to normalize left and right subvectors independently
##     norm_nan         => $value, ##-- value for -nan- & bad values (applied first) [default=0]
##     norm_zero_zero   => $zero,  ##-- zero value to feed into normalization method
##     norm_zero_p      => $value, ##-- post-normalized zero value
##     norm_nan_p       => $value, ##-- post-normalized -nan- & bad value (may be undef)
##     norm_min         => $min,   ##-- subtracted value: default=$pdl->min (for norm_how='pmass')
##     norm_how         => $how,   ##-- one of 'pmass', 'amass', 'alimits', 'slimits' : default='pmass'
##  + saves computed:
##     norm_min_prev    => $min,   ##-- for reversibility
##     norm_how_prev    => $how,   ##-- ditto
sub normalizePdl {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  ##-- bad & nan normalization: preliminary
  my ($norm_nan);
  $norm_nan    = 0 if (!defined($norm_nan=$lr->{norm_nan}));
  $pdl->inplace->setnantobad->inplace->setbadtoval($norm_nan);

  ##-- how
  my $norm_how = $lr->{norm_how};
  $norm_how    = 'pmass' if (!defined($norm_how));
  $lr->{norm_how_prev} = $norm_how;

  if ($norm_how eq 'pmass') {
    ##-- normalize by positive mass: force positive values
    my $min = $lr->{norm_min};
    $min  = $pdl->min if (!defined($min));
    $pdl -= $min      if ($min < 0);
    $lr->{norm_min_prev} = $min;
  }

  ##-- normalization: method
  my $norm_sub = $lr->can($norm_how);
  $norm_sub = $lr->can("normpdl_${norm_how}") if (!defined($norm_sub));
  confess(ref($lr), "::normalizePdl(): unknown normalization method '$norm_how'")
    if (!defined($norm_sub));

  ##-- normalization: guts
  my $norm_ind = !defined($lr->{norm_independent}) || $lr->{norm_independent};
  my ($z,$pz);
  if ($norm_ind) {
    ##-- normalize left- & right- subvectors independently
    foreach $z (0..($pdl->dim(0)-1)) {
      $pz = $pdl->slice("($z),:,:");
      $norm_sub->($lr, $pdl->slice("($z),:,:"));
    }
  }
  else {
    ##-- normalize left- & right- subvectors jointly
    $norm_sub->($lr, $pdl->clump(2));
  }

  ##-- zero (post-normalized)
  if (defined($lr->{norm_zero_p})) {
    my $zero = $lr->{norm_zero_zero};
    $zero = 0 if (!$zero);
    $pdl->where($pdl==$zero) .= $lr->{norm_zero_p};
  }

  ##-- nan (post-normalized)
  $pdl->inplace->setnantobad;
  $pdl->inplace->setbadtoval($lr->{norm_nan_p})
    if (defined($lr->{norm_nan_p}));

  return $pdl;
}

##--------------------------------------------------------------
## PDL-ization: normalization: subs

## undef = $lr->normpdl_pmass($pdl_2d);
##  + normalize $pdl rows by positive mass: pseudo-probabilities
sub normpdl_pmass {
  my ($lr,$pz) = @_;
  $pz /= sumover($pz)->slice("*1,");
  $pz->inplace->setnantobad->inplace->setbadtoval(0); ##-- bad value handling: zero
}

## undef = $lr->normpdl_amass($pdl_2d)
##  + normalize $pdl rows by absolute mass
##  + total absolute mass per row == 1
sub normpdl_amass {
  my ($lr,$pz) = @_;
  $pz /= sumover(abs($pz))->slice("*1,");
  $pz->inplace->setnantobad->inplace->setbadtoval(0); ##-- bad value handling: zero
}

## $lr->normpdl_alimits($pdl_2d)
##  + normalize $pdl rows by absolute limit
##  + most extreme component gets (+/-) 1
sub normpdl_alimits {
  my ($lr,$pz) = @_;
  $pz /= maximum(abs($pz))->slice("*1,");
  $pz->inplace->setnantobad->inplace->setbadtoval(0); ##-- bad value handling: zero
}

## $lr->normpdl_slimits($pdl_2d)
##  + normalize $pdl rows by stretched limits
##  + min-limit gets -1, max-limit gets 1, for every row
sub normpdl_slimits {
  my ($lr,$pz) = @_;
  my $pgt0 = $pz>0;              ##-- positive component mask
  my $plt0 = $pz<0;              ##-- negative component mask
  my $pmax = maximum($pz*$pgt0); ##-- positive extremes (fit to +1)
  my $pmin = minimum($pz*$plt0); ##-- negative extremes (fit to -1)
  $pz /= $pgt0*$pmax->slice("*1,") - $plt0*$pmin->slice("*1,");
  $pz->inplace->setnantobad->inplace->setbadtoval(0); ##-- sanity check
}



##--------------------------------------------------------------
## PDL-ization: normalization: OLD

sub normalizePdlOld {
  my ($lr,$pdl) = @_;

  my $min = $lr->{norm_min};
  $min  = $pdl->min if (!defined($min));
  $pdl -= $min      if ($min != 0);

  ##-- normalization
  my $norm_ind = !defined($lr->{norm_independent}) || $lr->{norm_independent};
  my ($ni,$v,$sum);
  foreach $ni (0..($pdl->dim(2)-1)) {
    ##-- normalize left- & right- subvectors

    if ($norm_ind) {
      ##-- ... independently : left subvector
      $v    = $pdl->slice("0,:,$ni");
      $sum  = $v->sum;
      $v   /= $sum if ($sum != 0);

      ##-- ... independently : right subvector
      $v    = $pdl->slice("1,:,$ni");
      $sum  = $v->sum;
      $v   /= $sum if ($sum != 0);
    }
    else {
      ##-- ... jointly
      $v    = $pdl->slice(":,:,$ni");
      $sum  = $v->sum;
      $v   /= $sum if ($sum != 0);
    }
  }

  ##-- zero (pre-normalized)
  if (defined($lr->{norm_zero_p})) {
    my $zero = $lr->{norm_zero_zero};
    $zero = 0 if (!$zero);
    $pdl->where($pdl==$zero) .= $lr->{norm_zero_p};
  }

  return $pdl;
}

##======================================================================
## I/O: libcluster
__PACKAGE__->registerIOMode('libcluster-data',
			    {saveFh=>\&saveLibclusterDataFh,});
__PACKAGE__->registerFileSuffix('\.lcd\.txt', 'libcluster-data');

## $bool = $lr->saveLibclusterDataFh($fh,%args)
##  + %args:
##      pdl => $pdl_2d   ##-- pdl to save
sub saveLibclusterDataFh {
  my ($lr,$fh,%args) = @_;

  #require PDL::IO::Misc;
  my $pdl = defined($args{pdl}) ? $args{pdl} : $lr->toPDL;
  my $tgs = $lr->{targets};
  my $bds = $lr->{bounds};

  my @bsyms = map { $bds->symbol($_) } (0..($pdl->dim(0)/2 -1));
  $fh->print(join("\t ", 'UNIQID', (map { ("$_:L","$_:R") } @bsyms)), "\n");
  my ($tid);
  foreach $tid (0..($pdl->dim(1)-1)) {
    $fh->print(join("\t ",
		    $tgs->symbol($tid),
		    $pdl->slice(",($tid)")->list),
	       "\n");
  }

  return $lr;
}


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Abstract class for left-/right-profiles wrt. a given boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  enum=ENUM_NARY   [default=new(enums=>[TARGETS, BOUNDS x N-1])]\n)
     .qq(  nfields=N        [default=2]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
     .qq(  norm_min=VALUE   [default=minimum]\n)
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
