#-*- Mode: CPerl -*-

## File: MUDL::PDL::Smooth.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL Smoothing utilities
##======================================================================

package MUDL::PDL::Smooth;
use MUDL::PDL::Ranks;
use PDL;
use PDL::CCS;

use strict;

##======================================================================
## Value Counts

## ($values,$valcounts) = $pdl->valcounts()  ##-- array context, suitable for PDL::primitive::interpol(ate)
##  + flat only (?)
##  + returned PDLs are suitable for back-fitting into $pdl indices with:
##    $i2valcount = $pdl->interpol($values,$valcounts);
##  + see interpol() and interpolate() in PDL::Primitive
BEGIN { *PDL::valcounts = \&valcounts; }
sub valcounts {
  my ($pdl,%opts) = @_;
  my ($counts,$values) = $pdl->flat->qsort->rle();
  my $counts_mask      = ($counts>0);
  return ($values->where($counts_mask),$counts->where($counts_mask));
}

##======================================================================
## Value Smearing (GT-style)

## ($v_smeared) = $vals->smearvals();
## ($v_smeared) = $vals->smearvals($keys);
## + as for MUDL::Dist::smear() (as used by MUDL::Dist::smoothGTLogLin())
BEGIN { *PDL::smearvals = \&smearvals; }
sub smearvals {
  my ($vals,$r) = @_;

  my ($r_qsi,$Nr);
  if (!defined($r)) {
    $r     = $vals->xvals;
    $r_qsi = $r->sequence;
    $Nr    = $vals;
  } else {
    $r_qsi = $r->qsorti;
    $r     = $r->index($r_qsi);
    $Nr    = $vals->index($r_qsi);
  }
  my $r_lo = $r->slice("0:-2")->append(0)->rotate(1);
  my $r_hi = $r->rotate(-1)->slice("0:-2")->append(0);
  $r_hi->slice("-1") .= 2*$r->slice("-1") - $r->slice("-2");

  #my $Zr  = 2*$Nr->double / ($r_hi-$r_lo);
  ##--
  my $Zrv = $vals->zeroes->double;
  $Zrv->index($r_qsi) .= 2*$Nr / ($r_hi-$r_lo);

  return $Zrv;
}

##======================================================================
## Zipf fit

## ($zipf_constant,$freq_fit) = zipf_fit($freq_pdl)   ##-- array context
## $zipf_constant             = zipf_fit($freq_pdl)   ##-- scalar context
## + fits $freq_pdl to best Zipfian distribution
##     $freq = $zipf_constant / $rank_desc
BEGIN { *PDL::zipf_fit = \&zipf_fit; }
sub zipf_fit {
  my $freq    = shift;
  my $f_ranks = $freq->ranks(order=>'desc')+1;
  my $total   = ($freq*$f_ranks)->sumover;
  my $nitems  = pdl(double,$freq->nelem);
  my $zipf_c  = $total / $nitems;
  return $zipf_c if (!wantarray);
  my $freq_fit = $zipf_c / $f_ranks;
  return ($zipf_c,$freq_fit);
}

##======================================================================
## Log-linear fit

## ($fit,$coeffs) = $vals->loglinfit()
## ($fit,$coeffs) = $vals->loglinfit($keys)
##  + $keys defaults to $vals->xvals()+1
##  + $fit are log-linear fitted values $vals as values for $keys
##  + $coeffs are [$a,$e] such that all($yfit == $a*($keys**$e))
##  + example:
##     use MUDL::PDL::Smooth;
##     $f1 = corpus_unigram_frequencies();
##     ($v,$c) = $f1->valcounts();
##     $Zc     = $c->smearvals($v);
##     ($fitc,$coeffs) = $Zc->loglinfit($v);
##
##     %plot = (axis=>'logxy',xtitle=>'freq',ytitle=>'count(freq)');
##     points($v->log10, $Zc->log10,2,{%plot,color=>'red'}); hold;
##     points($v->log10, $c->log10, 4); hold;
##     line($v->log10, ($coeffs->index(0)*($v**$coeffs->index(1)))->log10, {color=>'blue'});
##     release;
BEGIN { *PDL::loglinfit = \&loglinfit; }
use PDL::Fit::Linfit;
sub loglinfit {
  my ($Zc,$v) = @_;
  $v = ($Zc->xvals+1)->double if (!defined($v));
  my ($cfit,$coeffs) = $Zc->log->linfit1d($Zc->ones->cat($v->log->setnantobad->setbadtoval(0)));
  $cfit->inplace->exp;
  $coeffs->slice("(0)")->inplace->exp;
  return ($cfit,$coeffs);
}

##======================================================================
## Good-Turing smoothing

## ($valfit_pdl,$coeffs_pdl,$zmass) = $pdl->smoothGTLogLin()
## ($valfit_pdl,$coeffs_pdl,$zmass) = $pdl->smoothGTLogLin($minval=1)
##  + $minval is the minimum (1) value for GT-smoothing (default=1)
##  + in scalar context, returns only $valfit_pdl
BEGIN { *PDL::smoothGTLogLin = \&smoothGTLogLin; }
sub smoothGTLogLin {
  my ($pdl,$vmin) = @_;
  $vmin = 1 if (!defined($vmin));

  my ($v,$c) = $pdl->valcounts();
  my $Zc     = $c->double->smearvals($v);
  my ($fitc,$coeffs) = $Zc->loglinfit($v);
  my $S_a = $coeffs->index(0);
  my $S_e = $coeffs->index(1);

  my $p_fit  = zeroes(double,$pdl->dims);
  my $p_mask = ($pdl>=$vmin);
  my $p_srcv = $pdl->where($p_mask);
  my $p_fitv = $p_fit->where($p_mask);
  $p_fitv   .= ($p_srcv+1)*($S_a*($p_srcv+1)**$S_e) / ($S_a * $p_srcv**$S_e);

  my $N1    = $c->where($v<=$vmin)->sumover->double;
  my $zmass = $N1/$c->flat->sumover->double;

  my $z_mask = $p_mask->not;
  $p_fit->where($z_mask) .= $zmass / $z_mask->which->nelem if ($z_mask->any);

  return wantarray ? ($p_fit,$coeffs,$zmass) : $p_fit;
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

Copyright (c) 2008, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
