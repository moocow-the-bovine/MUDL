#-*- Mode: CPerl -*-

## File: MUDL::PDL::Smooth.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL Smoothing utilities
##======================================================================

package MUDL::PDL::Smooth;
use PDL;
use PDL::CCS;


##======================================================================
## Value Counts

## ($values,$valcounts) = $pdl->valcounts()  ##-- array context, suitable for PDL::primitive::interpol(ate)
##  + flat only (?)
BEGIN { *PDL::valcounts = \&valcounts; }
sub valcounts {
  my ($pdl,%opts) = @_;
  my ($counts,$values) = $pdl->flat->qsort->rle();
  my $counts_mask      = ($counts>0);
  return ($values->where($counts_mask),$counts->where($counts_mask));
}

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
  my ($cfit,$coeffs) = $Zc->log->linfit1d($Zc->ones->cat($v->log));
  $cfit->inplace->exp;
  $coeffs->slice("(0)")->inplace->exp;
  return ($cfit,$coeffs);
}

##======================================================================
## Good-Turing smoothing (TODO)


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
