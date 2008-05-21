#-*- Mode: CPerl -*-

## File: MUDL::PDL::Stats.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL statistical utilities
##======================================================================

package MUDL::PDL::Stats;
use PDL;
use Exporter;
use strict;

our @ISA = qw(Exporter);
our @EXPORT = qw();
our %EXPORT_TAGS =
  (
   all => [
	   'mean','variance','stddev',
	   'log2','logz','log2z','log10z',
	  ],
  );
our @EXPORT_OK = map {@$_} values(%EXPORT_TAGS);
$EXPORT_TAGS{all} = \@EXPORT_OK;


##======================================================================
## Variance, standard deviation

## $mean = $pdl->mean()
BEGIN {
  *mean = *PDL::mean = \&PDL::average;
}

## $var = $pdl->variance()
BEGIN {
  *PDL::variance0 = \&variance0;
  *PDL::variance1 = \&variance1;
  *PDL::variance = *variance = \&variance0; ##-- a tiny bit faster
  *PDL::CCS::Nd::variance    = \*variance0; ##--... variance1() doesn't work with CCS::Nd (no slices)
}
sub variance0 {
  my $p = shift;
  return ($p**2)->average - ($p->average**2);               ##-- Var(X) = E(X^2) - E(X)^2
}
sub variance1 {
  my $p = shift;
  return (($p - $p->average->slice("*1"))**2)->average;     ##-- Var(X) = E( (X - E(X))^2 )
}

## $stddev = $pdl->stddev()
BEGIN {
  *PDL::stddev          = \&stddev;
  *PDL::CCS::Nd::stddev = \&stddev;
}
sub stddev { return $_[0]->variance->sqrt(); }


##======================================================================
## Logarithms

## $pdl_log2 = $pdl->log2()
BEGIN {
  *PDL::log2 = *PDL::CCS::Nd::log2 = \&log2;
  our $LOG2  = log(2.0);
}
sub log2 {
  our ($LOG2);
  return log($_[0])/$LOG2 if (!ref($_[0]));
  if ($_[0]->is_inplace) {
    $_[0]->log();
    $_[0] /= $LOG2;
    return $_[0];
  }
  return $_[0]->log / $LOG2;
}

## $logz_pdl = $pdl->logz($z=0)
BEGIN { *PDL::logz = *PDL::CCS::Nd::logz = \&logz; }
sub logz {
  my $lp = $_[0]->log;
  $lp->inplace->setnantobad->inplace->setbadtoval(defined($_[1]) ? $_[1] : 0);
  return $lp;
}

## $log2z_pdl = $pdl->log2z($z=0)
BEGIN { *PDL::log2z = *PDL::CCS::Nd::log2z = \&log2z; }
sub log2z {
  my $lp = $_[0]->log2;
  $lp->inplace->setnantobad->inplace->setbadtoval(defined($_[1]) ? $_[1] : 0);
  return $lp;
}

## $log10z_pdl = $pdl->log10z($z=0)
BEGIN { *PDL::log10z = *PDL::CCS::Nd::log10z = \&log2z; }
sub log10z {
  my $lp = $_[0]->log10;
  $lp->inplace->setnantobad->inplace->setbadtoval(defined($_[1]) ? $_[1] : 0);
  return $lp;
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
