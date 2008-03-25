#-*- Mode: CPerl -*-

## File: MUDL::PDL::Stats.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL statistical utilities
##======================================================================

package MUDL::PDL::Stats;
use PDL;

use strict;

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
BEGIN { *PDL::stddev = \&stddev; }
sub stddev { return $_[0]->variance->sqrt(); }


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
