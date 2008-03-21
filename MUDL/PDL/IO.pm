#-*- Mode: CPerl -*-

## File: MUDL::PDL::Export.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL utilities: IO
##======================================================================

package MUDL::PDL::IO;
use PDL;
use PDL::IO::Misc;
use IO::File;

use strict;

##======================================================================
## I/O: export: text: flat: 1d

## $rc = $pdl->flat->saveCSV(%opts)
##  + uses PDL::IO::Misc::wcols
##  + %opts:
##     file   => $file_or_fh,
##     header => $str,
##     format => $sprintf_format,
BEGIN { *PDL::saveCSV = *PDL::saveDat = *saveDat = \&saveCSV; }
sub saveCSV {
  my ($pdl,%opts) = @_;
  my $file = $opts{file};
  $file    = \*STDOUT if (!defined($file));
  my $fh   = ref($file) ? $file : IO::File->new(">$file");

  my @pdls    = ($pdl->ndims > 1 ? ($pdl->dog) : ($pdl));
  my $wc_args = { (defined($opts{header}) ? (HEADER=>$opts{header}) : qw()) };
  my $fmt     = defined($opts{format}) ? $opts{format} : join("\t", map {"%.18g"} @pdls);
  my $rc      = wcols($fmt, @pdls, $fh, $wc_args);

  $fh->close() if (!ref($file));
  return $rc;
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
