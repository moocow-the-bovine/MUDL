#-*- Mode: CPerl -*-

## File: MUDL::PDL::Export.pm
## Author: Bryan Jurish <moocow@cpan.org>
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

##======================================================================
## I/O: import: text: flat: 1d

## $rc = $pdl->loadCSV(%opts)
##  + uses PDL::IO::Misc::rcols
##  + %opts:
##     file   => $file_or_fh,
##     cols   => \@col_index_array
##     header => \$str,           ##-- read from 1st line of file if present
##     ... other opts are passed to PDL::IO::Misc::rcols
BEGIN { *PDL::loadCSV = *PDL::loadDat = *loadDat = \&loadCSV; }
sub loadCSV {
  my ($pdl,%opts) = @_;
  my $file = $opts{file};
  $file    = \*STDIN if (!defined($file));
  my $fh   = ref($file) ? $file : IO::File->new("<$file");
  delete($opts{file});

  my @dog  = ($pdl->ndims > 1 ? ($pdl->dog) : ($pdl));
  my @cols = defined($opts{cols}) ? @{$opts{cols}} : (0..$#dog);
  if (defined($opts{header})) {
    my $tmp = '';
    my $hdr_ref = ref($opts{header}) ? $opts{header} : \$tmp;
    $$hdr_ref = <$fh>;
  }
  my @pdls = rcols($fh, @cols, \%opts);
  $dog[$_] .= $pdls[$_] foreach (0..$#dog);

  $fh->close() if (!ref($file));
  return 1;
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
