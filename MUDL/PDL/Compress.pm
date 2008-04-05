#-*- Mode: CPerl -*-

## File: MUDL::PDL::Compress.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL utilities: semi-transparent compression/decompression via rle(),rld()
##======================================================================

package MUDL::PDL::Compress;
use Exporter;
use PDL;
use strict;

our @ISA = qw(Exporter);

our @EXPORT = qw(guessCompressionRatio);
our %EXPORT_TAGS =
  (
   default => [qw(guessCompressionRatio)],
   utils   => [qw(sample rlenz nbytes)],
   vars    => [qw($MAX_SAMPLE_SIZE $MIN_COMPRESSION_RATIO)],
  );
our @EXPORT_OK = map {@$_} values(%EXPORT_TAGS);
$EXPORT_TAGS{all} = \@EXPORT_OK;

our $MAX_SAMPLE_SIZE       = 1024;
our $MIN_COMPRESSION_RATIO = 0.9;


##======================================================================
## Compression Utilities

## $sample = sample($pdl)
## $sample = sample($pdl,$MAX_SAMPLE_SIZE)
#BEGIN { *PDL::sample = \&sample; }
sub sample {
  my ($p,$mss) = @_;
  $mss = $MAX_SAMPLE_SIZE if (!defined($mss));
  return $p->nelem < $mss || $mss <= 0 ? $p->flat : $p->flat->slice("0:".($mss-1));
}

## ($clens,$cvals) = rlenz($pdl)
BEGIN { *PDL::rlenz = \&rlenz; }
sub rlenz {
  my $p = shift;
  my ($l,$v) = $p->rle();
  return ($l->where($l),$v->where($l));
}

## $size_in_bytes = nbytes($pdl1,...)
BEGIN { *PDL::nbytes = \&nbytes; }
sub nbytes {
  my $size = 0;
  $size += $_->nelem * PDL::howbig($_->type) foreach (@_);
  return $size;
}

## $guessed_compression_ration = guessCompressionRatio($pdl)
## $guessed_compression_ration = guessCompressionRatio($pdl,$maxSampleSize)
BEGIN { *PDL::guessCompressionRatio = \&guessCompressionRatio; }
sub guessCompressionRatio {
  my ($p,$mss) = @_;
  my $s        = sample($p,$mss);
  my ($sl,$sv) = rlenz($s->flat);
  return (nbytes($sl)+nbytes($sv)) / nbytes($s);
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
