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

our @ISA = qw(MUDL::Object Exporter);

our @EXPORT = qw();
our %EXPORT_TAGS =
  (
   default => [qw(guessCompressionRatio)],
   utils   => [qw(sample rlenz nbytes)],
   vars    => [qw($MAX_SAMPLE_SIZE $MIN_COMPRESSION_RATIO)],
  );
our @EXPORT_OK = map {@$_} values(%EXPORT_TAGS);
$EXPORT_TAGS{all} = \@EXPORT_OK;

our $MAX_SAMPLE_SIZE = 1024;
our $MAX_Z_TO_RAW    = 0.9;


##======================================================================
## Compression Utilities

## $sample = sample($pdl)
## $sample = sample($pdl,$MAX_SAMPLE_SIZE)
#BEGIN { *PDL::sample = \&samplePdl; }
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

## $guessed_zsize_to_rawsize = guessPdlCompressionRatio($pdl)
## $guessed_zsize_to_rawsize = guessPdlCompressionRatio($pdl,$maxSampleSize)
BEGIN { *PDL::guessCompressionRatio = \&guessCompressionRatio; }
sub guessCompressionRatio {
  my ($p,$mss) = @_;
  my $s        = sample($p,$mss);
  my ($sl,$sv) = rlenz($s->flat);
  return (nbytes($sl)+nbytes($sv)) / nbytes($s);
}


##======================================================================
## Compressed storage

##--------------------------------------------------------------
## $zp = CLASS->new(pdl=>$p,%args)
##  + %args:
##     maxSampleSize => $nelts, ##-- default: $MAX_SAMPLE_SIZE
##     maxZtoRaw     => $float, ##-- default: $MAX_Z_TO_RAW
##  + other data keys:
##     zpdl => [\@dims, $plens,$pvals], ##-- after encode(), maybe after compress()
##     pdl  => $rawpdl,                 ##-- raw data pdl, after decode()
sub new {
  my ($that,%args) = @_;
  return bless({
		maxSampleSize  => $MAX_SAMPLE_SIZE,
		maxZtoRaw      => $MAX_Z_TO_RAW,
		%args,
	       }, ref($that)||$that)
}

##--------------------------------------------------------------
## Compression

## $zp = $zp->compress()
##  + compresses $zp if guessed compression ratio is <= $maxZtoRaw
sub compress {
  my $zp = shift;
  my $max_z2r = $zp->{maxZtoRaw};
  $max_z2r    = $MAX_Z_TO_RAW if (!defined($max_z2r));
  if (defined($max_z2r) && $max_z2r >= 0) {
    my $z2r = $zp->_guessCompressionRatio;
    $zp->encode() if ($z2r <= $max_z2r);
  }
  return;
}

##--------------------------------------------------------------
## Encoding / Decoding

## [\@dims,$pl,$pv] = $zp->encode()
##  + compresses $zp, deletes $p->{pdl} in favor of $p->{zpdl};
sub encode {
  my $zp = shift;
  my $p        = $zp->rawPdl;
  my ($pl,$pv) = rlenz($p->flat);
  $pl->sever;
  $pv->sever;
  $zp->{zpdl}  = [ [$p->dims], $pl,$pv ];
  delete($zp->{pdl});
  return $zp->{zpdl};
}

## $rawPdl = $zp->decode()
##  + decompresses $zp, deletes $p->{zpdl} in favor of $p->{pdl};
sub decode {
  my $zp = shift;
  my ($dims,$pl,$pv) = @{$zp->{zpdl}};
  my $p = $pl->rld($pv);
  $p->reshape(@$dims);
  delete($zp->{zpdl});
  return $zp->{pdl} = $p;
}


##--------------------------------------------------------------
## Access

## $pdl = $zp->rawPdl()
##  + returns $zp->{pdl} if defined
##  + otherwise, decodes $zp->{zpdl} if defined
##  + otherwise returns null pdl
sub rawPdl {
  my $zp = shift;
  return $zp->{pdl}  if (defined($zp->{pdl}));
  return $zp->decode if (defined($zp->{zpdl}));
  return null;
}


##--------------------------------------------------------------
## Sampling

## $sample = $zp->sample_()
sub _sample {
  my $zp = shift;
  my $p   = $zp->rawPdl();
  my $mss = $zp->{maxSampleSize};
  $p      = null if (!defined($p));
  $mss    = $MAX_SAMPLE_SIZE if (!defined($mss));
  return $p->nelem < $mss || $mss <= 0 ? $p->flat : $p->flat->slice("0:".($mss-1));
}

## $guessed_compression_ratio = $zp->guessZtoR()
sub _guessCompressionRatio {
  my $zp = shift;
  my $s  = $zp->_sample();
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
