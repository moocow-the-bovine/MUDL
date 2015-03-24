##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Filter::PdlFilter::Viterbi.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpus filter
##    pdl-ized Viterbi filter for MUDL::HMM
##======================================================================

package MUDL::Corpus::Filter::PdlFilter::Viterbi;
use MUDL::Corpus::Filter::PdlFilter;
use MUDL::Corpus::Buffer::PdlTT;
use MUDL::HMM;
use PDL;
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Filter::PdlFilter); #)

##======================================================================
## Constructors etc.

## $obj = $class_or_obj->new(%args)
##  + %args for MUDL::Corpus::Filter::PdlFilter::Viterbi
##     hmm       => $hmm,                  ##-- MUDL::HMM
##     fromAttr  => $fromAttr,             ##-- source attribute index [default: 0 (text)]
##     toAttr    => $toAttr,               ##-- sink attribute index [default: 0 (text)]
##
##     keepAttrs => \@attributeList,       ##-- which attributes to keep (undef or empty for all)
##     clobberTo => $bool,                 ##-- if true, old 'to' field will be clobbered (default:no)
##
##  + %args for Corpus::Filter::PdlFilter:
##     buffer  => $corpus_buffer_pdltt,    ##-- buffer for generic API
##     bwriter => $bufwriter,              ##-- buffer writer
sub new {
  my $that = shift;
  return $that->MUDL::Corpus::Filter::new(
					  ##-- attributes
					  fromAttr=>0,
					  toAttr=>0,
					  hmm   =>undef,

					  ##-- pruning
					  #keepAttrs=>undef,
					  #clobberTo=>0
					  @_,
					 );
}


##======================================================================
## Corpus::Filter::PdlFilter Methods & overrides

## undef = $profile->processPdlBuffer(%args)
##  + batch-process buffered data
sub processPdlBuffer {
  my $cf  = shift;
  my $buf = $cf->{buffer};
  my $hmm = $cf->{hmm};

  ##-- sanity checks
  die(ref($cf),"::processPdlBuffer(): no HMM defined!") if (!defined($hmm));
  die(ref($cf),"::processPdlBuffer(): no buffer!")      if (!defined($buf));

  ##-- basic data
  my ($fromAttr,$toAttr) = @$cf{'fromAttr','toAttr'};
  my $wpdl = $buf->{pdls}[$fromAttr];

  ##-- enums & translation pdls
  my $oenum = $hmm->{oenum};
  my $qenum = $hmm->{qenum};
  my $Nq    = $qenum->size;
  my $uid   = $oenum->{sym2id}{$hmm->{unknown}};
  my $w2o   = $buf->{enums}[$fromAttr]->xlatePdlTo($oenum,badval=>$uid); ## $wid => $oid
  my $paths = zeroes(($Nq < 255 ? byte : ($Nq < 65535 ? ushort : long)), $wpdl->nelem);

  ##-- batch-process sentences
  my @soff = ($buf->{begins}->list, $wpdl->nelem);
  my ($slice,$si,$s,$path);
  foreach $si (1..$#soff) {
    $slice = $soff[$si-1].":".($soff[$si]-1);
    $s     = $w2o->index( $wpdl->slice($slice) );
    $path  = $hmm->viterbiPath($s);
    $paths->slice($slice) .= $path;
  }

  ##-- add new 'paths' as attribute to buffer
  splice(@{$buf->{pdls}},  $toAttr, ($buf->{clobberTo} ? 1 : 0), $paths);
  splice(@{$buf->{enums}}, $toAttr, ($buf->{clobberTo} ? 1 : 0), $qenum);

  ##-- attribute pruning
  if (defined($cf->{keepAttrs}) && @{$cf->{keepAttrs}}) {
    @{$buf->{enums}} = @{$buf->{enums}}[ @{$cf->{keepAttrs}} ];
    @{$buf->{pdls}}  = @{$buf->{pdls}} [ @{$cf->{keepAttrs}} ];
  }

  return $cf;
}


##======================================================================
## Corpus::Filter::PdlFilter : API

# (inherited)


##======================================================================
## Help string

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return (
	  ''
	  ."Deterministic map, PDL-based.\n"
	  ."Options:\n"
	  ."  hmm=MUDL::HMM        [default=none]\n"
	  ."  fromAttr=ATTR        [default=0]\n"
	  ."  toAttr=ATTR          [default=0]\n"
	  ."  keepAttrs=>\\\@ATTRS [default=undef (all)]\n"
	  ."  clobberTo=>BOOL      [default=0]\n"
	 );
}

1; ##-- make perl happy
