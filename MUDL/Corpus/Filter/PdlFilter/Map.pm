##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Filter::PdlFilter::Map.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus filter
##    pdl-ized map, for deterministic unigram taggers
##======================================================================

package MUDL::Corpus::Filter::PdlFilter::Map;
use MUDL::Corpus::Filter::PdlFilter;
use MUDL::Corpus::Buffer::PdlTT;
use PDL;
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Filter::PdlFilter); #)

##======================================================================
## Constructors etc.

## $obj = $class_or_obj->new(%args)
##  + %args for MUDL::Corpus::Filter::PdlFilter::Map
##     fromAttr  => $fromAttr,             ##-- source attribute index [default: 0 (text)]
##     toAttr    => $toAttr,               ##-- sink attribute index [default: 0 (text)]
##     toEnum    => $toEnum,               ##-- if defined, replaces $buffer->{enums}[$toAttr]
##     fromEnum  => $fromEnum,             ##-- if defined, indices from source pdl
##                                         ##       $buffer->{pdls}[$fromAttr]
##                                         ##   will be mapped to indices in $fromEnum
##                                         ##   via string matches
##                                         ##     $buffer->{enums}[$fromAttr] <-> $fromEnum
##     mapPdl    => $mapPdl,               ##-- pdl($nSourceValues)
##     mapBad    => $badval,               ##-- bad output value for map: default: $PDL::undefval
##     mapBadStr => $badstr,               ##-- bad output string for map: default: none
##                                         ##   + if defined, will be added to to $toEnum
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

					  ##-- enums
					  #fromEnum=>undef,
					  #toEnum=>undef

					  ##-- map
					  #mapPdl=>undef,
					  #mapBad=>undef,
					  mapBadStr=>'@UNKNOWN',

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

  ##-- basic data
  my ($fromAttr,$toAttr) = @$cf{'fromAttr','toAttr'};
  my $frompdl = $buf->{pdls}[$fromAttr];
  my $toEnum  = defined($cf->{toEnum}) ? $cf->{toEnum} : $buf->{enums}[$toAttr];

  ##-- get translation pdl
  my ($bfrom2mfrom);
  if (defined($cf->{fromEnum})) {
    $bfrom2mfrom = $buf->{enums}[$fromAttr]->xlatePdlTo($cf->{fromEnum}, badval=>-1);
  } else {
    $bfrom2mfrom = sequence(long,$buf->{enums}[$fromAttr]->size);
  }


  ##-- get bad value for output pdl
  my ($badval);
  if (defined($cf->{mapBadStr})) {
    $badval = $toEnum->addSymbol($cf->{mapBadStr});
  }
  $badval = $cf->{mapBad} if (defined($cf->{mapBad}));

  ##-- set output enum
  splice(@{$buf->{enums}}, $toAttr, ($buf->{clobberTo} ? 1 : 0), $toEnum);
  $buf->updatePdlTypes();

  ##-- get new output pdl
  my $topdl = zeroes($buf->{enums}[$toAttr]{pdltype}, $frompdl->nelem);
  $topdl   .= $badval;

  ##-- guts: actually map
  my $frompdl_isgood              = ($bfrom2mfrom >= 0)->index($frompdl);
  $topdl->where($frompdl_isgood) .= $cf->{mapPdl}->index($bfrom2mfrom->index($frompdl->where($frompdl_isgood)));
  splice(@{$buf->{pdls}}, $toAttr, ($buf->{clobberTo} ? 1 : 0), $topdl);

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
	  ."  fromAttr=ATTR        [default=0]\n"
	  ."  toAttr=ATTR          [default=0]\n"
	  ."  fromEnum=ENUM        [default=none (uses buffer enum)]\n"
	  ."  toEnum=ENUM          [default=none (uses buffer enum)]\n"
	  ."  mapPdl=PDL           [default=empty]\n"
	  ."  mapBad=>VAL          [default=toEnum->size+1]\n"
	  ."  mapBadStr=>VAL       [default='\@UNKNOWN']\n"
	  ."  keepAttrs=>\\\@ATTRS   [default=undef (all)]\n"
	 );
}

1; ##-- make perl happy
