#-*- Mode: CPerl -*-
## File: MUDL::Corpus::Profile::LRegamma.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R frequency: elias gamma coding
##======================================================================

package MUDL::Corpus::Profile::LRegamma;
use MUDL::Corpus::Profile::LRFBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Profile::LRFBigrams); #)

##======================================================================
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>0,%args);
}

##======================================================================
## Conversion: to PDL

## $pdl3d = $lr->finishPdl($pdl3d);
sub finishPdl {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  my $binbits = ($pdl+1)->log / log(2.0);
  $pdl .= 2*$binbits + 1;

  ##-- hack in case we still have infinite values [make 'em bad]
  #$pdl->inplace->setnantobad->inplace->setbadtoval(0);
  $pdl->inplace->setnantobad();

  return $pdl;
}

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right- log-frequency profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  eps=EPS          [default=1 or half-minimum]\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
     .qq(  smoothgt=WHICH   [default=0 (no smoothing)]\n)
    );
}

1; ##-- make perl happy
