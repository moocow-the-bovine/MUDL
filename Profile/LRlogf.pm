#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRlogf.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R log-frequency
##======================================================================

package MUDL::Corpus::Profile::LRlogf;
use MUDL::Corpus::Profile::LRFBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Profile::LRFBigrams); #)

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + new %args:
##       eps => $eps,    ##-- added to all frequency values; avoid log(0)==-inf: default=1
##
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
##       left=>$left_bigrams,       ## ($target,$lneighbor)
##       right=>$right_bigrams,     ## ($target,$rneighbor)
##       smoothgt=>$which,
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>0,%args);
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + inherited

## undef = $profile->addBigrams($bigrams,%args)
##  + inherited

##======================================================================
## Conversion: to PDL

##-- inherited from MUDL:::Corpus::Profile::LR

## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl)

## $pdl3d = $lr->smoothPdl($pdl3d);
##-- inherited

## $pdl3d = $lr->finishPdl($pdl3d);
sub finishPdl {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  ##-- get zero mask
  #my $nzmask = ($pdl != 0);

  ##-- get minimum value
  my $eps = $lr->{eps};
  ##-- eps: guess via non-zero minimum
  #if (!defined($eps)) {
  #  $eps  = $pdl->where($nzmask)->flat->minimum;
  #  if ($eps > 1) {
  #    $eps .= 1;
  #  } else {
  #    $eps *= 0.1;
  #  }
  #}
  ##-- eps: set zeroes
  #$pdl->where(!$nzmask) .= $eps;
  ##
  ##-- eps: simple: use constant
  $eps  = 1 if (!defined($eps));
  $pdl += $eps;

  ##-- do log
  $pdl->inplace->log;

  ##-- hack in case we still have infinite values [make 'em bad]
  #$pdl->inplace->setnantobad->inplace->setbadtoval(0);
  $pdl->inplace->setnantobad();

  return $pdl;
}

## undef = $lr->normalizePdl($pdl);
##
## (inherited)

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
    );
}

1; ##-- make perl happy
