#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRFBigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R raw frequency
##======================================================================

package MUDL::Corpus::Profile::LRFBigrams;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Profile::LRBigrams); #)

##======================================================================
## $lr = $class_or_obj->new(%args)
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
##-- inherited


## undef = $profile->finishPdlProfile(%args)
##  + perform pdl-sensitive finishing actions
#-- inherited

## undef = $lr->normalizePdl($pdl);
sub normalizePdl_OLD {
  my ($lr,$pdl) = @_;

  my $min = $lr->{norm_min};
  $min  = $pdl->min if (!defined($min));
  $pdl -= $min      if ($min != 0);

  return $lr;
}


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right-frequency profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
}


1;
