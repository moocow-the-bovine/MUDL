##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRHB.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile
##    : L-R conditional entropy (Bernoulli)
##======================================================================

package MUDL::Corpus::Profile::LRHB;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile::LRBigrams);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
##       left=>$left_bigrams,       ## ($target,$lneighbor)
##       right=>$right_bigrams,     ## ($target,$rneighbor)
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>1,norm_min=>0,%args);
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + inherited

##======================================================================
## Conversion: to PDL

##-- inherited from MUDL:::Corpus::Profile::LR
## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl,%args)
##
## $pdl3d = $lr->smoothPdl($pdl3d,%args);


## $pdl3d = $lr->finishPdl($pdl3d,%args);
sub finishPdl {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  my ($P_t, $P_bg, $P_nbg, $P_which_gt_half, $H_gt_half);
  foreach my $z (0,1) {
    $P_bg     = $pdl->slice("($z),,");
    $P_bg    /= $P_bg->sumover->slice("*1,");

    ##-- zero-check
    $P_bg->inplace->setnantobad->inplace->setbadtoval(0);

    ##-- get indices of p(b|t) > 0.5
    $P_which_gt_half = which($P_bg > 0.5);

    ##-- get alternative probabilities: p(¬b|t)
    $P_nbg    = 1-$P_bg;

    ## $P_bg .= -($P_bg*log($P_bg)/log(2) + $P_nbg*log($P_nbg)/log(2));
    $P_bg *= log($P_bg);
    $P_bg /= pdl(double,2)->log;

    $P_nbg *= log($P_nbg);
    $P_nbg /= pdl(double,2)->log;

    $P_bg  += $P_nbg;
    $P_bg  *= -1;

    ##-- raise values for p(b|t) > 0.5 [normalized to 0..1]
    $H_gt_half  = $P_bg->flat->index($P_which_gt_half);
    $H_gt_half *= -1;
    $H_gt_half += 2;
  }
  $pdl->inplace->setnantobad->inplace->setbadtoval(0);

  return $pdl;
}

## undef = $lr->normalizePdl($pdl);
##-- inherited


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right- monotonic Bernoulli entropy profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
     .qq(  smoothgt=WHICH   [default=0] : one of 'bigrams','pdl',0\n)
    );
}

1; ##-- make perl happy

