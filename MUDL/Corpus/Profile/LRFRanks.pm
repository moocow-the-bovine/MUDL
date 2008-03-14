#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRFRanks.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R ranks
##======================================================================

package MUDL::Corpus::Profile::LRFRanks;
use MUDL::Corpus::Profile::LRFBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use MUDL::PDL::Ranks;
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Profile::LRFBigrams); #)

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + new %args:
##       rank_order  => $asc_or_desc,    ## rank order, one of 'asc' or 'desc' (default: 'asc')
##       rank_shared => $bool,           ## allow Spearman-style rank-sharing (default: true)
##       rank_indep  => $bool,           ## rank left and right independently (default: false)
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
  return $that->SUPER::new(nfields=>1,
			   donorm=>0,
			   rank_order=>'asc',
			   rank_shared=>1,
			   rank_indep=>0,
			   %args,
			  );
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

## $pdl2d = $lr->rankpdl($pdl2d)
##-- rank pdl using MUDL::PDL::Ranks and $lr options
sub rankpdl {
  my ($lr,$vpdl) = @_;
  return $vpdl->avgranks(order=>$lr->{rank_order}) if ($lr->{rank_shared});
  return $vpdl->ranks(order=>$lr->{rank_order});
}

## $pdl3d = $lr->finishPdl($pdl3d);
sub finishPdl {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  my ($z, $zpdl);
  if ($lr->{rank_indep}) {
    ##-- independent left- and right-ranks
    foreach $z (0,1) {
      $zpdl  = $pdl->slice("($z),,");
      $zpdl .= $lr->rankpdl($zpdl);
    }
  } else {
    ##-- "dependent" left- and right-ranks
    $zpdl  = $lr->clump(2);
    $zpdl .= $lr->rankpdl($zpdl);
  }

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
    (qq(Extract left- and right- frequency-rank profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  rank_order=WHICH [default='asc']\n)
     .qq(  rank_shared=BOOL [default=1]\n)
     .qq(  rank_indep=BOOL  [default=0]\n)
     .qq(  eps=EPS          [default=1 or half-minimum]\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
}

1; ##-- make perl happy
