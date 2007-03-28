#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRMI.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile:
##    L-R mutual information
##======================================================================

package MUDL::Corpus::Profile::LRMI;
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
  return $that->SUPER::new(nfields=>1,donorm=>1,norm_min=>1,%args);
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + inherited

## undef = $profile->finish()
sub finishOld {
  my $pr = shift;
  $pr->dist2mi($pr->{left});
  $pr->dist2mi($pr->{right});
  return $pr;
}

## $midist = $pr->dist2mi($bgdist)
##   + computes pointwise-mi distribution over targets from a bigram distribution
sub dist2mi {
  my ($pr,$dist) = @_;

  $dist->normalize();
  my $Pt = $dist->project1(0);
  my $Pb = $dist->project1(1);

  my $Pmi = MUDL::Dist->new();

  my ($event,$ptb,$t,$b, $pt,$pb);
  while (($event,$ptb)=each(%{$dist->{nz}})) {
    ($t,$b) = $dist->split($event);
    $pt = $Pt->{$t};
    $pb = $Pb->{$b};
    if ($ptb && $pt && $pb) {
      $Pmi->{$event} = log($ptb/($pt*$pb))/log(2);
    } else {
      $Pmi->{$event} = 0;
    }
  }

  return $dist->{nz} = $Pmi;
}

##======================================================================
## Conversion: to PDL

##-- inherited from MUDL:::Corpus::Profile::LR

## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl)

## $pdl3d = $lr->smoothPdl($pdl3d);
##-- inherited

## $pdl3d = $lr->finishPdl($pdl3d);
sub finishPdl {
  my ($lr,$pdl) = @_;

  my ($Ptb, $Pt, $Pb);
  foreach my $dir (0,1) {
    $Ptb  = $pdl->slice("($dir),,");
    $Ptb /= $Ptb->sum;

    $Pt  = $Ptb->sumover;
    $Pb  = $Ptb->xchg(0,1)->sumover;

    #$Ptb .= log($Ptb/($Pt->slice("*1,")*$Pb))/log(2);
    $Ptb  /= $Pt->slice("*1,");
    $Ptb  /= $Pb;
    $Ptb->inplace->log;
    $Ptb  /= pdl(double,2)->log;
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
    (qq(Extract left- and right-MI profile wrt. fixed boundary set.\n)
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

