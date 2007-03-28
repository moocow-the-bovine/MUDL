#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRMI.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile:
##    L-R mutual information
##======================================================================

package MUDL::Corpus::Profile::LRPMI;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use PDL;
use Carp;
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
  return $that->SUPER::new(nfields=>1,donorm=>1,%args);
  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + inherited


##======================================================================
## Conversion: to PDL

## $pdl3d = $lr->finishPdl($pdl3d);
##   + $pdl3d : (2, $nbds, $ntgs)
sub finishPdl {
  my ($lr,$pdl) = @_;

  my ($Ptb, $Pt, $Pb);
  foreach my $dir (0,1) {
    $Ptb  = $pdl->slice("($dir),,");
    $Ptb /= $Ptb->sum;

    $Pt  = $Ptb->sumover->transpose;
    $Pb  = $Ptb->xchg(0,1)->sumover;

    $Ptb *= log($Ptb/($Pt*$Pb))/log(2);
  }
  $pdl->inplace->setnantobad->inplace->setbadtoval(0);

  return $pdl;
}



##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract L/R- joint-weighted MI profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
}

1; ##-- make perl happy
