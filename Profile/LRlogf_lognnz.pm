#-*- Mode: CPerl -*-
## File: MUDL::Corpus::Profile::LRlogf_lognnz.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R log-frequency + log-nnz
##======================================================================

package MUDL::Corpus::Profile::LRlogf_lognnz;
use MUDL::Corpus::Profile::LRFBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use PDL::CCS;
use MUDL::PDL::Stats qw(:all);
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Profile::LRFBigrams); #)

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + new %args:
##       log_eps => $eps,    ##-- added to all frequency values; avoid log(0)==-inf: default=1
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
  return $that->SUPER::new(nfields=>1,donorm=>0,log_eps=>1,dolog=>0,%args);
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

  ##-- get eps value
  my $eps = $lr->{eps};
  $eps  = 1 if (!defined($eps));

  ##-- dispatch by direction
  my ($z);
  foreach my $z (0,1) {
    my $zpdl = $pdl->slice("($z)");   ##-- [b,w] -> f(b,w)
    my $nnzw = $zpdl->nnz;            ##-- [b]   -> nnz(b)
    my $nnzb = $zpdl->xchg(0,1)->nnz; ##-- [w]   -> nnz(w)
    my ($nb,$nw) = $zpdl->dims;

    $zpdl .= (
	      ($zpdl+$eps)->log2
	      + $nnzb->log2 /$nw
	      + $nnzw->log2->slice("*1") /$nb
	     );
  }

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
     .qq(  log_eps=EPS      [default=1]\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
}

1; ##-- make perl happy
