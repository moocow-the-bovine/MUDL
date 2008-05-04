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
##   + new %$lr keys:
##      nnzt1 => $nnzt1,           ## pdl($nTgs) : [t] -> |{w : f(w,t) > 0}|
##      nnzt2 => $nnzt2,           ## pdl($nTgs) : [t] -> |{w : f(t,w) > 0}|
##      nnzb1 => $nnzb1,           ## pdl($nBds) : [b] -> |{w : f(w,b) > 0}|
##      nnzb2 => $nnzb2,           ## pdl($nBds) : [b] -> |{w : f(b,w) > 0}|
##      Nnz   => $Nnz,             ## scalar: |{(w,v) : f(w,v) > 0}|
##      Nw    => $Nw,              ## scalar: |Alphabet|
##
##   + %args:
##      eos => $eos_str,
##      bos => $bos_str,
##      bounds => $bounds_enum,
##      targets => $targets_enum,
##      left=>$left_bigrams,       ## ($target,$lneighbor)
##      right=>$right_bigrams,     ## ($target,$rneighbor)
##      smoothgt=>$which,
##      log_eps => $eps,    ##-- added to all frequency values; avoid log(0)==-inf: default=1
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>0,log_eps=>1,dolog=>0,%args);
}

##======================================================================
## Profiling

## undef = $profile->finishPdlProfile(%args)
#inherited

## $lr = $lr->addPdlBigrams($bgpd,%args);
sub addPdlBigrams {
  my ($lr,$bgpd,%args) = @_;

  ##-- call superclass method, caching translation pdls
  $lr->SUPER::addPdlBigrams($bgpd,%args,saveXpdls=>1)
    or croak(ref($lr)."::addPdlBigrams() failed!");

  ##-- get translation PDLs
  my $bds2bge = $lr->{bds2bge};
  my $tgs2bge = $lr->{tgs2bge};

  ##-- get {nnzl},{nnzr}
  my $f12  = $bgpd->{pdl};
  my $nnz2 = $f12->nnz->decode;             ##-- nnz2: [w] -> |{ v : f(v,w)>0 }| : w-is-second
  my $nnz1 = $f12->xchg(0,1)->nnz->decode;  ##-- nnz1: [w] -> |{ v : f(w,v)>0 }| : w-is-first

  ##-- get {nnzt*}:targets, {nnzb*}:bounds
  $lr->{nnzt1} = $nnz1->index($tgs2bge);    ##-- nnzt1: [t] -> nnz(t,*) : t-is-first
  $lr->{nnzt2} = $nnz2->index($tgs2bge);    ##-- nnzt2: [t] -> nnz(*,t) : t-is-second
  $lr->{nnzb1} = $nnz1->index($bds2bge);    ##-- nnzb1: [b] -> nnz(b,*) : b-is-first
  $lr->{nnzb2} = $nnz2->index($bds2bge);    ##-- nnzb2: [b] -> nnz(*,b) : b-is-second

  ##-- save total number of nonzero bigram events
  $lr->{Nnz}   = $f12->_nnz;
  $lr->{Nw}    = pdl(long,$f12->dims)->max;

  ##-- cleanup cached translation pdls
  delete(@$lr{'bds2bge','bds_msk','bge2bds', 'tgs2bge','tgs_msk','bge2tgs'});

  return $lr;
}

##======================================================================
## MetaProfile interface

## $lr = $lr->updateBoundsPostHook($xlateBoundsMatrix, $newBoundsEnum)
##  + $xlateBoundsMatrix : pdl($nOldBounds,$nNewBounds) : [$old,$new] --> p($new|$old)
sub updateBoundsPostHook {
  my ($lr,$xmatrix,$xenum) = @_;
  $lr->{nnzb1} = ($xmatrix x $lr->{nnzb1}->toccs->dummy(0,1))->todense->flat;
  $lr->{nnzb2} = ($xmatrix x $lr->{nnzb2}->toccs->dummy(0,1))->todense->flat;
  return $lr;
}

## $lr = $lr->updateTargetsPostHook($xlateTargetsMatrix, $newTargetsEnum)
##  + $xlateTargetsMatrix : pdl($nOldTargets,$nNewTargets) : [$old,$new] --> p($new|$old)
sub updateTargetsPostHook {
  my ($lr,$xmatrix,$xenum) = @_;
  $lr->{nnzt1} = ($xmatrix x $lr->{nnzt1}->toccs->dummy(0,1))->todense->flat;
  $lr->{nnzt2} = ($xmatrix x $lr->{nnzt2}->toccs->dummy(0,1))->todense->flat;
  return $lr;
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
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  ##-- get eps value
  my $eps = $lr->{log_eps};
  $eps  = 1 if (!defined($eps));

  ##-- dispatch by direction
  my ($z);
  foreach my $z (0,1) {
    my $zpdl = $pdl->slice("($z)");                ##-- [b,t] -> (z==0 ?   f(b,t) :   f(t,b))
    my $nnzt = $lr->{'nnzt'.($z==0 ? '2' : '1')};  ##-- [t]   -> (z==0 ? nnz(*,t) : nnz(t,*))
    my $nnzb = $lr->{'nnzb'.($z==0 ? '1' : '2')};  ##-- [b]   -> (z==0 ? nnz(b,*) : nnz(*,b))
    my ($nb,$nt) = $zpdl->dims;

    $zpdl .= (
	      ($zpdl+$eps)->log2              ##-- log frequency
	      ##--
	      + $nnzb->log2 /$nt              ##-- without /$nt,/$nb does really well stage=0, then craps out
	      + $nnzt->log2->slice("*1") /$nb ##-- ... ditto
	      ##--
	      #+ $nnzb->log2               ##-- without /$nt,/$nb does really well stage=0, then craps out
	      #+ $nnzt->log2->slice("*1")  ##-- ... ditto
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
