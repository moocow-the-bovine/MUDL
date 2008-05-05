#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRhff_ptr.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: corpus profile: model size: entropy of (smoothed) freq-freq + ptr-len
##    prof(b,w) = h( smear(f_nz( f(b,w))) )
##======================================================================

package MUDL::Corpus::Profile::LRhff_ptr;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use MUDL::PDL::Smooth qw(:all);
use MUDL::PDL::Stats qw(:all);
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Profile::LRBigrams); #)

##======================================================================
## $lr = $class_or_obj->new(%args)
##  + new %args:
##     smear_ff => $bool,             ## whether to "smear" freq-freqs (see MUDL::PDL::Smooth::smearvals), default=1
##  + inherited %args:
##     eos     => $eos_str,
##     bos     => $bos_str,
##     bounds  => $bounds_enum,
##     targets => $targets_enum,
##     left    =>$left_bigrams,       ## ($target,$lneighbor) : OBSOLETE (use pdls instead)
##     right   =>$right_bigrams,      ## ($target,$rneighbor) : OBSOLETE (use pdls instead)
##     smoothgt=>$which,              ## - best not to use?
##     log_eps =>$eps,                ##-- used to avoid zeroes in log(f(b,w))
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>0,smoothgt=>'',log_eps=>1,smear_ff=>1,%args);
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

  ##-- get common data: frequencies
  my $N  = pdl(double,$lr->{ftotal});
  my $fw = $lr->{ptugs}{pdl}->double;  ##--  [w]   -> f(w)
  my $fb = $lr->{pbugs}{pdl}->double;  ##--  [b]   -> f(b)

  foreach my $z (0,1) {
    my $zpdl      = $pdl->slice("($z)");
    my $zpdl_mask = ($zpdl>0);

    my $fwb        = $lr->{$z==0 ? 'pleft' : 'pright'}{pdl}->double;
    my $fwb_which  = $fwb->_whichND;            ##-- [nzi] -> [w_nzi,b_nzi]
    my $fwb_nzvals = $fwb->_nzvals;             ##-- [nzi] -> f(w_nzi,b_nzi)

    ##-- freq-freqs (local)
    my ($fwb_fv,$fwb_fc) = $fwb->valcounts;
    my ($fwb_fcz);
    if ($lr->{smear_ff}) {
      $fwb_fcz = $fwb_fc->double->smearvals($fwb_fv); ##-- WITH GT-style value smearing
    } else {
      $fwb_fcz = $fwb_fc->double;                     ##-- ... or without
    }

    ##-- freq-freqs: interpolate (including zeroes)
    my $fwb_ff_vals = $fwb->_vals->interpol($fwb_fv,$fwb_fcz);  ##-- [nzi] -> f_ff( f(w_nzi,b_nzi) )
    my $hwb_ff_vals = -log2z($fwb_ff_vals/$fwb_fcz->sumover);   ##-- [nzi] -> h_ff( f(w_nzi,b_nzi) )

    ##-- freq-freqs: log-linear fit (including zeroes)
    my ($fwb_fcz_fit,$fwb_fcz_coeffs) = $fwb_fcz->loglinfit($fwb_fv+1);
    print STDERR "<<<DEBUG>>>: ", ref($lr)."::finishPdl(z=$z): fwb_fcz_coeffs=".$fwb_fcz_coeffs."\n";

    ##-- freq-freqs: backfit: entropy
    my $fwb_ff_fit_vals = $fwb->_vals->interpol($fwb_fv,$fwb_fcz_fit);
    my $hwb_ff_fit_vals = -log2z($fwb_ff_fit_vals/$fwb_fcz_fit->sumover);

    ##-- freq-freqs: decode to $zpdl
    my $hwb_ff_nd = $fwb->shadow(which=>$fwb_which, vals=>$hwb_ff_fit_vals);
    $hwb_ff_nd->decode($zpdl->xchg(0,1));

    ##-- pointers: base data
    my $nnzw = $lr->{'nnzt'.($z==0 ? '2' : '1')}->double;  ##-- [w]   -> (z==0 ? nnz(*,w) : nnz(w,*))
    my $nnzb = $lr->{'nnzb'.($z==0 ? '1' : '2')}->double;  ##-- [b]   -> (z==0 ? nnz(b,*) : nnz(*,b))
    my $ptrlen_w = -log2z($nnzw/$fw)->slice("*1");
    my $ptrlen_b = -log2z($nnzb/$fb);
    #my ($nb,$nt) = $zpdl->dims;
    my $h_ptrs   = $ptrlen_w/2 + $ptrlen_b/2;
    $zpdl->where($zpdl_mask) += $h_ptrs->where($zpdl_mask);
  }

  return $pdl;
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
     .qq(  smoothgt=WHICH   [default='']\n)
     .qq(  smearff=BOOL     [default=1 ]\n)
    );
}


1;
