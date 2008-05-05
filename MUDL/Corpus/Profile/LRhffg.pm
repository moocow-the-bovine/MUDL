#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRhffg.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: corpus profile: model size: entropy of (smoothed) freq-freq (global)
##    prof(b,w) = h( smear(f_nz( f(b,w))) )
##======================================================================

package MUDL::Corpus::Profile::LRhffg;
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

  ##-- get smoothing coefficients
  my $f12              = $bgpd->{pdl};
  my ($f12_fv,$f12_fc) = $f12->valcounts;
  my ($f12_fcz);
  if ($lr->{smear_ff}) {
    $f12_fcz = $f12_fc->double->smearvals($f12_fv); ##-- WITH GT-style value smearing
  } else {
    $f12_fcz = $f12_fc->double;                     ##-- ... or without
  }

  ##-- log-linear fit (handle zeroes)
  my ($f12_fcz_fit,$f12_fcz_coeffs) = $f12_fcz->loglinfit($f12_fv+1);
  print STDERR "<<<DEBUG>>>: ", ref($lr)."::addPdlBigrams(): f12_fcz_coeffs=".$f12_fcz_coeffs."\n";

  ##-- save coefficients
  $lr->{f12_fcz_coeffs} = $f12_fcz_coeffs;

  ##-- DEBUG: save fit: ok: all($f12_fcz_fit->approx($coeffs->at(0)*($fv+1)**$coeffs->at(1)));
  #$lr->{f12_fcz_fit} = $f12_fcz_fit;

  ##-- cleanup cached translation pdls
  delete(@$lr{'bds2bge','bds_msk','bge2bds', 'tgs2bge','tgs_msk','bge2tgs'});

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
  my $N        = pdl(double,$lr->{ftotal});
  my ($ca,$ce) = $lr->{f12_fcz_coeffs}->dog;

  foreach my $z (0,1) {
    my $zpdl = $pdl->slice("($z)");
    my $fwb  = $lr->{$z==0 ? 'pleft' : 'pright'}{pdl}->double;

    my $fwb_which = $fwb->_whichND;            ##-- [nzi] -> [w_nzi,b_nzi]
    my $fwb_vals  = $fwb->_vals;               ##-- [nzi] -> f(w_nzi,b_nzi)+1

    my $fwb_vals_fit = $ca * (($fwb_vals+1)**$ce);
    my $hwb_vals_fit = -log2z($fwb_vals_fit/$N);
    my $fwb_fit      = $fwb->shadow(which=>$fwb_which, vals=>$hwb_vals_fit);

    $fwb_fit->decode($zpdl->xchg(0,1));
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
