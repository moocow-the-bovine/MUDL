#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRhff.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: corpus profile: model size: entropy of (smoothed) freq-freq
##    prof(b,w) = h( smear(f_nz( f(b,w))) )
##======================================================================

package MUDL::Corpus::Profile::LRhff;
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

  ##-- get common data: frequencies
  my $N  = pdl(double,$lr->{ftotal});
  my $fw = $lr->{ptugs}{pdl}->double;  ##--  [w]   -> f(w)
  my $fb = $lr->{pbugs}{pdl}->double;  ##--  [b]   -> f(b)

  foreach my $z (0,1) {
    my $zpdl    = $pdl->slice("($z)");

    my $fwb        = $lr->{$z==0 ? 'pleft' : 'pright'}{pdl}->double;
    my $fwb_which  = $fwb->_whichND;            ##-- [nzi] -> [w_nzi,b_nzi]
    my $fwb_nzvals = $fwb->_nzvals;             ##-- [nzi] -> f(w_nzi,b_nzi)

    ##-- sanity check: bad flags (now in LRBigrams::addPdlBigrams())
    $fwb_which->badflag(0)  if ($fwb_which->badflag && $fwb_which->isgood->all);
    $fwb_nzvals->badflag(0) if ($fwb_nzvals->badflag && $fwb_nzvals->isgood->all);

    ##-- nz-value index components
    my $fwb_wi     = $fwb_which->slice("(0),"); ##-- [nzi] -> w_nzi
    my $fwb_bi     = $fwb_which->slice("(1),"); ##-- [nzi] -> b_nzi

    ##-- freq-freqs
    my ($fwb_fv,$fwb_fc) = $fwb->valcounts;
    my ($fwb_fcz);
    if ($lr->{smear_ff}) {
      $fwb_fcz = $fwb_fc->double->smearvals($fwb_fv); ##-- WITH GT-style value smearing
    } else {
      $fwb_fcz = $fwb_fc->double;                     ##-- ... or without
    }
    my $fwb_ff_nzvals = $fwb_nzvals->interpol($fwb_fv,$fwb_fcz);  ##-- [nzi] -> f_ff( f(w_nzi,b_nzi) )
    my $hwb_ff_nzvals = -log2z($fwb_ff_nzvals/$fwb_fcz->sumover); ##-- [nzi] -> h_ff( f(w_nzi,b_nzi) )

    ##-- include zeroes in interpolation
    my $fwb_ff_vals = $fwb->_vals->interpol($fwb_fv,$fwb_fcz);
    my $hwb_ff_vals = -log2z($fwb_ff_vals/$fwb_fcz->sumover);

    ##-- log-linear fit (handle zeroes)
    my ($fwb_fcz_fit,$fwb_fcz_coeffs) = $fwb_fcz->loglinfit($fwb_fv+1);
    print STDERR "<<<DEBUG>>>: ", ref($lr)."::finishPdl(z=$z): fwb_fcz_coeffs=".$fwb_fcz_coeffs."\n";

    my $fwb_ff_fit_vals = $fwb->_vals->interpol($fwb_fv,$fwb_fcz_fit);
    my $hwb_ff_fit_vals = -log2z($fwb_ff_fit_vals/$fwb_fcz_fit->sumover);

    ##-- decode to $zpdl
    my $hwb_ff_nd = $fwb->shadow(which=>$fwb_which, vals=>$hwb_ff_fit_vals);
    $hwb_ff_nd->decode($zpdl->xchg(0,1));
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
