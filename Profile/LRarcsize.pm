#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRarcsize.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: corpus profile: L-R arc size
##    prof(b,w) = arcsize(b,w) = ?
##======================================================================

package MUDL::Corpus::Profile::LRarcsize;
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
##   + %args:
##       eos     => $eos_str,
##       bos     => $bos_str,
##       bounds  => $bounds_enum,
##       targets => $targets_enum,
##       left    =>$left_bigrams,       ## ($target,$lneighbor) : OBSOLETE (use pdls instead)
##       right   =>$right_bigrams,      ## ($target,$rneighbor) : OBSOLETE (use pdls instead)
##       smoothgt=>$which,              ## - best not to use?
##       log_eps =>$eps,                ##-- used to avoid zeroes in log(f(b,w))
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>0,smoothgt=>'',log_eps=>1,%args);
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
    my $fwb_nzvals = $fwb->_nzvals;             ##-- [nzi] -> f(w_nzi,b_nzi)
    my $fwb_which  = $fwb->_whichND;            ##-- [nzi] -> [w_nzi,b_nzi]

    ##-- sanity check: bad flags
    $fwb_which->badflag(0)  if ($fwb_which->badflag && $fwb_which->isgood->all);
    $fwb_nzvals->badflag(0) if ($fwb_nzvals->badflag && $fwb_nzvals->isgood->all);

    my $fwb_wi     = $fwb_which->slice("(0),"); ##-- [nzi] -> w_nzi
    my $fwb_bi     = $fwb_which->slice("(1),"); ##-- [nzi] -> b_nzi

    ##-- corpus probabilities
    my $fw_nzvals   = $fw->index($fwb_wi);       ##-- [nzi] -> f(w_nzi)
    my $fb_nzvals   = $fb->index($fwb_bi);       ##-- [nzi] -> f(b_nzi)

    my $pbgw_nzvals = $fwb_nzvals / $fw_nzvals;  ##-- [nzi] -> p(b_nzi|w_nzi)
    my $pwgb_nzvals = $fwb_nzvals / $fb_nzvals;  ##-- [nzi] -> p(w_nzi|b_nzi)

    my $hwb_nzvals  = -log2z($fwb_nzvals / $N);         ##-- [nzi] -> h( p(w_nzi,b_nzi) )
    my $hwgb_nzvals = -log2z($fwb_nzvals / $fb_nzvals); ##-- [nzi] -> h( p(w_nzi|b_nzi) )
    my $hbgw_nzvals = -log2z($fwb_nzvals / $fw_nzvals); ##-- [nzi] -> h( p(b_nzi|w_nzi) )

    ##-- freq-freqs
    my ($fwb_fv,$fwb_fc) = $fwb_nzvals->valcounts;
    my $SMEAR_FF = 1; ##-- ?
    #$SMEAR_FF    = 0; ##-- ?
    my ($fwb_fcz);
    if ($SMEAR_FF) {
      $fwb_fcz = $fwb_fc->smearvals($fwb_fv); ##-- WITH GT-style value smearing (?)
    } else {
      $fwb_fcz = $fwb_fc->double;             ##-- ... or without
    }
    my $fwb_ff_nzvals = $fwb_nzvals->interpol($fwb_fv,$fwb_fcz);  ##-- [nzi] -> f_ff( f(w_nzi,b_nzi) )
    my $hwb_ff_nzvals = -log2z($fwb_ff_nzvals/$fwb_fcz->sumover); ##-- [nzi] -> h_ff( f(w_nzi,b_nzi) )

    ##-- align unigram data
    #my $fw_nzvals  = $fw->index($fwb_wi);       ##-- [nzi] -> f(w_nzi)
    #my $fb_nzvals  = $fb->index($fwb_bi);       ##-- [nzi] -> f(b_nzi)

    ##-- promiscuity (nnz)
    my $nnzw       = $fwb->xchg(0,1)->nnz->decode; ##-- [w] -> |{ b : f(w,b)>0 }| ~ f_nz(W=w)
    my $nnzb       = $fwb->nnz->decode;            ##-- [b] -> |{ w : f(w,b)>0 }| ~ f_nz(B=b)

    ##-- nnz-freqs
    my ($nnzw_v,$nnzw_vc) = $nnzw->valcounts;
    my ($nnzb_v,$nnzb_vc) = $nnzb->valcounts;
    my ($nnzw_vcz,$nnzb_vcz);
    my $SMEAR_NNZ_FF = 1;
    #$SMEAR_NNZ_FF = 0;
    if ($SMEAR_NNZ_FF) {
      $nnzw_vcz = $nnzw_vc->smearvals($nnzw_v);
      $nnzb_vcz = $nnzb_vc->smearvals($nnzb_v);
    } else {
      $nnzw_vcz = $nnzw_vc->double;
      $nnzb_vcz = $nnzb_vc->double;
    }
    my $nnzw_ff = $nnzw->interpol($nnzw_v,$nnzw_vcz); ##-- [w] -> f( f_nz(W=w) ) == f_fnz(W=w)
    my $nnzb_ff = $nnzb->interpol($nnzb_v,$nnzb_vcz); ##-- [b] -> f( f_nz(B=b) ) == f_fnz(B=b)
    my $nnzw_hf = -log2z($nnzw_ff/$nnzw_ff->sumover); ##-- [w] -> f( f_nz(W=w) ) == f_fnz(W=w)
    my $nnzb_hf = -log2z($nnzb_ff/$nnzb_ff->sumover); ##-- [b] -> f( f_nz(B=b) ) == f_fnz(B=b)

    my $nnzw_hf_nzvals = $nnzw_hf->index($fwb_wi);    ##-- [nzi] -> h( f_fnz(W=w_nzi)/NNZF )
    my $nnzb_hf_nzvals = $nnzb_hf->index($fwb_bi);    ##-- [nzi] -> h( f_fnz(B=b_nzi)/NNZF )

    ##-- get final size
    #my $arcsize_nzvals       = ($nnzw_hf_nzvals + $nnzb_hf_nzvals + $hwb_ff_nzvals);         ##-- arcs only
    #my $arcsize_nzvals = ($nnzw_hf_nzvals + $nnzb_hf_nzvals + $hwb_ff_nzvals + $hwb_nzvals); ##-- arcs + joint codes
    #my $arcsize_nzvals = ($hwb_ff_nzvals + $nnzw_hf_nzvals + $nnzb_hf_nzvals + $hwgb_nzvals + $hbgw_nzvals); ##-- arcs + cond codes
    ##-- DEBUG: try to use h() only : should be equivalent to lrlogf() ... but isn't, by a ___long___ shot @ stage>1
    #my $arcsize_nzvals = $hwb_nzvals;
    #my $arcsize_nzvals = -log2z( ($fwb_nzvals+1)/$N );  ##-- 83%, 25%, ...
    #my $arcsize_nzvals = -log( ($fwb_nzvals+1)/$N );    ##-- 83%, 25%, ...
    my $arcsize_nzvals = log2z($N)-log2z($fwb_nzvals+1); ##-- 83%, 25%, ...
    #my $arcsize_nzvals = log( $fwb_nzvals+1 );    ##-- ==logf: 83%, 74%, 66%, ...
    #my $arcsize_nzvals = log2z( $fwb_nzvals+1 );  ##-- ==logf: 83%, 74%, 66%, ...
    #my $arcsize_nzvals = log2z( $fwb_nzvals );     ##-- <=logf, same pattern but slightly worse

    my $arcsize_nd = $fwb->shadow(which=>$fwb_which,vals=>$arcsize_nzvals->append(0));

    ##-- decode into $zpdl
    $arcsize_nd->decode( $zpdl->xchg(0,1) );
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
    );
}


1;
