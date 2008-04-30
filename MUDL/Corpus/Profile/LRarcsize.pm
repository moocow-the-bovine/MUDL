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
    my $fwb_which  = $fwb->_whichND;            ##-- [nzi] -> [w_nzi,b_nzi]
    my $fwb_nzvals = $fwb->_nzvals;             ##-- [nzi] -> f(w_nzi,b_nzi)

    ##-- sanity check: bad flags (now in LRBigrams::addPdlBigrams())
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
    #my ($fwb_fv,$fwb_fc) = $fwb_nzvals->valcounts;
    my ($fwb_fv,$fwb_fc) = $fwb->valcounts;
    my $SMEAR_FF = 1; ##-- GT-style value-smearing?
    #$SMEAR_FF    = 0; ##-- ... or none
    my ($fwb_fcz);
    if ($SMEAR_FF) {
      $fwb_fcz = $fwb_fc->double->smearvals($fwb_fv); ##-- WITH GT-style value smearing (?)
    } else {
      $fwb_fcz = $fwb_fc->double;             ##-- ... or without
    }
    my $fwb_ff_nzvals = $fwb_nzvals->interpol($fwb_fv,$fwb_fcz);  ##-- [nzi] -> f_ff( f(w_nzi,b_nzi) )
    my $hwb_ff_nzvals = -log2z($fwb_ff_nzvals/$fwb_fcz->sumover); ##-- [nzi] -> h_ff( f(w_nzi,b_nzi) )

    my $fwb_ff_vals = $fwb->_vals->interpol($fwb_fv,$fwb_fcz);
    my $hwb_ff_vals = -log2z($fwb_ff_vals/$fwb_fcz->sumover);

    ##-- promiscuity (nnz)
    my $nnzw = $fwb->xchg(0,1)->nnz->decode; ##-- [w] -> |{ b : f(w,b)>0 }| ~ f_nz(W=w)
    my $nnzb = $fwb->nnz->decode;            ##-- [b] -> |{ w : f(w,b)>0 }| ~ f_nz(B=b)

    ##-- nnz-freqs
    my ($nnzw_v,$nnzw_vc) = $nnzw->valcounts;
    my ($nnzb_v,$nnzb_vc) = $nnzb->valcounts;
    my ($nnzw_vcz,$nnzb_vcz);
    my $SMEAR_NNZ_FF = 1; ##-- GT-style value-smearing?
    $SMEAR_NNZ_FF = 0;    ##-- ... or none
    if ($SMEAR_NNZ_FF) {
      $nnzw_vcz = $nnzw_vc->double->smearvals($nnzw_v);
      $nnzb_vcz = $nnzb_vc->double->smearvals($nnzb_v);
    } else {
      $nnzw_vcz = $nnzw_vc->double;
      $nnzb_vcz = $nnzb_vc->double;
    }
    my $nnzw_ff = $nnzw->interpol($nnzw_v,$nnzw_vcz); ##-- [w] -> f( f_nz(W=w) ) == f_fnz(W=w)
    my $nnzb_ff = $nnzb->interpol($nnzb_v,$nnzb_vcz); ##-- [b] -> f( f_nz(B=b) ) == f_fnz(B=b)
    my $nnzw_hf = -log2z($nnzw_ff/$nnzw_ff->sumover); ##-- [w] -> f( f_nz(W=w) ) == f_fnz(W=w)
    my $nnzb_hf = -log2z($nnzb_ff/$nnzb_ff->sumover); ##-- [b] -> f( f_nz(B=b) ) == f_fnz(B=b)

    #my $nnzw_hf_nzvals = $nnzw_hf->index($fwb_wi);    ##-- [nzi] -> h( f_fnz(W=w_nzi)/NNZF )
    #my $nnzb_hf_nzvals = $nnzb_hf->index($fwb_bi);    ##-- [nzi] -> h( f_fnz(B=b_nzi)/NNZF )

    if (0) {
      ##-- profile by external-promiscuity-weighted h: crappyish
      my $zt    = $zpdl->xchg(0,1);
      my $fb1   = $fb->slice("*1");
      my $nnzb1 = $nnzb->slice("*1");
      my $pw    = $fw/$N;
      my $pb1   = $fb1/$N;
      $zt .= ($fw-$nnzw)/$fw * ($fb1-$nnzb1)/$fb1 * -log2z( ($zt+1)/$N ); ##-- p(old|w) * p(old|b) * h(old,w,b)
      $zt += $nnzw/$fw * $nnzb1/$fb1 * (-log2z($pw) -log2z($pb1));        ##-- p(new|w) * p(new|b) * (h(w)+h(b))
    }
    elsif (0) {
      ##-- profile by internal-promiscuity-weighted h : >90% at stage=0 (>logf=89%), then craps out to 4% at stage=1
      ##  + maybe saving "real" (word-based) nnz values would help here?
      my $zt    = $zpdl->xchg(0,1);
      my $fb1   = $fb->slice("*1");
      my $nnzb1 = $nnzb->slice("*1");
      my $pw  = $fw/$N;
      my $pb1 = $fb1/$N;
      $zt .= -log2z( ($fw-$nnzw)/$fw * ($fb1-$nnzb1)/$fb1 * ($zt+1)/$N );
      $zt += -log2z( $nnzw/$fw       * $nnzb1/$fb1        * $pw * $pb1);
    }
    elsif (1) {
      ##-- profile by distributed promiscuity*p + h: ALSO similar to but slightly better than logf alone
      my $zt  = $zpdl->xchg(0,1);
      my $fb1    = $fb->slice("*1");
      my $nnzb1  = $nnzb->slice("*1");
      my $pw  = $fw/$N;
      my $pb1 = $fb1/$N;
      $zt .= -log2z( ($zt+1)/$N );                       ##-- ~= h( p(w,b) )
      $zt += -log2z( $nnzw/$fw * $pb1 )  / $zt->dim(1);  ##-- ~= h( p(B=new,B=b|W=w) ) / Nbds [~indepdt code for W=w]
      $zt += -log2z( $nnzb1/$fb1 * $pw ) / $zt->dim(0);  ##-- ~= h( p(W=new,W=w|B=b) ) / Ntgs [~indepdt code for B=b]
    }
    elsif (0) {
      ##-- profile by distributed promiscuity + h: similar to but slightly better than logf alone
      my $zt  = $zpdl->xchg(0,1);
      $zt .= -log2z( ($zt+1)/$N );                               ##-- ~= h( f(W=w,B=b)/N )
      $zt += (-log2z( $nnzw/$fw  ) / $zt->dim(1));               ##-- ~= h( p(B=new|W=w) ) / Nbds
      $zt += (-log2z( $nnzb/$fb  ) / $zt->dim(0))->slice("*1");  ##-- ~= h( p(W=new|B=b) ) / Ntgs
    }
    elsif (0) {
      ##-- seems to work well: next thing to try: gt-smoothed log-freqs
      ##   WHERE GT-smoothing is applied at the raw-frequency bigram level
      ## + this technique now implemented as Corpus::Profile::LRhff
      my ($fwb_fcz_fit,$fwb_fcz_coeffs) = $fwb_fcz->loglinfit($fwb_fv+1);
      my $fwb_ff_fit_vals = $fwb->_vals->interpol($fwb_fv,$fwb_fcz_fit);
      my $hwb_ff_fit_vals = -log2z($fwb_ff_fit_vals/$fwb_fcz_fit->sumover);
      my $hwb_ff_nd = $fwb->shadow(which=>$fwb_which, vals=>$hwb_ff_fit_vals);
      $hwb_ff_nd->decode($zpdl->xchg(0,1));
      print STDERR "<<<DEBUG>>>: ", ref($lr)."::finishPdl(z=$z): fwb_fcz_coeffs=".$fwb_fcz_coeffs."\n";
    }
    elsif (0) {
      my $hwb_ff_nd = $fwb->shadow(which=>$fwb_which, vals=>$hwb_ff_vals);
      $zpdl .= $hwb_ff_nd->xchg(0,1)->decode;
    }
    elsif (0) {
      my $zmask = ($zpdl>0);                   ##-- [b,w] --> I[f(b,w)>0]
      $zpdl .= log2($zpdl+1);                  ##-- [b,w] --> len(code(f(w,b)))

      ##-- pointers: constant size (==logf)
      #$zpdl += log2($zpdl->dim(0));
      #$zpdl += log2($zpdl->dim(1));

      ##-- pointers: non-zeroes only
      #$zpdl->where($zmask) += $nnzb_hf->index($zpdl->xvals->where($zmask));  ##-- [b,w] +-> len(ptr(b))
      #$zpdl->where($zmask) += $nnzw_hf->index($zpdl->yvals->where($zmask));  ##-- [b,w] +-> len(ptr(w))

      ##-- pointers: all: WORSE
      #$zpdl += $nnzb_hf->index($zpdl->xvals); ##-- hurts less at attachment stages than $nnzw_hf
      #$zpdl += $nnzw_hf->index($zpdl->yvals);
    }
    elsif (0) {
      $zpdl .= 0;
      $zpdl += $nnzb_hf->index($zpdl->xvals);
      $zpdl += $nnzw_hf->index($zpdl->yvals);

      my $hwb_ff_nd = $fwb->shadow(which=>$fwb_which, vals=>$hwb_ff_vals);
      $zpdl += $hwb_ff_nd->xchg(0,1)->decode;

      my $hwb_vals = $hwb_nzvals->append( -log2z(0.5/$N) );
      my $hwb_nd   = $fwb->shadow(which=>$fwb_which, vals=>$hwb_vals);
      $zpdl += $hwb_nd->xchg(0,1)->decode;
    }
    ##-- final arc size (BUGGY)
    else {
      #my $arcsize_nzvals = ($nnzw_hf_nzvals + $nnzb_hf_nzvals + $hwb_ff_nzvals); ##-- arcs only
      #my $arcsize_nzvals = ($nnzw_hf_nzvals + $nnzb_hf_nzvals + $hwb_ff_nzvals + $hwb_nzvals); ##-- arcs + joint codes
      #my $arcsize_nzvals = ($hwb_ff_nzvals + $nnzw_hf_nzvals + $nnzb_hf_nzvals + $hwgb_nzvals + $hbgw_nzvals); ##-- arcs + cond codes
      ##--


      ##-- DEBUG: try to use h() only : should be equivalent to lrlogf() ... but isn't, by a ___long___ shot @ stage>1
      #my $arcsize_nzvals = $hwb_nzvals;
      my $arcsize_nzvals = -log2z( ($fwb_nzvals+1)/$N );  ##-- 83%, 25%, ... !=logf --why?!
      #my $arcsize_missing = 0;             ##-- THIS is the bug!
      my $arcsize_missing = -log2z( 1/$N ); ##-- THIS should work (and it does: 83%, 74%, ... == logf
      #my $arcsize_missing = -log2z( .75/$N ); ##-- 
      #my $arcsize_missing = -log2z( .5/$N ); ##-- better: 74+%, 68%, ... >~ logf (but craps out a bit in later stages)
      #my $arcsize_missing = -log2z( .1/$N ); ##-- <~ logf
      ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ##-- see data/argh.perl, func test_lrarcsize_update()
      ##   + problem are freq-zeroes (missing in $arcsize_nd, assigned below)
      ##   + these get val=0 in arcsize profile pdl, which is just decode()d from $arcsize_nd
      ##   + BUT, h=0 means p=2**-h=1 means E(f)=N*p=N, which is Just Plain Wrong!
      ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      #my $arcsize_nzvals = -log( ($fwb_nzvals+1)/$N );    ##-- 83%, 25%, ... !=logf --why?!
      #my $arcsize_nzvals = log2z($N)-log2z($fwb_nzvals+1); ##-- 83%, 25%, ... !=logf --why?!
      #my $arcsize_nzvals = -log( ($fwb_nzvals+1)/$N );    ##-- 83%, 25%, ... !=logf --why?!
      ##
      #my $arcsize_nzvals0 = -log2z( ($fwb_nzvals+1)/$N );
      #my $arcsize_nzvals  = log( (2**-$arcsize_nzvals0)*$N ); ##-- 83%, 74%, ... == logf
      ##--
      #my $arcsize_nzvals = log( $fwb_nzvals+1 );          ##-- 83%, 74%, ... ==logf
      #my $arcsize_nzvals = log2z( $fwb_nzvals+1 );        ##-- 83%, 74%, ... ==logf
      ##~~
      #my $arcsize_nzvals = log2z( $fwb_nzvals );          ##-- <=logf, same pattern but slightly worse

      my $arcsize_nd = $fwb->shadow(which=>$fwb_which,vals=>$arcsize_nzvals->append($arcsize_missing));

      ##-- decode into $zpdl
      $arcsize_nd->decode( $zpdl->xchg(0,1) );
    }
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
