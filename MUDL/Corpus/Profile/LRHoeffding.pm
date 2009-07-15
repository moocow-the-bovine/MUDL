#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRHoeffding.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile:
##    L-R Hoeffding alpha (monotonic): p=p_ML(b), f=f(b,w), n=f(w)
##
##  + prof(w,b) ~= d_{Hoef,mono} ( p=p_ML(b), f=f(b,w), n=f(w) )
##               = sgn(f/n-p) * (2 - 2 / exp( 2 * n * abs(p-f/n)**2 ))
##               = sgn(f/n-p) * (2 - \alpha_\min(p,f,n))
##               = sgn(f/n-p) * d_{Hoef}(p,f,n)
##======================================================================

package MUDL::Corpus::Profile::LRHoeffding;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use MUDL::PDL::Stats ':all';
use PDL;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile::LRBigrams);
use strict;

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
##       left=>$left_bigrams,       ## ($target,$lneighbor)
##       right=>$right_bigrams,     ## ($target,$rneighbor)
##       log_eps=>$eps,             ## used as frequency smoother, default=0
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(donorm=>0,dolog=>0,log_eps=>0,%args);
}

##======================================================================
## Conversion: to PDL

## $pdl3d = $lr->finishPdl($pdl3d);
##   + $pdl3d : (2, $nbds, $ntgs)
sub finishPdl {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  ##-- get common data: frequencies
  my $fw = $lr->{ptugs}{pdl}->double->slice("*1,");  ##-- [0,w] -> f(w)
  my $fb = $lr->{pbugs}{pdl}->double;                ##-- [b  ] -> f(b)
  my $N  = pdl(double,$lr->{ftotal});

  ##-- get common data: probabilities
  my $pb = $fb/$N;                                   ##-- [b  ] -> p(b)

  foreach my $z (0,1) {
    my $zpdl = $pdl->slice("($z)");                  ##-- [b,w] -> f(b,w) [--> PROFILE(w,b)]

    my $pbgw    = $zpdl / $fw;                       ##-- [b,w] -> p(b|w)
    my $Delta_p = $pbgw-$pb;                         ##-- [b,w] -> p(b|w)-p(b)

    $zpdl .= sgn($Delta_p) * (2-2/exp(2*$fw*abs($Delta_p)**2));
    $zpdl->where(($fw==0)|($fb==0)) .= 0;            ##-- bash to independence (0)
  }
  $pdl->inplace->setnantobad->inplace->setbadtoval(0); ##-- bash to independence (0)

  return $pdl;
}



##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract L/R- relative pointwise MI PROFILE wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(...\n)
    );
}

1; ##-- make perl happy
