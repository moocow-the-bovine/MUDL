#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRRPMI.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile:
##    L-R relative pointwise mutual information
##
##  + prof(w,b) ~= i(w;b)             / h(w,b)
##              := (h(w)+h(b)-h(w,b)) / h(w,b)  , if f(w,b)>0
##              := 0                            , otherwise
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
##       log_eps=>$eps,             ## used as frequency smoother, default=0
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(donorm=>0,dolog=>0,log_eps=>0,%args);
  return $self;
}

##======================================================================
## Conversion: to PDL

## $pdl3d = $lr->finishPdl($pdl3d);
##   + $pdl3d : (2, $nbds, $ntgs)
sub finishPdl {
  my ($lr,$pdl) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  ##-- get common data: frequencies
  my $N  = pdl(double,$lr->{ftotal});
  my $fw = $lr->{ptugs}{pdl}->double;  ##--  [w]   -> f(w)
  my $fb = $lr->{pbugs}{pdl}->double;  ##--  [b]   -> f(b)
  my $eps = $lr->{log_eps}||0;

  ##-- get common data: entropies
  my $hw  = -log2(($fw+$eps)/$N)->slice("*1,");
  my $hb  = -log2(($fb+$eps)/$N);

  foreach my $z (0,1) {
    my $zpdl = $pdl->slice("($z)");             ##-- [b,w] -> f(b,w) [--> PROFILE(w,b)]

    my $hbw = -log2(($zpdl+$eps)/$N);           ##-- [b,w] -> h(b,w)
    $zpdl  .= $hw + $hb - $hwb;
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
