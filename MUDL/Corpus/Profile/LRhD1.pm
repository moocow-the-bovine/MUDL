#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRhD1.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL: corpus profile: L-R conditional-vs-global pointwise entropy divergence, pre-smoothed:
##    prof(b,w) =    d( p^(b|w) || p^(b) )
##              = log2( p^(b|w) /  p^(b) )
##              = log2(p^(b|w)) - log2(p^(b))
##              = h(p^(b)) - h(p^(b|w))
##    where:
##     p^(b)   = p_ML(b) = f(b)/N
##     p^(b|w) = (f(b,w)+1)/f(w)
##======================================================================
package MUDL::Corpus::Profile::LRhD1;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use MUDL::PDL::Stats ':all';
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
  return $that->SUPER::new(nfields=>1,donorm=>1,norm_min=>0,%args);
  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + inherited

##======================================================================
## Conversion: to PDL

##-- inherited from MUDL:::Corpus::Profile::LR
## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl,%args)
##
## $pdl3d = $lr->smoothPdl($pdl3d,%args);


## $pdl3d = $lr->finishPdl($pdl3d,%args);
sub finishPdl {
  my ($lr,$pdl) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  ##-- get global pointwise bounds-entropies
  my $N   = $lr->{ftotal};
  my $h_b = -log2($lr->{pbugs}{pdl}->double / $N);     ##-- [b] -> h(b)
  my $f_t = $lr->{ptugs}{pdl}->double;                 ##-- [w] -> h(w)
  #$h_b->inplace->setnantobad;

  foreach my $z (0,1) {
    my $zpdl  = $pdl->slice("($z)");
    my $h_bgt = -log2( ($zpdl+1) / (1+$f_t->slice("*1")) ); ##-- [b,w] -> h^(b|w)
    #$zpdl    .= $h_b - $h_bgt;
  }
  #$pdl->inplace->setnantobad->inplace->setbadtoval(0);
  $pdl->inplace->setnantobad;

  return $pdl;
}

## undef = $lr->normalizePdl($pdl);
##-- inherited


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right-profile wrt. fixed boundary set: pointwise conditional-vs-global divergence.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
     .qq(  smoothgt=WHICH   [default=0] : one of 'bigrams','pdl',0\n)
    );
}

1; ##-- make perl happy

