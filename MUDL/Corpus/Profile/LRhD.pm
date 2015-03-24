#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRhD.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL: corpus profile: L-R conditional-vs-global pointwise entropy divergence:
##    prof(b,w) =    d( p(b|w) || p(b) )
##              = log2( p(b|w) /  p(b) )
##              = log2(p(b|w)) - log2(p(b))
##              = h(p(b)) - h(p(b|w))        : if p(b|w)>0
##              = 0                          : otherwise
##======================================================================
package MUDL::Corpus::Profile::LRhD;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use MUDL::EDist;
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

  ##-- get log2
  my $log2 = pdl(double,2)->log;

  ##-- get global pointwise bounds-entropies
  my $h_bds = $lr->{pbugs}{pdl}->double / $lr->{ftotal};
  $h_bds->inplace->log;
  $h_bds /= -$log2;
  $h_bds->inplace->setnantobad->inplace->setbadtoval(0);

  my ($P_t, $P_bgt);
  foreach my $z (0,1) {
    $P_bgt  = $pdl->slice("($z),,");
    $P_bgt /= $P_bgt->sumover->slice("*1,"); ##-- [b,w] -> p(b|w)

    $P_bgt->inplace->log;
    $P_bgt /= $log2;
    #$P_bgt->inplace->setnantobad->inplace->setbadtoval(0);
    $P_bgt += $h_bds;
  }
  $pdl->inplace->setnantobad->inplace->setbadtoval(0);

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

