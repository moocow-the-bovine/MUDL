#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRngd.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: corpus profile: L-R NGD ("normalized google distance")
##    prof(b,w) = NGD(b,w) = (max{log(f(b)),log(f(w))} - log(f(b,w))) / (log(N)-min{log(f(b)),log(f(w))})
##  + see: http://www.scholarpedia.org/article/Applications_of_algorithmic_information_theory
##    and:
##      Rudi Cilibrasi and Paul M.B. Vit\'{a}nyi, "Clustering by Compression",
##      IEEE Transactions on Information Theory, 51:4, pp. 1523-1545, April 2005.
##      URL: http://www.cwi.nl/~paulv/papers/cluster.pdf
##======================================================================

package MUDL::Corpus::Profile::LRngd;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
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

  ##-- get unigram dists
  my $log_eps = defined($lr->{log_eps}) ? $lr->{log_eps} : 0;
  my $log_fb = ($lr->{pbugs}{pdl}+$log_eps)->log;
  my $log_fw = ($lr->{ptugs}{pdl}+$log_eps)->log;
  my $log_N  = pdl(double,$lr->{ftotal})->log;

  ##-- prof(b,w) = NGD(b,w) = (max{log(f(b)),log(f(w))} - log(f(b,w))) / (log(N)-min{log(f(b)),log(f(w))})
  foreach my $z (0,1) {
    my $zpdl    = $pdl->slice("($z),,");
    my $log_fbw = ($zpdl+$log_eps)->log;

    my $log_fb_z    = $log_fb->index($log_fbw->xvals);
    my $log_fw_z    = $log_fw->index($log_fbw->yvals);
    my $log_fzbw    = $log_fb_z->cat($log_fw_z)->mv(2,0);
    my $log_fbw_max = $log_fzbw->maximum;
    my $log_fbw_min = $log_fzbw->minimum;

    $zpdl .= ($log_fbw_max - $log_fbw) / ($log_N - $log_fbw_min);
  }
  ##-- prune bad values (?)
  #my $maxval = $pdl->where($pdl->isfinite)->max;
  #if    ($maxval < 1) { $maxval = 1; }
  ##elsif ($maxval > 1) { $maxval++; }
  #$pdl->inplace->setnantobad->inplace->setbadtoval($maxval);
  ##
  ##-- just mark infinite values 'bad'
  $pdl->inplace->setnantobad();

  ##-- re-normalize (?)
  #$pdl /= $maxval if ($maxval != 1);

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
