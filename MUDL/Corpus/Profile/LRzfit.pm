#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRzfit.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL: corpus profile: L-R zipf fit
##    prof(b,w) = arcsize(b,w) = ?
##======================================================================

package MUDL::Corpus::Profile::LRzfit;
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

## $lr = $lr->addPdlBigrams($bg,%args);
##   + profiles to PDLs
sub addPdlBigrams {
  my ($lr,$bg,%args) = @_;

  ##-- save ftotal
  my $N = $bg->{pdl}->sum;

  ##-- log-linear fit
  my $bgp    = $bg->{pdl} = $bg->{pdl}->double;
  my $f      = $bgp->_vals->qsort;
  my $r      = $f->avgranks1_dsc;
  my $fu     = $f->uniq;
  my $ru     = $r->qsort->uniq->slice("-1:0");
  my ($fit,$coeffs) = ($fu+1)->loglinfit($ru);
  my ($a,$e) = $coeffs->dog;
  $bgp->_vals( $bgp->_vals->interpol($fu,$a*$ru**$e) );
  $bgp->missing( ($bgp->missing/$bgp->nmissing)->sclr );

  ##-- pass on to inherited method
  $lr = $lr->SUPER::addPdlBigrams($bg,%args);

  ##-- restore ftotal
  #$lr->{ftotal} = $N if ($lr);

  ##-- return
  return $lr;
}


##======================================================================
## Conversion: to PDL

## $pdl3d = $lr->finishPdl($pdl3d);
sub finishPdl {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args;   ##-- args: clobber

  $pdl->inplace->log;
  $pdl->inplace->setnantobad;

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
