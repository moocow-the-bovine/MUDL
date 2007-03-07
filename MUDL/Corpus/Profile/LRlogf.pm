#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRlogf.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R log-frequency
##======================================================================

package MUDL::Corpus::Profile::LRlogf;
use MUDL::Corpus::Profile::LRFBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Profile::LRFBigrams); #)

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + new %args:
##       eps => $eps,               ##-- replaces log(0): default: 1 or 0.1*(minimum non-zero value)
##
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
##       left=>$left_bigrams,       ## ($target,$lneighbor)
##       right=>$right_bigrams,     ## ($target,$rneighbor)
##       smoothgt=>$which,
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>0,%args);
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

  ##-- get zero mask
  my $nzmask = ($pdl != 0);

  ##-- get minimum value
  my $eps = $lr->{eps};
  if (!defined($eps)) {
    $eps  = $pdl->where($nzmask)->flat->minimum;
    if ($eps > 1) {
      $eps .= 1;
    } else {
      $eps *= 0.1;
    }
  }

  ##-- set eps
  $pdl->where(!$nzmask) .= $eps;

  ##-- do log
  $pdl->inplace->log;

  ##-- hack
  $pdl->inplace->setnantobad->inplace->setbadtoval(0);

  return $pdl;
}

## undef = $lr->normalizePdl($pdl);
##
## (inherited)

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right- log-frequency profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  eps=EPS          [default=1 or half-minimum]\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
}

##======================================================================
## I/O: Native
## - (output only!)

## $bool = $obj->saveNativeFh($fh,%args)
sub saveNativeFh {
  my ($obj,$fh) = @_;
  $obj->{left}->toDist->saveNativeFh($fh,@_);
  $fh->print("\n\n\n");
  $obj->{right}->toDist->saveNativeFh($fh,@_);
  return $obj;
}


1;

##======================================================================
## Docs
=pod

=head1 NAME

MUDL - MUDL Unsupervised Dependency Learner

=head1 SYNOPSIS

 use MUDL;

=cut

##======================================================================
## Description
=pod

=head1 DESCRIPTION

...

=cut

##======================================================================
## Footer
=pod

=head1 ACKNOWLEDGEMENTS

perl by Larry Wall.

=head1 AUTHOR

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
