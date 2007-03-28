#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRActivation.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R base activation
##======================================================================

package MUDL::Corpus::Profile::LRActivation;
use MUDL::Corpus::Profile::LR;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile::LR);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##     ##-- new in MUDL::Corpus::Profile::LRActivation:
##       decay=>$decay,    ## decay coefficient (default=1)
##       #thresh=>$thresh, ## activation threshhold [minimum] (default=0 (none))
##       nmax=>$n,         ## maximum distance [number of words] between target and bound (default=3)
##     ##-- inherited from MUDL::Corpus::Profile::LR:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
##       left=>$left_pseudo_bigrams,   ## ($target,$lneighbor)
##       right=>$right_pseudo_bigrams, ## ($target,$rneighbor)
##       smoothgt=>$which,
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>0,decay=>1,nmax=>3,%args); #thresh=>0
}

##======================================================================
## Profiling: addSentence()

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($pr,$s) = @_;

  ##-- sanity checks: bos/eos
  if (defined($pr->{bos})) {
    $pr->{bounds}->addSymbol($pr->{bos});
  }
  if (defined($pr->{eos})) {
    $pr->{bounds}->addSymbol($pr->{eos});
  }


  ##------ temporary sentence index profiles

  ##-- get profile properties
  my ($lact,$ract,$decay,$nmax) = @$pr{qw(left right decay nmax)};

  ##-- @st: sentence text
  my @st = ((defined($pr->{bos}) ? (map { $pr->{bos} } (1..$nmax)) : qw()),
	    (map { $_->text } @$s),
	    (defined($pr->{eos}) ? (map { $pr->{eos} } (1..$nmax)) : qw()));

  ##-- @tids, @bids: sentence target (bound) ids
  my @tids = @{$pr->{targets}{sym2id}}{@st};
  my @bids = @{$pr->{bounds}{sym2id}}{@st};


  my ($i,$j, $tid,$bid);
  for ($i=0; $i <= $#st; $i++) {
    next if (!defined($tid=$tids[$i]));

    ##-- left
    for ($j=1; ($i-$j)>=0 && $j<=$nmax; $j++) {
      next if (!defined($bid=$bids[$i-$j]));
      $lact->{nz}{$tid.$lact->{sep}.$bid} += $j**(-$decay);
    }

    ##-- right
    for ($j=1; ($i+$j)<=$#st && $j<=$nmax; $j++) {
      next if (!defined($bid=$bids[$i+$j]));
      $ract->{nz}{$tid.$ract->{sep}.$bid} += $j**(-$decay);
    }
  }

  return $pr;
}


##======================================================================
## Conversion: to PDL

##-- inherited from MUDL:::Corpus::Profile::LR

## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl)

## $pdl3d = $lr->smoothPdl($pdl3d);
##-- inherited from MUDL:::Corpus::Profile::LR

## $pdl3d = $lr->finishPdl($pdl3d);
##-- does nothing

## undef = $lr->normalizePdl($pdl);
##-- inherited from MUDL:::Corpus::Profile::LR


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right-activation profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
     .qq(  decay=COEFF      [default=1]\n)
     .qq(  nmax=N           [default=3 (use (n<=4)-grams)]\n)
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
