#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::LR.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R profiles
##======================================================================

package MUDL::Corpus::Profile::LR;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use MUDL::EDist;
use MUDL::Object;
use PDL;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       left => $left_edist,
##       right=> $right_edist,
##       donorm => $do_normalize,
##       nfields => $number_of_bound_fields, # ($nbf)
##  + event structure:
##       $lr->{$dir \in qw(left right)} : ($target_id, @nbf_bounds) = $lr->{dir}->split($event);
sub new {
  my ($that,%args) = @_;

  my $bounds = $args{bounds} || MUDL::Enum->new();
  my $targets = $args{targets} || MUDL::Enum->new();
  my $nfields = $args{nfields} || 1;
  my $enum = $args{enum} || MUDL::Enum::Nary->new(enums=>[$targets,(map { $bounds } (1..$nfields))]);
  delete($args{qw(bounds targets nfields enum)});

  my $self = $that->SUPER::new
    (eos=>'__$',
     bos=>'__$',
     targets=>$targets,
     bounds=>$bounds,
     nfields=>$nfields,
     enum=>$enum,
     left=>MUDL::EDist::Nary->new(nfields=>($nfields+1), enum=>$enum),
     right=>MUDL::EDist::Nary->new(nfields=>($nfields+1), enum=>$enum),
     donorm=>1, ##-- normalize on pdl-ization ?
     %args);

  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##-- not implemented here!


##======================================================================
## Conversion: to independent PDL

## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl)
##   + converts to pdl
##   + returned pdl is of dimensions: ($d,$n), where:
##     - $n == number-of-targets
##     - $d == 2 * (number-of-bounds ^ $nfields)   ##-- left-bounds & right-bounds
*toPDLi = \&toPDL;
sub toPDL {
  my ($lr,$pdl) = @_;

  ##-- enum
  my $nfields  = $lr->{nfields};
  my ($eb,$et) = @$lr{qw(bounds targets)};
  my $net      = $et->size;
  my $neb      = $eb->size;

  ##-- pdl
  $pdl = zeroes(double,1) if (!defined($pdl));
  $pdl->reshape(2*($neb**$nfields), $net)
    if ($pdl->dim(0) != 2*($neb**$nfields) || $pdl->dim(1) != $net);
  $pdl .= 0;

  ##-- data: left-context
  my ($k,$v,$tid,@bids);
  while (($k,$v)=each(%{$lr->{left}{nz}})) {
    ($tid,@bids) = $lr->{left}->split($k);
    foreach $i (0..$#bids) {
      $pdl->slice(($i*$neb + $bids[$i]) . ',' . $tid) += $v;
    }
  }
  ##-- data: right-context
  my $offset = $neb**$nfields;
  while (($k,$v)=each(%{$lr->{right}{nz}})) {
    ($tid,@bids) = $lr->{right}->split($k);
    foreach $i (0..$#bids) {
      $pdl->slice(($offset + ($i*$neb) + $bids[$i]) . ',' . $tid) += $v
    }
  }

  ##-- normalization
  $lr->normalizePdl($pdl) if ($lr->{donorm});

  return $pdl;
}


## undef = $lr->normalizePdl($pdl)
##  + normalize a pdl
sub normalizePdl {
  my ($lr,$pdl) = @_;
  my ($sum);

  my $offset = $pdl->dim(0)/2;
  $pdl -= $pdl->min;
  foreach $ni (0..$pdl->dim(1)-1) {
    ##-- normalize left- & right- subvectors independently
    ##-- : left subvector
    $v    = $pdl->slice('0:'.($offset-1).",$ni");
    $sum  = $v->sum;
    $v   /= $sum if ($sum != 0);

    ##-- : right subvector
    $v    = $pdl->slice("$offset:".($pdl->dim(0)-1).",$ni");
    $sum  = $v->sum;
    $v   /= $sum if ($sum != 0);
  }
  return $pdl;
}

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Abstract class for left-/right-profiles wrt. a given boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  enum=ENUM_NARY   [default=new(enums=>[TARGETS, BOUNDS x N-1])]\n)
     .qq(  nfields=N        [default=2]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
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
