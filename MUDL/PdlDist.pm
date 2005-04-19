##-*- Mode: Perl -*-

## File: MUDL::PdlDist.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: PDL-based distributions
##======================================================================

package MUDL::PdlDist;
use MUDL::EDist;
use IO::File;
use PDL;
use PDL::Fit::Linfit;
use PDL::IO::Storable; ##-- very tricky
use Carp;

our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $pd = $class_or_obj->new(%args)
##  + object structure:
##     pdl=>$pdl,   ##-- pdl representing the distribution
##     enum=>$enum, ##-- event enum
##  + additional %args:
##     dims=>\@dimensions, ##-- specify initial pdl size
sub new {
  my ($that,%args) = @_;

  #my ($PDL,$pdl,$dims) = @args{qw(PDL pdl dims)};
  #$PDL = $pdl if (!defined($PDL) && defined($pdl));
  ##--
  my ($PDL,$dims) = @args{qw(pdl dims)};
  delete(@args{qw(PDL pdl dims)});

  $dims = [1] if (!defined($dims));
  if (!defined($PDL)) {
    $PDL = zeroes(double,@$dims);
  }

  my $self = $that->SUPER::new(
			       #PDL=>$PDL,
			       pdl=>$PDL,
			       enum=>MUDL::Enum->new(),
			       %args);
}

##======================================================================
## Copy

## $cpy = $pd->copy()
##   + also copies pdl
sub copy {
  my $d = shift;
  my $d2 = $d->SUPER::copy(@_);
  $d2->{pdl} = $d->{pdl}->copy;
  return $d2;
}


##======================================================================
## Accessors

## $size = $pd->size
sub size { return $_[0]->{pdl}->nelem; }

## $d = $d->clear()
sub clear {
  $_[0]{pdl} = null;
  $_[0]{enum}->clear;
  return $_[0];
}

## $nzero = $d->nZero()
*nzero = \&nZero;
sub nZero { return where($_[0]{pdl}==0)->nelem; }

## $nzero = $d->nzero()
*nnonzero = \&nNonZero;
sub nNonZero { return which($_[0]{pdl})->nelem; }

## $total = $d->total()
##   + includes "missing mass"
sub total { return sum($_[0]{pdl}); }

##======================================================================
## Batch-Conversions

## $d = $d->normalize()
sub normalize { $_[0]{pdl} /= $_[0]{pdl}->sum; return $_[0]; }

## $d = $d->conditionalize(\@givenDims)
sub conditionalize {
  my ($d,$given) = @_;
  my $p = $d->{pdl};
  my $ndims = $p->ndims;

  ##-- build dimension reordering list
  ##   + reroder given fields as primary, (at the front of the reorder list),
  ##     in preparation for clump()->sumover()
  my %givenHash = map { $_=>undef } @$given;
  my @targets   = grep { !exists($givenHash{$_}) } (0..($ndims-1));
  my @reordering = (@$given, @targets);

  ##-- prep for sumover()
  ##   + sumover() eliminates only 1st dim, so we reorder and clump
  my $pc = $p->reorder(@reordering)->clump($ndims-scalar(@targets));

  ##-- actual normalization
  ##   + operate on the mutated $pc, propagate back to $p
  $pc /= $pc->sumover->transpose;

  return $d;
}

## $ccounts = $d->countcounts()
sub countcounts {
  my $pd = shift;
  my $ccounts = MUDL::Dist->new();
  ++$ccounts->{$_} foreach ($pd->{pdl}->list);
  return $ccounts;
}

## $d = $d->pslice($sliceExpr)
##   + can be used as a projection operator
sub pslice { $_[0]{pdl}->slice($_[1]); return $_[0]; }

## $d = $d->project1($dim)
sub project1 { return $_[0]->projectN([$_[1]]); }

## $d2 = $d->projectN(\@dims)
##   + projects primary dimensions \@dims,
##     summing over all dimensions not in \@dims
sub projectN {
  my ($d,$pdims) = @_;
  my $p = $d->{pdl};
  my $ndims = $p->ndims;

  ##-- build dimension reordering list
  ##   + reroder sum-target fields as primary, (at the front of the reorder list),
  ##     in preparation for clump()->sumover()
  my %projHash   = map { $_=>undef } @$pdims;
  my @sumtargets = grep { !exists($projHash{$_}) } (0..($ndims-1));
  my @reordering = (@sumtargets, @$pdims);

  ##-- prep for sumover()
  ##   + sumover() eliminates only 1st dim, so we reorder and clump
  my $pc = $p->reorder(@reordering)->clump($ndims-scalar(@sumtargets));

  ##-- actual normalization
  ##   + operate on the mutated $pc, propagate back to $p
  my $p2 = $pc->sumover;

  return ref($d)->new(%$d, pdl=>$p2);
}

##======================================================================
## Metrics, etc.

## $mean = $d->mean()
sub mean {
  my $pd = shift;
  return $pd->{pdl}->avg;
}

## $H = $d->entropy()
## $H = $d->entropy($total)
*H = \&entropy;
sub entropy {
  my $d = shift;
  my $total = shift||$d->total;
  my $p = $d->{pdl};

  my $nzp = $p->where($p!=0) / $total;
  my $H   = sum($nzp * -log($nzp)/log(2));
  return $H;
}

## $I = $d->mutualInformation()
##   + get mutual information
##   + $d should be a 2d square joint probability matrix for
##     this method to produce meaningful results
##   + TODO: check the definition on this one...
*I = \&mutualInformation;
sub mutualInformation {
  my $d = shift;
  my $p = $d->{pdl};
  if ($p->ndims < 2 || $p->dim(0) != $p->dim(1)) {
    confess(ref($d), "::mutualInformation(): PDL matrix is not square!");
  }

  my $total = shift||$d->total;
  my $pxy = $p / $total;
  my $py  = $pxy->sumover;
  my $px  = $pxy->xchg(0,1)->sumover->transpose;

  my $mi  = $pxy * log($pxy/($px*$py))/log(2);
  $mi->inplace->setnantobad->inplace->setbadtoval(0); ##-- handle bad values

  return $mi->sum;
}

##======================================================================
## Pruning
##======================================================================

##-- NYI: not yet implemented


##======================================================================
## Conversion: enumeration
##======================================================================

## $edist_nary = $pd->toEDist();
## $edist_nary = $pd->toEDist($edist_nary);
sub toEDist {
  my ($pd,$ed) = @_;
  $ed = MUDL::EDist::Nary->new(enum=>$pd->{enum}) if (!defined($ed));

  my $pdl = $pd->{pdl};
  my @nzipdls = $pdl->whichND;
  return $ed if (!@nzipdls);

  my @nzi;
  foreach $i (0..($nzipdls[0]->nelem-1)) {
    @nzi = map { $_->at($i) } @nzipdls;
    $ed->{nz}{join($ed->{sep}, @nzi)} = $pdl->at(@nzi);
  }

  return $ed;
}

##======================================================================
## I/O: Native
##======================================================================

## nyi

##======================================================================
## I/O: XML
##======================================================================

##-- inherited

##======================================================================
## AUTOLOAD: pass to PDL
##  + doesn't seem to work well
##======================================================================

##-- ... but don't try to autoload DESTROY ...
sub DESTROY {}

our $AUTOLOAD;
sub AUTOLOAD {
  my $d = shift;
  return undef if (!defined($d) || !defined($d->{pdl}));
  (my $name = $AUTOLOAD) =~ s/.*:://; ##-- strip qualification

  my ($sub);
  if (!($sub=UNIVERSAL::can($d->{pdl},$name))) {
    croak( ref($d) , "::$name() not defined in ", __PACKAGE__ , "::AUTOLOAD.\n");
  }

  return $sub->($d->{pdl},@_);
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
