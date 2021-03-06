##-*- Mode: CPerl -*-

## File: MUDL::PdlDist.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: PDL-based distributions
##======================================================================

package MUDL::PdlDist;
use MUDL::EDist;
use IO::File;
use PDL;
use PDL::Fit::Linfit;
use MUDL::PDL::Smooth;

## PDL::IO::Storable
##  + very tricky: can cause errors "%Config::Config is read-only"
##    when require-ing PDL::Core::Dev.
##  + especially hairy when used in conjunction with Inline::Pdlpp
#require PDL::IO::Storable;

use Carp;

our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $pd = $class_or_obj->new(%args)
##  + object structure:
##     pdl=>$pdl,    ##-- pdl representing the distribution
##     enum=>$enum,  ##-- event enum
##     zmass=>$mass, ##-- "missing" mass (may be undef)
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
    $PDL = zeroes(PDL::double,@$dims);
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
  my @pdl_keys = qw(pdl);
  my @saved    = @$d{@pdl_keys};
  delete(@$d{@pdl_keys});
  my $d2 = $d->SUPER::copy(@_);
  @$d2{@pdl_keys} = map { pdl($_) } @saved;
  @$d{@pdl_keys} = @saved;
  return $d2;
}

## $cpy = $pd->clone()
##   + also copies pdl
sub clone {
  my $d = shift;
  my @pdl_keys = qw(pdl);
  my @saved    = @$d{@pdl_keys};
  delete(@$d{@pdl_keys});
  my $d2 = $d->SUPER::clone(@_);
  @$d2{@pdl_keys} = map { pdl($_) } @saved;
  @$d{@pdl_keys} = @saved;
  return $d2;
}


##======================================================================
## Accessors

## $size = $pd->size
sub size { return $_[0]->{pdl}->nelem; }

## $d = $d->clear()
sub clear {
  $_[0]{pdl} = PDL::null;
  $_[0]{enum}->clear;
  return $_[0];
}

## $nzero = $d->nZero()
*nzero = \&nZero;
sub nZero { return which($_[0]{pdl}==0)->nelem; }

## $nzero = $d->nzero()
*nnonzero = \&nNonZero;
sub nNonZero { return which($_[0]{pdl})->nelem; }

## $total = $d->total()
##   + includes "missing mass"
sub total { return sum($_[0]{pdl}); }

##======================================================================
## Batch-Conversions

## $d = $d->normalize()
sub normalize { $_[0]{pdl} /= $_[0]{pdl}->flat->sumover; return $_[0]; }

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
##  + returns a MUDL::Dist
##  + see also MUDL::Smooth::valcounts()
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
## Smoothing
##======================================================================

## $zmass = $d->missingMass()
##  + get total missing mass
*zmass = \&missingMass;
sub missingMass { return defined($_[0]{zmass}) ? $_[0]{zmass} : 0; }

## $zmass1 = $d->zeroCount()
##  + returns quantity of missing mass alotted to any single zero-count event
##  + uses $d->size()
sub zeroCount {
  my $nz = $_[0]->nZero;
  return $nz ? ($_[0]->{zmass} / $_[0]->nZero) : 0;
}

## $zmass = $dist->smoothGTLogLin()
## $zmass = $dist->smoothGTLogLin($minval=1)
##  + Good-Turing smoothing, log-linear
##  + sets key 'zmass' for missing mass
##  + sets key 'coeffs' = [$a,$e] for GT-smoothing (see MUDL::PDL::Smooth::loglinfit())
sub smoothGTLogLin {
  my ($pd,$minval)=@_;
  @$pd{qw(pdl coeffs zmass)} = $pd->{pdl}->smoothGTLogLin($minval);
  return $zmass;
}



##======================================================================
## Pruning
##======================================================================

##-- NYI: not yet implemented


##======================================================================
## Conversion: enumerated dist
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


## $edist = $pd->toEDistFlat();
## $edist = $pd->toEDistFlat($edist);
##  + conversion to a flat EDist, for 1d-pdls with 1d-enums ONLY
sub toEDistFlat {
  my ($pd,$ed) = @_;
  $ed = MUDL::EDist->new(enum=>$pd->{enum}) if (!defined($ed));

  my $pdl   = $pd->{pdl};
  my $which = $pdl->which;
  return $ed if (!defined($which) || $which->isempty);

  @{$ed->{nz}}{ $which->list } = $pdl->index($which)->list;

  return $ed;
}

##======================================================================
## I/O: Native
##======================================================================

## nyi
*saveNativeFh = \&saveTextFh;
*loadNativeFh = \&loadTextFh;

##======================================================================
## I/O: Dense text
##======================================================================
__PACKAGE__->registerIOMode('txt',{saveFh=>'saveTextFh',loadFh=>'loadTextFh'});
__PACKAGE__->registerFileSuffix('.txt','txt');

## $bool = $obj->saveTextFh($fh,%args)
##   + %args:
##      dims => $bool, ##-- print header-line @dims
##      fmt  => $fmt,  ##-- save format (%g)
##      sep  => $str,  ##-- separator string (default=TAB)
##   + only works for 2d matrices
sub saveTextFh {
  my ($pd,$fh,%args) = @_;

  my $pdl = $pd->{pdl};
  $args{dims} = 1 if (!defined($args{dims}));
  $args{sep} = "\t" if (!defined($args{sep}));
  $args{fmt} = "%g" if (!defined($args{fmt}));

  #require PDL::IO::Misc;
  #wcols((map { $pdl->sdlice("($_),:") } (0..($pdl->dim(0)-1))),
	#$fh,
	#{
	# HEADER=>join($args{sep}, $pdl->dims),
	#});

 $fh->print(join($args{sep}, $pdl->dims), "\n") if ($args{dims});

  my $template = join($args{sep}, map { $args{fmt} } (1..($pdl->dim(1))))."\n";
  my ($row,$col);
  foreach $row (0..($pdl->dim(0)-1)) {
    $fh->printf($template, $pdl->slice("($row)")->list);
  }

  return $pd;
}

## $obj = $class_or_obj->loadTextFh($fh,%args)
##   + %args:
##      sep  => $str,  ##-- separator string (default=TAB)
##   + only works for 2d matrices
sub loadTextFh {
  my ($pd,$fh,%args) = @_;
  $pd = $pd->new() if (!ref($pd));

  my $pdl = $pd->{pdl};
  my $sep = defined($args{sep}) ? $args{sep} : "\t";

  ##-- header
  my $line = $fh->getline;
  chomp($line);
  my @vals = CORE::split(/\Q${sep}\E+/,$line);
  $pd->{pdl} = $pdl->reshape(@vals);

  ##-- read data
  my $i=0;
  while (defined($line=$fh->getline)) {
    chomp($line);
    @vals = CORE::split(/\Q${sep}\E+/,$line);
    $pdl->slice("($i),") .= pdl($pdl->type, \@vals);
    $i++;
  }

  return $pd;
}


##======================================================================
## I/O: Sparse text (CCS)
##======================================================================
#__PACKAGE__->registerIOMode('ccs',{saveFh=>'saveCcsFh',loadFh=>'loadCcsFh'});
#__PACKAGE__->registerFileSuffix('.ccs','ccs');

##-- todo

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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
