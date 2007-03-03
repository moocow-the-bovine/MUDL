##-*- Mode: CPerl -*-

## File: MUDL::PdlDist::Sparse2d.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: sparse PDL-based distributions (2d)
##======================================================================

package MUDL::PdlDist::Sparse2d;
use MUDL::PdlDist;
use MUDL::EDist;
use IO::File;
use PDL;
use PDL::CCS;

use strict;

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
##     enum  =>$enum,       ##-- event enum
##     ptr   =>$ccs_ptr,    ##-- see PDL::CCS
##     rowids=>$ccs_rowids, ##-- see PDL::CCS
##     nvals =>$ccs_nzvals, ##-- see PDL::CCS
##  + additional %args:
##     dims=>\@dimensions, ##-- specify underlying matrix size
##     dense=>$dense_pdl,  ##-- initialize from $dense_pdl
sub new {
  my ($that,%args) = @_;

  my ($dense,$dims) = @args{qw(dense dims)};
  delete(@args{qw(PDL pdl dims dense)});

  $dims = [1] if (!defined($dims));
  my ($ptr,$rowids,$nzvals);
  if (defined($dense)) {
    @$dims = $dense->dims;
    ($ptr,$rowids,$nzvals) = ccsencode($dense);
  } else {
    $ptr    = null;
    $rowids = null;
    $nzvals = null;
  }

  my $self = $that->SUPER::new(
			       ptr=>$ptr,
			       rowids=>$rowids,
			       nzvals=>$nzvals,
			       enum=>MUDL::Enum::Nary->new(nfields=>2),
			       dims=>$dims,
			       %args,
			      );
}

##======================================================================
## Copy

## $cpy = $pd->copy()
##   + also copies pdl
sub copy {
  my $d = shift;
  my @pdl_keys = qw(ptr rowids nzvals);
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
  my @pdl_keys = qw(ptr rowids nzvals);
  my @saved    = @$d{@pdl_keys};
  delete(@$d{@pdl_keys});
  my $d2 = $d->SUPER::clone(@_);
  @$d2{@pdl_keys} = map { pdl($_) } @saved;
  @$d{@pdl_keys} = @saved;
  return $d2;
}

##======================================================================
## Encode / Decode & PDL ops

## $pdl = $d->densePdl()
## $pdl = $d->densePdl($dense)
sub densePdl {
  my ($d,$dense) = @_;
  return ccsdecode(@$d{qw(ptr rowids nzvals)}, $d->{dims}[1], $dense);
}

## ($coli,$rowi) = $d->dwhich()
##   + indices are guaranteed (?) to be returned in $nzvals() order
sub dwhich { return ccswhichND(@{$_[0]}{qw(ptr rowids nzvals)}); }

## $dt = $d->dtranspose()
##  + we can't use 'transpose()' because PDL defines it (argh)
sub dtranspose {
  my $d = shift;

  my $ptrT    = zeroes(long,$d->{dims}[1]);
  my $rowidsT = zeroes(long,$d->{rowids}->nelem);
  my $nzvalsT = zeroes($d->{nzvals}->type, $d->{nzvals}->nelem);
  ccstranspose(@$d{qw(ptr rowids nzvals)}, $ptrT,$rowidsT,$nzvalsT);

  my $dt = ref($d)->new(
			dims=>[@{$d->{dims}}[1,0]],
			ptr=>$ptrT,
			rowids=>$rowidsT,
			nzvals=>$nzvalsT,
		       );
  return $dt;
}


##======================================================================
## Accessors

## $size = $pd->size
sub size { return pdl(double,$_[0]->{dims})->prodover->at(0); }

## $d = $d->clear()
sub clear {
  $_[0]{ptr} = null;
  $_[0]{rowids} = null;
  $_[0]{nzvals} = null;
  $_[0]{enum}->clear;
  return $_[0];
}

## $nzero = $d->nZero()
*nzero = \&nZero;
sub nZero { return $_[0]->size - $_[0]{nzvals}->nelem; }

## $nzero = $d->nzero()
*nnonzero = \&nNonZero;
sub nNonZero { return $_[0]{nzvals}->nelem; }

## $total = $d->total()
##   + should include "missing mass", but doesn't :::: FIXME
sub total { return $_[0]{nzvals}->sum; }



##======================================================================
## Batch-Conversions

## $d = $d->normalize()
sub normalize {
#  $_[0]{pdl} /= $_[0]{pdl}->flat->sumover; return $_[0];
  $_[0]{nzvals} /= $_[0]{nzvals}->sumover;
  return $_[0];
}

## $d = $d->conditionalize(\@givenDims)
sub conditionalize {
  my ($d,$given) = @_;

  ##-- HACK
  if (@$given==1) {
    if    ($given->[0] == 0) { return $d->conditionalize_byrow(); }
    elsif ($given->[0] == 1) { return $d->conditionalize_bycol(); }
  }
  die(ref($d), "::conditionalize(): can't handle 'given' list [", join(',',@$given),"]");
}

## $d = $d->conditionalize_bycol()
##  + each column is a given
sub conditionalize_bycol {
  my $d = shift;
  my ($ptr,$rowids,$nzvals) = @$d{qw(ptr rowids nzvals)};
  ccsdiv_rv($ptr,$rowids, $nzvals->inplace, ccssumovert($ptr,$rowids,$nzvals));
  return $d;
}

## $d = $d->conditionalize_byrow()
##  + each row is a given: hack: just transpose
sub conditionalize_byrow {
  my $d = shift;
  my ($ptr,$rowids,$nzvals) = @$d{qw(ptr rowids nzvals)};
  ccsdiv_cv($ptr,$rowids, $nzvals->inplace, ccssumover($ptr,$rowids,$nzvals));
  return $d;
}

## $d = $d->conditionalize_byrow()
##  + each row is a given: hack: just transpose
sub conditionalize_byrow_old {
  my $d = shift;
  my $dt = $d->dtranspose->conditionalize_bycol();
  ccstranspose(@$dt{qw(ptr rowids nzvals)}, @$d{qw(ptr rowids nzvals)});
  return $d;
}



##======================================================================
## Metrics, etc.

## $mean = $d->mean()
sub mean {
  my $d = shift;
  return $d->{nzvals}->sumover / $d->size;
}

## $H = $d->entropy()
## $H = $d->entropy($total)
*H = \&entropy;
sub entropy {
  my $d = shift;
  my $total = shift||$d->total;
  my $nzvals = $d->{nzvals};

  my $nzp = $nzvals->where($nzvals!=0) / $total;
  my $H   = sum($nzp * -log($nzp)/log(2));
  return $H;
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
  $ed = MUDL::EDist::Nary->new(nfields=>2,enum=>$pd->{enum}) if (!defined($ed));

  my $nzvals    = $pd->{nzvals};
  my @whichpdls = ccswhichND(@$pd{qw(ptr rowids)}, $nzvals);
  return $ed if (!@whichpdls);

  my (@nzi,$i);
  foreach $i (0..($whichpdls[0]->nelem-1)) {
    @nzi = map { $_->at($i) } @whichpdls;
    $ed->{nz}{join($ed->{sep}, @nzi)} = $nzvals->at($i);
  }

  return $ed;
}

##======================================================================
## I/O: Native
##======================================================================

## nyi
#*saveNativeFh = \&saveTextFh;
#*loadNativeFh = \&loadTextFh;

##======================================================================
## I/O: Dense text
##======================================================================
#__PACKAGE__->registerIOMode('txt',{saveFh=>'saveTextFh',loadFh=>'loadTextFh'});
#__PACKAGE__->registerFileSuffix('.txt','txt');

##-- todo

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
## AUTOLOAD: pass to nonzero-PDL
##  + doesn't seem to work well
##======================================================================

##-- ... but don't try to autoload DESTROY ...
sub DESTROY {}

our $AUTOLOAD;
sub AUTOLOAD {
  my $d = shift;
  return undef if (!defined($d) || !defined($d->{nzvals}));
  (my $name = $AUTOLOAD) =~ s/.*:://; ##-- strip qualification

  my ($sub);
  if (!($sub=UNIVERSAL::can($d->{nzvals},$name))) {
    croak( ref($d) , "::$name() not defined in ", __PACKAGE__ , "::AUTOLOAD.\n");
  }

  return $sub->($d->{nzvals},@_);
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
