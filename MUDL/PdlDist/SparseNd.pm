##-*- Mode: CPerl -*-

## File: MUDL::PdlDist::SparseND.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: sparse PDL-based distributions (N-dimensional)
##======================================================================

package MUDL::PdlDist::SparseNd;
use MUDL::PdlDist;
use MUDL::EDist;
use MUDL::PDL::Smooth;
use IO::File;
use PDL;
use PDL::CCS;
use PDL::CCS::Nd;

use strict;

## PDL::IO::Storable
##  + very tricky: can cause errors "%Config::Config is read-only"
##    when require-ing PDL::Core::Dev.
##  + especially hairy when used in conjunction with Inline::Pdlpp
#require PDL::IO::Storable;

use Carp;

our @ISA = qw(MUDL::Object);

our $CCSND_FLAGS_DEFAULT = (
			    $PDL::CCS::Nd::CCSND_BAD_IS_MISSING
			    |$PDL::CCS::Nd::CCSND_NAN_IS_MISSING
			   );

##======================================================================
## Constructor
## $pd = $class_or_obj->new(%args)
##  + object structure:
##     enum  =>$enum,       ##-- event enum
##     pdl   =>$pdl_nd,     ##-- a PDL::CCS::Nd object, or a dense pdl
##  + additional %args:
##     #dims=>\@dimensions, ##-- specify underlying matrix size
##     #dense=>$dense_pdl,  ##-- initialize from $dense_pdl
sub new {
  my ($that,%args) = @_;

  my ($pdl,$dense,$dims,$nfields) = @args{qw(pdl dense dims nfields)};
  delete(@args{qw(PDL pdl dims dense nfields)});
  $nfields ||= 2;
  $pdl = $dense if (!defined($pdl));
  $pdl = null if (!defined($pdl));

  $dims = [1] if (!defined($dims));
  return $that->SUPER::new(
			   pdl =>$pdl->toccs,
			   enum=>MUDL::Enum::Nary->new(nfields=>$pdl->ndims),
			   %args,
			  );
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
  @$d2{@pdl_keys} = map { $_->copy } @saved;
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
  @$d2{@pdl_keys} = map { $_->clone } @saved;
  @$d{@pdl_keys} = @saved;
  return $d2;
}

##======================================================================
## Encode / Decode & PDL ops

## $pdl = $d->densePdl()
## $pdl = $d->densePdl($dense)
sub densePdl { $_[0]{pdl}->decode($_[1]); }

## ($coli,$rowi) = $d->dwhich()
##   + indices are guaranteed (?) to be returned in $nzvals() order
sub dwhich { $_[0]{pdl}->whichND; }

## $dt = $d->dtranspose()
##  + we can't use 'transpose()' because PDL defines it (argh)
sub dtranspose {
  my $d = shift;

  my $dt = ref($d)->new(
			pdl =>$d->{pdl}->xchg(0,1),
			enum=>$d->{enum},
		       );
  return $dt;
}


##======================================================================
## Accessors

## $size = $pd->size
sub size { $_[0]{pdl}->nelem; }

## $d = $d->clear()
sub clear {
  $_[0]{pdl} = null->toccs;
  $_[0]{enum}->clear;
  return $_[0];
}

## $nzero = $d->nZero()
*nzero = \&nZero;
sub nZero { $_[0]{pdl}->nmissing; }

## $nzero = $d->nzero()
*nnonzero = \&nNonZero;
sub nNonZero { $_[0]{pdl}->_nnz; }

## $total = $d->total()
##   + should include "missing mass", but doesn't
sub total { $_[0]{pdl}->_nzvals->dsum + $_[0]{pdl}->nmissing * $_[0]{pdl}->missing->sclr; }

##======================================================================
## Batch-Conversions

## $d = $d->normalize()
sub normalize {
#  $_[0]{pdl} /= $_[0]{pdl}->flat->sumover; return $_[0];
  my $vals = $_[0]{pdl}->_vals;
  $vals /= $_[0]->total;
  return $_[0];
}

##-- TODO: conditionalize, etc.

##======================================================================
## Smoothing

## $zmass = $d->missingMass()
##  + get total missing mass
*zmass = \&missingMass;
sub missingMass {
  return defined($_[0]{zmass}) ? $_[0]{zmass} : $_[0]{pdl}->missing*$_[0]{pdl}->nmissing;
}

## $zmass1 = $d->zeroCount()
##  + returns quantity of missing mass alotted to any single zero-count event
sub zeroCount { return $_[0]{pdl}->missing(); }

## $zmass = $dist->smoothGTLogLin()
## $zmass = $dist->smoothGTLogLin($minval=1)
##  + Good-Turing smoothing, log-linear
##  + sets key 'zmass' for missing mass
##  + sets key 'coeffs' = [$a,$e] for GT-smoothing (see MUDL::PDL::Smooth::loglinfit())
sub smoothGTLogLin {
  my ($pd,$minval)=@_;
  my $ccs = $pd->{pdl};
  $ccs->inplace->convert(double);

  my $whichVals = $ccs->whichVals;
  my ($vfit,$coeffs,$zmass) = $whichVals->smoothGTLogLin($minval);
  $whichVals .= $vfit;
  $ccs->missing($zmass/$ccs->nmissing);

  @$pd{qw(coeffs zmass)} = ($coeffs,$zmass);
  return $zmass;
}


##======================================================================
## Metrics, etc.

## $mean = $d->mean()
sub mean { $_[0]->total / $_[0]->size; }

## $H = $d->entropy()
## $H = $d->entropy($total)
*H = \&entropy;
sub entropy {
  my $d = shift;
  my $total = shift||$d->total;
  my $nzvals = $d->{pdl}->_nzvals;

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
  $ed = MUDL::EDist::Nary->new(nfields=>$pd->{pdl}->ndims, enum=>$pd->{enum}) if (!defined($ed));

  my @which = map {[$_->list]} $pd->{pdl}->whichND;
  my ($i);
  @{$ed->{nz}}{
    map {
      $i=$_;
      join($ed->{sep}, map {$which[$_][$i]} (0..$#which))
    } (0..$#{$which[0]})
  } = $pd->{pdl}->_nzvals->list;

  return $ed;
}

## $dist_nary = $pd->toDist();
## $dist_nary = $pd->toDist($dist_nary);
sub toDist {
  my ($pd,$d) = @_;
  $d = MUDL::Dist::Nary->new(nfields=>$pd->{pdl}->ndims) if (!defined($d));

  my $which = $pd->{pdl}->whichND;
  my @which = (
	       map {
		 [ @{$pd->{enum}{enums}[$_]{id2sym}}[ $which->slice("($_),")->list ] ]
	       } (0..($pd->{pdl}->ndims-1))
	      );

  my ($i);
  @{$d->{nz}}{
    map {
      $i=$_;
      join($d->{sep}, map {$which[$_][$i]} (0..$#which))
    } (0..$#{$which[0]})
  } = $pd->{pdl}->_nzvals->list;

  return $d;

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

#our $AUTOLOAD;
#sub AUTOLOAD {
#  my $d = shift;
#  return undef if (!defined($d) || !defined($d->{nzvals}));
#  (my $name = $AUTOLOAD) =~ s/.*:://; ##-- strip qualification
#
#  my ($sub);
#  if (!($sub=UNIVERSAL::can($d->{nzvals},$name))) {
#    croak( ref($d) , "::$name() not defined in ", __PACKAGE__ , "::AUTOLOAD.\n");
#  }
#
#  return $sub->($d->{nzvals},@_);
#}



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
