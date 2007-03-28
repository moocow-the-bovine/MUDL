##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::Pdl::MapTagger.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile
##    pdl-ized map
##======================================================================

package MUDL::Corpus::Profile::Pdl::Map;
use MUDL::Corpus::Profile::Pdl;
use MUDL::Corpus::Profile::Buffer::PdlTT;
use Carp;
use strict;
our @ISA = qw(MUDL::Corpus::Profile::Pdl MUDL::Corpus::Profile::Buffer::PdlTT); #)

##======================================================================
## Constructors etc.

## $obj = $class_or_obj->new(%args)
##  + %args for MUDL::Corpus::Profile::Pdl::Map
##     mapfrom => $attributeNumber,      ##-- default: 0 (text)
##     mapto   => $attributeNumber,      ##-- default: 0 (text)
##     mapkeep => \@attributeList,       ##-- which attributes to keep (undef or empty for all)
##     mapmap  => $map_pdl,              ##-- pdl($nSourceValues
##     mapenum => $enum,                 ##-- if defined, replaces $buffer->{enums}[$mapto]
##
##  + %args for Corpus::Profile::Pdl:
##     buffer => $corpus_buffer_pdltt,  ##-- buffer for generic Corpus::Profile API
##                                      ##   + NOT DELETED!
##     writer => $bufwriter,            ##-- buffer writer
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   mapfrom=>0,
			   mapto=>0,
			   mapkeep=>undef,
			   mapenum=>undef,
			   mapmap=>undef,
			   buffer=>MUDL::Corpus::Buffer::PdlTT->new(),
			   @_,
			  );
}


##======================================================================
## Corpus::Profile::Pdl Methods & overrides

## undef = $profile->finish(%args)
##  + perform any finishing actions
sub finish {
  my $prf = shift;
  my $buf = $prf->{buffer};
  $buf->packPdls();

  ##-- basic data
  my ($from,$to) = @$prf{qw(mapfrom mapto)};
  my $frompdl    = $buf->{pdls}[$from];

  ##-- set output enum if requested
  $buf->{enums}[$to] = $prf->{mapenum} if (defined($prf->{mapenum}));

  ##-- get new output pdl
  my $topdl = zeroes($buf->{enums}[$to]->type, $frompdl->nelem);
  $topdl   .= 

  return $prf;
}


##======================================================================
## Corpus::Profile Methods

# (inherited)


##======================================================================
## MUDL::Unigrams Methods: Accessors

## @words = $ug->vocabulary()
sub vocabulary { return @{ $_[0]{enum}{id2sym} }; }

## getSize()
##   + returns number of unigrams
sub getSize { return defined($_[0]{pdl}) ? $_[0]{pdl}->nelem : 0; }

##======================================================================
## MUDL::Corpus::Model Methods

## $log_p_sentence = $model->sentenceProbability(\@sent,%args)
##   + %args : 'unknown'=>$unknown_symbol
##             'punknown'=>p($unknown_symbol)
##nyi

## $log_psent = $model->sentenceProbability(\@sent,%args)
##   + %args : 'unknown'=>$unknown_symbol
##             'punknown'=>p($unknown_symbol)
##   + OBSOLETE!
##nyi


## \%info = $model->readerProbability($corpusReader,%args)
##   + %args : 'unknown'=>$unknown_symbol (default='@UNKNOWN')
##   + OBSOLETE
#*readerp = *readerP = *readerrob = *readerProb = \&readerProbability;
#nyi

##======================================================================
## Profile: Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return (
	  qq(Extract pdl-ized token-text unigrams\n)
	  .qq(- see also MUDL::Corpus::Profile::Unigrams::Pdl\n)
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
