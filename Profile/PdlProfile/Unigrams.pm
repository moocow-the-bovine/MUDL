##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::PdlProfile::Unigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: unigram distribution: pdl-ized
##======================================================================

package MUDL::Corpus::Profile::PdlProfile::Unigrams;
use MUDL::Corpus::Profile::PdlProfile;
use MUDL::Unigrams;
#use MUDL::LogUtils qw(:all);
#use MUDL::Enum;
use MUDL::PdlDist;
#use PDL;
use Carp;
use strict;
our @ISA = qw(MUDL::PdlDist MUDL::Corpus::Profile::PdlProfile MUDL::Corpus::Model); #)

our $DEFAULT_ZERO_PROB = $MUDL::Unigrams::DEFAULT_ZERO_PROB;

## OBJECT STRUCTURE: 
##   + from MUDL::PdlDist
##      enum=>$enum, ##-- 1d
##      pdl =>$pdl,  ##-- flat 1d pdl


##======================================================================
## Corpus::Profile::PdlProfile Methods

## undef = $profile->finishPdlProfile(%args)
##  + perform pdl-sensitive finishing actions
##  + called by default finish() method
##    - when this method is called, the buffer (if any) has been filled and pdl-ized
##    - after this completes, the buffer (if any) is deleted
sub finishPdlProfile {
  my $prf = shift;
  my $buf = $prf->{buffer};
  my $txtpdl  = $buf->{pdls}[0];
  my $txtenum = $buf->{enums}[0];

  my $ugp      = $txtpdl->hist(0,$txtenum->size,1); ##-- ca. 6x faster than rle()
  $prf->{enum} = $txtenum;
  $prf->{pdl}  = $ugp;

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
