##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::Pdl::Bigrams.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: bigram distribution
##======================================================================

package MUDL::Corpus::Profile::Pdl::Bigrams;
#use MUDL::LogUtils qw(:all);
use MUDL::Bigrams;
use MUDL::Corpus::Profile::Pdl;
use MUDL::PdlDist::Sparse2d;
use PDL;
use PDL::Ngrams;
use Carp;
use strict;
our @ISA = qw(MUDL::PdlDist::Sparse2d MUDL::Corpus::Profile::Pdl MUDL::Corpus::Model);

our $DEFAULT_ZERO_PROB = $MUDL::Bigrams::DEFAULT_ZERO_PROB;

## OBJECT STRUCTURE:
##   + new in MUDL::Corpus::Profile::Pdl::Bigrams:
##      bos=>$bos_marker    # default "__$"
##      eos=>$eos_marker    # default "__$"
##
##   + from MUDL::PdlDist::Sparse2d:
##      enum   =>$enum_nary,   # event enum
##      ptr    =>$ptr,         # pdl($NW1): CCS-encoded matrix: pointer
##      rowids =>$rowids,      # pdl($Nnz): CCS-encoded matrix: rowids
##      nzvals =>$nzvals,      # pdl($Nnz): CCS-encoded matrix: nzvals
##
##   + from MUDL::Corpus::Profile::Pdl:
##      buffer =>$pdltt_buffer,
##      writer =>$buf_writer,
##
##   + new in Profile::Pdl::Bigrams
##      #colids =>$colids,      # pdl($Nnz): expanded column ids, for easy access
##
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(
				     bos=>'__$',
				     eos=>'__$',
				     nfields=>2,
				     @_
				    ), ref($that)||$that;
  return $self;
}

##======================================================================
## MUDL::Bigrams API: Accessors

## @words = $bg->vocabulary()
sub vocabulary { return @{ $_[0]{enum}{enums}[0]{id2sym} }; }

## size()
##   + returns potential number of bigrams
## inhierted from PdlDist::Sparse2d

## $unigrams = $bg->unigrams
sub unigrams {
  require MUDL::Corpus::Profile::Pdl::Unigrams;
  return MUDL::Corpus::Profile::Pdl::Unigrams->new('pdl'=>ccssumover(@{$_[0]}{qw(ptr rowids nzvals)}),
						   'enum'=>$_[0]{enum}{enums}[1],
						  );
}

##======================================================================
## MUDL::Bigrams API: conditionalize

## $bg = $bg->conditionalize()
## $bg = $bg->conditionalize($unigrams)
## $bg = $bg->conditionalize($unigrams,$totalunigrams)
##   + converts $bg->{nz} to { "$w1$fs$w2" => P($w2|$w1), ... }
#nyi/inherited

##======================================================================
## MUDL::Bigrams API: metrics, etc

## $H = $bg->conditionalEntropy($unigrams)
## $H = $bg->conditionalEntropy($unigrams,$ugtotal)
## $H = $bg->conditionalEntropy($unigrams,$ugtotal,$bgtotal)
#nyi


##======================================================================
## Corpus::Model API: Modelling
#nyi


##======================================================================
## Corpus::Profile::Pdl API

## undef = $profile->finishPdlProfile(%args)
##  + perform pdl-sensitive finishing actions
##  + called by default finish() method
##    - when this method is called, the buffer (if any) has been filled and pdl-ized
##    - after this completes, the buffer (if any) is deleted
sub finishPdlProfile {
  my $prf = shift;
  my $buf = $prf->{buffer};

  ##-- get basic data
  my $toks   = $buf->{pdls}[0];
  my $enum   = $buf->{enums}[0];

  ##-- get delimiters
  my $bosid   = defined($prf->{bos}) ? $enum->addSymbol($prf->{bos}) : undef;
  my $eosid   = defined($prf->{eos}) ? $enum->addSymbol($prf->{eos}) : $bosid;
  my ($offsets,$delims);
  if (defined($bosid) || defined($eosid)) {
    ##-- delimiters
    $bosid = $eosid if (defined($eosid) && !defined($bosid));
    $eosid = $bosid if (defined($bosid) && !defined($eosid));
    $offsets = PDL->pdl(PDL::long,[0])->append($buf->{ends});
    $delims  = PDL->pdl(PDL::long,[$bosid,($eosid==$bosid ? qw() : $eosid)])->slice("*2,");
  } else {
    ##-- no bos,eos: no delimiters
    $offsets = undef;
    $delims  = undef;
  }

  ##-- count bigrams
  my ($bgfreqs,$bgelts) = $toks->slice("*2,")->ng_cofreq(boffsets=>$offsets, delims=>$delims);

  ##-- setup MUDL::PdlDist properties
  my $NTypes      = $enum->size;
  $prf->{dims}    = [] if (!defined($prf->{dims}));
  @{$prf->{dims}} = ($NTypes,$NTypes);
  $prf->{enum}    = MUDL::Enum::Nary->new(nfields=>2,enums=>[$enum,$enum]);

  @$prf{qw(ptr rowids nzvals)}   = PDL::CCS::ccsencode_i2d($bgelts->slice("(0),"),
							   $bgelts->slice("(1),"),
							   $bgfreqs);

  ##-- all done
  return $prf;
}


##======================================================================
## Corpus::Profile API
# inherited


##======================================================================
## Corpus::Profile API: Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return (
	  qq(Extract token-text bigrams.\n)
	  .qq(- see also MUDL::Bigrams\n)
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
