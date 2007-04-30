#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Filter::Viterbi.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus filters: HMM / Viterbi
##======================================================================

package MUDL::Corpus::Filter::Viterbi;
use MUDL::Corpus::Filter;
use MUDL::HMM;
use Carp;
our @ISA = qw(MUDL::Corpus::Filter MUDL::HMM);

##----------------------------------------------------------------------
## Constructor
sub new {
  my $that = shift;
  return $that->MUDL::HMM::new(%{$that->MUDL::Corpus::Filter::new()}, @_);
}

##----------------------------------------------------------------------
## Sentence Processing
*doSentence = \&MUDL::HMM::viterbiTagSentence;

##----------------------------------------------------------------------
## Help String

## $string = $class_or_obj->helpString()
sub helpString {
  return
    (''
     ."Map tokens to best-path tags using Viterbi algorithm.\n"
     ."Options:\n"
     ."  bos=BOS           [default=__\$]\n"
     ."  eos=EOS           [default=__\$]\n"
     ."  unknown=UNK       [default=".'@UNKNOWN'."]\n"
     ."  oenum=ENUM        [observation enum]\n"
     ."  qenum=ENUM        [state/tag enum]\n"
     ."  ... and more\n"
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
