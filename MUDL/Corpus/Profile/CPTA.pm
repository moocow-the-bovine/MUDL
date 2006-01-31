#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::CPTA.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: Character Prefix Tree Acceptor
##======================================================================

package MUDL::Corpus::Profile::CPTA;
use MUDL::Corpus::Profile::PTA;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile::PTA);

##======================================================================
## Constants & class methods

## $bool = $class_or_obj->reverseDefault()
##  + returns true if this class handles suffixes and not prefixes by default
sub reverseDefault { return 0; }

##======================================================================
## $prof = $class_or_obj->new(%args)
##   + %args:
##     ##-- inherited from Gfsm::FreqTrie
##     fsm=>$mudl_gfsm_automaton,
##     abet=>$mudl_gfsm_alphabet,
##     reversed=>$bool
##     ##-- new in Corpus::Profile::PTA
##     eos => $eos_str,
##     bos => $bos_str,
sub new {
  my ($that,%args) = @_;

  my $self = $that->SUPER::new
    (
     bos=>'#',
     eos=>'#',
     reversed=>$that->reverseDefault,
     %args,
    );

  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + addds characters for each word in \@sentence
sub addSentence {
  my ($prof,$sent) = @_;
  $prof->addPathChars(((defined($prof->{bos}) ? $prof->{bos} : '')
		       .(ref($_) ? $_->text : $_)
		       .(defined($prof->{eos}) ? $prof->{eos} : '')),
		      1)
    foreach (@$sent);
}



##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Class for character-level prefix tree acceptor corpus profiles.\n)
     .qq(  eos=EOS_CHAR   [default='#']\n)
     .qq(  bos=BOS_CHAR   [default='#']\n)
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
