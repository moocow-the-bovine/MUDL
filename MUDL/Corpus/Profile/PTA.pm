#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::PTA.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: Prefix Tree Acceptor
##======================================================================

package MUDL::Corpus::Profile::PTA;
use MUDL::Gfsm::FreqTrie;
use MUDL::Corpus::Profile;
use Carp;

use strict;
our @ISA = qw(MUDL::Gfsm::FreqTrie MUDL::Corpus::Profile);

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
     eos=>'__$',
     bos=>'__$',
     reversed=>$that->reverseDefault,
     %args,
    );

  return $self;
}

##======================================================================
## Profile Methods: Reset

## $prof = $prog->reset()
##   + resets profile distributions
sub reset {
  my $prof = shift;
  $prof->clear;
  return $prof;
}

##======================================================================
## Profile Methods: Shadow

## $prof = $prof->shadow(%args)
##  + return new profile of same form
##    - empty {left},{right} distributions
##    - everything else copied
sub shadow {
  my $prof = shift;
  my $prof2 = $prof->copy(@_);
  return $prof2;
}


##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($prof,$sent) = @_;
  $prof->addPathStrings([
			 (defined($prof->{bos}) ? $prof->{bos} : qw()),
			 @$sent,
			 (defined($prof->{eos}) ? $prof->{eos} : qw()),
			], 1);
}



##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Class for word-level prefix tree acceptor corpus profiles.\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
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
