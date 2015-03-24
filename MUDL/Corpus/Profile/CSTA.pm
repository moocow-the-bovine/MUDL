#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::CSTA.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: Character Suffix Tree Acceptor
##======================================================================

package MUDL::Corpus::Profile::CSTA;
use MUDL::Corpus::Profile::CPTA;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile::CPTA);

##======================================================================
## Constants & class methods

## $bool = $class_or_obj->reverseDefault()
##  + returns true if this class handles suffixes and not prefixes by default
sub reverseDefault { return 1; }

##======================================================================
## Methods

##-- Everything else inherited from Corpus::Profile::CPTA

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Class for character-level suffix tree acceptor corpus profiles.\n)
     .qq(  eos=EOS_STRING   [default='#']\n)
     .qq(  bos=BOS_STRING   [default='#']\n)
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
