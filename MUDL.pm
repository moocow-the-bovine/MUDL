#-*- Mode: Perl -*-

## File: MUDL.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner
##======================================================================

package MUDL;
our $VERSION = 0.01;

##-- just load all sub-modules
use MUDL::Utils;    ##-- very old, needs work
##use MUDL::FsaUtils; ##-- useful hacks: should update getArcs() to use perl-floats as weights

use MUDL::XML;
use MUDL::Corpus::Buffer;
use MUDL::Corpus;
use MUDL::CorpusIO;

#use MUDL::PDL; ##-- pretty much obsolete

use MUDL::Map;
use MUDL::Enum;
use MUDL::Enum::Nary;
use MUDL::Set;

use MUDL::Token;
use MUDL::Sentence;

use MUDL::Dist;
use MUDL::Dist::Partial;
use MUDL::Dist::Nary;
use MUDL::EDist;
use MUDL::Ranks;

use MUDL::Corpus::Profile;
use MUDL::Corpus::Profile::LRBound;

use MUDL::Unigrams;
use MUDL::Bigrams;

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
