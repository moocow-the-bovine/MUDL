#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::SUnigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: supervised unigram tagger
##======================================================================

package MUDL::Corpus::Profile::SUnigrams;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use MUDL::Map;
use MUDL::Object;
use MUDL::Set;
use PDL;
use Carp;
our @ISA = qw(MUDL::Dist::Nary MUDL::Corpus::Profile);

##======================================================================
## new
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new('ugmap'=>MUDL::Map->new(),
			       nfields=>2,
			       %args);
  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($pr,$s) = @_;
  foreach my $tok (@$s) {
    ++$pr->{nz}{join($pr->{sep}, $tok->text, $tok->tag)};
  }
  return $pr;
}

##======================================================================
## Evaluation: Unigram modelling

## undef = $pr->finish()
sub finish {
  my $pr = shift;
  $pr->{ugmap} = $pr->toBestMap([0],[1]);
  return $pr;
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
