#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profiles
##======================================================================

package MUDL::Corpus::Profile;
use Module::Pluggable search_path=>['MUDL::Corpus::Profile'], require=>1;
use MUDL::Object qw(dummy);
use MUDL::CorpusIO;
use Carp;
our @ISA = qw(MUDL::Object);

##======================================================================
## Corpus::Profile: Abstract Methods

## undef = $profile->addCorpus($Corpus,@args)
##  + calls addReader()
sub addCorpus {
  return $_[0]->addReader(MUDL::CorpusReader::Corpus->new(corpus=>$_[1]),@_[2..$#_]);
}

## undef = $profile->addBuffer($buffer,@args)
##  + calls addReader()
sub addBuffer {
  return $_[0]->addReader($_[1]->reader(), @_[2..$#_]);
}

## undef = $profile->addReader($CorpusReader,@args)
##  + calls addSentence($s) for every sentence
sub addReader {
  my ($pr,$cr) = splice(@_,0,2);
  my ($s);
  $pr->addSentence($s,@_) while (defined($s=$cr->getSentence));
}

## undef = $profile->addSentence(\@sentence)
##  + dummy
*addSentence = dummy('addSentence');

## undef = $profile->finish(%args)
##  + perform any finishing actions
sub finish { ; }

## string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  if ((ref($that) && ref($that) eq __PACKAGE__) || $that eq __PACKAGE__) {
    return "Abstract corpus profiling plugin host class.";
  }
  return (ref($that)||$that)."::helpString() not implemented.";
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
