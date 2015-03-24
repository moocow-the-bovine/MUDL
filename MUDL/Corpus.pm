#-*- Mode: CPerl -*-

## File: MUDL::Corpus.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpora: DEPRECATED
##======================================================================

package MUDL::Corpus;
use MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
use MUDL::Object;
use IO::File;
use Carp;

our $VERSION = 0.01;

our @ISA = qw(MUDL::Corpus::Buffer);

##======================================================================
## Constructor
##
## $corpus = MUDL::Corpus->new(%args)
##  + %args:
##    see MUDL::Corpus::Buffer
##  + deprecated: use MUDL::Corpus::Buffer instead
sub new {
  my $that = shift;
  return $that->SUPER::new(@_);
}

##======================================================================
## accessors

## \@sentences = $corpus->sentences()
sub sentences { return $_[0]->{sents}; }

##======================================================================
## I/O: TT
##
##  - OBSOLETE

## $corpus = $corpus->loadTTFile($filename_or_fh)
*loadNativeFile = *loadNativeFh = \&loadTTFile;
sub loadTTFile {
  my ($c,$file,%args) = @_;
  my $cr = MUDL::CorpusReader::TT->new(%args);
  $cr->fromFile($file);
  my $sents = $c->{sents};
  my $sent;
  while (defined($sent=$cr->getSentence(%args))) {
    push(@{$c->{sents}}, $sent);
  }
  return $c;
}

## $corpus = $corpus->saveTTFile($filename_or_fh)
*saveNativeFile = *saveNativeFh = \&saveTTFile;
sub saveTTFile {
  my ($c,$file,%args) = @_;
  my $cw = MUDL::CorpusWriter::TT->new(%args);
  $cw->toFile($file);
  my $sents = $c->{sents};
  foreach $sent (@{$c->{sents}}) {
    $cw->putSentence($sent);
  }
  return $c;
}

##======================================================================
## I/O: XML
##
##  - OBSOLETE

## $corpus = $corpus->loadXMLFile($filename_or_fh)
*loadXMLFh = \&loadXMLFile;
sub loadXMLFile {
  my ($c,$file,%args) = @_;
  my $cr = MUDL::CorpusReader::XML->new(%args);
  $cr->fromFile($file);
  my $sents = $c->{sents};
  my $sent;
  while (defined($sent=$cr->getSentence(%args))) {
    push(@{$c->{sents}}, $sent);
  }
  return $c;
}
## $corpus = $corpus->saveXMLFile($filename_or_fh)
*saveXMLFh = \&saveXMLFile;
sub saveXMLFile {
  my ($c,$file,%args) = @_;
  my $cw = MUDL::CorpusWriter::XML->new(%args);
  $cw->toFile($file);
  my $sents = $c->{sents};
  foreach $sent (@{$c->{sents}}) {
    $cw->putSentence($sent);
  }
  $cw->flush();
  return $c;
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
