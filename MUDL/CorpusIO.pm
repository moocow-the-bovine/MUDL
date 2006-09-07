##-*- Mode: Perl -*-
##
## File: MUDL::CorpusIO.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpora: I/O
##======================================================================

package MUDL::CorpusIO;
use MUDL::Object;
use MUDL::Token;
use MUDL::Sentence;
use IO::File;
use Carp;
use Encode qw(encode decode);

our $VERSION = 0.01;
our @ISA = qw(MUDL::Object);

########################################################################
## I/O : Abstract: CorpusIO
########################################################################
package MUDL::CorpusIO;
use File::Basename;
use MUDL::Object qw(dummy);
use strict;
use Carp;

## $cr = $class_or_object->fileReader($filename,%args)
##  + new reader for $filename
##  + $filename may be prefixed with 'fmt:'
our %FORMATS =
  (
   ##-- FileList
   listed => 'FileList',
   files  => 'FileList',
   ##
   ##-- XML
   xml => 'XML',
   ##
   ##-- TT
   ttt  => 'TT',
   tt  => 'TT',
   t => 'TT',
   ##
   ##-- Brown
   bt => 'Brown',
   btt => 'Brown',
   bttt => 'Brown',
   brown => 'Brown',
   ##
   ##-- Lob
   lob => 'LOB',
   ##
   ##-- aliases
   native => 'TT',
   tnt => 'TT',
   ##
   ##-- default
   #DEFAULT => 'XML',
   DEFAULT => 'TT',
  );
*fileReader = \&formatReader;
sub formatReader {
  my ($that,$file,@args) = @_;
  my $class = "MUDL::CorpusReader::";
  my $fmt = 'DEFAULT';
  if ($file =~ s/^(\S+)://) {
    $fmt = $1;
  } else {
    foreach (keys(%FORMATS)) {
      if ("\L$file\E" =~ /\.$_$/) {
	$fmt = $_;
	last;
      }
    }
  }
  $class .= $FORMATS{$fmt};
  my $obj;
  if (ref($that) && UNIVERSAL::isa($that,$class)) {
    $obj = $that;
  } else {
    $obj = $class->new(@args);
  }
  $obj->fromFile($file,@args);
  return $obj;
}

## $cr = $class_or_object->fileWriter($filename,%args)
##  + new writer for $filename
##  + $filename may be prefixed with '${FORMAT}:'
*fileWriter = \&formatWriter;
sub formatWriter {
  my ($that,$file,@args) = @_;
  my $class = "MUDL::CorpusWriter::";
  my $fmt = 'DEFAULT';
  if ($file =~ s/^(\S+)://) {
    $fmt = $1;
  }
  else {
    foreach (keys(%FORMATS)) {
      if ("\L$file\E" =~ /\.$_$/) {
	$fmt = $_;
	last;
      }
    }
  }
  $class .= $FORMATS{$fmt};
  my $obj;
  if (ref($that) && UNIVERSAL::isa($that,$class)) {
    $obj = $that;
  } else {
    $obj = $class->new(@args);
  }
  $obj->toFile($file,@args);
  return $obj;
}


########################################################################
## I/O : Abstract: Corpus Reader
########################################################################
package MUDL::CorpusReader;
use strict;
use Carp;
our @ISA = qw(MUDL::CorpusIO);
MUDL::Object->import('dummy');

## $bool = $cr->eof
*eof = dummy('eof');

## \@sentence = $cr->getSentence();
*getSentence = dummy('getSentence');

## \%token_or_undef = $cr->getToken();
*getToken = dummy('getToken');

## undef = $cr->fromString($string)
*fromString = dummy('fromString');

## undef = $cr->fromFile($filename_or_fh);
*fromFile = dummy('fromFile');
*fromFh = dummy('fromFh');

## $n = $cr->nSentences()
*nSentences = *nSents = *nsents = dummy('nSentences');

## undef = $cr->reset()
*reset = dummy('reset');

## $n = $cr->nTokens()
*nTokens = *nToks = *ntoks = dummy('nTokens');

########################################################################
## I/O : Abstract: Corpus Writer
########################################################################
package MUDL::CorpusWriter;
use strict;
use Carp;
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusIO);

## $bool = $cw->flush
*flush = dummy('flush');

## undef = $cw->putSentence(\@sent);
*putSentence = dummy('putSentence');

## undef = $cr->putToken($text_or_hashref);
*putToken = dummy('putToken');

## undef = $cr->toString(\$string)
*toString = dummy('toString');

## undef = $cr->toFile($filename_or_fh);
*toFile = dummy('toFile');
*toFh = dummy('toFh');

########################################################################
## I/O : Subclasses
########################################################################

use MUDL::CorpusIO::TT;
use MUDL::CorpusIO::Separated;
#use MUDL::CorpusIO::LOB;
#use MUDL::CorpusIO::Brown;
use MUDL::CorpusIO::XML;
use MUDL::CorpusIO::FileList;

########################################################################
## I/O : Memory
########################################################################

##-- OBSOLETE
##   + use MUDL::CorpusIO::BufReader, MUDL::CorpusIO::BufWriter
##   + defined in MUDL::Corpus::Buffer;
package MUDL::CorpusIO::Corpus;      our @ISA = qw(MUDL::CorpusIO::BufReader MUDL::CorpusIO::BufWriter);
package MUDL::CorpusReader::Corpus;  our @ISA=qw(MUDL::CorpusIO::BufReader);
package MUDL::CorpusWriter::Corpus;  our @ISA=qw(MUDL::CorpusIO::BufWriter);

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
