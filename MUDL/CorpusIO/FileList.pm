##-*- Mode: CPerl -*-
##
## File: MUDL::CorpusIO::FileList.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpora: I/O: split (distributed) corpora
##======================================================================

package MUDL::CorpusIO::FileList;
use MUDL::Object;
use MUDL::CorpusIO;
use IO::File;

use strict;
use Carp;


########################################################################
## I/O : FileList : Reader
########################################################################
package MUDL::CorpusReader::FileList;
use MUDL::CorpusIO;
use strict;
use Carp;
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusReader);

sub new {
  my ($that,%args) = @_;
  my $self = bless {
		    subclass => 'MUDL::CorpusIO', ##-- subcorpus reader class
		    subargs  => [],               ##-- args to $subcr->fileReader()

		    ##-- File list
		    subcr => undef,               ##-- subcorpus reader object
		    files => [],                  ##-- subcorpus list (filenames)
		    filei => -1,                  ##-- current file number

		    ##-- Stats
		    nsents=>0,
		    ntoks =>0,

		    ##-- file list construction
		    split_re  => "\\Q$/\\E",      ##-- file list split regex for fromString()
		    select_re => undef,           ##-- file item select regex (overrides ignore_re)
		    ignore_re => '^\s*$',         ##-- file item ignore regex

		    ##-- user args
		    %args,
		   }, ref($that)||$that;
  return $self;
}

sub DESTROY {
  my $cr = shift;
  delete($cr->{subcr});
}

##------------------------------------------------------
## CorpusReader::FileList: API

## $n = nSentences()
*nSents = *nsents = \&nSentences;
sub nSentences { return $_[0]{nsents}; }

## $n = $cr->nTokens()
*nToks = *ntoks = \&nTokens;
sub nTokens { return $_[0]{ntoks}; }

## reset()
sub reset {
  my $cr = shift;
  delete($cr->{subcr});
  $cr->{nsents} = $cr->{ntoks} = 0;
  @{$cr->{files}} = qw();
  $cr->{filei} = -1;
  return $cr;
}

## $bool = $cr->eof
sub eof {
  my $subcr = $_[0]->subreader();
  return (!$subcr || $subcr->eof());
}

## undef = $cr->fromString($list_str)
sub fromString {
  my ($cr,$string) = @_;
  return $cr->fromArray([split($cr->{split_re}, $string)]);
}

## undef = $cr->fromFile($filename)
## undef = $cr->fromFh($fh)
*fromFh = \&fromFile;
sub fromFile {
  my ($cr,$file,%args) = @_;
  @$cr{keys %args} = values(%args);
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  croak(__PACKAGE__, "::fromFile(): open failed for '$file': $!") if (!$fh);
  my $rc = $cr->fromArray([map { chomp; $_ } $fh->getlines]);
  $fh->close() if (!ref($file));
  return $rc;
}

## \@sentence_or_undef = $cr->getSentence();
sub getSentence {
  my $cr    = shift;
  my $subcr = $cr->subreader();
  my $s     = undef;
  while ($subcr) {
    last if (defined($s = $subcr->getSentence(@_)));
    $subcr = $cr->subreader();
  }
  ++$cr->{nsents} if ($s);
  return $s;
}

## \%token_or_undef = $cr->getToken();
##  + NOT DEFINED
*getToken = dummy('getToken');

##------------------------------------------------------
## CorpusReader::FileList: new methods

## $cr = $cr->fromArray(\@fileList)
sub fromArray {
  my ($cr,$files) = @_;
  $cr->reset();
  @{$cr->{files}} =
    grep {
      (defined($_) && (defined($cr->{select_re})
		       ? ($_ =~ $cr->{select_re})
		       : (defined($cr->{ignore_re})
			  ? ($_ !~ $cr->{ignore_re})
			  : 1)))
    } @$files;
  return $cr;
}

## $reader_or_undef = $cr->subreader()
##  + returns actual reader
##  + undef indicates eof
sub subreader {
  my $cr = shift;
  while (!$cr->{subcr} || $cr->{subcr}->eof()) {
    delete($cr->{subcr});
    return undef if ($cr->{filei} >= $#{$cr->{files}});
    ++$cr->{filei};
    $cr->{subcr} = $cr->{subclass}->fileReader($cr->{files}[$cr->{filei}], @{$cr->{subargs}})
      or confess(ref($cr)."::subreader(): open failed for '$cr->{files}[$cr->{filei}]': $!");
  }
  return $cr->{subcr};
}



########################################################################
## I/O : FileList : Writer
########################################################################
package MUDL::CorpusWriter::FileList;
use Carp;
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusWriter);


## $cw = class->new(%args)
##   + known %args:
##      ##-- Subcorpus List Construction
##      sep_str => $str,               ##-- separator string (default=$/)
sub new {
  my ($that,%args) = @_;
  my $cw = bless {
		  ##-- list filehandle
		  fh => undef,

		  ##-- Subcorpus list construction
		  sep_str => $/,

		  ##-- user args
		  %args,
		 }, ref($that)||$that;
  return $cw;
}

sub DESTROY {
  my $cw = shift;
  $cw->{fh}->close if (defined($cw->{fh}));
}

##------------------------------------------------------
## CorpusWriter::FileList: API

## $bool = $cw->flush
sub flush {
  return $_[0]->{fh} ? $_[0]->{fh}->flush : undef;
}

## undef = $cw->toString(\$str)
sub toString {
  my ($cw,$sref) = @_;
  $cw->{fh}->close() if (defined($cw->{fh}));
  $cw->{fh} = IO::Scalar->new(\$sref)
    or croak( __PACKAGE__ , "::toString(): open failed: $!");
}

## undef = $cw->toFile($filename_or_fh)
*toFh = \&toFile;
sub toFile {
  my ($cw,$file,%args) = @_;
  $cw->{fh}->close() if (defined($cw->{fh}));
  $cw->{fh} = ref($file) ? $file : IO::File->new(">$file");
  croak( __PACKAGE__ , "::toFile(): open failed for '$file': $!") if (!$cw->{fh});
  binmode($cw->{fh}, $_) foreach (@{$cw->{layers}});
  @$cw{keys %args} = values(%args);
}

## undef = $cw->putSentence(\@sent);
##  + DUMMY
*putSentence = dummy('putSentence');

## undef = $cw->putToken($text_or_hashref)
*putToken = dummy('putToken');

##------------------------------------------------------
## CorpusWriter::FileList: new

## $rc = $cw->putFiles(@filenames)
*putFile = \&putFiles;
sub putFiles {
  my $cw = shift;
  croak(ref($cw)."::putFiles(): no output handle!") if (!$cw->{fh});
  $cw->{fh}->print(map { ($_,$cw->{sep_str}) } @_);
  return 1;
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
