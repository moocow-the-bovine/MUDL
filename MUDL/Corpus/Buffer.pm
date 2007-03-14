##-*- Mode: CPerl -*-
##
## File: MUDL::Corpus::Buffer.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: in-memory corpus buffers
##======================================================================

package MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
use MUDL::Object;
use Carp;

our @ISA = qw(MUDL::Object);

##======================================================================
## MUDL::Corpus::Buffer: Constructor
##======================================================================

## $cb = MUDL::Corpus::Buffer->new(%args)
##   + %args:
##      sents => \@array_of_mudl_sentences,
##      offset => $logical_offset_of_first_sentence, # >= 0
sub new {
  my $that = shift;
  return bless { sents=>[],offset=>0,@_ }, ref($that)||$that;
}

##======================================================================
## accessors

## $cb = $cb->clear()
##  + full clear
sub clear {
  my $cb = shift;
  $cb->{offset} = 0;
  @{$cb->{sents}} = qw();
  return $cb;
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
sub flush {
  my $cb = shift;
  $cb->{offset} += @{$cb->{sents}};
  @{$cb->{sents}} = qw();
  return $cb;
}

##======================================================================
## I/O: on buffer
##======================================================================

## $cr = $buf->reader(%args)
##  + return a new MUDL::CorpusReader which takes input from the buffer
##  + %args are passed to MUDL::CorpusIO::BufReader->new()
sub reader {
  my $cb = shift;
  return MUDL::CorpusIO::BufReader->new(buffer=>$cb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to MUDL::CorpusIO::BufWriter->new()
sub writer {
  my $cb = shift;
  return MUDL::CorpusIO::BufWriter->new(buffer=>$cb,@_);
}

##======================================================================
## I/O: generic (CorpusIO)
##======================================================================

## $cb = $class_or_obj->fromReader($corpusReader,%args)
##  + fills buffer from $corpusReader
##  + %args are passed to $cb->writer()
sub fromReader {
  my ($cb,$cr,%args) = @_;
  $cb = $cb->new(%args) if (!ref($cb));
  my $bw = $cb->writer(%args);
  my ($s);
  $bw->putSentence($s) while (defined($s=$cr->getSentence));
  return $cb;
}

## $cb = $cb->toWriter($corpusWriter,%args)
##  + writes buffer contents to $corpusWriter
##  + %args are passed to $cb->reader()
sub toWriter {
  my ($cb,$cw,%args) = @_;
  my $br = $cb->reader(%args);
  my ($s);
  $cw->putSentence($s) while (defined($s=$br->getSentence));
  return $cb;
}

##======================================================================
## I/O: files
##======================================================================

## $cb = $class_or_obj->fromFile($filename,%args)
##  + populates buffer from $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileReader()
sub fromFile {
  my ($cb,$file,%args) = @_;
  $cb = $cb->new() if (!ref($cb));
  my $bw = $cb->writer;
  my $fr = MUDL::CorpusIO->fileReader($file,%args);
  my ($s);
  $bw->putSentence($s) while (defined($s=$fr->getSentence));
  return $cb;
}

## $cb = $cb->toFile($filename,%args)
##  + writes buffer contents to file $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileWriter()
sub toFile {
  my ($cb,$file,%args) = @_;
  my $br = $cb->reader;
  my $fw = MUDL::CorpusIO->fileWriter($file,%args);
  my ($s);
  $fw->putSentence($s) while (defined($s=$br->getSentence));
  $fw->flush();
  return $cb;
}


########################################################################
## I/O: BufReader
##
##  + read from a MUDL::Corpus::Buffer
##
########################################################################


package MUDL::CorpusIO::BufReader;
use MUDL::CorpusIO;
our @ISA = qw(MUDL::CorpusReader);

## $cr = MUDL::CorpusIO::BufReader->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer, ##-- underlying buffer (alias='corpus')
##    rpos  => $buffer_index,  ##-- logical read position in underlying buffer
##
sub new {
  my ($that,%args) = @_;
  $args{buffer} = $args{corpus} if (!defined($args{buffer}));
  delete($args{corpus});
  return bless {
		buffer=>undef,
		rpos=>0,
		nsents=>0,
		##
		##--loadArgs=>[], ##-- args for loadGenericFile if called on buffer
		%args
	       }, ref($that)||$that;
}

## $buf_or_undef = $cr->buffer()
## $buf          = $cr->buffer($buf)
##   + get/set buffer (may return undef)
sub buffer {
  my $cr = shift;
  if (@_) {
    $cr->{buffer} = $_[0];
    $cr->{rpos}   = 0;
  }
  return $cr->{buffer};
}

## $bool = $cr->eof
sub eof {
  my $cr = shift;
  return (!defined($cr->{buffer})
	  || $cr->{rpos}-$cr->{buffer}{offset} > $#{$cr->{buffer}{sents}});
}

## \@sentence = $cr->getSentence();
sub getSentence {
  my $cr = shift;
  return undef if (!defined($cr->{buffer})
		   || $cr->{rpos}-$cr->{buffer}{offset} > $#{$cr->{buffer}{sents}});
  my $s = $cr->{buffer}{sents}[$cr->{rpos}++ - $cr->{buffer}{offset}];
  $cr->{nsents}++;
  $cr->{ntoks} += scalar(@$s);
  return $s;
}

## \%token_or_undef = $cr->getToken();
##  + not implemented

## $bufferOrClass = $cr->getBuffer()
##  + get buffer or an appropriate class
##  + used by fromString, fromFile, etc. methods
sub getBuffer { return $_[0]{buffer} || 'MUDL::Corpus::Buffer'; }

## undef = $cr->fromString($string)
sub fromString {
  my $cr = shift;
  $cr->{buffer} = $cr->getBuffer->loadBinString(shift);
  $cr->reset;
  return $cr;
}

## undef = $cr->fromFile($filename_or_fh);
sub fromFile {
  my ($cr,$file) = @_;
  if (ref($file)) {
    $cr->{buffer} = $cr->getBuffer->loadBinFh($file);
  } else {
    $cr->{buffer} = $cr->getBuffer->loadFile($file, (defined($cr->{loadArgs})
						     ? @{$cr->{loadArgs}}
						     : qw()));
  }
  $cr->reset;
  return $cr;
}

## $n = $cr->nSentences()
##  + returns number of sentences already read
sub nSentences { return $_[0]{nsents}; }

## undef = $cr->reset();
sub reset { $_[0]{rpos} = 0; }

## $n = $cr->nTokens()
sub nTokens { return $_[0]{ntoks}; }



########################################################################
## I/O: BufWriter
##
##  + write to a MUDL::Corpus::Buffer
########################################################################

package MUDL::CorpusIO::BufWriter;
use MUDL::CorpusIO;
use Carp;
our @ISA = qw(MUDL::CorpusWriter);

## $cr = MUDL::CorpusIO::BufWriter->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer, ##-- underlying buffer (alias='corpus')
##    #wpos  => $buffer_index,  ##-- write position in underlying buffer
sub new {
  my ($that,%args) = @_;
  my $buf = defined($args{buffer}) ? $args{buffer} : $args{corpus};
  $buf = MUDL::Corpus::Buffer->new() if (!defined($buf));
  delete(@args{qw(buffer corpus)});
  return bless {
		buffer=>$buf,
		#wpos=>undef,     ##-- undef: end-of-buffer
		#clobber=>0,      ##-- how many old elements to clobber
		##
		##-- CorpusWriter wrapping
		#saveSub=>\&saveSub,   ##-- method to use for buffer saving
		#                      ##   + this gets set by toString(), toFile(), etc.
		#saveArgs=>\@args,     ##-- user args to \&saveSub, e.g. 'mode','iolayers'; default: none
		%args
	       }, ref($that)||$that;
}

## DESTROY: close fh if defined
#sub DESTROY {
#  my $cw=shift;
#  $cw->{fh}->close if (defined($cw->{fh}));
#}

## $buf_or_undef = $cw->buffer()
## $buf          = $cw->buffer($buf)
##   + get/set buffer (may return undef)
sub buffer {
  my $cw = shift;
  if (@_) {
    $cw->{buffer} = $_[0];
    #$cw->{wpos}   = undef;
  }
  return $cw->{buffer};
}

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$s) = @_;

  #if (!defined($cw->{wpos})) {
  #  push(@{$cw->{buffer}{sents}}, $s);
  #} else {
  #  splice(@{$cw->{buffer}{sents}}, $cw->{wpos}++ - $cw->{buffer}{offset}, $cw->{clobber}, $s);
  #}
  ##--
  push(@{$cw->{buffer}{sents}}, $s);

  return $cw;
}


## undef = $cr->putToken($text_or_hashref);
##  + not implemented

## $bool = $cw->flush
sub flush {
  my $cw = shift;
  return 1 if (!defined($cw->{buffer}) || !defined($cw->{saveSub}));
  return $cw->{saveSub}->(defined($cw->{saveArgs}) ? @{$cw->{saveArgs}} : qw());
}

## undef = $cr->toString(\$string)
sub toString {
  #my ($cw,$strref)=@_;
  #$cw->{saveMethod} = sub { $cw->{buffer}->saveBinString($strref); };
  ##--
  my $buf    = $_[0]{buffer};
  my $strref = $_[1];
  $_[0]{saveSub} = sub { $$strref = $buf->saveBinString(@_); };
}

## undef = $cr->toFile($filename_or_fh);
## undef = $cr->toFh($fh);
*toFh = \&toFile;
sub toFile {
  #my ($cw,$file) = @_;
  #$cw->{saveMethod} = (ref($file)
	#	       ? UNIVERSAL::can($cw->{buffer},'saveBinFh')
	#	       : UNIVERSAL::can($cw->{buffer},'saveGenericFile'));
  ##--
  my $buf  = $_[0]{buffer};
  my $file = $_[1];
  return $_[0]{saveSub} = sub { $buf->saveBinString($file,@_) } if (ref($file));
  return $_[0]{saveSub} = sub { $buf->saveGenericFile($file,@_) };
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
