##-*- Mode: CPerl -*-
##
## File: MUDL::Corpus::EBuffer.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: in-memory corpus buffers (enumerated)
##======================================================================

package MUDL::Corpus::EBuffer;
use MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
use MUDL::EToken;
use MUDL::Enum;
use MUDL::Object;
use Carp;

our @ISA = qw(MUDL::Corpus::Buffer);

##======================================================================
## MUDL::Corpus::EBuffer: Constructor
##======================================================================

## $cb = MUDL::Corpus::EBuffer->new(%args)
##   + %args:
##      enums => \%attr2enum,                        ##-- enumerators
##      sents => \@array_of_mudl_sentences,
##      offset => $logical_offset_of_first_sentence, # >= 0
sub new {
  my $that = shift;
  return bless { enums=>{}, sents=>[],offset=>0,@_ }, ref($that)||$that;
}

##======================================================================
## accessors

## $cb = $cb->clear()
##  + full clear, including enums
##  + any references still hanging around to the OLD 'enums' hashref should still be valid!
sub clear {
  my $cb = shift;
  $cb->{enums} = {};
  return $cb->SUPER::clear();
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
#
# (inherited)

##======================================================================
## I/O: on buffer
##======================================================================

## $cr = $buf->reader(%args)
##  + return a new MUDL::CorpusReader which takes input from the buffer
##  + %args are passed to MUDL::CorpusIO::EBufReader->new()
sub reader {
  my $cb = shift;
  return MUDL::CorpusIO::EBufReader->new(buffer=>$cb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to MUDL::CorpusIO::EBufWriter->new()
sub writer {
  my $cb = shift;
  return MUDL::CorpusIO::EBufWriter->new(buffer=>$cb,@_);
}

##======================================================================
## I/O: generic (CorpusIO)
##======================================================================

## $cb = $class_or_obj->fromReader($corpusReader,%args)
##  + fills buffer from $corpusReader
##  + %args are passed to $cb->writer()

# (inherited: TODO: optimize!)

## $cb = $cb->toWriter($corpusWriter,%args)
##  + writes buffer contents to $corpusWriter
##  + %args are passed to $cb->reader()

# (inherited: TODO: optimize!)

##======================================================================
## I/O: files
##======================================================================

## $cb = $class_or_obj->fromFile($filename,%args)
##  + populates buffer from $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileReader()

# (inherited: TODO: optimize!)

## $cb = $cb->toFile($filename,%args)
##  + writes buffer contents to file $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileWriter()

# (inherited: TODO: optimize!)


########################################################################
## I/O: EBufReader
##
##  + read from a MUDL::Corpus::EBuffer
##
########################################################################


package MUDL::CorpusIO::EBufReader;
use MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
our @ISA = qw(MUDL::CorpusIO::BufReader);

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

## $buf = $cr->buffer()
## $buf = $cr->buffer($buf)
##   + get/set buffer (may return undef)
#(inherited)

## $bool = $cr->eof
#(inherited)

## \@sentence = $cr->getSentence();
sub getSentence {
  my $cr = shift;
  return undef if (!defined($cr->{buffer})
		   || $cr->{rpos}-$cr->{buffer}{offset} > $#{$cr->{buffer}{sents}});
  my $s = $cr->{buffer}{sents}[$cr->{rpos}++ - $cr->{buffer}{offset}];
  $cr->{nsents}++;
  $cr->{ntoks} += scalar(@$s);
  return bless [
		map { bless({ %$_, _enums=>$cr->{buffer}{enums} }, 'MUDL::EToken') } @$s
	       ], 'MUDL::Sentence';
}

## \%token_or_undef = $cr->getToken();
##  + not implemented

## $bufferOrClass = $cr->getBuffer()
##  + get buffer or an appropriate class
##  + used by fromString, fromFile, etc. methods
sub getBuffer { return $_[0]{buffer} || 'MUDL::Corpus::EBuffer'; }

## undef = $cr->fromString($string)
#(inherited)

## undef = $cr->fromFile($filename_or_fh);
#(inherited)

## $n = $cr->nSentences()
##  + returns number of sentences already read
#(inherited)

## undef = $cr->reset();
#(inherited)

## $n = $cr->nTokens()
#(inherited)


########################################################################
## I/O: EBufWriter
##
##  + write to a MUDL::Corpus::EBuffer
########################################################################

package MUDL::CorpusIO::EBufWriter;
use MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
use MUDL::EToken;
use Carp;
our @ISA = qw(MUDL::CorpusIO::BufWriter);

## $cr = MUDL::CorpusIO::BufWriter->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer, ##-- underlying buffer (alias='corpus')
##    #wpos  => $buffer_index,  ##-- write position in underlying buffer
sub new {
  my ($that,%args) = @_;
  my $buf = defined($args{buffer}) ? $args{buffer} : $args{corpus};
  $buf = MUDL::Corpus::EBuffer->new() if (!defined($buf)); ##-- ensure buffer exists
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

## $buf = $cw->buffer()
## $buf = $cw->buffer($buf)
##   + get/set buffer (may return undef)
#(inherited)

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$s) = @_;

  ##-- convert sentence to ETokens
  my ($tok);
  push(@{$cw->{buffer}{sents}},
       bless [
	      map {
		$tok=MUDL::EToken->fromToken($_,_enums=>$cw->{buffer}{enums});
		delete($tok->{_enums});
		$tok
	      } @$s
	     ], 'MUDL::Sentence');

  return $cw;
}


## undef = $cr->putToken($text_or_hashref);
##  + not implemented

## $bool = $cw->flush
#(inherited)

## undef = $cr->toString(\$string)
#(inherited)

## undef = $cr->toFile($filename_or_fh);
## undef = $cr->toFh($fh);
#(inherited)


########################################################################
##
## EBuffer::TT
##
##  + array-based enumerating buffer
##
########################################################################
package MUDL::Corpus::EBuffer::TT;
use MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
use MUDL::EToken; ##-- for MUDL::EToken::TT
use MUDL::Enum;
use Carp;

use strict;

our @ISA = qw(MUDL::Corpus::EBuffer);

##======================================================================
## MUDL::Corpus::EBuffer::TT: Constructor
##======================================================================

## $cb = MUDL::Corpus::EBuffer::TT->new(%args)
##   + %args:
##      enums => \@attr2enum,                        ##-- enumerators
##      sents => \@array_of_mudl_sentences,
##      offset => $logical_offset_of_first_sentence, # >= 0
sub new {
  my $that = shift;
  return bless { enums=>[], sents=>[],offset=>0,@_ }, ref($that)||$that;
}

##======================================================================
## accessors

## $cb = $cb->clear()
##  + full clear, including enums
##  + any references still hanging around to the OLD 'enums' array-ref should still be valid!
sub clear {
  my $cb = shift;
  $cb->{enums} = [];
  return $cb->SUPER::clear();
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
# (inherited)

##======================================================================
## I/O: on buffer
##======================================================================

## $cr = $buf->reader(%args)
##  + return a new MUDL::CorpusReader which takes input from the buffer
##  + %args are passed to MUDL::CorpusIO::EBufTTReader->new()
sub reader {
  my $cb = shift;
  return MUDL::CorpusIO::EBufTTReader->new(buffer=>$cb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to MUDL::CorpusIO::EBufTTWriter->new()
sub writer {
  my $cb = shift;
  return MUDL::CorpusIO::EBufTTWriter->new(buffer=>$cb,@_);
}

##======================================================================
## I/O: generic (CorpusIO)
##======================================================================

## $cb = $class_or_obj->fromReader($corpusReader,%args)
##  + fills buffer from $corpusReader
##  + %args are passed to $cb->writer()
# (inherited: TODO: optimize!)

## $cb = $cb->toWriter($corpusWriter,%args)
##  + writes buffer contents to $corpusWriter
##  + %args are passed to $cb->reader()
# (inherited: TODO: optimize!)

##======================================================================
## I/O: files
##======================================================================

## $cb = $class_or_obj->fromFile($filename,%args)
##  + populates buffer from $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileReader()
# (inherited: TODO: optimize!)

## $cb = $cb->toFile($filename,%args)
##  + writes buffer contents to file $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileWriter()
# (inherited: TODO: optimize!)


########################################################################
## I/O: EBufTTReader
##
##  + read from a MUDL::Corpus::EBuffer::TT
##
########################################################################


package MUDL::CorpusIO::EBufTTReader;
use MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
use strict;
our @ISA = qw(MUDL::CorpusIO::EBufReader);

## $cr = MUDL::CorpusIO::EBufTTReader->new(buffer=>$buffer,%args)
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

## $buf = $cr->buffer()
## $buf = $cr->buffer($buf)
##   + get/set buffer (may return undef)
#(inherited)

## $bool = $cr->eof
#(inherited)

## \@sentence = $cr->getSentence();
sub getSentence {
  my $cr = shift;
  return undef if (!defined($cr->{buffer})
		   || $cr->{rpos}-$cr->{buffer}{offset} > $#{$cr->{buffer}{sents}});
  my $s = $cr->{buffer}{sents}[$cr->{rpos}++ - $cr->{buffer}{offset}];
  $cr->{nsents}++;
  $cr->{ntoks} += scalar(@$s);
  return bless [
		map { bless([$cr->{buffer}{enums}, @$_[1..$#$_]], 'MUDL::EToken::TT') } @$s
	       ], 'MUDL::Sentence';
}

## \%token_or_undef = $cr->getToken();
##  + not implemented

## $bufferOrClass = $cr->getBuffer()
##  + get buffer or an appropriate class
##  + used by fromString, fromFile, etc. methods
sub getBuffer { return $_[0]{buffer} || 'MUDL::Corpus::EBuffer::TT'; }

## undef = $cr->fromString($string)
#(inherited)

## undef = $cr->fromFile($filename_or_fh);
#(inherited)

## $n = $cr->nSentences()
##  + returns number of sentences already read
#(inherited)

## undef = $cr->reset();
#(inherited)

## $n = $cr->nTokens()
#(inherited)


########################################################################
## I/O: EBufTTWriter
##
##  + write to a MUDL::Corpus::EBuffer::TT
########################################################################

package MUDL::CorpusIO::EBufTTWriter;
use MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
use MUDL::EToken;
use Carp;
use strict;
our @ISA = qw(MUDL::CorpusIO::EBufWriter);

## $cr = MUDL::CorpusIO::BufWriter->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer, ##-- underlying buffer (alias='corpus')
##    #wpos  => $buffer_index,  ##-- write position in underlying buffer
sub new {
  my ($that,%args) = @_;
  my $buf = defined($args{buffer}) ? $args{buffer} : $args{corpus};
  $buf = MUDL::Corpus::EBuffer::TT->new() if (!defined($buf)); ##-- ensure buffer exists
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

## $buf = $cw->buffer()
## $buf = $cw->buffer($buf)
##   + get/set buffer (may return undef)
#(inherited)

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$s) = @_;

  ##-- convert sentence to ETokens
  my ($tok);
  push(@{$cw->{buffer}{sents}},
       bless [
	      map {
		$tok=MUDL::EToken::TT->fromToken($_,_enums=>$cw->{buffer}{enums});
		$tok->[0]=undef;
		$tok
	      } @$s
	     ], 'MUDL::Sentence');

  return $cw;
}


## undef = $cr->putToken($text_or_hashref);
##  + not implemented

## $bool = $cw->flush
#(inherited)

## undef = $cr->toString(\$string)
#(inherited)

## undef = $cr->toFile($filename_or_fh);
## undef = $cr->toFh($fh);
#(inherited)

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
