##-*- Mode: CPerl -*-
##
## File: MUDL::Corpus::Buffer::Packed.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: in-memory corpus buffer: packed strings with enums
##
## --> as yet UNUSED
##
##======================================================================

package MUDL::Corpus::Buffer::Packed;
use MUDL::Corpus::Buffer;
use MUDL::Enum;
use MUDL::PToken;
#use PDL;
use Carp;

use strict;

our @ISA = qw(MUDL::Corpus::Buffer);

##======================================================================
## MUDL::Corpus::Buffer::Packed: Constructor
##======================================================================

## $cb = MUDL::Corpus::Buffer::Packed->new(%args)
##   + %args:
##      ##
##      ##-- basic data
##      sents => \@packed_sentences,   ## list of sentences of packed tokens
##      ##
##      ##-- general buffer stuff
##      offset   => $zero_offset,      ## >= 0, logical offset for {sents}[0]
##      ##
##      ##-- enums
##      wantattrs => $bool_or_hashref,  ## boolean: keep all attrs yes/no (default=yes)
##      enums     => \%attr2enum,       ## (text=>$textEnum, tag=>$tagEnum, ..., $attrN=>$attrNEnum)
##      _ptenums  => \@ptoken_addstack, ## if defined, suitable for passing to MUDL::PToken::pushAttributes()
sub new {
  my $that = shift;
  return bless {
		##-- basic data
		sents    => [],

		##-- buffer stuff
		offset   => 0,

		##-- enums
		enums     => {},
		_ptenums  => undef,
		wantattrs => 1,

		@_
	       }, ref($that)||$that;
}

##======================================================================
## methods: new

## \@ptenums = $buf->ensureEnumsR()
##  + ensures that MUDL::PToken is set up to decode from our enums
sub ensureEnumsR ($) {
  return $_[0]{_ptenums} = [MUDL::PToken->pushEnums($_[0]{enums})]
    if (!defined($_[0]{_ptenums}));
  MUDL::PToken->pushAttributes(@{$_[0]{_ptenums}})
    if (!defined($MUDL::PToken::STACK->[0]) || $_[0]{_ptenums}[0] ne $MUDL::PToken::STACK->[0]);
  return $_[0]{_ptenums};
}


##======================================================================
## accessors

## $cb = $cb->clear()
sub clear {
  my $cb = shift;
  @{$cb->{sents}}  = qw();
  %{$cb->{enums}} = qw();
  $cb->{_ptenums} = undef;
  return $cb;
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
sub flush {
  my $cb = shift;
  @{$cb->{sents}}  = qw();
  $cb->{offset}  += $cb->{nsents};
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
  return MUDL::CorpusIO::PackedBufReader->new(buffer=>$cb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to MUDL::CorpusIO::BufWriter->new()
sub writer {
  my $cb = shift;
  return MUDL::CorpusIO::PackedBufWriter->new(buffer=>$cb,@_);
}

##======================================================================
## I/O: generic (CorpusIO)
##======================================================================

## $cb = $class_or_obj->fromReader($corpusReader,%args)
##  + fills buffer from $corpusReader
##  + %args are passed to $cb->writer()
## (inherited)

## $cb = $cb->toWriter($corpusWriter,%args)
##  + writes buffer contents to $corpusWriter
##  + %args are passed to $cb->reader()
## (inherited)

##======================================================================
## I/O: files
##======================================================================

## $cb = $class_or_obj->fromFile($filename,%args)
##  + populates buffer from $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileReader()
## (inherited)

## $cb = $cb->toFile($filename,%args)
##  + writes buffer contents to file $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileWriter()
## (inherited)


########################################################################
##
## I/O: PackedBufReader
##
##  + read from a MUDL::Corpus::Buffer::Packed
##
########################################################################


package MUDL::CorpusIO::PackedBufReader;
use MUDL::Corpus::Buffer;
use strict;
our @ISA = qw(MUDL::CorpusIO::BufReader);

## $cr = MUDL::CorpusIO::PackedBufReader->new(buffer=>$buffer,%args)
##  + structure:
##    buffer => $corpus_buffer,     ##-- underlying buffer (alias='corpus')
##    rpos   => $sentence_index,    ##-- *logical*  read position in underlying buffer (sentence index)
##    ntoks => $ntoks,              ##-- track total number of tokens read
##    nsents=> $nsents,             ##-- track total number of sentences read
##
sub new {
  my ($that,%args) = @_;
  $args{buffer} = $args{corpus} if (!defined($args{buffer}));
  delete($args{corpus});
  return bless {
		buffer=>undef,
		rpos  =>0,
		nsents=>0,
		ntoks =>0,
		%args
	       }, ref($that)||$that;
}


## $buf_or_undef = $cr->buffer()
## $buf          = $cr->buffer($buf)
##   + get/set buffer (may return undef)
#(inherited)

## $bufferOrClass = $cr->getBuffer()
##  + get buffer or an appropriate class
##  + used by fromString, fromFile, etc. methods
sub getBuffer { return $_[0]{buffer} || 'MUDL::Corpus::Buffer::Packed'; }

## $bool = $cr->eof
# (inherited)

## \@sentence = $cr->getSentence();
##   + ye olde guttes
sub getSentence {
  my $cr = shift;
  my ($buf,$s);
  return undef if (!defined($buf=$cr->{buffer})
		   || $cr->{rpos}-$buf->{offset} > $#{$buf->{sents}});

  ##-- instantiate enums
  if (!defined($buf->{_ptenums})) {
    $buf->{_ptenums} = [MUDL::PToken->pushEnums($buf->{enums})];
  } elsif (!defined($MUDL::PToken::STACK->[0]) || $buf->{_ptenums}[0] ne $MUDL::PToken::STACK->[0]) {
    MUDL::PToken->pushAttributes(@{$buf->{_ptenums}});
  }

  $s = $cr->{buffer}{sents}[$cr->{rpos}++ - $cr->{buffer}{offset}];
  $cr->{nsents}++;
  $cr->{ntoks} += scalar(@$s);
  return $s;
}

## \%token_or_undef = $cr->getToken();
##  + not implemented

## undef = $cr->fromString($string)
##  + inherited from MUDL::Corpus::Buffer

## undef = $cr->fromFile($filename_or_fh);
##  + inherited from MUDL::Corpus::Buffer

## $n = $cr->nSentences()
##  + returns number of sentences already read
#(inherited) : $cr->{nsents}

## undef = $cr->reset();
# + inherited

## $n = $cr->nTokens()
#(inherited): $cr->{ntoks}


########################################################################
## I/O: PackedBufWriter
##
##  + write to a MUDL::Corpus::Buffer::Packed
########################################################################

package MUDL::CorpusIO::PackedBufWriter;
use PDL;
use Carp;
our @ISA = qw(MUDL::CorpusIO::BufWriter);

## $cr = CLASS->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer, ##-- underlying buffer (alias='corpus')
##    type=>   $pdl_datatype,  ##-- default=long
sub new {
  my ($that,%args) = @_;
  my $buf = defined($args{buffer}) ? $args{buffer} : $args{corpus};
  $buf = MUDL::Corpus::Buffer::Packed->new() if (!defined($buf));
  delete(@args{qw(buffer corpus)});
  return bless({
		buffer=>$buf,
		##
		##-- CorpusWriter wrapping
		#saveSub=>\&saveSub,   ##-- method to use for buffer saving
		#                      ##   + this gets set by toString(), toFile(), etc.
		#saveArgs=>\@args,     ##-- user args to \&saveSub, e.g. 'mode','iolayers'; default: none
		##
		##-- user args
		%args,
	       }, ref($that)||$that);
}

## $bool = $cw->flush
# (inherited)

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$s) = @_; ##-- really just ($cw,$s)
  my $buf = $cw->{buffer};

  ##-- check for easy answer
  #(there isn't one (yet))

  ##-- ensure enums are setup
  if (!defined($buf->{_ptenums})) {
    $buf->{_ptenums} = [MUDL::PToken->pushEnums($buf->{enums})];
  } elsif (!defined($MUDL::PToken::STACK->[0]) || $buf->{_ptenums}[0] ne $MUDL::PToken::STACK->[0]) {
    MUDL::PToken->pushAttributes(@{$buf->{_ptenums}});
  }

  ##-- ensure enums are consistent
  my $bs = bless [], 'MUDL::Sentence';
  my ($srcHash,$enum,$id,$aval);
  foreach (@$s) {
    if (UNIVERSAL::isa($_,'MUDL::PToken')) {
      push(@$bs,$_);
      next;
    } elsif (UNIVERSAL::isa($_,'MUDL::EToken') && (!defined($_->{_enums}) || $_->{_enums} eq $buf->{enums})) {
      my $str = pack('N*', map { defined($_) ? $_ : -1 } @$_{@$MUDL::PToken::NAMES});
      push(@$bs, bless(\$str,'MUDL::PToken'));
    } elsif (UNIVERSAL::isa($_,'MUDL::EToken::TT') && (!defined($_->[0]) || $_->[0] eq $buf->{_ptenums}[0])) {
      my $str = pack('N*', map { defined($_) ? $_ : -1 } @$_[1..$#$_]);
      push(@$bs, bless(\$str,'MUDL::PToken'));
    } else {
      $srcHash = $_->asHash;
      my $str = pack('N*',
		     map {
		       $aval=$srcHash->{$MUDL::PToken::NAMES->[$_]};
		       $enum=$MUDL::PToken::ENUMS->[$_];
		       if (!defined($id=$enum->{sym2id}{$aval})) {
			 $id = $enum->{sym2id}{$aval} = scalar(@{$enum->{id2sym}});
			 push(@{$enum->{id2sym}}, $aval);
		       }
		       $id
		     } (0..$#$MUDL::PToken::NAMES));
      push(@$bs, bless(\$str,'MUDL::PToken'));
    }
  }
  push(@{$buf->{sents}},$bs);

  return $cw;
}


## undef = $cr->putToken($text_or_hashref);
##  + not implemented

## undef = $cr->toString(\$string)
#(inherited?)

## undef = $cr->toFile($filename_or_fh);
## undef = $cr->Fh($fh);
#(inherited?)


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

Copyright (c) 2007, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
