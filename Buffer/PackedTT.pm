##-*- Mode: CPerl -*-
##
## File: MUDL::Corpus::Buffer::PackedTT.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: in-memory corpus buffers
##  + can be "compiled" to enums
##  + tokens are bin-saved as packed id-strings
##======================================================================

package MUDL::Corpus::Buffer::PackedTT;
use MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
use MUDL::Enum;
use MUDL::Object;
use Carp;

our @ISA = qw(MUDL::Corpus::Buffer);

##======================================================================
## MUDL::Corpus::Buffer::PackedTT : Constructor
##======================================================================

## $cb = $class_or_object->new(%args)
##   + %args:
##      enums     => \@enums,   ##-- enumerators by attribute-position
##      sents     => \@sents,   ##-- array of sentences
##      offset    => $n,        ##-- logical offset of 1st stored sentence, >= 0
##      fixedWidth=> $bool,     ##-- whether this is a fixed-width format (default=true)
##      poffset   => $index,    ##-- physical index of 1st unpacked sentence
##      eoffset   => $index,    ##-- physical index of 1st un-enumerated sentence
##      packas    => $template, ##-- pack template, default='ww' [see $pb->updatePackTemplate()]
sub new {
  my $that = shift;
  my $pb=bless {
		enums=>undef,
		sents=>[],
		offset=>0,
		fixedWidth=>1,
		poffset=>0,
		eoffset=>0,
		packas=>'',
		@_
	       }, ref($that)||$that;
  ##-- create default enums
  if (!defined($pb->{enums})) {
    $pb->{enums} = [];
#    $pb->{enums} = [
#		    MUDL::Enum->new(), ##-- text
#		    MUDL::Enum->new(), ##-- tag
#		   ];
  }
  return $pb;
}

##======================================================================
## new API

## \@enums = $pb->updateEnums()
##  + fills $pb->{enums} based on pending data in $pb->{sents}
##  + if $pb->{fixedWidth} is set and true, only the first pending token is checked
sub updateEnums {
  my $pb = shift;
  #return $pb->{enums} if ($pb->{eoffset} > $#{$pb->{sents}}); ##-- skip this: update {packas}, etc.

  my $enums    = $pb->{enums};
  my $nfields  = scalar @$enums;
  ##-- get maximum number of fields
  my ($ts,$tok,$ntfields);
  my $isFixedWidth = $pb->{fixedWidth};
 FILLENUMS_SENTENCE:
  foreach (@{$pb->{sents}}[$pb->{eoffset}..$#{$pb->{sents}}]) {
    ##
    ##-- check for pre-packed sentences
    next if (ref($ts) eq 'MUDL::Sentence::PackedTT');

    ##-- force "alien" sentences to MUDL::Sentence::TT
    $_ = MUDL::Sentence::TT->stealSentence($_) if (ref($_) ne 'MUDL::Sentence::TT');

    ##-- count attributes, assuming we have MUDL::Token::TT objects
    foreach (@$_) {
      $nfields  = @$_ if (@$_ > $nfields);
      last FILLENUMS_SENTENCE if ($isFixedWidth);
    }
  }

  ##-- create & populate enums
  my ($attri,$enum,$sym2id,$id2sym);
  foreach $attri (0..($nfields-1)) {
    $enum   = $enums->[$attri] = MUDL::Enum->new if (!defined($enum=$enums->[$attri]));
    $sym2id = $enum->{sym2id};
    $id2sym = $enum->{id2sym};
    foreach (@{$pb->{sents}}[$pb->{eoffset}..$#{$pb->{sents}}]) {
      next if (ref($_) eq 'MUDL::Sentence::Buffered::PackedTT'); ##-- skip compatible sentences
      foreach (@$_) {
	if (!exists($sym2id->{$_->[$attri]})) {
	  $sym2id->{$_->[$attri]} = scalar(@$id2sym);
	  push(@$id2sym,$_->[$attri]);
	}
      }
    }
  }

  ##-- update/set $pb->{eoffset}
  $pb->{eoffset} = scalar(@{$pb->{sents}});

  return $enums;
}

## $packas = $pb->updatePackTemplate()
##  + updates pack template $pb->{enums}[$_]{packas}, $pb->{packas} based on $pb->{enums}[$_]->size
##  + enums should have already been filled
sub updatePackTemplate {
  my $pb = shift;

  my ($enum);
  foreach $enum (@{$pb->{enums}}) {
    ##-- update/set $enum->{packas}
    $esize = scalar(@{$enum->{id2sym}});
    if    ($esize < 2**8 -1) { $enum->{packas}='C'; } ##-- signed char  (exactly  8-bit)
    elsif ($esize < 2**16-1) { $enum->{packas}='S'; } ##-- signed short (exactly 16-bit)
    elsif ($esize < 2**32-1) { $enum->{packas}='L'; } ##-- signed long  (exactly 32-bit)
    else                     { $enum->{packas}='Q'; } ##-- signed quad  (exactly 64-bit) [if supported]
    #else                    { $enum->{packas}='w'; } ##-- BER-compressed integer, variable length NON-NEGATIVE

    ##-- set 'unknown' id for enums, based on enum-size (UNUSED by MUDL::Enum)
    $enum->{noLabel} = $esize;
  }

  return $pb->{packas} = join('', map {$_->{packas}} @{$pb->{enums}});
}

## $pb = $pb->packSentences()
##  + packs sentences in $pb->{sents}
sub packSentences {
  my $pb = shift;

  ##-- create & populate enums
  my $enums  = $pb->updateEnums();
  my $packas = $pb->updatePackTemplate();

  ##-- pack sentences in the buffer
  my ($tok,$val);
  foreach (@{$pb->{sents}}[$pb->{poffset}..$#{$pb->{sents}}]) {
    next if (ref($_) eq 'MUDL::Sentence::PackedTT');           ##-- skip compatible sentences
    foreach (@$_) {
      $tok = $_;
      $_ = pack($packas,
		map {
		  $val=$enums->[$_]{sym2id}{$tok->[$_]};
		  defined($val) ? $val : $enums->[$_]{noLabel} ##-- bash undefined attributes to 'noLabel'
		} (0..$#$tok));
    }
    bless($_,'MUDL::Sentence::PackedTT');
  }

  ##-- update 'poffset'
  $pb->{poffset} = scalar(@{$pb->{sents}});

  return $pb;
}

## $pb = $pb->unpackSentences()
##  + unpacks all packed sentences in $pb->{sents}
sub unpackSentences {
  my $pb = shift;

  my ($tok);
  my $packas = $pb->{packas};
  my $enums  = $pb->{enums};
  foreach (@{$pb->{sents}}) {
    next if (ref($_) eq 'MUDL::Sentence::TT');
    foreach (@$_) {
      $_  = $tok = bless [unpack($packas,$_)], 'MUDL::Token::TT';
      @$_ = map { $enums->[$_]{id2sym}[$tok->[$_]] } (0..$#$enums);
    }
    bless($_, 'MUDL::Sentence::TT');
  }

  $pb->{poffset} = 0;
  return $pb;
}


##======================================================================
## accessors

## $cb = $cb->clear()
##  + full clear, including enums
##  + any references still hanging around to the OLD 'enums' hashref should still be valid!
sub clear {
  my $cb = shift;
  $cb->{enums} = [];
  @{$cb->{sents}} = qw();
  $cb->{offset} = 0;
  $cb->{eoffset} = 0;
  $cb->{poffset} = 0;
  $cb->{packas}  = '';
  return $cb;
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
sub flush {
  my $cb = shift;
  $cb->{offset}  += @{$cb->{sents}};

  $cb->{eoffset} -= @{$cb->{sents}};
  $cb->{eoffset}  = 0 if ($cb->{eoffset} < 0);

  $cb->{poffset} -= @{$cb->{sents}};
  $cb->{poffset}  = 0 if ($cb->{poffset} < 0);

  @{$cb->{sents}} = qw();
  return $cb;
}

##======================================================================
## I/O: on buffer
##======================================================================

## $cr = $buf->reader(%args)
##  + return a new MUDL::CorpusReader which takes input from the buffer
##  + %args are passed to $reader_class->new()
sub reader {
  my $cb = shift;
  return MUDL::CorpusReader::Buffer::PackedTT->new(buffer=>$cb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to $writer_class->new()
sub writer {
  my $cb = shift;
  return MUDL::CorpusWriter::Buffer::PackedTT->new(buffer=>$cb,@_);
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

## $cb = $cb->toFile($filename,%args)
##  + writes buffer contents to file $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileWriter()


##======================================================================
## I/O: MUDL::Object: Binary
##======================================================================

## ($serialized_string, @other_refs) = STORABLE_freeze($obj,$cloning_flag)
##  + implicitly calls packSentences()
sub STORABLE_freeze {
  my ($obj,$cloning) = @_;
  $obj->packSentences();
  return ('',[%$obj]);
}

## ($serialized_string, @other_refs) = STORABLE_thaw($obj,$cloning_flag)
##  + does NOT implicitly call unpackSentences()
sub STORABLE_thaw {
  my ($obj,$cloning,$str,$ar) = @_;
  %$obj = @$ar;
  return $obj;
}

########################################################################
##
## MUDL::Sentence::PackedTT
##  + dummy class for use in packed files
##
#######################################################################
package MUDL::Sentence::PackedTT;
use strict;
our @ISA = qw();



########################################################################
##
## I/O: Reader: Buffer::PackedTT
##
########################################################################

package MUDL::CorpusReader::Buffer::PackedTT;
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
##   + may call $buf->unpackSentences()
sub getSentence {
  my $cr = shift;
  my $ppos = $cr->{rpos}-$cr->{buffer}{offset};
  return undef if (!defined($cr->{buffer}) || $ppos > $#{$cr->{buffer}{sents}});
  $cr->{buffer}->unpackSentences() if ($ppos <= $cr->{buffer}{poffset});

  my $s = $cr->{buffer}{sents}[$ppos];
  $cr->{rpos}++;
  $cr->{nsents}++;
  $cr->{ntoks} += scalar(@$s);
  return $s;
}

## \%token_or_undef = $cr->getToken();
##  + not implemented

## $bufferOrClass = $cr->getBuffer()
##  + get buffer or an appropriate class
##  + used by fromString, fromFile, etc. methods
sub getBuffer { return $_[0]{buffer} || 'MUDL::Corpus::Buffer::PackedTT'; }

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
##
## I/O: Writer: Buffer::PackedTT
##
########################################################################

package MUDL::CorpusWriter::Buffer::PackedTT;
use MUDL::CorpusIO;
use Carp;
our @ISA = qw(MUDL::CorpusIO::BufWriter);

## $cr = $CLASS->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer, ##-- underlying buffer (alias='corpus')
sub new {
  my ($that,%args) = @_;
  my $buf = defined($args{buffer}) ? $args{buffer} : $args{corpus};
  $buf = MUDL::Corpus::Buffer::PackedTT->new() if (!defined($buf)); ##-- ensure buffer exists
  delete(@args{qw(buffer corpus)});
  return bless {
		buffer=>$buf,
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
  push(@{$_[0]{buffer}{sents}},$_[1]);
  return $_[0];
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

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
