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

use strict;
our @ISA = qw(MUDL::Corpus::Buffer);

##======================================================================
## MUDL::Corpus::Buffer::PackedTT : Constructor
##======================================================================

##-- globals: state flags
our $STATE_UNKNOWN  = 0;                      ##-- mystery state
our $STATE_UNPACKED = 1;                      ##-- all UNpacked
our $STATE_INDEXED  = 2;                      ##-- all enumerated
our $STATE_PACKED   = 4;                      ##-- all packed up (only useful with +INDEXED)

## $cb = $class_or_object->new(%args)
##   + %args:
##      enums     => \@enums,   ##-- enumerators by attribute-position
##      sents     => \@sents,   ##-- array of sentences
##      offset    => $n,        ##-- logical offset of 1st stored sentence, >= 0
##      fixedWidth=> $bool,     ##-- whether this is a fixed-width format (default=true)
##      packas    => $template, ##-- pack template, default='ww' [see $pb->updatePackTemplate()]
##      #edirty    => \@sentis,  ##-- indices of "enum-dirty" sentences (getSentence() returns) in the buffer
##      #pdirty    => \@sentis,  ##-- indices of "pack-dirty" sentences (TT-expanded) in the buffer
##      ##--
##      state      => $bufstate, ##-- bitmask of $STATE_(PACKED|INDEXED|UNPACKED|UNKNOWN)
##                               ##    "packed"   : all sentences +tt, +enumerated, +packed
##                               ##    "indexed"  : all sentences +tt, +enumerated, ?packed
##                               ##    "unpacked" : all sentences ?tt, ?enumerated, -packed
##                               ##    "unknown"  : all sentences ?tt, ?enumerated, ?packed
sub new {
  my $that = shift;
  my $pb=bless {
		enums=>[],
		sents=>[],
		offset=>0,
		fixedWidth=>1,
		packas=>'',
		##
		#poffset=>0,
		#eoffset=>0,
		##--
		#pdirty=>[],
		#edirty=>[],
		##--
		state=>$STATE_UNKNOWN, ##-- mystery state
		@_
	       }, ref($that)||$that;
  return $pb;
}

##======================================================================
## new API

## \@enums = $pb->updateEnums()
##  + fills $pb->{enums} based on pending data in $pb->{sents}
##  + if $pb->{fixedWidth} is set and true, only the first pending token is checked for its width
##  + sets 'INDEXED' state flag
sub updateEnums {
  my $pb = shift;
  return $pb->{enums} if ($pb->{state} & $STATE_INDEXED);

  my $enums    = $pb->{enums};
  my $nfields  = scalar @$enums;
  ##-- get maximum number of fields
  my $isFixedWidth = $pb->{fixedWidth};
  my ($ts,$tok,$ntfields);
 FILLENUMS_SENTENCE:
  foreach (@{$pb->{sents}}) {
    ##
    ##-- check for pre-packed sentences (?)
    next if (ref($ts) eq 'MUDL::Sentence::PackedTT');
    ##
    ##-- force "alien" sentences to MUDL::Sentence::TT (this should really happen in putSentence())
    $_ = MUDL::Sentence::TT->stealSentence($_) if (ref($_) ne 'MUDL::Sentence::TT');
    ##
    ##-- count attributes, assuming we have MUDL::Token::TT objects
    foreach (@$_) {
      $nfields  = @$_ if (@$_ > $nfields);
      last FILLENUMS_SENTENCE if ($pb->{fixedWidth});
    }
  }

  ##-- create & populate enums
  my ($attri,$enum,$sym2id,$id2sym);
  foreach $attri (0..($nfields-1)) {
    $enum   = $enums->[$attri] = MUDL::Enum->new if (!defined($enum=$enums->[$attri]));
    $sym2id = $enum->{sym2id};
    $id2sym = $enum->{id2sym};
    foreach (@{$pb->{sents}}) {
      ##
      ##-- skip compatible sentences
      next if (ref($_) eq 'MUDL::Sentence::PackedTT');
      ##
      ##-- force "alien" sentences to MUDL::Sentence::TT (only check this for variable-width formats)
      ##    + we should probably really do this in putSentence()
      $_ = MUDL::Sentence::TT->stealSentence($_) if (ref($_) ne 'MUDL::Sentence::TT');
      ##
      ##-- actually populate enums
      foreach (@$_) {
	if (!exists($sym2id->{$_->[$attri]})) {
	  $sym2id->{$_->[$attri]} = scalar(@$id2sym);
	  push(@$id2sym,$_->[$attri]);
	}
      }
    }
  }

  ##-- update state flag
  $pb->{state} |= $STATE_INDEXED;

  return $enums;
}

## $packas = $pb->updatePackTemplate()
##  + updates pack template $pb->{enums}[$_]{packas}, $pb->{packas} based on $pb->{enums}[$_]->size
##  + enums should have already been filled (called auto-magically)
sub updatePackTemplate {
  my $pb = shift;
  $pb->updateEnums() if (!($pb->{state} & $STATE_INDEXED)); ##-- sanity check

  my ($enum,$esize);
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
##  + does nothing if state is already 'packed'
##  + sets 'packed' state bit
sub packSentences {
  my $pb = shift;
  return $pb if ($pb->{state} & $STATE_PACKED);

  ##-- create & populate enums, pack template
  my $enums  = $pb->updateEnums();
  my $packas = $pb->updatePackTemplate();

  ##-- pack all "pack-dirty" sentences in the buffer
  my ($tok,$val);
  foreach (@{$pb->{sents}}) {
    ##
    ##-- skip compatible sentences
    next if (ref($_) eq 'MUDL::Sentence::PackedTT');
    ##
    ##-- pack it up...
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

  ##-- update state flag
  $pb->{state} |=  $STATE_PACKED;
  $pb->{state} &= ~$STATE_UNPACKED;

  return $pb;
}

## $pb = $pb->unpackSentences()
##  + unpacks all packed sentences in $pb->{sents}
##  + does nothing if state is already $STATE_UNPACKED
sub unpackSentences {
  my $pb = shift;
  return $pb if ($pb->{state} & $STATE_UNPACKED);

  my ($tok);
  my $packas = $pb->{packas};
  my $enums  = $pb->{enums};
  foreach (@{$pb->{sents}}) {
    next if (ref($_) eq 'MUDL::Sentence::TT'); ##-- already unpacked
    foreach (@$_) {
      $_  = $tok = bless [unpack($packas,$_)], 'MUDL::Token::TT';
      @$_ = map { $enums->[$_]{id2sym}[$tok->[$_]] } (0..$#$enums);
    }
    bless($_, 'MUDL::Sentence::TT');
  }

  ##-- set state to 'unpacked'
  $pb->{state} = $STATE_UNPACKED;

  return $pb;
}

##======================================================================
## state-field accessors

## \%stateHash = $pb->state()
## \%stateHash = $pb->state(\%stateHash)
##   + get/set boolean hash of state bits: only use this if you know what you're doing
our %STATE_NAMES = (packed=>$STATE_PACKED,indexed=>$STATE_INDEXED,unpacked=>$STATE_UNPACKED);
sub state {
  my $pb    = shift;
  my $state = $pb->{state}; 
 if (@_) {
    my $sh    = $_[1];
     foreach (keys(%STATE_NAMES)) {
       next if (!defined($sh->{$_})); ##-- not defined: skip
       if ($sh->{$_})  { $state |=  $STATE_NAMES{$_}; }
       else            { $state &= ~$STATE_NAMES{$_}; }
     }
    $pb->{state} = $state;
  }
  return { map { $_=>($state&$STATE_NAMES{$_}) } keys(%STATE_NAMES) };
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
  $cb->{offset}   = 0;
  $cb->{packas}   = '';
  $cb->{state}    = $STATE_UNKNOWN;
  return $cb;
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
sub flush {
  my $cb     = shift;
  my $nsents = @{$cb->{sents}};
  $cb->{offset}    += $nsents;
  @{$cb->{sents}}   = qw();
  $cb->{state}      = $STATE_UNKNOWN;
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
  return ('',{%$obj});
}

## ($serialized_string, @other_refs) = STORABLE_thaw($obj,$cloning_flag)
##  + does NOT implicitly call unpackSentences()
sub STORABLE_thaw {
  my ($obj,$cloning,$str,$hr) = @_;
  %$obj = %$hr;
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
##   + may call $buf->unpackSentences() if there are any 
sub getSentence {
  my $cr   = shift;
  my $ppos = $cr->{rpos}-$cr->{buffer}{offset};
  return undef if (!defined($cr->{buffer}) || $ppos > $#{$cr->{buffer}{sents}});
  $cr->{buffer}->unpackSentences() if (! ($cr->{buffer}{state} & $MUDL::Corpus::Buffer::PackedTT::STATE_UNPACKED));

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
  ##
  ##-- force "alien" sentences to MUDL::Sentence::TT ... elsewhere
  #$_[1] = MUDL::Sentence::TT->stealSentence($_[1]) if (ref($_[1]) ne 'MUDL::Sentence::TT');

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
