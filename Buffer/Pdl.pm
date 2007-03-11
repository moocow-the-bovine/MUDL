##-*- Mode: CPerl -*-
##
## File: MUDL::Corpus::Buffer::Pdl.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: in-memory corpus buffer: Pdls
##======================================================================

package MUDL::Corpus::Buffer::Pdl;
use MUDL::Corpus::Buffer;
use MUDL::Enum;
use PDL;
use Carp;

our @ISA = qw(MUDL::Corpus::Buffer);

##======================================================================
## MUDL::Corpus::Buffer: Constructor
##======================================================================

## $cb = MUDL::Corpus::Buffer->new(%args)
##   + %args:
##      ##
##      ##-- basic
##      sents => \@array_of_pdls,      ## buffer content
##      txtenum => $text_enum,         ## token-text enum
##      offset  => $zero_offset,       ## >= 0, logical offset for {sents}[0]
##      ##
##      ##-- unknown-bashing
##      dobash     => $bool,           ## default=1
##      bashto     => $symbol,         ## default='@UNKNOWN'
##      bashed     => \@bashed_tokens, ## MUDL::Token descendants, not copied
##      bashoffset => $zero_offset,    ## >= 0, logical offset for {bashed}[0]
##      ##
##      ##-- bos/eos-handling
##      dobos      => $bool,           ## implicitly add/strip {bos}? default=0
##      doeos      => $bool,           ## implicitly add/strip {eos}? default=0
##      bos        => $bos_symbol,     ## default='__$'
##      eos        => $eos_symbol,     ## default='__$'
##
##  + sentence-pdls are stored as TEXT-ONLY
sub new {
  my $that = shift;
  return bless {
		##-- basic
		sents=>[],
		offset =>0,
		txtenum=>MUDL::Enum->new(),
		#tagenum=>MUDL::Enum->new(),

		##-- bashing
		dobash=>1,
		bashto=>'@UNKNOWN',
		bashed=>[],
		bashoffset=>0,

		##-- bos/eos
		dobos=>0,
		doeos=>0,
		bos=>'__$',
		eos=>'__$',
		@_
	       }, ref($that)||$that;
}

##======================================================================
## accessors

## $cb = $cb->clear()
sub clear {
  my $cb = shift;
  @{$cb->{sents}} = qw();
  @{$cb->{bashed}} = qw();
  $cb->{bashoffset} = 0;
  $cb->{offset} = 0;
  return $cb;
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
sub flush {
  my $cb = shift;

  $cb->{offset}     += @{$cb->{sents}};
  @{$cb->{sents}}    = qw();

  $cb->{bashoffset} += @{$cb->{bashed}};
  @{$cb->{bashed}}   = qw();

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
  return MUDL::CorpusIO::PdlBufReader->new(buffer=>$cb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to MUDL::CorpusIO::BufWriter->new()
sub writer {
  my $cb = shift;
  return MUDL::CorpusIO::PdlBufWriter->new(buffer=>$cb,@_);
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
## I/O: PdlBufReader
##
##  + read from a MUDL::Corpus::Buffer::Pdl
##
########################################################################


package MUDL::CorpusIO::PdlBufReader;
use PDL;
our @ISA = qw(MUDL::CorpusIO::BufReader);

## $cr = MUDL::CorpusIO::PdlBufReader->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer,    ##-- underlying buffer (alias='corpus')
##    rpos  => $buffer_index,     ##-- *logical* read position in underlying buffer
##    tclass => $TOKEN_CLASSNAME, ##-- Default: MUDL::Token
##
sub new {
  my ($that,%args) = @_;
  $args{buffer} = $args{corpus} if (!defined($args{buffer}));
  delete($args{corpus});
  return bless {
		buffer=>undef,
		rpos=>0,
		nsents=>0,
		#tclass=>'MUDL::Token::Raw',
		tclass=>'MUDL::Token',
		bashpos=>0,
		%args
	       }, ref($that)||$that;
}


## $buf = $cr->buffer()
## $buf = $cr->buffer($buf)
##
## (inherited)

## $bool = $cr->eof
## (inherited)

## \@sentence = $cr->getSentence();
##   + ye olde guttes
sub getSentence {
  my $cr = shift;
  my ($buf);

  return undef if (!defined($buf=$cr->{buffer})
		   || $cr->{rpos}-$buf->{offset} > $#{$buf->{sents}});

  my $sp = $buf->{sents}[$cr->{rpos}++ - $buf->{offset}];
  $cr->{nsents}++;
  $cr->{ntoks} += $sp->dim(0);

  my ($txt,$tag);
  my $s = bless([
		 map {
		   (defined($txt = $buf->{txtenum}->symbol($_))
		    && (!$buf->{dobash} || $txt ne $buf->{bashto})
		    ? $cr->{tclass}->new(text=>$txt)
		    : $buf->{bashed}[$cr->{bashpos}++ - $buf->{bashoffset}])
		 } $sp->list
		], 'MUDL::Sentence');

  shift(@$s) if ($buf->{dobos});
  pop  (@$s) if ($buf->{doeos});

  return $s;
}

## \%token_or_undef = $cr->getToken();
##  + not implemented

## undef = $cr->fromString($string)
##  + not implemented

## undef = $cr->fromFile($filename_or_fh);
##  + not implemented

## $n = $cr->nSentences()
##  + returns number of sentences already read
sub nSentences { return $_[0]{nsents}; }

## undef = $cr->reset();
sub reset {
  $_[0]{rpos} = 0;
  $_[0]{bashpos} = 0;
}

## $n = $cr->nTokens()
sub nTokens { return $_[0]{ntoks}; }



########################################################################
## I/O: BufWriter
##
##  + write to a MUDL::Corpus::Buffer
########################################################################

package MUDL::CorpusIO::PdlBufWriter;
use PDL;
use Carp;
our @ISA = qw(MUDL::CorpusIO::BufWriter);

## $cr = MUDL::CorpusIO::PdlBufWriter->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer, ##-- underlying buffer (alias='corpus')
##    type=>   $pdl_datatype,  ##-- default=long
sub new {
  my ($that,%args) = @_;
  my $buf = defined($args{buffer}) ? $args{buffer} : $args{corpus};
  $buf = MUDL::Corpus::Buffer::Pdl->new() if (!defined($buf));
  delete(@args{qw(buffer corpus)});
  return bless {
		buffer=>$buf,
		type=>long,
		%args
	       }, ref($that)||$that;
}

## $bool = $cw->flush
sub flush { return 1; }

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$s) = @_;

  my ($id);
  my $buf = $cw->{buffer};
  push(@{$buf->{sents}},
       pdl($cw->{type},
	   [
	    map {
	      if ($buf->{dobash}) {
		$id  = $buf->{txtenum}->index(ref($_) ? $_->text : $_);
		if (!defined($id)) {
		  push(@{$buf->{bashed}}, (ref($_) ? $_ : MUDL::Token->new(text=>$_)));
		  $id = $buf->{txtenum}->index($buf->{bashto});
		}
	      } else {
		$id = $buf->{txtenum}->addSymbol(ref($_) ? $_->text : $_);
	      }
	      $id
	    } (
	       ($buf->{dobos} ? $buf->{bos} : qw()),
	       @$s,
	       ($buf->{doeos} ? $buf->{eos} : qw()),
	      )
	   ]));

  return $cw;
}


## undef = $cr->putToken($text_or_hashref);
##  + not implemented

## undef = $cr->toString(\$string)
##  + not implemented

## undef = $cr->toFile($filename_or_fh);
## undef = $cr->Fh($fh);
##  + not implemented


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
