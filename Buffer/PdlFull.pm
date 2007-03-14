##-*- Mode: CPerl -*-
##
## File: MUDL::Corpus::Buffer::PdlFull.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: in-memory corpus buffer: Pdls with attributes
##
## --> UNUSED: basic testing performed, seems to work, but:
##     NOT SIGNIFICANTLY FASTER than plain .tt FILES (for unigram counting, i.e. text() access)
##
## --> this class WOULD make sense if all data was handled INTERNALLY (i.e. modulo I/O)
##     by "global" Enums & id-translation pdls, but that's a major undertaking
##     at this point...
##
##======================================================================

package MUDL::Corpus::Buffer::PdlFull;
use MUDL::Corpus::Buffer;
use MUDL::Enum;
use PDL;
use Carp;

use strict;

our @ISA = qw(MUDL::Corpus::Buffer);

##======================================================================
## MUDL::Corpus::Buffer: Constructor
##======================================================================

## $cb = MUDL::Corpus::Buffer->new(%args)
##   + %args:
##      ##
##      ##-- basic data
##      pdls     => \%attr2pdl         ## (text=>pdl(long,$nTokens_or_more), ..., $attrN=>pdl(long,$circa_nTokens))
##      soffsets => $lenpdl,           ## pdl(long,$nSents+1) : sentence-start offset
##      ##
##      ##-- dynamic re-allocation
##      tget     => $n,                ## number of token-slots to grow by on overflow (default=8192)
##      sget     => $n,                ## number of sentence-slots to grow by on overflow (default=512)
##      ##
##      ##-- general buffer stuff
##      offset   => $zero_offset,      ## >= 0, logical offset for {sents}[0]
##      nsents   => $nSents,           ## number of actually stored sentences (should by $cb->{soffsets}->ngood)
##      ntoks    => $nTokens,          ## number of actually stored tokens
##      ##
##      ##-- enums
##      enums     => \%attr2enum,      ## (text=>$textEnum, tag=>$tagEnum, ..., $attrN=>$attrNEnum)
##      wantattrs => $bool_or_hashref, ## ###hashref or boolean (default: boolean=1)
##                                     ##  + boolean: keep all attrs yes/no
##                                     ## ### + hashref: attr-name pseudo-set to keep
sub new {
  my $that = shift;
  return bless {
		##-- basic data
		pdls => {},
		soffsets => zeroes(long,1),

		##-- allocation stuff
		tget     => 8192,
		sget     => 512,

		##-- buffer stuff
		offset   => 0,
		nsents   => 0,
		ntoks    => 0,

		##-- enums
		enums     => {},
		wantattrs => 1,

		  @_
	       }, ref($that)||$that;
}

##======================================================================
## new methods

## $cb = $cb->trimPdls()
## $cb = $cb->trimPdls($nsents,$ntoks)
##  + trims buffer pdls to $nsents sentences, $ntoks tokens, defaults are @$cb{qw(nsents ntoks)}
sub trimPdls {
  my ($cb,$nsents,$ntoks) = @_;
  $nsents = $cb->{nsents} if (!defined($nsents));
  $ntoks  = $cb->{ntoks} if (!defined($ntoks));
  $cb->{soffsets}->inplace->reshape($nsents+1);
  foreach (values(%{$cb->{pdls}})) {
    $_->inplace->reshape($ntoks);
  }
  return $cb;
}


##======================================================================
## accessors

## $cb = $cb->clear()
sub clear {
  my $cb = shift;
  $cb->{soffsets}     = zeroes(long,1),
  %{$cb->{pdls}}  = qw();
  %{$cb->{enums}} = qw();
  $cb->{offset} = 0;
  $cb->{nsents} = 0;
  $cb->{ntoks} = 0;
  return $cb;
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
sub flush {
  my $cb = shift;
  %{$cb->{pdls}} = qw();
  $cb->{soffsets} = zeroes(long,1);
  $cb->{offset} += $cb->{nsents};
  $cb->{nsents}  = 0;
  $cb->{ntoks}   = 0;
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
  return MUDL::CorpusIO::PdlFullBufReader->new(buffer=>$cb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to MUDL::CorpusIO::BufWriter->new()
sub writer {
  my $cb = shift;
  return MUDL::CorpusIO::PdlFullBufWriter->new(buffer=>$cb,@_);
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
## I/O: PdlFullBufReader
##
##  + read from a MUDL::Corpus::Buffer::Pdl
##
########################################################################


package MUDL::CorpusIO::PdlFullBufReader;
use MUDL::Corpus::EBuffer;
use MUDL::EToken;
use PDL;
use strict;
our @ISA = qw(MUDL::CorpusIO::EBufReader);

## $cr = MUDL::CorpusIO::PdlBufReader->new(buffer=>$buffer,%args)
##  + structure:
##    buffer => $corpus_buffer,    ##-- underlying buffer (alias='corpus')
##    rpos   => $buffer_index,     ##-- *logical* read position in underlying buffer
##    #tclass => $TOKEN_CLASSNAME,  ##-- Default: MUDL::EToken: NO
##
sub new {
  my ($that,%args) = @_;
  $args{buffer} = $args{corpus} if (!defined($args{buffer}));
  delete($args{corpus});
  return bless {
		buffer=>undef,
		rpos  =>0,
		nsents=>0,
		#tclass=>'MUDL::EToken',
		%args
	       }, ref($that)||$that;
}


## $buf_or_undef = $cr->buffer()
## $buf          = $cr->buffer($buf)
##   + get/set buffer (may return undef)
#(inherited)

## $bool = $cr->eof
sub eof {
  my $cr = shift;
  return (!defined($cr->{buffer})
	  || $cr->{rpos}-$cr->{buffer}{offset} >= $cr->{buffer}{nsents});
}

## \@sentence = $cr->getSentence();
##   + ye olde guttes
sub getSentence {
  my $cr = shift;
  my ($buf);

  return undef if (!defined($buf=$cr->{buffer})
		   || $cr->{rpos}-$buf->{offset} >= $buf->{nsents});

  my $si0 = $buf->{soffsets}->at(  $cr->{rpos} - $buf->{offset});
  my $si1 = $buf->{soffsets}->at(++$cr->{rpos} - $buf->{offset})-1;
  $cr->{nsents}++;
  $cr->{ntoks} += 1+$si1-$si0;

  my $s = bless([
		 map {
		   bless { _enums=>$buf->{enums}, text=>$_ }, 'MUDL::EToken'
		 } (defined($buf->{pdls}{text}) ? $buf->{pdls}{text}->slice("$si0:$si1")->list : qw())
		], 'MUDL::Sentence');

  ##-- maybe add some attributes
  if ($buf->{wantattrs}) {
    my ($i,$attr,$apdl,$asize);
    while (($attr,$apdl)=each(%{$buf->{pdls}})) {
      next if ($attr eq 'text');
      $asize = $apdl->nelem;
      foreach $i (0..$#$s) {
	next if ($si0+$i >= $asize);
	$s->[$i]{$attr} = $apdl->at($si0+$i);
      }
    }
  }

  return $s;
}

## \%token_or_undef = $cr->getToken();
##  + not implemented

## $bufferOrClass = $cr->getBuffer()
##  + get buffer or an appropriate class
##  + used by fromString, fromFile, etc. methods
sub getBuffer { return $_[0]{buffer} || 'MUDL::Corpus::Buffer::PdlFull'; }

## undef = $cr->fromString($string)
##  + inherited from MUDL::Corpus::Buffer

## undef = $cr->fromFile($filename_or_fh);
##  + inherited from MUDL::Corpus::Buffer

## $n = $cr->nSentences()
##  + returns number of sentences already read
sub nSentences { return $_[0]{nsents}; }

## undef = $cr->reset();
# + inherited

## $n = $cr->nTokens()
# + inherited


########################################################################
## I/O: PdlFullBufWriter
##
##  + write to a MUDL::Corpus::Buffer::PdlFull
########################################################################

package MUDL::CorpusIO::PdlFullBufWriter;
use PDL;
use Carp;
our @ISA = qw(MUDL::CorpusIO::EBufWriter);

## $cr = MUDL::CorpusIO::PdlFullBufWriter->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer, ##-- underlying buffer (alias='corpus')
##    type=>   $pdl_datatype,  ##-- default=long
sub new {
  my ($that,%args) = @_;
  my $buf = defined($args{buffer}) ? $args{buffer} : $args{corpus};
  $buf = MUDL::Corpus::Buffer::PdlFull->new() if (!defined($buf));
  delete(@args{qw(buffer corpus)});
  return bless {
		buffer=>$buf,
		##
		##-- CorpusWriter wrapping
		#saveSub=>\&saveSub,   ##-- method to use for buffer saving
		#                      ##   + this gets set by toString(), toFile(), etc.
		#saveArgs=>\@args,     ##-- user args to \&saveSub, e.g. 'mode','iolayers'; default: none
		#
		%args
	       }, ref($that)||$that;
}

## $bool = $cw->flush
sub flush {
  ##-- trim buffer pdls
  my $cw = shift;
  $cw->{buffer}->trimPdls();
  $cw->SUPER::flush();
  return $cw;
}

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$s) = @_;
  my $buf = $cw->{buffer};

  ##-- maybe re-allocate: sentences
  my $oldsize = $buf->{soffsets}->nelem;
  if ($oldsize <= $buf->{nsents}+1) {
    $buf->{soffsets}->inplace->reshape($oldsize + $buf->{sget});
    $buf->{soffsets}->slice("${oldsize}:-1") .= -1;
  }
  ##-- maybe re-allocate: text
  my $txtpdl = $buf->{pdls}{text};
  $txtpdl = $buf->{pdls}{text} = zeroes(long,$buf->{tget})-1 if (!defined($txtpdl));
  $oldsize=$txtpdl->nelem;
  while ($oldsize <= $buf->{ntoks}+scalar(@$s)) {
    $txtpdl->inplace->reshape($oldsize + $buf->{tget});
    $txtpdl->slice("${oldsize}:-1") .= -1;
    $oldsize = $txtpdl->nelem;
  }

  ##-- store NEXT sentence offset
  my $s0 = $buf->{soffsets}->at($buf->{nsents});
  my $s1 = $s0 + scalar(@$s) - 1;
  $buf->{nsents}++;
  $buf->{soffsets}->slice($buf->{nsents}) .= $s1+1;
  $buf->{ntoks} = $s1+1;

  ##-- store tokens
  my ($i,$tok);
  foreach $i (0..$#$s) {
    $tok = $s->[$i];

    ##-- general case: anything else: make it an EToken (handle Enum extension, etc.)
    if (!ref($tok) || ref($tok) ne 'MUDL::EToken' || $tok->{_enums} ne $buf->{enums}) {
      $tok = MUDL::EToken->newFromToken($tok,_enums=>$buf->{enums});
    }

    ##-- special case: all MUDL::ETokens which share our enums
    $txtpdl->slice($s0+$i) .= $tok->{text};

    ##-- attrs: assume enums are safe
    next if (!$buf->{wantattrs});
    foreach (grep {$_ ne '_enums' && $_ ne 'text'} keys(%$tok)) {
      $buf->{pdls}{$_} = zeroes(long,$txtpdl->nelem)-1 if (!defined($buf->{pdls}{$_}));
      $oldsize = $buf->{pdls}{$_}->nelem;
      if ($oldsize != $txtpdl->nelem) {
	$buf->{pdls}{$_}->inplace->reshape($txtpdl->nelem);
	$buf->{pdls}{$_}->slice("${oldsize}:-1") .= -1;
      }
      $buf->{pdls}{$_}->slice($s0+$i) .= $tok->{$_};
    }
  }

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
