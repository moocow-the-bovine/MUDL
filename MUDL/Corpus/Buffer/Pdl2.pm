##-*- Mode: CPerl -*-
##
## File: MUDL::Corpus::Buffer::Pdl2.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: in-memory corpus buffer: Pdls with attributes
##
## --> UNUSED: basic testing performed, seems to work, but:
##     NOT SIGNIFICANTLY FASTER than plain .tt FILES (for unigram counting)
##
##    ##-- testing getSentence():
##    cmpthese(4,{buf=>\&test_buf_io, file=>\&test_file_io, binfile=>\&test_binfile_io, pb2=>\&test_pb2_io})
##              Rate    file     pb2     pb1 binfile     buf
##    file    1.22/s      --    -32%    -57%    -57%    -91%  # MUDL::CorpusIO::TT
##    pb2     1.79/s     46%      --    -37%    -38%    -88%  # MUDL::Corpus::Buffer::Pdl2 (this package)
##    pb1     2.84/s    133%     59%      --     -1%    -80%  # MUDL::Corpus::Bffer::Pdl   (text only)
##    binfile 2.86/s    134%     60%      1%      --    -80%  # MUDL::CorpusIO::TT::Bin
##    buf     14.3/s   1071%    700%    404%    400%      --  # MUDL::Corpus::Buffer / MUDL::Token::TT
##
## --> this class WOULD make sense if all data was handled INTERNALLY (i.e. modulo I/O)
##     by "global" Enums & id-translation pdls, but that's a major undertaking
##     at this point...
##
##======================================================================

package MUDL::Corpus::Buffer::Pdl2;
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
##      #sents => \@array_of_pdls,     ## buffer content
##      txtpdl   => $txtpdl,           ## pdl(long,$nTokens)             : token-text Ids
##      attr2pdl => \%attr2pdl,        ## {$attrName=>pdl(long,$nTokens) : token-attribute-value Ids
##      soffsets => $lenpdl,           ## pdl(long,$nSents+1)            : sentence-start offset
##      tget     => $n,                ## number of token-slots to grow by on overflow (default=8192)
##      sget     => $n,                ## number of sentence-slots to grow by on overflow (default=512)
##      offset   => $zero_offset,      ## >= 0, logical offset for {sents}[0]
##      nsents   => $n,                ## number of actually stored sentences (should by $cb->{soffsets}->ngood)
##      ntoks    => $n,                ## number of actually stored tokens
##      ##
##      ##-- enums
##      txtenum   => $text_enum,       ## token-text enum
##      attr2enum => \%attr2enum,      ## token-attr enums, indexed by attribute name
##      wantattrs => $bool_or_hashref, ## boolean: keep all attrs yes/no, hashref: attr-name pseudo-set to keep: default=1
sub new {
  my $that = shift;
  return bless {
		  ##-- basic data
		  txtpdl   => null->convert(long),
		  attr2pdl => {},
		  soffsets => zeroes(long,1),
		  tget     => 8192,
		  sget     => 512,
		  offset   => 0,
		  nsents   => 0,
		  ntoks    => 0,

		  ##-- enums
		  txtenum  => MUDL::Enum->new(),
		  attr2enum=> {},
		  wantattrs=> 1,

		  @_
	       }, ref($that)||$that;
}

##======================================================================
## accessors

## $cb = $cb->clear()
sub clear {
  my $cb = shift;
  $cb->{txtpdl}       = null->convert(long),
  $cb->{soffsets}     = zeroes(long,1),
  %{$cb->{attr2pdl}}  = qw();
  %{$cb->{attr2enum}} = qw();
  $cb->{offset} = 0;
  $cb->{nsents} = 0;
  $cb->{ntoks} = 0;
  return $cb;
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
sub flush {
  my $cb = shift;

  $cb->{offset} += $cb->{nsents};
  $cb->{sents}   = undef;
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
  return MUDL::CorpusIO::Pdl2BufReader->new(buffer=>$cb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to MUDL::CorpusIO::BufWriter->new()
sub writer {
  my $cb = shift;
  return MUDL::CorpusIO::Pdl2BufWriter->new(buffer=>$cb,@_);
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


package MUDL::CorpusIO::Pdl2BufReader;
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
		rpos  =>0,
		nsents=>0,
		tclass=>'MUDL::Token',
		%args
	       }, ref($that)||$that;
}


## $buf = $cr->buffer()
## $buf = $cr->buffer($buf)
##
## (inherited)

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
		   $cr->{tclass}->new(text=>$buf->{txtenum}{id2sym}[$_])
		 } $buf->{txtpdl}->slice("$si0:$si1")->list
		], 'MUDL::Sentence');

  ##-- maybe add some attributes
  if ($buf->{wantattrs}) {
    my ($i,$attr,$apdl,@avals);
    while (($attr,$apdl)=each(%{$buf->{attr2pdl}})) {
      @avals = @{$buf->{attr2enum}{$attr}{id2sym}}[ $apdl->slice("$si0:$si1")->list ];
      foreach $i (0..$#$s) {
	$s->[$i]->attribute($attr,$avals[$i]);
      }
    }
  }

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
}

## $n = $cr->nTokens()
sub nTokens { return $_[0]{ntoks}; }


########################################################################
## I/O: BufWriter
##
##  + write to a MUDL::Corpus::Buffer
########################################################################

package MUDL::CorpusIO::Pdl2BufWriter;
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
  $buf = MUDL::Corpus::Buffer::Pdl2->new() if (!defined($buf));
  delete(@args{qw(buffer corpus)});
  return bless {
		buffer=>$buf,
		%args
	       }, ref($that)||$that;
}

## $bool = $cw->flush
sub flush {
  ##-- trim buffer pdls
  my $cw = shift;
  my $buf = $cw->{buffer};
  $buf->{soffsets}->reshape($buf->{nsents}+1);
  foreach (values(%{$buf->{attr2pdl}})) {
    $_->reshape($buf->{soffsets}->at(-1));
  }
  return $cw;
}

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$s) = @_;
  my $buf = $cw->{buffer};

  ##-- maybe re-allocate
  if ($buf->{nsents}+1 >= $buf->{soffsets}->nelem) {
    $buf->{soffsets}->reshape($buf->{soffsets}->nelem + $buf->{sget});
  }
  while ($buf->{ntoks}+scalar(@$s) >= $buf->{txtpdl}->nelem) {
    $buf->{txtpdl}->reshape($buf->{txtpdl}->nelem + $buf->{tget});
    foreach (values(%{$buf->{attr2pdl}})) {
      $_->reshape($buf->{txtpdl}->nelem + $buf->{tget});
    }
  }

  ##-- store NEXT sentence offset
  my $s0 = $buf->{soffsets}->at($buf->{nsents});
  my $s1 = $s0 + scalar(@$s) - 1;
  $buf->{nsents}++;
  $buf->{soffsets}->slice($buf->{nsents}) .= $s1+1;
  $buf->{ntoks} = $s1+1;

  ##-- store tokens
  $buf->{txtpdl}->slice("$s0:$s1") .= pdl(long,[map { $buf->{txtenum}->addSymbol(ref($_) ? $_->text : $_) } @$s]);

  ##-- store attributes
  if ($buf->{wantattrs}) {
    ##-- attrs: get wanted attributes & allocate pdls, enums
    my ($wantattrs);
    if (ref($buf->{wantattrs})) {
      ##-- attrs: get & alloc: buffer-local specific attributes at user request
      $wantattrs = $buf->{wantattrs};
    } else {
      ##-- attrs: get & alloc: find all attributes in sentence
      $wantattrs = { tag=>undef };
      @$wantattrs{ keys(%{$buf->{attr2pdl}}) }                       = undef;
      @$wantattrs{ map {ref($_) ? $_->attributeNames() : qw()} @$s } = undef;
    }
    ##-- attrs: get & alloc: allocate
    my ($attr,$apdl,$aenum);
    foreach $attr (keys(%$wantattrs)) {
      $aenum = $buf->{attr2enum}{$attr} = MUDL::Enum->new()
	if (!defined($aenum=$buf->{attr2enum}{$attr}));
      $apdl  = $buf->{attr2pdl}{$attr}  = zeroes(long,$buf->{txtpdl}->nelem)->setvaltobad(0)
	if (!defined($apdl=$buf->{attr2pdl}{$attr}));
      $apdl->reshape($buf->{txtpdl}->nelem) if ($apdl->nelem < $buf->{txtpdl}->nelem);

      ##-- attrs: store
      $apdl->slice("$s0:$s1") .= pdl(long,[map {$aenum->addSymbol(ref($_) ? $_->attribute($attr) : 'BAD')} @$s]);
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
