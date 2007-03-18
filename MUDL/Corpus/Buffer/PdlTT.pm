##-*- Mode: CPerl -*-
##
## File: MUDL::Corpus::Buffer::PdlTT.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: in-memory corpus Pdl buffers
##  + enum-indexed fixed-width binary storage as attribute-wise pdls
##  + 
##======================================================================

package MUDL::Corpus::Buffer::PdlTT;
use MUDL::Corpus::Buffer;
use MUDL::CorpusIO;
use MUDL::Enum;
use MUDL::Object;
use PDL;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Buffer);

##-- globals: state flags
our $STATE_UNKNOWN  = 0;                      ##-- mystery state
our $STATE_UNPACKED = 1;                      ##-- all UNpacked
our $STATE_INDEXED  = 2;                      ##-- all enumerated
our $STATE_PACKED   = 4;                      ##-- all packed up (only useful with +INDEXED)

##======================================================================
## MUDL::Corpus::Buffer::PackedTT : Constructor
##======================================================================

## $cb = $class_or_object->new(%args)
##   + %args:
##      enums     => \@enums,   ##-- enumerators by attribute-position
##      sents     => \@sents,   ##-- array of expanded sentences (MUDL::Sentence::TT)
##      pdls      => \@pdls,    ##-- array of full-corpus pdls, by attribute position: [pdl(INTTYPE,$nToks),...]
##      fullpdl   => $pdl,      ##-- pdl($fulltype,$nattrs,$ntoks) : full-corpus pdl
##      begins    => $pdl,      ##-- pdl(long,$nsents): sentence start indices
##      ends      => $pdl,      ##-- pdl(long,$nsents): sentence end indices
##      offset    => $n,        ##-- (inherited, for reader): logical offset of 1st stored sentence, >= 0
##      fixedWidth=> $bool,     ##-- whether this is a fixed-width format (default=true)
##      pdltypes  => \@types,   ##-- array of pdl types to use for generating pdls
##      state     => $bufstate, ##--  ##-- bitmask of $STATE_(PACKED|INDEXED|UNPACKED|UNKNOWN)
##                               ##    "packed"   : all sentences +tt, +enumerated, +packed
##                               ##    "indexed"  : all sentences +tt, +enumerated, ?packed
##                               ##    "unpacked" : all sentences ?tt, ?enumerated, -packed
##                               ##    "unknown"  : all sentences ?tt, ?enumerated, ?packed
sub new {
  my $that = shift;
  my $pb=bless {
		enums=>[],
		sents=>[],
		pdls =>[],
		fullpdl=>undef,
		begins=>undef,
		ends  =>undef,
		offset=>0,
		fixedWidth=>1,
		pdltypes=>[],
		state =>$STATE_UNKNOWN,
		@_
	       }, ref($that)||$that;
  return $pb;
}

##======================================================================
## new API

## \@enums = $pb->updateEnums()
##  + fills $pb->{enums} based on unpacked sentences in $pb->{sents}
##  + if $pb->{fixedWidth} is set and true, only the first pending token is checked for width
##  + sets 'indexed' state bit
sub updateEnums {
  my $pb = shift;
  return $pb->{enums} if ($pb->{state} & $STATE_INDEXED);

  my $enums    = $pb->{enums};
  my $nfields  = scalar @$enums;

  ##-- get maximum number of fields
  my ($ts,$tok,$ntfields);
  my $isFixedWidth = $pb->{fixedWidth};
 FILLENUMS_SENTENCE:
  foreach (@{$pb->{sents}}) {
    ##
    ##-- check for pre-packed sentences (?)
    next if (ref($ts) eq 'MUDL::Sentence::PdlTT');
    ##
    ##-- force "alien" sentences to MUDL::Sentence::TT
    $_ = MUDL::Sentence::TT->stealSentence($_) if (ref($_) ne 'MUDL::Sentence::TT');
    ##
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
    foreach (@{$pb->{sents}}) {
      ##
      ##-- skip compatible sentences
      next if (ref($_) eq 'MUDL::Sentence::PdlTT');
      ##
      ##-- force "alien" sentences to MUDL::Sentence::TT
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

## \@pdlTypes = $pb->updatePdlTypes()
##  + updates pdl types $pb->{enums}[$_]{pdltype}, $pb->{pdltypes} based on $pb->{enums}[$_]->size
##  + enums should have already been filled (now happens auto-magically)
sub updatePdlTypes {
  my $pb = shift;
  $pb->updateEnums() if (!($pb->{state} & $STATE_INDEXED)); ##-- sanity check

  my ($enum,$ptype,$etype,$esize);
  foreach $enum (@{$pb->{enums}}) {
    ##-- update/set $enum->{pdltype}
    $esize = scalar(@{$enum->{id2sym}});
    $etype = 0;
    foreach $ptype (byte,ushort) {     ##-- unsigned types
      if ($esize < 2**(8*PDL::howbig($ptype))-1) {
	$etype = $ptype;
	last;
      }
    }
    if (!defined($etype)) {
      foreach $ptype (long,longlong) { ##-- signed types
	if ($esize < 2**(8*PDL::howbig($ptype)-1)-1) {
	  $etype = $ptype;
	  last;
	}
      }
    }
    confess(ref($pb),"::updatePdlTypes: enum of size=$esize is too big!")
      if (!defined($etype));

    ##-- set 'unknown' id for enums, based on enum-size (UNUSED by MUDL::Enum)
    $enum->{pdltype} = $etype;
    $enum->{noLabel} = -1;
  }

  @{$pb->{pdltypes}} = map {$_->{pdltype}} @{$pb->{enums}};
  return $pb->{pdltypes};
}

## $pb = $pb->packPdls()
##  + updates pdls in $pb->{pdls} from expanded ("unpacked") sentences in $pb->{sents}
##  + does nothing if 'packed' state bit is set
##  + sets 'packed' state bit
sub packPdls {
  my $pb = shift;
  return $pb if ($pb->{state} & $STATE_PACKED);

  ##-- create & populate enums
  my $enums  = $pb->updateEnums();
  my $ptypes = $pb->updatePdlTypes();

  ##-- get sentences requiring update (hack: all of 'em)
  my $sents  = $pb->{sents};
  ##
  ##-- (hack): skip "compatible" sentences
  #my @sentis_new = grep { ref($sents->[$_]) ne 'MUDL::Sentence::PdlTT' } ($pb->{poffset}..$#{$pb->{sents}});
  #my @sents_new  = @{$pb->{sents}}[@sentis_new];
  #return $pb if (!@sents_new); ##-- sanity check
  ##--
  my $sent_isnew_mask = pdl(byte,map { ref($sents->[$_]) eq 'MUDL::Sentence::PdlTT' ? 0 : 1} (0..$#{$pb->{sents}}));
  return $pb if (!any($sent_isnew_mask)); ##-- sanity check
  my @sentis_new = which($sent_isnew_mask)->list;

  ##-- cache old PDL::undefval
  my $PDL_undefval_old = $PDL::undefval;

  ##-- update: create new pdls: sentences
  my $slens_new = pdl(long, map {scalar(@$_)} @$sents[@sentis_new]); #@sents_new
  my $end_new   = $slens_new->cumusumover;
  my ($beg_old,$end_old,$which_toks_old);
  if (!defined($beg_old=$pb->{begins}) || $beg_old->isempty
      || !defined($end_old=$pb->{ends}) || $end_old->isempty)
    {
      ##-- easy way out: just assign
      $pb->{ends}   = $end_new;
      $pb->{begins} = $end_new - $slens_new;
    }
  else
    {
      ##-- tough cookies: shove compatible sentences to the front
      my $which_old = which(!$sent_isnew_mask);

      ##-- get indices of old tokens
      my @beg_old_list = $beg_old->list;
      my @end_old_list = $end_old->list;
      $which_toks_old = pdl(long,map { $beg_old_list[$_]..$end_old_list[$_] } (0..$#beg_old_list));

      ##-- update old sentence lengths
      my $slens_old = $end_old->index($which_old) - $beg_old->index($which_old);
      $end_old      = $slens_old->cumusumover;
      $beg_old      = $end_old - $slens_old;

      ##-- update new sentence lengths
      my $ntoks_old  = $which_toks_old->nelem;
      my $nsents_new = $end_new->nelem;

      $end_new += $ntoks_old;
      $end_old->inplace->reshape($end_old->nelem + $nsents_new);
      $end_old->slice("-$nsents_new:-1") .= $end_new;

      $beg_old->inplace->reshape($beg_old->nelem + $nsents_new);
      $beg_old->slice("-$nsents_new:-1") .= $end_new - $slens_new;

      ##-- update: keep old tokens: see below
      #$_ = $_->index($which_toks_old) foreach (@{$pb->{pdls}});
    }


  ##-- update: create & append new data pdls
  my $pdls  = $pb->{pdls};
  my ($attri,$pdl_new,$pdl_old);
  foreach $attri (0..$#{$pb->{enums}}) {
    $PDL::undefval = $enums->[$attri]{noLabel}; ##-- set 'undefined' value
    $pdl_new = pdl($ptypes->[$attri],
		   @{$enums->[$attri]{sym2id}}{
		     map {
		       map {$_->[$attri]} @$_
		     } @$sents[@sentis_new]
		   });

    ##-- append new pdl to old
    if (!defined($pdl_old=$pdls->[$attri]) || $pdl_old->isempty
	|| !defined($which_toks_old) || $which_toks_old->isempty)
      {
	$pdls->[$attri] = $pdl_new;
      }
    else
      {
	##
	##-- append: select only "safe" old tokens
	$pdl_old = $pdl_old->index($which_toks_old);
	$pdl_old->sever;
	##
	##-- append: perform type promotion
	$pdl_old = $pdl_old->convert($ptypes->[$attri]) if ($pdl_old->type != $pdl_new->type);
	##
	##-- append: reshape
	$pdl_old = $pdl_old->inplace->reshape($pdl_old->nelem + $pdl_new->nelem);
	##
	##-- append: assign
	$pdl_old->slice("-".($pdl_new->nelem).":-1") .= $pdl_new;
	$pdls->[$attri] = $pdl_old;
      }
  }

  ##-- update state flag
  $pb->{state} |=  $STATE_PACKED;
  #$pb->{state} &= ~$STATE_UNPACKED; ##... buffer CAN be simultaneously "packed" and "unpacked"

  ##-- cleanup
  $PDL::undefval = $PDL_undefval_old;

  return $pb;
}

## $bigtype = $pb->fullPdlType()
##  + get largest pdl type (for full pdl)
sub fullPdlType {
  my $pb   =shift;
  my $ptype=byte; ##-- default
  foreach (@{$pb->{pdltypes}}) {
    $ptype = $_ if (PDL::howbig($_) > PDL::howbig($ptype));
  }
  return $ptype;
}

## $fullpdl = $pb->fullPdl()
##  + get full pdl from single-attribute pdls
##  + just return $pb->{fullpdl} if defined & current
sub fullPdl {
  return $_[0]{fullpdl} if (defined($_[0]{fullpdl}) && ($_[0]{state} & $STATE_PACKED));
  my $pb = shift;
  $pb->packPdls();
  my $ftype = $pb->fullPdlType();
  my $pdl   = $pb->{fullpdl} = zeroes($ftype, scalar(@{$pb->{enums}}), $pb->{ends}->at(-1));
  foreach (0..$#{$pb->{enums}}) {
    $pdl->slice("($_),:") .= $pb->{pdls}[$_];
  }
  return $pdl;
}

## $pb = $pb->unpackPdls()
##  + unpacks pdl-ized sentences to $pb->{sents}
##  + this does NOT invalidate the pdls, but calling getSentence() from a writer SHOULD
##  + sets 'unpacked' state flag, does NOT clear 'packed' or 'indexed' flags
sub unpackPdls {
  my $pb = shift;
  return $pb if (($pb->{state} & $STATE_UNPACKED)
		 || !defined($pb->{begins})
		 || $pb->{begins}->isempty);
  my ($tok);
  my @begins = $pb->{begins}->list;
  my @ends   = ($pb->{ends}-1)->list;
  my @id2sym = map {$_->{id2sym}} (@{$pb->{enums}});
  my $pdls   = $pb->{pdls};
  my @pdata  = map {[$_->list]} @{$pb->{pdls}};

  my ($toki);
  push(@{$pb->{sents}},
       map {
	 bless([
		map {
		  $toki=$_;
		  bless([
			 map {
			   $id2sym[$_][ $pdata[$_][$toki] ]
			   ##--
			   ####$id2sym[$_][ $pdls->[$_]->at($toki) ]  ##-- too many at() calls: WAY SLOW!
			 } (0..$#id2sym)
			], 'MUDL::Token::TT');
		} ($begins[$_]..$ends[$_])
	       ], 'MUDL::Sentence::PdlTT')
       } (0..$#begins));

  ##-- set 'state' flag
  $pb->{state}  |= $STATE_UNPACKED;
  #$pb->{state} &= ~$STATE_PACKED;   ##-- not entirely true...

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
##  + any references still hanging around to the OLD 'enums' array-ref should still be valid!
sub clear {
  my $cb = shift;
  $cb->{enums}    = [];  ##-- don't trash old references; probably not needed
  @{$cb->{sents}} = qw();
  @{$cb->{pdls}}  = qw();
  delete($cb->{fullpdl});
  delete($cb->{begins});
  delete($cb->{ends});
  $cb->{offset} = 0;
  @{$cb->{pdltypes}} = qw();
  $cb->{state} = $STATE_UNKNOWN;
  return $cb;
}

## $cb = $cb->flush()
##   + clear buffered data, update offset
sub flush {
  my $cb = shift;
  $cb->{offset}  += @{$cb->{sents}};

  @{$cb->{pdls}} = qw();
  delete($cb->{fullpdl});
  delete($cb->{begins});
  delete($cb->{ends});
  @{$cb->{sents}} = qw();

  $cb->{state} = $STATE_UNKNOWN;

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
  return MUDL::CorpusReader::Buffer::PdlTT->new(buffer=>$cb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to $writer_class->new()
sub writer {
  my $cb = shift;
  return MUDL::CorpusWriter::Buffer::PdlTT->new(buffer=>$cb,@_);
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
our %storableSkipKeys = (sents=>undef,fullpdl=>undef,begins=>undef);
sub STORABLE_freeze {
  my ($obj,$cloning) = @_;
  $obj->packPdls();
  return ('', {
	       (map {exists($storableSkipKeys{$_}) ? qw(): ($_=>$obj->{$_})} keys(%$obj)),
	      });
}

## ($serialized_string, @other_refs) = STORABLE_thaw($obj,$cloning_flag)
##  + does NOT implicitly call unpackPdls()
sub STORABLE_thaw {
  my ($obj,$cloning,$str,$hr) = @_;
  %$obj = %$hr;
  $obj->{begins}  = $obj->{ends}->rotate(1);
  $obj->{begins}->sever;
  $obj->{begins}->set(0,0);
  $obj->{sents}   = [] if (!defined($obj->{sents}));
  return $obj;
}

########################################################################
##
## MUDL::Sentence::PdlTT
##  + dummy class for marking sentence as already-pdlized
##
#######################################################################
package MUDL::Sentence::PdlTT;
use strict;
our @ISA = qw();



########################################################################
##
## I/O: Reader: Buffer::PdlTT
##
########################################################################

package MUDL::CorpusReader::Buffer::PdlTT;
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
sub eof {
  #my $cr = shift;
  return (!defined($_[0]{buffer})
	  || (
	      ($_[0]{buffer}{state}&$MUDL::Corpus::Buffer::PdlTT::STATE_UNPACKED)
	      && $_[0]{rpos}-$_[0]{buffer}{offset} > $#{$_[0]{buffer}{sents}}
	     )
	  || (
	      (defined($_[0]{buffer}{begins})
	       && $_[0]{rpos}-$_[0]{buffer}{offset} >= $_[0]{buffer}{begins}->nelem
	      )
	     )
	 );
}

## \@sentence = $cr->getSentence();
##   + may call $buf->unpackSentences()
sub getSentence {
  my $cr = shift;
  my ($buf,$ppos);
  return undef if (!defined($buf=$cr->{buffer}) || !defined($ppos=$cr->{rpos}-$buf->{offset}));
  $buf->unpackPdls() if ( !($buf->{state}&$MUDL::Corpus::Buffer::PdlTT::STATE_UNPACKED) );
  return undef if ($ppos > $#{$buf->{sents}});

  my $s = bless $cr->{buffer}{sents}[$ppos], 'MUDL::Sentence::TT'; ##-- mark sentence as read
  $buf->{state} = 0;                                               ##-- mark buffer as dirty
  $cr->{rpos}  ++;
  $cr->{nsents}++;
  $cr->{ntoks} += scalar(@$s);
  return $s;
}

## \%token_or_undef = $cr->getToken();
##  + not implemented

## $bufferOrClass = $cr->getBuffer()
##  + get buffer or an appropriate class
##  + used by fromString, fromFile, etc. methods
sub getBuffer { return $_[0]{buffer} || 'MUDL::Corpus::Buffer::PdlTT'; }

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
## I/O: Writer: Buffer::PdlTT
##
########################################################################

package MUDL::CorpusWriter::Buffer::PdlTT;
use MUDL::CorpusIO;
use Carp;
our @ISA = qw(MUDL::CorpusIO::BufWriter);

## $cr = $CLASS->new(buffer=>$buffer,%args)
##  + structure:
##    buffer=> $corpus_buffer, ##-- underlying buffer (alias='corpus')
sub new {
  my ($that,%args) = @_;
  my $buf = defined($args{buffer}) ? $args{buffer} : $args{corpus};
  $buf = MUDL::Corpus::Buffer::PdlTT->new() if (!defined($buf)); ##-- ensure buffer exists
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
  push(@{$_[0]{buffer}{sents}},
       ref($_[1]) eq 'MUDL::Sentence::TT'
       ? $_[1]                                    ##-- just adopt TT sentences
       : MUDL::Sentence::TT->stealSentence($_[1]) ##-- force "alien" sentences to MUDL::Sentence::TT
      );
  $_[0]{buffer}{state} = 0;                       ##-- mark buffer as dirty
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

Copyright (c) 2007, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
