##-*- Mode: CPerl -*-
##
## File: MUDL::Corpus::Buffer::PdlFull.pm
## Author: Bryan Jurish <moocow@cpan.org>
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
##      #pdls    => \%attr2pdl         ## (text=>pdl(long,$nTokens_or_more), ..., $attrN=>pdl(long,$circa_nTokens))
##      pdl      => $datapdl,          ## pdl(long,$nAttrs,$nToks) : attribute-ids
##      soffsets => \@soffsets,        ## ARRAY(long,$nSents+1)    : sentence-start offset
##      ##
##      ##-- dynamic re-allocation
##      #tget     => $n,                ## number of token-slots to grow by on overflow (default=16384)
##      #sget     => $n,                ## number of sentence-slots to grow by on overflow (default=1024)
##      talloc   => $ntoks_alloc,      ## number of allocated tokens
##      #salloc   => $nsents_alloc,     ## number of allocated sentences
##      ##
##      ##-- general buffer stuff
##      offset   => $zero_offset,      ## >= 0, logical offset for {sents}[0]
##      #nsents   => $nSents,           ## number of actually stored sentences (should by $cb->{soffsets}->ngood)
##      ntoks    => $nTokens,          ## number of actually stored tokens
##      ##
##      ##-- enums
##      enums     => \%attr2enum,      ## (text=>$textEnum, tag=>$tagEnum, ..., $attrN=>$attrNEnum)
##      attrids   => \%enum,           ## maps ($attrName <=> $attrId)
##      wantattrs => $bool_or_hashref, ## ###hashref or boolean (default: boolean=1)
##                                     ##  + boolean: keep all attrs yes/no
##                                     ## ### + hashref: attr-name pseudo-set to keep
##      ##
##      ##-- sub-buffer (for fast putSentence())
##      cb        => $corpus_buffer,   ## buffer of arbitrary token types, see 'compile()' method
sub new {
  my $that = shift;
  my $pb = bless {
		  ##-- basic data
		  #pdls=>{},
		  pdl      => pdl(long,[]),
		  soffsets => [0],

		  ##-- allocation stuff
		  #tget     => 16384,
		  #sget     => 1024,
		  talloc=>0,
		  salloc=>0,

		  ##-- buffer stuff
		  offset   => 0,
		  #nsents   => 0,
		  ntoks    => 0,

		  ##-- enums
		  attrids   => MUDL::Enum->new(),
		  enums     => {},
		  wantattrs => 1,

		  ##-- sub-buffer
		  cb => MUDL::Corpus::Buffer->new(),
		  @_
		 }, ref($that)||$that;
  ##-- default attributes
  if ($pb->{attrids}->size==0) {
    $pb->{attrids}{sym2id} = {text=>0,tag=>1};
    $pb->{attrids}{id2sym} = [qw(text tag)];
  }

  return $pb;
}

##======================================================================
## new methods

## $pb = $pb->fillPdls()
##  + appends contents of sub-buffer to pdls
##  + clears the sub-buffer
sub fillPdls {
  return $_[0] if (!@{$_[0]{cb}{sents}});

  ##-- hack: set undefval
  my $old_undefval = $PDL::undefval;
  $PDL::undefval = -1;

  my $pb = shift;
  my $cb = $pb->{cb};
  my $sents = $cb->{sents};

  ##-- get number of new tokens
  my $slens_new  = pdl(long,[map {scalar(@$_)} @$sents]);
  #my $nsents_new = scalar(@$sents);
  my $ntoks_new  = $slens_new->sum;

  ##-- generally useful variables
  my $attrids = $pb->{attrids};

  ##-- check for new attributes
  my ($ts,$tok);
  if ($pb->{wantattrs}) {
    foreach (@$sents) {
      $ts = tied($_);
      next if ($ts && ref($ts) eq 'MUDL::Sentence::Buffered::PdlFull' && ($tok->[0]{enums} eq $pb->{enums}));
      foreach $tok (@$_) {
	foreach ($tok->attributeNames) {
	  if (!defined($attrids->{sym2id}{$_})) {
	    $attrids->{sym2id}{$_} = scalar(@{$attrids->{id2sym}});
	    push(@{$attrids->{id2sym}},$_);
	  }
	}
      }
    }
  }
  ##-- ensure attribute enums exist
  my $enums = $pb->{enums};
  foreach (@{$attrids->{id2sym}}) {
    $enums->{$_} = MUDL::Enum->new() if (!defined($enums->{$_}));
  }

  ##-- get new token pdl
  my ($enum,$id,$toka);
  my $pdl_new = pdl(long,
		    [
		     map {
		       $ts = tied($_);
		       (($ts
			 && ref($ts) eq 'MUDL::Sentence::Buffered::PdlFull'
			 && $tok->[0]{enums} eq $enums)
			? (
			   ##-- adopt a compatible MUDL::Sentence::Buffered::PdlFull object
			   $ts->[0]{pdl}->slice(":,".($ts->{soffsets}[$ts->[1]]
						      .':'
						      .($ts->{soffsets}[$ts->[1]+1]-1)))->dog
			  )
			: (
			   ##-- adopt tokens from some other MUDL::Sentence subtype
			   map {
			     $toka=$_->asArray($enums->{id2sym});
			     [map {
			       $enum=$enums->{$attrids->{id2sym}[$_]};
			       if (!defined($toka->[$_])) {
				 $id = -1;
			       } elsif (!defined($id=$enum->{sym2id}{$toka->[$_]})) {
				 $id = $enum->{sym2id}{$toka->[$_]} = scalar(@{$enum->{id2sym}});
				 push(@{$enum->{id2sym}},$toka->[$_]);
			       }
			       $id
			     } (0..$#{$attrids->{id2sym}})]
			   } @$_
			  ))
		     } @$sents
		    ]);


#  ##-- maybe re-allocate: sentences (NOT NEEDED)
#  if ($pb->{salloc} <= $pb->{nsents}+$nsents_new) {
#    $pb->{soffsets}->inplace->reshape($buf->{nsents}+$nsents_new);
#    $pb->{salloc} = $pb->{soffsets}->nelem;
#  }

  ##-- maybe re-allocate: tokens
  my $pdl = $pb->{pdl};
  my ($nAttrsOld,$nToksOld) = ($pdl->dims,0,0);
  my ($nAttrsNew,$nToksNew) = ($nAttrsOld,$nToksOld);
  $nAttrsNew = scalar(@{$attrids->{id2sym}})   if (scalar(@{$attrids->{id2sym}}) > $nAttrsOld);
  $nToksNew  = scalar($pb->{ntoks}+$ntoks_new) if ($pb->{ntoks}+$ntoks_new > $nToksOld);
  if ($pdl->isempty || $nAttrsNew != $nAttrsOld || $nToksNew != $nToksOld) {
    my $newpdl = zeroes(long,$nAttrsNew,$nToksNew)-1;
    $newpdl->slice("0:".($nAttrsOld-1).",0:".($nToksOld-1)) .= $pdl if (!$pdl->isempty);
    $pdl = $pb->{pdl} = $newpdl;
  }

  ##-- guts: gobble sentence offsets
  #  $pb->{soffsets}->slice(($pb->{nsents}+1).":".($pb->{nsents}+1+$nsents_new))
  #    .= ($slens_new->cumusumover)+($pb->{soffsets}->at($pb->{nsents}+1));
  ##--
  push(@{$pb->{soffsets}}, ($slens_new->cumusumover+$pb->{soffsets}[-1])->list);

  ##-- guts: gobble tokens
  $pb->{pdl}->slice("0:".($nAttrsNew-1).",$nToksOld:".($nToksNew-1)) .= $pdl_new;

  ##-- guts: update flags
  $pb->{talloc} = $pb->{pdl}->dim(1);
  #$buf->{salloc} = $buf->{soffsets}->nelem;
  #$buf->{nsents} += $nsents_new;
  $pb->{ntoks}  += $ntoks_new;

  ##--cleanup
  $PDL::undefval=$old_undefval;
  $cb->flush();

  return $pb;
}

## $pb = $pb->trimPdls()
## $pb = $pb->trimPdls($ntoks,$nattrs)
##  + trims buffer pdls to $nsents sentences, $ntoks tokens, defaults are @$pb{qw(nsents ntoks)}
sub trimPdls {
  my ($pb,$ntoks,$nattrs) = @_;
  #$nsents = $pb->{nsents} if (!defined($nsents));
  #$pb->{soffsets}->inplace->reshape($nsents+1);
  #$pb->{salloc} = $nsents;
  $ntoks  = $pb->{ntoks}       if (!defined($ntoks));
  $nattrs = $pb->{pdl}->dim(0) if (!defined($nattrs));
  $pb->{pdl} = $pb->{pdl}->slice("0:".($nattrs-1).",0:".($ntoks-1));
  $pb->{pdl}->sever;
  $pb->{talloc} = $ntoks;
  return $pb;
}


##======================================================================
## accessors

## $pb = $pb->clear()
##  + does NOT clear 'attrids' enum
sub clear {
  my $pb = shift;
  #$pb->{soffsets} = pdl(long,[0]);
  @{$pb->{soffsets}} = qw(0);
  $pb->{pdl}      = pdl(long,[]);
  %{$pb->{enums}} = qw();
  #$pb->{attrids}->clear;
  $pb->{offset} = 0;
  #$pb->{nsents} = 0;
  $pb->{ntoks} = 0;
  $pb->{talloc} = 0;
  #$pb->{salloc} = 0;
  $pb->{cb}->clear();
  return $pb;
}

## $pb = $pb->flush()
##   + clear buffered data, update offset
sub flush {
  my $pb = shift;
  #$pb->{soffsets}  = pdl(long,[0]);
  #$pb->{nsents}    = 0;
  #$pb->{salloc}    = 0;
  @{$pb->{soffsets}} = qw(0);
  $pb->{pdl}       = pdl(long,[]);
  $pb->{offset}   += $pb->{nsents};
  $pb->{ntoks}     = 0;
  $pb->{talloc}    = 0;
  return $pb;
}

##======================================================================
## I/O: on buffer
##======================================================================

## $cr = $buf->reader(%args)
##  + return a new MUDL::CorpusReader which takes input from the buffer
##  + %args are passed to MUDL::CorpusIO::BufReader->new()
sub reader {
  my $pb = shift;
  return MUDL::CorpusIO::PdlFullBufReader->new(buffer=>$pb,@_);
}

## $cw = $buf->writer(%args)
##  + return a new MUDL::CorpusWriter which writes to the buffer
##  + %args are passed to MUDL::CorpusIO::BufWriter->new()
sub writer {
  my $pb = shift;
  return MUDL::CorpusIO::PdlFullBufWriter->new(buffer=>$pb,@_);
}

##======================================================================
## I/O: generic (CorpusIO)
##======================================================================

## $pb = $class_or_obj->fromReader($corpusReader,%args)
##  + fills buffer from $corpusReader
##  + %args are passed to $pb->writer()
## (inherited)

## $pb = $pb->toWriter($corpusWriter,%args)
##  + writes buffer contents to $corpusWriter
##  + %args are passed to $pb->reader()
## (inherited)

##======================================================================
## I/O: files
##======================================================================

## $pb = $class_or_obj->fromFile($filename,%args)
##  + populates buffer from $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileReader()
## (inherited)

## $pb = $pb->toFile($filename,%args)
##  + writes buffer contents to file $filename, which may be prefixed with "${fmt}:"
##  + %args are passed to MUDL::CorpusIO::fileWriter()
## (inherited)

########################################################################
## Sentence class
##  + READ-ONLY (no push(), pop(), shift(), unshift(), etc.)
##  + sentences from a MUDL::Corpus::Buffer::Pdl
##  + allow generic array-style sentenceaccess with Tie::Array
########################################################################
package MUDL::Sentence::Buffered::PdlFull;
use Tie::Array;
use MUDL::Object qw(dummy);
use strict;
our @ISA = qw(Tie::Array MUDL::Sentence);

##=========================================================
## Sentence: Constructors etc.

## $s = $class_or_obj->new(@tokens)
##  + dummy: instantiates a new buffer
##  + object structure:
##    [$buffer, $sentenceIndex]
sub new {
  warn(ref($_[0])||$_[0], "::new(): not yet sensibly implemented!");
  return bless [], ref($_[0])||$_[0];
}

##=========================================================
## Sentence: Tied: Read Methods (Mandatory)

## $class->TIEARRAY($buffer,$sentenceIndex)
sub TIEARRAY { return bless [@_[1,2]], ref($_[0])||$_[0]; }

## $val = $obj->FETCH($index)
sub FETCH {
  my $buf = $_[0][0];
  return undef if (!defined($buf) || $_[0][1] >= $#{$buf->{soffsets}});
  return bless [$buf, $buf->{soffsets}[$_[0][1]]+$_[1]], 'MUDL::Token::Buffered::PdlFull';
}

## $size = $obj->FETCHSIZE()
##  + like scalar(@a)
sub FETCHSIZE {
  my $buf  = $_[0][0];
  return undef if (!defined($buf) || $_[0][1] >= $#{$buf->{soffsets}});
  return $buf->{soffsets}[$_[0][1]+1]-$buf->{soffsets}[$_[0][1]];
}


##=========================================================
## Sentence: Tied: Write Methods (disabled)

#sub STORE     ($$$)  { $_[0]{a}[$_[1]]=$_[2]; }                      ## STORE this, index, val
#sub STORESIZE ($$)   { $#{$_[0]{a}} = $_[1]-1 }                      ## STORESIZE this, count
#sub DELETE    ($$)   { delete $_[0]{a}[$_[1]] }                      ## DELETE this, index
*STORE     = dummy('STORE');
*STORESIZE = dummy('STORESIZE');
*DELETE    = dummy('DELETE');

## $obj->EXISTS(index)
##   + used by perl debugger
sub EXISTS {
  my $buf  = $_[0][0];
  return (defined($buf)
	  && $_[0][1] <= $#{$buf->{soffsets}}
	  && $_[1]    <  $buf->{soffsets}[$_[0][1]+1]-$buf->{soffsets}[$_[0][1]]);
}

##=========================================================
## Sentence: Tied: Optional Methods (disabled)

#sub CLEAR     ($)    { @{$_[0]{a}} = () }                            ## CLEAR this
#sub POP       ($)    { pop(@{$_[0]{a}}) }                            ## POP this
#sub PUSH      ($@)   { push(@{$_[0]{a}},@_[1..$#_]) }                ## PUSH this, list
#sub SHIFT     ($)    { shift(@{$_[0]{a}}) }                          ## SHIFT this
#sub UNSHIFT   ($@)   { unshift(@{$_[0]->{a}},@_[1..$#_]) }           ## UNSHIFT this, list
#sub SPLICE    ($$$@) { splice(@{$_[0]{a}},$_[1],$_[2],@_[3..$#_]); } ## SPLICE this,offset,length,list
##sub EXTEND    ($$)   { $#{$_[0]{a}} = $_[1]-1; }                    ## EXTEND this,count: see also STORESIZE


########################################################################
## MUDL::Corpus::Buffer::PdlFull::Token
##  + tokens from a MUDL::Corpus::Buffer::Pdl
##  + allow generic token-object access with Tie::Array
########################################################################
package MUDL::Token::Buffered::PdlFull;
use MUDL::Object qw(dummy);
use strict;
our @ISA = qw(MUDL::Token::Base);

##======================================================================
## MUDL::Token::Base API
##======================================================================

##======================================================================
## MUDL::Token::Base API: Constructor

## $tok = $class->new(text=>$text, tag=>$tag, %attributes);
##  + object structure: ARRAY: [ $buffer, $tokenIndex ]
sub new {
  warn(ref($_[0])||$_[0], "::new(): not yet sensibly implemented!");
  return bless [], ref($_[0])||$_[0];
}

## $tok = $class_or_obj->newFromToken($srcTok,@args_to_new)
##  + OPTIONAL: general typecasting method, always copies
sub newFromToken {
  return bless [@{$_[1]}], ref($_[0])||$_[0]
    if (!ref($_[0]) || ref($_[0]) eq ref($_[1]));
  confess(ref($_[0])||$_[0], "::newFromToken(): incompatible source token class ", ref($_[1]));
}

## $tok_or_srcTok = $class->fromToken($srcTok)
## $tok_or_newTok = $tok->fromToken($srcTok)
##  + general typecasting method, may just return $srcTok
##  + OPTIONAL: default dispatches to newFromToken()
##  + READ-ONLY
sub fromToken {
  return $_[1] if (!ref($_[0]) || ref($_[1]) && $_[0] eq ref($_[1]));
  confess(ref($_[0])||$_[0], "::newFromToken(): incompatible source token class ", ref($_[1]));
}

## $tok_or_newTok =   $tok->fromHash(\%srcHash)
##  + general constructor; \%srcHash has structure of args to 'new()' method
*fromArray = dummy('fromArray'); #FIXME


## $tok_or_newTok =   $tok->fromArray(\@srcArray)
## $tok_or_newTok =   $tok->fromArray(\@srcArray,\@srcAttributeNames)
##  + general constructor
##    - \@srcArray has structure of a TT token: [ $val_attr1, ..., $val_attrN ]
##    - if \@srcAttributeNames is unspecified, it defaults to ['text','tag',0..$#$srcArray]
##  + OPTIONAL: default calls fromHash()
#FIXME: dies
*fromHash = dummy('fromHash'); #FIXME

##  %tokHash = $tok->asHash #-- list context
## \%tokHash = $tok->asHash #-- scalar context, may be dangerous to change
##  + OPTIONAL: default is (text=>$tok->text, tag=>$tok->tag, $tok->attributes());
sub asHash {
  my ($buf,$toki) = @{$_[0]};
  if (!defined($buf) || !defined($toki)) { return wantarray ? qw() : {}; }
  my ($attr,$id);
  my $tokh = {
	      map {
		$attr = $buf->{attrids}{id2sym}[$_];
		$id   = $buf->{pdl}->at($_,$toki);
		($id < 0 ? qw() : ($attr=>$buf->{enums}{$attr}{id2sym}[$id]))
	      } (0..$#{$buf->{attrids}{id2sym}})
	     };
  return wantarray ? %$tokh : $tokh;
}

##  @tokArray = $tok->asArray()            #-- list context
## \@tokArray = $tok->asArray()            #-- scalar context
##  @tokArray = $tok->asArray(\@attrNames) #-- list context, given names
## \@tokArray = $tok->asArray(\@attrNames) #-- scalar context, given names
##  + if @attrNames is unspecified, it defaults to all numeric attribute names
##  + like @ary[0,1,@attrNames]=($tok->text, $tok->tag, @attrNames)
sub asArray {
  my ($buf,$toki) = @{$_[0]};
  if (!defined($buf) || !defined($toki)) { return wantarray ? qw() : []; }
  my $attrNames = $_[1];
  $attrNames = [
		'text','tag',
		grep {defined($_) && $_ =~ /^\d+$/}
		@{$buf->{attrids}{id2sym}}
	       ] if (!defined($attrNames));
  my $attrIds = [ @{$buf->{attrids}{sym2id}}{ @$attrNames } ];
  my ($id);
  my $ary = [
	     map {
	       if (!defined($attrIds->[$_])) {
		 $id=-1;
	       } else {
		 $id=$buf->{pdl}->at($_,$toki);
	       }
	       ($id < 0 ? qw() : $buf->{enums}{$attrNames->[$_]}{id2sym}[$id])
	     } (0..$#$attrIds)
	    ];
  return wantarray ? @$ary : $ary;
}

##======================================================================
## MUDL::Token API: Accessors

## $txt = $tok->text()
## $txt = $tok->text($txt)
sub text {
  #my ($buf,$toki) = @{$_[0]};
  return undef if (!defined($_[0][0])||!defined($_[0][1]));
  my ($enum,$ai) = ($_[0][0]{enums}{text},$_[0][0]{attrids}{sym2id}{text});
  return undef if (!defined($enum) || !defined($ai));
  my ($id);
  if ($#_ <= 0) {
    $id = $_[0][0]{pdl}->at($ai,$_[0][1]);
    return $id < 0 ? undef : $enum->{id2sym}[$id];
  }
  if (!defined($id=$enum->{sym2id}{$_[1]})) {
    $id=$enum->{sym2id}{$_[1]}=scalar(@{$enum->{id2sym}});
    push(@{$enum->{id2sym}},$_[1]);
  }
  $_[0][0]{pdl}->set($ai,$_[0][1],$id);
  return $_[1];
}

## $tag = $tok->tag()
## $tag = $tok->tag($tag)
sub tag {
  #my ($buf,$toki) = @{$_[0]};
  return undef if (!defined($_[0][0])||!defined($_[0][1]));
  my ($enum,$ai) = ($_[0][0]{enums}{tag},$_[0][0]{attrids}{sym2id}{tag});
  return undef if (!defined($enum) || !defined($ai));
  my ($id);
  if ($#_ <= 0) {
    $id = $_[0][0]{pdl}->at($ai,$_[0][1]);
    return $id < 0 ? undef : $enum->{id2sym}[$id];
  }
  if (!defined($id=$enum->{sym2id}{$_[1]})) {
    $id=$enum->{sym2id}{$_[1]}=scalar(@{$enum->{id2sym}});
    push(@{$enum->{id2sym}},$_[1]);
  }
  $_[0][0]{pdl}->set($ai,$_[0][1],$id);
  return $_[1];
}

## @attributeNames = $tok->attributeNames()
sub attributeNames { return grep {defined($_) && $_ ne 'text' && $_ ne 'tag'} @{$_[0][0]{attrids}{id2sym}}; }

## @attributeValues = $tok->attributeValues
sub attributeValues {
  my $attrids = $_[0][0]{attrids};
  my ($attr,$id);
  return (
	  map {
	    $id = $_[0][0]{pdls}{$_}->at($_,$_[0][1]);
	    ($id < 0 || !defined($attr=$attrids->{id2sym}[$_]) || $attr eq 'text' || $attr eq 'tag'
	     ? undef
	     : $_[0][0]{enums}{$attr}{id2sym}[$id])
	  } (0..$#{$attrids->{id2sym}})
	 );
}

## %attributes = $tok->attributes
sub attributes {
  my $attrids = $_[0][0]{attrids};
  my ($attr,$id);
  return (
	  map {
	    $id = $_[0][0]{pdls}{$_}->at($_,$_[0][1]);
	    ($id < 0 || !defined($attr=$attrids->{id2sym}[$_])
	     ? undef
	     : ($attr=>$_[0][0]{enums}{$attr}{id2sym}[$id]))
	  } (0..$#{$attrids->{id2sym}})
	 );
}

## $val = $tok->getAttribute($attr)
## $val = $tok->setAttribute($attr,$val)
#*getAttribute = *getAttr = *getattr = *get = *setAttribute = *setAttr = *setattr = *set = \&attribute;
sub attribute {
  return undef if (!defined($_[0][0])||!defined($_[0][1]));
  my ($enum,$ai) = ($_[0][0]{enums}{$_[1]},$_[0][0]{attrids}{sym2id}{$_[1]});
  return undef if (!defined($enum) || !defined($ai));
  my ($id);
  if ($#_ <= 0) {
    $id = $_[0][0]{pdl}->at($ai,$_[0][1]);
    return $id < 0 ? undef : $enum->{id2sym}[$id];
  }
  if (!defined($id=$enum->{sym2id}{$_[1]})) {
    $id=$enum->{sym2id}{$_[2]}=scalar(@{$enum->{id2sym}});
    push(@{$enum->{id2sym}},$_[2]);
  }
  $_[0][0]{pdl}->set($ai,$_[0][1],$id);
  return $_[2];
}

##======================================================================
## MUDL::Token API: I/O: Native (TT)
#(inherited)

##======================================================================
## MUDL::Token API: I/O: LOB | Brown
#(inherited)

##======================================================================
## MUDL::Token API: I/O: XML
#(inherited)



########################################################################
## I/O: PdlFullBufReader
##
##  + read from a MUDL::Corpus::Buffer::PdlFull
##
########################################################################

package MUDL::CorpusIO::PdlFullBufReader;
use MUDL::Corpus::Buffer;
#use MUDL::EToken;
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
	  || $cr->{rpos}-$cr->{buffer}{offset} >= $#{$cr->{buffer}{soffsets}});
}

## \@sentence = $cr->getSentence();
##   + ye olde guttes
sub getSentence {
  my $cr=shift;
  my ($buf);
  return undef if (!defined($buf=$cr->{buffer})
		   || $cr->{rpos}-$buf->{offset} >= $#{$cr->{buffer}{soffsets}});

  $cr->{nsents}++;
  $cr->{ntoks} += $buf->{soffsets}[$cr->{rpos}+1]-$buf->{soffsets}[$cr->{rpos}];

  my $a = [];
  tie(@$a, 'MUDL::Sentence::Buffered::PdlFull', $buf, $cr->{rpos}++);
  return $a;
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
##    cbw   => $subwriter,     ##-- for writing to pdl's sub-buffer
##    #type=>   $pdl_datatype,  ##-- default=long
sub new {
  my ($that,%args) = @_;
  my $buf = defined($args{buffer}) ? $args{buffer} : $args{corpus};
  $buf = MUDL::Corpus::Buffer::PdlFull->new() if (!defined($buf));
  delete(@args{qw(buffer corpus)});
  my $cw = bless {
		buffer=>$buf,
		##
		##-- CorpusWriter wrapping
		#saveSub=>\&saveSub,   ##-- method to use for buffer saving
		#                      ##   + this gets set by toString(), toFile(), etc.
		#saveArgs=>\@args,     ##-- user args to \&saveSub, e.g. 'mode','iolayers'; default: none
		#
		%args
	       }, ref($that)||$that;
  $cw->{cbw} = $cw->{buffer}{cb}->writer(); ##-- sub-buffer writer
  return $cw;
}

## $bool = $cw->flush
sub flush {
  ##-- fill & trim buffer pdls
  my $cw = shift;
  $cw->{buffer}->fillPdls();
  $cw->{buffer}->trimPdls();
  $cw->{cbw} = $cw->{buffer}{cb}->writer; ##-- new writer
  return $cw->SUPER::flush();
}

## undef = $cw->putSentence(\@sent);
sub putSentence {
  return $_[0]{cbw}->putSentence($_[1]);
}

## undef = $cr->putToken($text_or_hashref);
##  + not implemented

## undef = $cr->toString(\$string)
#(inherited)

## undef = $cr->toFile($filename_or_fh);
## undef = $cr->Fh($fh);
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
