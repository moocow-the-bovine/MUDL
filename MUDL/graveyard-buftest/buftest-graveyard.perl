#use MUDL::Corpus::Buffer::PdlFull; ##-- OLD
#sub create_corpus_buffer_pdlfull {
#  our $cb = load("$cfile.buf.bin");
#  our $pbw = MUDL::CorpusIO->fileWriter("$cfile.pdlbuf.bin");
#  our $pb = $pbw->{buffer};
#  #$s = $cb->{sents}[0];
#  #$pbw->putSentence($s);
#  our $cbr=$cb->reader;
#  $pbw->putReader($cbr);
#  $pbw->flush;
#}
##create_corpus_buffer_pdlfull();

##use MUDL::Corpus::Buffer::Packed; ##-- OLD
#sub create_corpus_packed {
#  our $eb   = load("$cfile.ebuf.bin");
#  our $pkbw = MUDL::CorpusIO->fileWriter("$cfile.packed.bin",
#					 buffer=>($pkb=MUDL::Corpus::Buffer::Packed->new(enums=>$eb->{enums})));
#  our $pkb  = $pkbw->{buffer};
#  $pkbw->putReader($eb->reader);
#  $pkbw->flush;
#}
##create_corpus_packed();


##----------------------------------------------------------------------
## Bench: Subs: Process: getSentence()
##----------------------------------------------------------------------

## $nsents = count_sents($reader)
##  + benchmarking sub
sub count_sents {
  my $cr = shift;
  my $nsents = 0;
  my ($s);
  ++$nsents while (defined($s=$cr->getSentence));
  return $nsents;
}

BEGIN {
  #our %count_subs  = map { ($_ =>"count_sents(\$reader_subs{'$_'}->());") } keys(%reader_subs);
  our %count_subs = map { my $ext=$_; ($_=>sub { count_sents($reader_subs{$ext}->()); }) } keys(%reader_subs);
}

sub bench_getSentence {
  $ttr = timethese(-3, \%count_subs);
  store_timeresults($ttr,"getS");
  #cmpthese($ttr);
}
#bench_getSentence();
cmpbench('getS');
#
#                 Rate   ttt ttt.bin ttt.ptt.bin ttt.ebuftt.bin ttt.buf.bin ttt.pdlbuf.bin
#ttt            1.40/s    --    -59%        -80%           -84%        -92%           -95%
#ttt.bin        3.38/s  142%      --        -51%           -60%        -82%           -89%
#ttt.ptt.bin    6.84/s  388%    102%          --           -20%        -63%           -77%
#ttt.ebuftt.bin 8.55/s  510%    153%         25%             --        -53%           -72%
#ttt.buf.bin    18.4/s 1211%    442%        168%           115%          --           -39%
#ttt.pdlbuf.bin 30.2/s 2060%    794%        342%           254%         65%             --

##----------------------------------------------------------------------
## Bench: Subs: Process: getSentence() + $tok->text()
##----------------------------------------------------------------------

## $nsents = bench_get_text_reader($reader)
##  + benchmarking sub
sub bench_get_text_reader {
  my $cr = shift;
  my ($s,$tok,$text);
  while (defined($s=$cr->getSentence)) {
    foreach $tok (@$s) {
      $text = $tok->text;
    }
  }
}

BEGIN {
  our %get_text_subs = map {
    my $ext=$_;
    ($_=>sub { bench_get_text_reader($reader_subs{$ext}->())})
  } keys(%reader_subs);
}

sub bench_get_text {
  $ttr = timethese(-3,\%get_text_subs);
  #cmpthese($ttr);
  store_timeresults($ttr,"getText");
}
#bench_get_text();
cmpbench('getText');

#                 Rate ttt.pdlbuf.bin  ttt ttt.bin ttt.ptt.bin ttt.ebuftt.bin ttt.cbuf.bin
#ttt.pdlbuf.bin 1.15/s             --  -7%    -54%        -72%           -72%        -81%
#ttt            1.23/s             7%   --    -51%        -70%           -70%        -80%
#ttt.bin        2.53/s           120% 105%      --        -38%           -39%        -59%
#ttt.ptt.bin    4.09/s           255% 231%     61%          --            -2%        -34%
#ttt.ebuftt.bin 4.18/s           263% 239%     65%          2%             --        -33%
#ttt.cbuf.bin   6.23/s           440% 405%    146%         52%            49%          --


##----------------------------------------------------------------------
## Bench: build enum from text buffer
##----------------------------------------------------------------------

## $enum = bench_get_enum_ttsents(\@sents,$i)
sub bench_get_enum_ttsents {
  my ($sents,$attri) = @_;
  my $enum = MUDL::Enum->new();
  my $sym2id = $enum->{sym2id};
  my $id2sym = $enum->{id2sym};
  foreach (@$sents) {
    foreach (@$_) {
      if (!exists($sym2id->{$_->[$attri]})) {
	$sym2id->{$_->[$attri]} = scalar(@$id2sym);
	push(@$id2sym,$_->[$attri]);
      }
    }
  }
  return $enum;
}
#our $e0=bench_get_enum_ttsents($_cbr->{buffer}{sents},0);
#our @es=map {bench_get_enum_ttsents($_cbr->{buffer}{sents},$_)} (0..2);

## bench_get_enum_ttsort($sents,$attr_i)
sub bench_get_enum_ttsort {
  my ($sents,$attri) = @_;
  my $enum = MUDL::Enum->new();
  my $sym2id = $enum->{sym2id};
  my $id2sym = $enum->{id2sym};
  my $ptxt=undef;
  my ($val);
  @$id2sym =
    grep { defined($_) }
    map { $val=!defined($ptxt) || $_ ne $ptxt ? $_ : undef; $ptxt=$_; $val }
    sort map { map {$_->[$attri] } @$_ } @$sents;
  @$sym2id{@$id2sym} = (0..$#$id2sym);
  return $enum;
}
#bench_get_enum_ttsort($_cbr->{buffer}{sents},0);

sub bench_get_enums {
  our ($es,$efe);
  cmpthese(-3,{
	       'enum:sort'   =>sub{$es=bench_get_enum_ttsort($_cbr->{buffer}{sents},0);},
	       'enum:foreach'=>sub{$efe=bench_get_enum_ttsents($_cbr->{buffer}{sents},0);},
	      });
}
#bench_get_enums

#               Rate    enum:sort enum:foreach
#enum:sort    22.4/s           --         -58%
#enum:foreach 52.8/s         136%           --


##----------------------------------------------------------------------
## Bench: Subs: Process: get corpus as a list of sentence PDLs (text only)
##----------------------------------------------------------------------


## $nsents = bench_get_sent_pdls_ttreader($reader)
##  + benchmarking sub for readers returning MUDL::Token:TT objects
sub bench_get_sent_pdls_ttreader {
  my $cr = shift;
  my $enum = MUDL::Enum->new;
  my $sym2id = $enum->{sym2id};
  my $id2sym = $enum->{id2sym};
  my $i=0;
  my @sents = qw();
  my ($s,$id);
  push(@sents,$s) while (defined($s=$cr->getSentence));
  my $psents = [
		map {
		  pdl(long, [
			     map {
			       if (!defined($id=$sym2id->{$_->[0]})) {
				 $id = $sym2id->{$_->[0]} = $i;
				 $id2sym->[$i++] = $_->[0];
			       }
			       $id
			     } @$_
			    ])
		} @sents
	       ];
  return ($enum,$psents);
}
#bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile"));
#bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile.bin"));
#bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile.cbuf.bin"));

sub bench_get_sent_pdls_buffer {
  my $buf = shift;
  my $sents = $buf->{sents};
  my $enum = MUDL::Enum->new;
  my $psents = [];
  foreach $s (@$sents) {
    push(@$psents, pdl(long,[map {$enum->addSymbol($_->text)} @$s]));
  }
  return ($enum,$psents);
}
#bench_get_sent_pdls_buffer(load("$cfile.buf.bin"));

sub bench_get_sent_pdls_ebuffer {
  my $buf = shift;
  my $sents = $buf->{sents};
  my $enum  = $buf->{enums}{text};
  my $psents = [map { pdl(long,map {$_->{text}} @$_) } @$sents];
  return ($enum,$psents);
}
#bench_get_sent_pdls_ebuffer(load("$cfile.ebuf.bin"));

sub bench_get_sent_pdls_ebuffer_tt {
  my $buf = shift;
  my $sents = $buf->{sents};
  my $enum  = $buf->{enums}[0];
  my $psents = [map { pdl(long,map {$_->[1]} @$_) } @$sents];
  return ($enum,$psents);
}
#bench_get_sent_pdls_ebuffer_tt(load("$cfile.ebuftt.bin"));

sub bench_get_sent_pdls_pdlbuffer {
  my $buf = shift;
  my $psents = [];
  my $soff = $buf->{soffsets};
  my $txtpdl = $buf->{pdls}{text};
  my $si0=0;
  my ($si1);
  foreach $si1 ($soff->slice("1:-1")->list) {
    push(@$psents, $txtpdl->slice("$si0:".($si1-1)));
    $si0=$si1;
  }
  return ($buf->{enums}{text},$psents);
}
#bench_get_sent_pdls_pdlbuffer(load("$cfile.pdlbuf.bin"));

sub bench_get_sent_pdls_packed {
  my $pbuf   = shift;
  my $psents = [map {pdl(long,[map {vec($$_,0,32)} @$_])} @{$pbuf->{sents}}];
  return ($pbuf->{enums}{text},$psents);
}
#bench_get_sent_pdls_packed(load("$cfile.packed.bin"));


BEGIN {
  our %get_sent_pdls_subs =
    (
     'ttt'     => sub { bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile")); },
     'ttt.bin' => sub { bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile.bin")); },
     'ttt.cbuf.bin' => sub { bench_get_sent_pdls_ttreader(load("$cfile.cbuf.bin")->reader); },
     'ttt.prebuf.bin' => sub { $_cbr->reset; bench_get_sent_pdls_ttreader($_cbr); },
     #'ttt.ebuf.bin'  => sub { bench_get_sent_pdls_ebuffer(load("$cfile.ebuf.bin")); },
     #'ttt.ebuftt.bin'  => sub { bench_get_sent_pdls_ebuffer_tt(load("$cfile.ebuftt.bin")); },
     #'ttt.pdlbuf.bin' => sub { bench_get_sent_pdls_pdlbuffer(load("$cfile.pdlbuf.bin")); },
     #'ttt.packed.bin' => sub { bench_get_sent_pdls_packed(load("$cfile.packed.bin")); },
    );

}

sub bench_get_sent_pdls {
  cmpthese(3,\%get_sent_pdls_subs);
}
#bench_get_sent_pdls();

#                 Rate   ttt ttt.bin ttt.buf.bin ttt.ebuf.bin ttt.ebuftt.bin ttt.packed.bin ttt.pdlbuf.bin
#ttt            1.04/s    --    -44%        -70%         -89%           -91%           -92%           -97%
#ttt.bin        1.85/s   78%      --        -47%         -80%           -83%           -85%           -94%
#ttt.buf.bin    3.49/s  236%     88%          --         -63%           -69%           -72%           -90%
#ttt.ebuf.bin   9.38/s  803%    406%        169%           --           -16%           -25%           -72%
#ttt.ebuftt.bin 11.1/s  970%    500%        219%          19%             --           -11%           -67%
#ttt.packed.bin 12.5/s 1104%    575%        258%          33%            13%             --           -62%
#ttt.pdlbuf.bin 33.3/s 3111%   1700%        856%         256%           200%           167%             --


##----------------------------------------------------------------------
## Bench: Subs: Process: get whole corpus as a single PDL (text only)
##  + FIXME
##----------------------------------------------------------------------

## bench_get_full_pdls_reader($reader)
##  + benchmarking sub
sub bench_get_full_pdl_reader {
  my $cr = shift;
  my $enum = MUDL::Enum->new;
  my $sents = [];
  my ($s);
  push(@$sents,$s) while (defined($s=$cr->getSentence));
  my $soffsets = pdl(long,[0,map {scalar(@$_)} @$sents])->cumusumover;
  my $txtpdl   = zeroes(long,$soffsets->at(-1));
  my $i=0;
  foreach $s (@$sents) {
    $txtpdl->slice($i++) .= $enum->addSymbol($_->text) foreach (@$s);
  }
  return ($enum,$soffsets,$txtpdl);
}
#bench_get_full_pdl_reader(MUDL::CorpusIO->fileReader("$cfile"));

sub bench_get_full_pdl_buffer {
  my $buf = shift;
  my $enum = MUDL::Enum->new;
  my $sents = $buf->{sents};
  my $soffsets = pdl(long,[0,map {scalar(@$_)} @$sents])->cumusumover;
  my $txtpdl   = zeroes(long,$soffsets->at(-1));
  my $i=0;
  foreach $s (@$sents) {
    $txtpdl->slice($i++) .= $enum->addSymbol($_->text) foreach (@$s);
  }
  return ($enum,$soffsets,$txtpdl);
}
#bench_get_full_pdl_buffer(load("$cfile.buf.bin"));

sub bench_get_full_pdl_ebuffer {
  my $buf = shift;
  my $sents = $buf->{sents};
  my $enum  = $buf->{enums}{text};
  my $soffsets = pdl(long,[0,map {scalar(@$_)} @$sents])->cumusumover;
  my $txtpdl   = pdl(long,[ map { map {$_->{text}} @$_ } @$sents ]);
  return ($buf->{enums}{text},$soffsets,$txtpdl);
}
#bench_get_full_pdl_ebuffer(load("$cfile.ebuf.bin"));

sub bench_get_full_pdl_ebuffer_tt {
  my $buf = shift;
  my $sents = $buf->{sents};
  my $enum  = $buf->{enums}[0];
  my $soffsets = pdl(long,[0,map {scalar(@$_)} @$sents])->cumusumover;
  my $txtpdl   = pdl(long,[map { map {$_->[1]} @$_ } @$sents ]);
  return ($enum,$soffsets,$txtpdl);
}
#bench_get_full_pdl_ebuffer_tt(load("$cfile.ebuftt.bin"));

sub bench_get_full_pdl_pdlbuffer {
  my $buf = shift;
  return ($buf->{enums}{text},$buf->{soffsets},$buf->{pdls}{text});
}
#bench_get_full_pdl_pdlbuffer(load("$cfile.pdlbuf.bin"));

sub bench_get_full_pdl_packed {
  my $pbuf = shift;
  my $soffsets = pdl(long, [ 0, map {scalar(@$_)} @{$pbuf->{sents}}])->cumusumover;
  my $txtpdl   = pdl(long, [    map { map {vec($$_,0,32)} @$_ } @{$pbuf->{sents}} ]);
  return ($pbuf->{enums}{text},$soffsets,$txtpdl);
}
#bench_get_full_pdl_packed(load("$cfile.packed.bin"));

BEGIN {
  our %get_full_pdl_subs =
    (
     'ttt'     => sub { bench_get_full_pdl_reader(MUDL::CorpusIO->fileReader("$cfile")); },
     'ttt.bin' => sub { bench_get_full_pdl_reader(MUDL::CorpusIO->fileReader("$cfile.bin")); },
     'ttt.cbuf.bin' => sub { bench_get_full_pdl_buffer(load("$cfile.cbuf.bin")); },
     'ttt.ebuf.bin'  => sub { bench_get_full_pdl_ebuffer(load("$cfile.ebuf.bin")); },
     'ttt.ebuftt.bin'  => sub { bench_get_full_pdl_ebuffer_tt(load("$cfile.ebuftt.bin")); },
     'ttt.pdlbuf.bin'  => sub { bench_get_full_pdl_pdlbuffer(load("$cfile.pdlbuf.bin")); },
     'ttt.packed.bin'  => sub { bench_get_full_pdl_packed(load("$cfile.packed.bin")); },
    );
}

sub bench_get_full_pdl {
  cmpthese(4,\%get_full_pdl_subs);
}
#bench_get_full_pdl();

#                  Rate   ttt ttt.bin ttt.buf.bin ttt.ebuf.bin ttt.ebuftt.bin ttt.packed.bin ttt.pdlbuf.bin
#ttt            0.708/s    --    -30%        -48%         -95%           -95%           -96%           -99%
#ttt.bin         1.01/s   43%      --        -27%         -92%           -93%           -95%           -98%
#ttt.buf.bin     1.37/s   94%     36%          --         -89%           -91%           -93%           -98%
#ttt.ebuf.bin    12.9/s 1723%   1177%        839%           --           -16%           -35%           -81%
#ttt.ebuftt.bin  15.4/s 2073%   1423%       1019%          19%             --           -23%           -77%
#ttt.packed.bin  20.0/s 2725%   1880%       1355%          55%            30%             --           -70%
#ttt.pdlbuf.bin  66.7/s 9317%   6500%       4750%         417%           333%           233%             --



##----------------------------------------------------------------------
## test storable stuff: MUDL::Enum
##----------------------------------------------------------------------

sub test_enum {
  our $e = MUDL::Enum->new();
  $e->addSymbol($_) foreach ('foo','bar','baz');
  $e->addIndexedSymbol('four',4);
  our $ef = $e->saveBinString;
  our $eft = ref($e)->loadBinString($ef);

  our $eh  = { %$e };
  our $ehf = Storable::freeze($eh);
  our $eht = Storable::thaw($ehf);
}
#test_enum;

##----------------------------------------------------------------------
## test enum functionality
##----------------------------------------------------------------------

## undef = get_test_symbols();
##  + gets all symbols
sub test_get_all_symbols {
  my $cbr = MUDL::CorpusIO->fileReader("$cfile.cbuf.bin");
  my ($s);
  our @test_symbols = map {map {$_->text} @$_} @{$cbr->{buffer}{sents}};
  return undef;
}

sub test_addsymbol_enum_methods {
  my $e = MUDL::Enum->new();
  $e->addSymbol($_) foreach (@test_symbols);
  return $e;
}
sub test_addsymbol_enum_direct {
  my $e = MUDL::Enum->new();
  foreach (@test_symbols) {
    next if (exists($e->{sym2id}{$_}));
    $e->{sym2id}{$_} = scalar(@{$e->{id2sym}});
    push(@{$e->{id2sym}},$_);
  }
  return $e;
}
sub test_addsymbol_enum_direct_myvars {
  my $e = MUDL::Enum->new();
  my $s2i = $e->{sym2id};
  my $i2s = $e->{id2sym};
  foreach (@test_symbols) {
    next if (exists($s2i->{$_}));
    $s2i->{$_} = scalar(@$i2s);
    push(@$i2s,$_);
  }
  return $e;
}

use Gfsm;
sub test_addsymbol_abet {
  my $abet = Gfsm::Alphabet->new();
  $abet->get_label($_) foreach (@test_symbols);
  return $abet;
}

BEGIN {
  our %bench_addsymbol_subs =
    (
     'enum:methods'=>\&test_addsymbol_enum_methods,
     'enum:direct' =>\&test_addsymbol_enum_direct,
     'enum:direct:my' =>\&test_addsymbol_enum_direct_myvars,
     'gfsm:abet'   =>\&test_addsymbol_abet,
    );
}

##-- benchmark
sub bench_addsymbol {
  test_get_all_symbols();
  cmpthese(-3,\%bench_addsymbol_subs);
}
#bench_addsymbol();

#                 Rate  enum:methods      gfsm:abet    enum:direct enum:direct:my
#enum:methods   9.84/s            --           -11%           -80%           -87%
#gfsm:abet      11.1/s           13%             --           -77%           -86%
#enum:direct    48.3/s          390%           334%             --           -37%
#enum:direct:my 76.9/s          681%           592%            59%             --



##----------------------------------------------------------------------
## test buffered I/O wrappers
##----------------------------------------------------------------------

sub test_bufio_wrap_writer {
  $cb = load("$cfile.buf.bin");
  $bw = MUDL::CorpusIO->fileWriter("$cfile.cbuf.bin");
  our $bwb = $bw->{buf};
  $bw->putReader($cb->reader);
  $bw->flush();
  undef($bw);
}
#test_bufio_wrap_writer();

sub test_bufio_wrap_stringwriter {
  $cb = load("$cfile.buf.bin");
  $bw = MUDL::CorpusIO::BufWriter->new;
  our $s='';
  $bw->toString(\$s);
  $bw->putReader($cb->reader);
  $bw->flush();
}
#test_bufio_wrap_stringwriter;

sub test_bufio_wrap_reader {
  $cbr = MUDL::CorpusIO->fileReader("$cfile.cbuf.bin");
  $cb  = MUDL::Corpus::Buffer->new();
  $cb->writer->putReader($cbr);
}
#test_bufio_wrap_reader();

##----------------------------------------------------------------------
## test: packed tokens
##----------------------------------------------------------------------

sub test_ptoken {
  our $etext = MUDL::Enum->new();
  our $etag  = MUDL::Enum->new();
  our $e0    = MUDL::Enum->new();
  our $enums = {text=>$etext,tag=>$etag,0=>$e0};

  MUDL::PToken->pushEnums($enums);
  our $et1 = MUDL::EToken->new(_enums=>$enums, text=>'foo', tag=>'A',0=>'~a');
  our $pt1 = MUDL::PToken->fromToken($et1);

  $ttw->putToken($et1);
  $ttw->putToken($pt1);
}
#test_ptoken;

use MUDL::Corpus::Buffer::Packed;
sub test_ptoken_rw {
  our $etext = MUDL::Enum->new();
  our $etag  = MUDL::Enum->new();
  our $e0    = MUDL::Enum->new();
  our $enums = {text=>$etext,tag=>$etag,0=>$e0};

  our $cb  = load("$cfile.cbuf.bin");
  our $s0  = $cb->{sents}[0];

  our $pkb = MUDL::Corpus::Buffer::Packed->new(enums=>$enums);
  our $pkbw = $pkb->writer();
  our $pkbr = $pkb->reader();

  $pkbw->putSentence($s0);
  $s1 = $pkbr->getSentence;

  $ttw->putSentence($s1);
}
#test_ptoken_rw;

##----------------------------------------------------------------------
## test: tied array
##----------------------------------------------------------------------
package myarray;
use Tie::Array;
#use strict;
BEGIN { our @ISA = qw(Tie::StdArray); }

## TIEARRAY classname, LIST
sub TIEARRAY {
  my $that = shift;
  return bless { a=>[], @_ }, ref($that)||$that;
}
##-- Read Methods (Mandatory)
sub FETCH     ($$)   { $_[0]{a}[$_[1]]; }                            ## FETCH this, index
sub FETCHSIZE ($)    { scalar(@{$_[0]{a}}); }                        ## FETCHSIZE this

##-- Write Methods
sub STORE     ($$$)  { $_[0]{a}[$_[1]]=$_[2]; }                      ## STORE this, index, val
sub STORESIZE ($$)   { $#{$_[0]{a}} = $_[1]-1 }                      ## STORESIZE this, count
sub EXISTS    ($$)   { exists $_[0]{a}[$_[1]] }                      ## EXISTS this, index
sub DELETE    ($$)   { delete $_[0]{a}[$_[1]] }                      ## DELETE this, index

##-- Optional Methods
sub CLEAR     ($)    { @{$_[0]{a}} = () }                            ## CLEAR this
sub POP       ($)    { pop(@{$_[0]{a}}) }                            ## POP this
sub PUSH      ($@)   { push(@{$_[0]{a}},@_[1..$#_]) }                ## PUSH this, list
sub SHIFT     ($)    { shift(@{$_[0]{a}}) }                          ## SHIFT this
sub UNSHIFT   ($@)   { unshift(@{$_[0]->{a}},@_[1..$#_]) }           ## UNSHIFT this, list
sub SPLICE    ($$$@) { splice(@{$_[0]{a}},$_[1],$_[2],@_[3..$#_]); } ## SPLICE this,offset,length,list
#sub EXTEND    ($$)   { }                                            ## EXTEND this,count

package main;

sub setup_myarray {
  our (@ta);
  tie(@ta, 'myarray', foo=>'bar',baz=>'bonk');
  our @aa = 0..42;
  @ta = @aa;
}
sub bench_myarray {
  setup_myarray;
  cmpthese(-3,{
	       'native:foreach'=>sub { $sum=0; $sum += $_ foreach (@aa); },
	       'tied:foreach'=>sub { $sum=0; $sum += $_ foreach (@ta); },
	      });
  #print "\n";
  #cmpthese(100000,{ ##-- no discernible difference
	#       'native:scalar'=>sub { $size=scalar(@aa); },
	#       'tied:scalar'  =>sub { $size=scalar(@ta); },
	#      });
}
#bench_myarray();

#                  Rate   tied:foreach native:foreach
#tied:foreach    4476/s             --           -95%
#native:foreach 96701/s          2060%             --
#
#                   Rate   tied:scalar native:scalar
#tied:scalar    322581/s            --          -94%
#native:scalar 5000000/s         1450%            --

##----------------------------------------------------------------------
## test: memory management: scalar ref + global hash
##----------------------------------------------------------------------
package mymm_sg;
BEGIN { our %ASSOC = qw(); }
sub new {
  #my ($that,$assoc,$i) = @_;
  my $i = $_[2];
  my $obj = bless \$i, ref($_[0])||$_[0];
  $ASSOC{$obj} = $_[1];
  return $obj;
}
sub DESTROY { delete($ASSOC{$_[0]}); }

package mymm_ary;
sub new {
  #my ($that,$assoc,$i) = @_;
  return bless [@_[1,2]], ref($_[0])||$_[0];
}

package main;

sub setup_mymm {
  #our $nmm   = 42 if (!defined($nmm));
  #our $assoc = bless {assoc=>'dummy'}, 'MUDL::Asscociated';
}
sub bench_mymm_c {
  my $class = shift;
  our $nmm  = 512 if (!defined($nmm));
  our $assoc = bless  {assoc=>'dummy'}, 'MUDL::Asscociated' if (!defined($assoc));
  my ($obj);
  foreach (1..$nmm) {
    $obj = $class->new($assoc,$_);
  }
}
sub bench_mymm {
  $nmm   = 560;
  $assoc = load("$cfile.pdlbuf.bin");
  cmpthese(-3, {'scalar+global'=>sub{bench_mymm_c('mymm_sg')}, 'array'=>sub{bench_mymm_c('mymm_ary')}});
}
#bench_mymm();

#               Rate scalar+global         array
#scalar+global 107/s            --          -37%
#array         171/s           60%            --

##----------------------------------------------------------------------
## test: tie hash (not yet)
##----------------------------------------------------------------------

#sub TIEHASH  { bless {}, $_[0] }                             ## $obj = $class->TIEHASH(@args)
#sub STORE    { $_[0]->{$_[1]} = $_[2] }                      ## $val = $obj->STORE($key,$val)
#sub FETCH    { $_[0]->{$_[1]} }                              ## $val = $obj->FETCH($key)
#sub FIRSTKEY { my $a = scalar keys %{$_[0]}; each %{$_[0]} } ## $key = $obj->FIRSTKEY()
#sub NEXTKEY  { each %{$_[0]} }                               ## $key = $obj->NEXTKEY()
#sub EXISTS   { exists $_[0]->{$_[1]} }                       ## $bool = $obj->EXISTS($key)
#sub DELETE   { delete $_[0]->{$_[1]} }                       ## ? = $obj->DELETE($key)
#sub CLEAR    { %{$_[0]} = () }                               ## ? = $obj->CLEAR()
#sub SCALAR   { scalar %{$_[0]} }                             ## $sclr = $obj->SCALAR()

##----------------------------------------------------------------------
## test: MUDL::Corpus::Buffer::PackedTT;
##----------------------------------------------------------------------

use MUDL::Corpus::Buffer::PackedTT;
sub test_buf_ptt {
  our $cb = load("$cfile.cbuf.bin");
  our $pb = MUDL::Corpus::Buffer::PackedTT->new();

  our $cbr = $cb->reader;
  our $pbw = $pb->writer;

  #our $s = $cbr->getSentence;
  #$pbw->putSentence($s);
  #our $s2 = $cbr->getSentence;
  #$pbw->putSentence(bless($s2,'MUDL::Sentence::TT'));
  ##--
  $pbw->putReader($cbr);

  #$pb->packSentences(); ##-- implicit
  $pb->saveFile("$cfile.ptt.bin");

  $pb2 = load("$cfile.ptt.bin");
  #$pb2->unpackSentences();

  $pb2r = $pb2->reader;
  $s    = $pb2r->getSentence;
  $ttw->putSentence($s);

  print "done.\n";
}
#test_buf_ptt;
