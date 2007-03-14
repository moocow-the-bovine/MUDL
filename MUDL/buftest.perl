#!/usr/bin/perl -wd

use lib qw(..);
use MUDL;
use MUDL::CmdUtils;
use MUDL::CorpusIO;
use MUDL::Token;
use PDL;
use Benchmark qw(cmpthese timethese);

#BEGIN { $, = ' '; }

##======================================================================
## Buffer creation
##======================================================================
BEGIN {
  our $cbase = "buftest";
  our $cfile = "${cbase}.ttt";
  our @exts  = (qw(ttt ttt.bin ttt.buf.bin ttt.ebuf.bin ttt.ebuftt.bin ttt.pdlbuf.bin),
		#qw(ttt.pbuf2.bin);
	       );
  our $ttw   = MUDL::CorpusIO->fileWriter("tt:-");
}


use MUDL::Corpus::Buffer;
sub create_corpus_buffer {
  our $cb = MUDL::Corpus::Buffer->fromFile($cfile);
  $cb->saveFile("$cfile.buf.bin");
}

use MUDL::Corpus::EBuffer;
sub create_corpus_ebuffer {
  our $cb  = load("$cfile.buf.bin");
  our $ebw = MUDL::CorpusIO->fileWriter("$cfile.ebuf.bin");
  our $eb = $ebw->{buffer};
  $ebw->putReader($cb->reader);
  $ebw->flush;
}
#create_corpus_ebuffer();

use MUDL::Corpus::EBuffer;
sub create_corpus_ebuffer_tt {
  our $cb  = load("$cfile.buf.bin");
  our $ebw = MUDL::CorpusIO->fileWriter("$cfile.ebuftt.bin");
  our $eb = $ebw->{buffer};
  $ebw->putReader($cb->reader);
  $ebw->flush;
}
#create_corpus_ebuffer_tt();

use MUDL::Corpus::Buffer::PdlFull;
sub create_corpus_buffer_pdlfull {
  our $cb = load("$cfile.buf.bin");
  our $pbw = MUDL::CorpusIO->fileWriter("$cfile.pdlbuf.bin");
  our $pb = $pbw->{buffer};
  #@$pb{qw(sget tget)} = qw(32 64);
  #$s = $cb->{sents}[0];
  #$pbw->putSentence($s);
  $pbw->putReader($cbr=$cb->reader);
  $pbw->flush;
}
#create_corpus_buffer_pdlfull();

use MUDL::CorpusIO::TT::Bin;
sub create_corpus_binio {
  our $cb = load("$cfile.buf.bin");
  our $bw = MUDL::CorpusIO->fileWriter("$cfile.bin");
  $bw->putReader($cb->reader);
  $bw->flush;
}

##--------------------------
## create: all
sub create_all {
  create_corpus_buffer();
  create_corpus_ebuffer();
  create_corpus_buffer_pdl2();
  create_corpus_binio();
}
#create_all();

##----------------------------------------------------------------------
## Bench: File sizes
##----------------------------------------------------------------------

BEGIN {
  our %files = map { ($_=>"${cbase}.$_") } @exts;
}

sub bench_filesize {
  my $extlen=0;
  foreach (@exts) { $extlen=length($_) if (length($_) > $extlen); }
  my %ext2size = map { ($_=>(-s $files{$_})) } keys(%files);

  my $refsize   = (-s $cfile);
  my %ext2delta = qw();
  my ($ext,$size);
  while (($ext,$size)=each(%ext2size)) {
    if ($refsize < $size) {
      $ext2delta{$ext}=sprintf("+%6.2f%% / -%6.2f%%",
			       100.0*($size-$refsize)/$refsize, 100.0*($size-$refsize)/$size);
    } else {
      $ext2delta{$ext}=sprintf("+%6.2f%% / -%6.2f%%",
			       100.0*($refsize-$size)/$refsize, 100.0*($refsize-$size)/$size);
    }
  }

  print map {
    sprintf("%-${extlen}s  %7d  (%7s)\n", $_, $ext2size{$_}||0, $ext2delta{$_}||0)
  } sort { $ext2size{$a}<=>$ext2size{$b} } keys(%ext2size)
}
#bench_filesize;

#-- Using fully-stored Enums:
#ttt              152479  (+  0.00%)
#ttt.bin          173041  (- 13.49%)
#ttt.pbuf2.bin    223284  (- 46.44%) ##-- ???
#ttt.buf.bin      260348  (- 70.74%)
#
#-- Using trimmed Enums (via STORABLE_freeze, STORABLE_thaw):
#ttt              152479  (+  0.00% / -  0.00%)
#ttt.pdlbuf.bin   155421  (+  1.93% / -  1.89%)
#ttt.bin          173041  (+ 13.49% / - 11.88%)
#ttt.ebuftt.bin   202741  (+ 32.96% / - 24.79%)
#ttt.buf.bin      260348  (+ 70.74% / - 41.43%)
#ttt.ebuf.bin     419900  (+175.38% / - 63.69%)


##----------------------------------------------------------------------
## Bench: Subs: Load
##----------------------------------------------------------------------

BEGIN {
  our %load_subs =
    (
     #'ttt'     => sub { return MUDL::CorpusIO->fileReader("$cfile"); },
     #'ttt.bin' => sub { return MUDL::CorpusIO->fileReader("$cfile.bin"); },
     'ttt.buf.bin' => sub { return MUDL::Corpus::Buffer->loadFile("$cfile.buf.bin"); },
     #'ttt.pbuf2.bin' => sub { return MUDL::Corpus::Buffer::Pdl2->loadFile("$cfile.pbuf2.bin"); },
    );
}

sub bench_load {
  cmpthese(4,\%load_subs);
}
#bench_load;

##----------------------------------------------------------------------
## Test: Buffer::PdlFull: Reader
##----------------------------------------------------------------------

sub test_pdlfull_reader {
  $pbr = MUDL::CorpusIO->fileReader("$cfile.pdlbuf.bin");
  $pb  = $pbr->{buffer};
  $s   = $pbr->getSentence;
  $ttw->putSentence($s);
}
#test_pdlfull_reader();

##----------------------------------------------------------------------
## Bench: Data: get reader
##----------------------------------------------------------------------

BEGIN {
  our %reader_subs =
    (
     'ttt'     => sub { return MUDL::CorpusIO->fileReader("$cfile"); },
     'ttt.bin' => sub { return MUDL::CorpusIO->fileReader("$cfile.bin"); },
     'ttt.buf.bin' => sub { return MUDL::Corpus::Buffer->loadFile("$cfile.buf.bin")->reader; },
     #'ttt.pbuf2.bin' => sub { return MUDL::Corpus::Buffer::Pdl2->loadFile("$cfile.pbuf2.bin")->reader; },
     'ttt.ebuf.bin'  => sub { return MUDL::CorpusIO->fileReader("$cfile.ebuf.bin"); },
     'ttt.pdlbuf.bin'  => sub { return MUDL::CorpusIO->fileReader("$cfile.pdlbuf.bin"); },
    );
}


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
  cmpthese(4, \%count_subs);
}
#bench_getSentence();
#
#-- Trimmed Enums
#                 Rate       ttt ttt.pdlbuf.bin  ttt.bin ttt.ebuf.bin ttt.buf.bin
#ttt            1.25/s        --           -35%     -62%         -84%        -93%
#ttt.pdlbuf.bin 1.93/s       55%             --     -41%         -75%        -89%
#ttt.bin        3.25/s      160%            68%       --         -58%        -82%
#ttt.ebuf.bin   7.69/s      515%           298%     137%           --        -58%
#ttt.buf.bin    18.2/s     1355%           841%     459%         136%          --


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
  cmpthese(2,\%get_text_subs);
}
#bench_get_text();
#
#                 Rate       ttt ttt.pdlbuf.bin  ttt.bin ttt.ebuf.bin ttt.buf.bin
#ttt            1.20/s        --           -22%     -52%         -69%        -83%
#ttt.pdlbuf.bin 1.55/s       29%             --     -38%         -60%        -78%
#ttt.bin        2.50/s      108%            61%       --         -36%        -65%
#ttt.ebuf.bin   3.92/s      225%           153%      57%           --        -45%
#ttt.buf.bin    7.14/s      493%           361%     186%          82%          --

##----------------------------------------------------------------------
## Bench: Subs: Process: get whole corpus as a single PDL (text only)
##----------------------------------------------------------------------

## $nsents = bench_get_full_pdls_reader($reader)
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

sub bench_get_full_pdl_pdlbuffer {
  my $buf = shift;
  return ($buf->{enums}{text},$buf->{soffsets},$buf->{pdls}{text});
}
#bench_get_full_pdl_pdlbuffer(load("$cfile.pdlbuf.bin"));

BEGIN {
  our %get_full_pdl_subs =
    (
     'ttt'     => sub { bench_get_full_pdl_reader(MUDL::CorpusIO->fileReader("$cfile")); },
     'ttt.bin' => sub { bench_get_full_pdl_reader(MUDL::CorpusIO->fileReader("$cfile.bin")); },
     'ttt.buf.bin' => sub { bench_get_full_pdl_buffer(load("$cfile.buf.bin")); },
     'ttt.ebuf.bin'  => sub { bench_get_full_pdl_ebuffer(load("$cfile.ebuf.bin")); },
     'ttt.pdlbuf.bin'  => sub { bench_get_full_pdl_pdlbuffer(load("$cfile.pdlbuf.bin")); },
    );
}

sub bench_get_full_pdl {
  cmpthese(4,\%get_full_pdl_subs);
}
#bench_get_full_pdl();

#                  Rate      ttt  ttt.bin ttt.buf.bin ttt.ebuf.bin ttt.pdlbuf.bin
#ttt            0.710/s       --     -31%        -48%         -94%           -99%
#ttt.bin         1.02/s      44%       --        -25%         -92%           -98%
#ttt.buf.bin     1.36/s      91%      33%          --         -89%           -98%
#ttt.ebuf.bin    12.5/s    1659%    1122%        822%           --           -81%
#ttt.pdlbuf.bin  66.7/s    9283%    6417%       4817%         433%             --


##----------------------------------------------------------------------
## Bench: Subs: Process: get corpus as a list of sentence PDLs (text only)
##----------------------------------------------------------------------

## $nsents = bench_get_sent_pdls_reader($reader)
##  + benchmarking sub
sub bench_get_sent_pdls_reader {
  my $cr = shift;
  my $enum = MUDL::Enum->new;
  my $sents = [];
  my ($s);
  push(@$sents,$s) while (defined($s=$cr->getSentence));
  my $psents = [];
  foreach $s (@$sents) {
    push(@$psents, pdl(long,[map {$enum->addSymbol($_->text)} @$s]));
  }
  return ($enum,$psents);
}
#bench_get_sent_pdls_reader(MUDL::CorpusIO->fileReader("$cfile"));

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

BEGIN {
  our %get_sent_pdls_subs =
    (
     'ttt'     => sub { bench_get_sent_pdls_reader(MUDL::CorpusIO->fileReader("$cfile")); },
     'ttt.bin' => sub { bench_get_sent_pdls_reader(MUDL::CorpusIO->fileReader("$cfile.bin")); },
     'ttt.buf.bin' => sub { bench_sent_pdls_buffer(load("$cfile.cbuf.bin")); },
     'ttt.ebuf.bin'  => sub { bench_get_sent_pdls_ebuffer(load("$cfile.ebuf.bin")); },
     'ttt.pdlbuf.bin' => sub { bench_get_sent_pdls_pdlbuffer(load("$cfile.pdlbuf.bin")); },
    );
}

sub bench_get_sent_pdls {
  cmpthese(2,\%get_full_pdl_subs);
}
#bench_get_sent_pdls();

#                  Rate      ttt  ttt.bin ttt.buf.bin ttt.ebuf.bin ttt.pdlbuf.bin
#ttt            0.706/s       --     -30%        -49%         -94%           -99%
#ttt.bin         1.01/s      44%       --        -26%         -91%           -98%
#ttt.buf.bin     1.38/s      95%      36%          --         -88%           -98%
#ttt.ebuf.bin    11.5/s    1535%    1038%        738%           --           -81%
#ttt.pdlbuf.bin  60.0/s    8400%    5820%       4260%         420%             --



##----------------------------------------------------------------------
## Bench: Subs: output: plain sentence
##----------------------------------------------------------------------

## bench_putreader($writer)
sub bench_putreader {
  my $cw = shift;
  our ($_cbr);
  $_cbr->reset();
  $cw->putReader($_cbr);
  $cw->flush();
}

BEGIN {
  our $_cbr = MUDL::CorpusIO->fileReader("$cfile.cbuf.bin");
  our %bench_putreader_subs =
    (
     'ttt'     => sub { bench_putreader(MUDL::CorpusIO->fileWriter("$cfile.tmp.ttt")); },
     'ttt.bin' => sub { bench_putreader(MUDL::CorpusIO->fileWriter("$cfile.tmp.ttt.bin")); },
     'ttt.cbuf.bin' => sub { bench_putreader(MUDL::CorpusIO->fileWriter("$cfile.tmp.cbuf.bin")); },
     'ttt.ebuf.bin'  => sub { bench_putreader(MUDL::CorpusIO->fileWriter("$cfile.tmp.ebuf.bin")); },
     'ttt.pdlbuf.bin'  => sub { bench_putreader(MUDL::CorpusIO->fileWriter("$cfile.tmp.pdlbuf.bin")); },
    );
}

sub bench_writer_putreader {
  cmpthese(2,\%bench_putreader_subs);
}
#bench_writer_putreader();

#                  Rate ttt.pdlbuf.bin ttt.ebuf.bin      ttt ttt.bin ttt.cbuf.bin
#ttt.pdlbuf.bin 0.326/s             --         -65%     -77%    -88%         -99%
#ttt.ebuf.bin   0.930/s           186%           --     -34%    -66%         -96%
#ttt             1.42/s           335%          52%       --    -48%         -94%
#ttt.bin         2.74/s           741%         195%      93%      --         -89%
#ttt.cbuf.bin    25.0/s          7575%        2588%    1663%    813%           --

##----------------------------------------------------------------------
## Bench: Subs: output: enumerated sentences
##----------------------------------------------------------------------

## bench_putreader_e($writer)
sub bench_putreader_e {
  my $cw = shift;
  our ($_ebr);
  $_ebr->reset();
  $cw->putReader($_ebr);
  $cw->flush();
}

BEGIN {
  our $_ebr     = MUDL::CorpusIO->fileReader("$cfile.ebuf.bin");
  our $_ebenums = $_ebr->{buffer}{enums};
  our %bench_putreader_e_subs =
    (
     'ttt'     => sub { bench_putreader_e(MUDL::CorpusIO->fileWriter("$cfile.tmp.ttt")); },
     'ttt.bin' => sub { bench_putreader_e(MUDL::CorpusIO->fileWriter("$cfile.tmp.ttt.bin")); },
     'ttt.cbuf.bin' => sub { bench_putreader_e(MUDL::CorpusIO->fileWriter("$cfile.tmp.cbuf.bin")); },
     'ttt.ebuf.bin' => sub {
       bench_putreader_e(MUDL::CorpusIO->fileWriter("$cfile.tmp.ebuf.bin",
						    buffer=>MUDL::Corpus::EBuffer->new(enums=>$_ebenums)))
     },
     'ttt.pdlbuf.bin'  => sub {
       bench_putreader_e(MUDL::CorpusIO->fileWriter("$cfile.tmp.pdlbuf.bin",
						    buffer=>MUDL::Corpus::Buffer::PdlFull->new(enums=>$_ebenums)))
     },
    );
}

sub bench_writer_putreader_e {
  cmpthese(2,\%bench_putreader_e_subs);
}
#bench_writer_putreader_e();

#                  Rate ttt.pdlbuf.bin ttt.bin      ttt ttt.ebuf.bin ttt.cbuf.bin
#ttt.pdlbuf.bin 0.483/s             --    -53%     -55%         -69%         -95%
#ttt.bin         1.03/s           112%      --      -4%         -35%         -89%
#ttt             1.06/s           120%      4%       --         -32%         -88%
#ttt.ebuf.bin    1.57/s           226%     54%      48%           --         -83%
#ttt.cbuf.bin    9.09/s          1782%    786%     755%         477%           --




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
## Dummy
##----------------------------------------------------------------------

foreach $i (0..3) {
  print "--dummy[$i]--\n";
}
