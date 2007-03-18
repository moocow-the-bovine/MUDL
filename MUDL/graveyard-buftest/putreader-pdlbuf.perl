#!/usr/bin/perl -w
use lib qw(..);
use MUDL;
use MUDL::CorpusIO;

BEGIN {
  our $cbase = "buftest";
  our $cfile = "$cbase.ttt";
  our $_cbr = MUDL::CorpusIO->fileReader("$cfile.cbuf.bin");
}

sub bench_putreader {
  my $cw = shift;
  our ($_cbr);
  $_cbr->reset();
  $cw->putReader($_cbr);
  $cw->flush();
}

use MUDL::Corpus::Buffer::PdlFull;
bench_putreader(MUDL::CorpusIO->fileWriter("$cfile.tmp.pdlbuf.bin"));

#use MUDL::Corpus::Buffer::Packed;
#bench_putreader(MUDL::CorpusIO->fileWriter
#		("$cfile.tmp.packed.bin",
#		 buffer=>MUDL::Corpus::Buffer::Packed->new(enums=>{map{$_=>MUDL::Enum->new} qw(text tag 0)})));

