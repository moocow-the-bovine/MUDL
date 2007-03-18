#!/usr/bin/perl -w

use lib qw(..);
use MUDL;
use MUDL::CorpusIO;
use MUDL::Corpus::Buffer::PdlTT;

BEGIN {
  our $cbase = "de-negra.tiny";
  our $cfile = "$cbase.ttt";
}

our $pdlb = MUDL::Corpus::Buffer::PdlTT->loadFile("$cfile.pdltt.bin");
$pdlb->unpackPdls();

