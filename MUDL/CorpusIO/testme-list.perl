#!/usr/bin/perl -w

use lib qw(../..);
use MUDL::CorpusIO;

our $crl = MUDL::CorpusReader::FileList->new();
$crl->fromFile("testme.files")
  or die("MUDL::CorpusIO::FileList::fromFile() failed: $!");

our $cw = MUDL::CorpusIO->fileWriter("tt:-")
  or die("MUDL::CorpusIO->fileWriter('tt:-') failed: $!");

while (defined($s=$crl->getSentence)) {
  $cw->putSentence($s);
}
$cw->flush();

##-- test file list
print "%%--BEGIN FILES--\n%";
$cwl = MUDL::CorpusIO->fileWriter("files:-", sep_str=>"\n%")
  or die("MUDL::CorpusIO->fileWriter('files:-') failed: $!");
$cwl->putFiles(@{$crl->{files}});
$cwl->flush();
print "%--END FILES--\n";


