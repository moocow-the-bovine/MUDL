#!/usr/bin/perl -wd

use lib qw(.);
use MUDL;
use MUDL::Make;

sub test1 {
  our $mak = MUDL::Make->new();
  our $colfile = 'test.pl.gz';
  $mak->loadCollection($colfile);
}
test1;


foreach $i (0..100) {
  print STDERR "--dummy($i)--\n";
}
