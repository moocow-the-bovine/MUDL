#!/usr/bin/perl -wd

use lib '../..';
use MUDL::Morph::Editor;

sub main {
  $me = MUDL::Morph::Editor->new();
  $me->view(loop=>0);
  $me->loadCorpus('test.t');
  $me->selectWord(1);
  Tk::MainLoop;
}
main;


