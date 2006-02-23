#!/usr/bin/perl -w

use lib '../..';
use MUDL::Morph::Editor;
use MUDL::Morph::Editor::Gtk2;

sub main {
  Gtk2->init;
  our $me  = MUDL::Morph::Editor->new();
  our $gui = MUDL::Morph::Editor::Gtk2->new(data=>$me);

  #debug_select_word();

  Gtk2->main;
}
main;


sub debug_select_word {
  $gui->{data} = $gui->loadFile('00-utest-tiny+morph.bin');
  $gui->populateWordList;
  $gui->select_word();
}

