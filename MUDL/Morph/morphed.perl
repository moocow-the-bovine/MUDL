#!/usr/bin/perl -w

use lib '../..';
use MUDL::Morph::Editor;
use MUDL::Morph::Editor::Gtk2;

sub main {
  Gtk2->init;
  our $me  = MUDL::Morph::Editor->new();
  our $gui = MUDL::Morph::Editor::Gtk2->new(data=>$me);
  Gtk2->main;
}
main;


