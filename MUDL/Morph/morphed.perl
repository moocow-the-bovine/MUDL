#!/usr/bin/perl -w

use lib '../..';
use MUDL::Morph::Editor;
use MUDL::Morph::Editor::Gtk;

sub main {
  Gtk->init;
  our $me  = MUDL::Morph::Editor->new();
  our $gui = MUDL::Morph::Editor::Gtk->new(data=>$me);
  Gtk->main;
}
main;


