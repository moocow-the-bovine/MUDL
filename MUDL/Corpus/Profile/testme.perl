#!/usr/bin/perl -wd

use lib qw(../../..);
use MUDL;
use MUDL::CmdUtils;
use MUDL::Corpus::Profile::LRMI;
use MUDL::PdlDist;
use PDL;

BEGIN { $,=' '; }

sub test1 {
  $et   = MUDL::Enum->new;
  $eb   = MUDL::Enum->new;
  $et->addSymbol($_) foreach qw(a b c);
  $eb->addSymbol($_) foreach qw(a b c __$);

  $lrmi = MUDL::Corpus::Profile::LRMI->new(bounds=>$eb,targets=>$et, donorm=>0);
  %{$lrmi->{left}{nz}}  = ("0\t1"=>10,"0\t2"=>15,"1\t2"=>20,"1\t3"=>40);
  %{$lrmi->{right}{nz}} = ("1\t0"=>10,"2\t1"=>20,);

  return $lrmi;
}
