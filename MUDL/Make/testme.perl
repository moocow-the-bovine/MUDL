#!/usr/bin/perl -wd

use lib '../..';
use MUDL::Make;
use MUDL::Make::Vars;

##--------------------------------------------------------------
## test: perl i/o on shared & self-referential structures
sub testr {
  our $h = {name=>'foo'};
  our $obj1 = MUDL::Object->new(foo=>$h,bar=>$h);
  $obj1->{self} = $obj1;
  our $str1 = $obj1->savePerlString;

  our $obj2 = ref($obj1)->loadPerlString($str1);
  our $str2 = $obj2->savePerlString;
}
#testr;

##--------------------------------------------------------------
## test: Make::Vars: I/O
sub test1 {
  $vars = MUDL::Make::Vars->new();
  $vars->parse('test.mak');
  our $str   = $vars->savePerlString();
  our $vars2 = ref($vars)->loadPerlString($str);
}
#test1;


##--------------------------------------------------------------
## test2: Make::Vars: parse(), parse_p()
sub test2 {
  our $vars = MUDL::Make::Vars->new();
  $vars->parse('test.mak');

  our $str = $vars->saveNativeString();
  our $vars2 = ref($vars)->loadNativeString($str);

  our $varsp = MUDL::Make::Vars->new();
  $varsp->parse_p('test.mak');

  our $xvars = $vars->expand();
  our $xvarsp = $varsp->expand();
}
test2;


##--------------------------------------------------------------
foreach $i (1..10) {
  print "--dummy[$i]--\n";
}
