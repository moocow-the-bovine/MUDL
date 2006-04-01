#!/usr/bin/perl -wd

use lib '../..';
use MUDL::Make;
use MUDL::Make::Vars;
use MUDL::Make::Config;
use MUDL::Make::Collection;

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
#test2;

##--------------------------------------------------------------
## test3: Make::Collection
sub test3 {
  #our @makefiles = qw(test.mak);
  our @makefiles = qw(Config.mak);

  our $mcol = MUDL::Make::Collection->new(makefiles=>\@makefiles);
  $mcol->parse();
  #$mcol->parse(makefiles=>[qw(test.mak)]);
  #$mcol->parse(makefiles=>[qw(test.mak test2.mak)]);

  #our $uvars1 = { baz=>'baz1', bonk=>'bonk1', };
  #our $uvars2 = { baz=>'baz2', bonk=>'bonk2', };
  ##--
  our $uvars1 = { lrwhich=>'hb', tccd=>'x+bb', };
  our $uvars2 = { lrwhich=>'fbg', tccd=>'x+bb', };

  our $doexpand = 1;
  our $cfg1 = $mcol->uget($uvars1, expand=>$doexpand);
  our $cfg2 = $mcol->uget($uvars2, expand=>$doexpand);

  ##-- test find()
  our $cfg1f = $mcol->ufind($uvars1);
  our $cfg2f = $mcol->ufind($uvars2);
}
test3;



##--------------------------------------------------------------
foreach $i (1..10) {
  print "--dummy[$i]--\n";
}
