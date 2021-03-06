#!/usr/bin/perl -wd

use lib qw(../ ../MUDL ../.. ../../MUDL ../../.. ../../../MUDL);
use MUDL::Make;
use MUDL::Make::Vars;
use MUDL::Make::Config;
use MUDL::Make::Collection;


##----------------------------------------------------------------------
## test: vars

sub testVars {
  our $vars = MUDL::Make::Vars->new();
  $vars->parse('test.mak');
  $vars->writeMakefile(file=>'test-tmp.mak', target=>'vars');

  our $xvars = $vars->expandMakefile('test-tmp.mak', target=>'vars', unlink=>0);

  print "Vars:\n", (map { "$_=$xvars->{$_}\n" } sort(keys(%$xvars)));
}
testVars;


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
  our @makefiles = ('Default.mak');

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
#test3;


##--------------------------------------------------------------
## test4: Make::Collection: biggish
sub test4 {
  our @makefiles = ('Default.mak');
  our $mcol = MUDL::Make::Collection->new(makefiles=>\@makefiles);
  $mcol->parse();

  our $doexpand=1;

  our @baseconfigs = ( { icorpus=>'utrain-nl.t', tcorpus=>'utest.tiny.ttt' },
		       { icorpus=>'btrain.t', tcorpus=>'btrain.btiny.ttt' },
		     );
  #our @lrwhichs = qw(hb fbg);
  our @lrwhichs = qw(hb);
  our @stages  = (1..4);
  our @emis    = (0..1);
  $| = 1;
  foreach $base (@baseconfigs) {
    foreach $lrwhich (@lrwhichs) {
      foreach $stage (@stages) {
	foreach $emi (@emis) {
	  print ".";
	  $uvars = { %$base, lrwhich=>$lrwhich,stage=>$stage,,emi=>$emi };
	  $mcol->uget($uvars,expand=>$doexpand);
	}
      }
    }
  }
  print "\n";

  $mcol->expandMissing();
  #$mcol->savePerlFile('mcol.pl');

  our @configs = $mcol->usearch('$_{stage} == 1');
  $cfg = $configs[0];
  $cfg->writeUserMakefile('-');
}
test4;

##--------------------------------------------------------------
foreach $i (1..10) {
  print "--dummy[$i]--\n";
}
