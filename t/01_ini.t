# -*- Mode: Perl -*-
# t/01_ini.t; just to load module(s) by using it (them)

$TEST_DIR = './t';
#use lib qw(../blib/lib); $TEST_DIR = '.'; # for debugging

# load common subs
do "$TEST_DIR/common.plt";

@classes = qw(MUDL);
plan(test => scalar(@classes));

# 1--N: load subclasses (1 subtest/class)
foreach $class (@classes) {
  print "Test 'use $class;'\n";
  ok(eval("require $class;"));
}

print "\n";
# end of t/01_ini.t

