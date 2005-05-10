#!/usr/bin/perl -w

use lib qw(..);
use MUDL;
use MUDL::Cluster::Tree;
use MUDL::CmdUtils;

use PDL;
use Inline Pdlpp => Config =>
  CLEAN_AFTER_BUILD => 0,
  BUILD_NOISY => 1,
  NOISY => 1,
  CCFLAGS => '-mcpu=athlon-xp',
  ;
use Inline Pdlpp;
#use Inline 'NOISY';
#use Inline Config => DISABLE => CLEAN_AFTER_BUILD;
#use Inline Pdlpp => Config => 'NOISY' => 1, 'BUILD_NOISY' => 1;
#use Inline Pdlpp => Config => 'NOISY' => 1;
#use Inline Pdlpp => Config => 'NOISY' => 1;
#use Inline Pdlpp => Config => 'BUILD_NOISY';
#use Inline Pdlpp => Config => 'BUILD_NOISY' => 1;
#use Inline Pdlpp => 'Config'  => 'CCFLAGS' => '-mcpu=athlon-xp';

#Inline->init;


#require PDL::IO::Storable; ##-- must this load AFTER everything else PDL-related?

BEGIN { $, = ' '; }

#test();
$p = zeroes(4);
print "p = $p ; incr(p) = ", $p->incr, "\n";

##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#foreach $i (0..100) {
#  print "--dummy[$i]--\n";
#}


__DATA__

__Pdlpp__

pp_def('incr',
       Pars => 'i(); [o] o()',
       Code => '$o() = $i() + 1;',
      );
