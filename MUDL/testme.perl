#!/usr/bin/perl -wd

use lib qw(..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use Benchmark qw(cmpthese timethese);

use MUDL::Corpus::Buffer::Pdl;

BEGIN { $, = ' '; }

##----------------------------------------------------------------------
## Corpus methods / mem usage:
##
## + input=.t, 18533 sents , 266603 toks (avg=14.39 tok/sent)
##   - MUDL::Corpus (array of arrays of MUDL::Token::TT)
##     * 64200 / 56172
##   - MUDL::Enum population
##     * 31996 / 23360
##   - hack: Enum + array of pdls (long)
##     * 37740 / 29192
##   - hack: Enum + array of pack('I*', ...) strings
##     * 33696 / 25120
##   - hack: single pack('I*',...) string + array of bos-indices
##     * 33656 / 25052
##   - hack: single pack('I*',...) string + pack('I*',...) string bos-indices
##     * 33336 / 24748

##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#ltest1;
foreach $i (0..100) {
  print "--dummy[$i]--\n";
}
