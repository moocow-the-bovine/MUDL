#!/usr/bin/perl -wd

use lib qw(..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use Benchmark qw(cmpthese timethese);

#use MUDL::Corpus::Buffer::Pdl;
#use MUDL::Cluster::Brown;

use MUDL::SVD;

BEGIN { $, = ' '; }

##----------------------------------------------------------------------
## brown clustering
sub btest1 {
  require MUDL::Cluster::Brown;
  require MUDL::Bigrams;

  $bgs = load('bg200.bin');
  $mic = MUDL::Cluster::Brown->new();
  $mic->initialize($bgs);
  $mic->saveFile('mic0+bs.bin');
}
sub btest1b {
  require MUDL::Cluster::Brown;
  require MUDL::Bigrams;
  $mic = load('mic0+bs.bin');
  $bgs = load('bg200.bin');
}
sub btest2 {
  ($i,$j,$L)=$mic->findBestMerge;
  print "   i=$i ($mic->{enum}{id2sym}[$i]) ; j=$j ($mic->{enum}{id2sym}[$j]) ; L=$L\n";
  $mic->mergePair($i,$j); ##-- bad: iter1: i=152(samstag), j=194(sonntag), m=57(am) [OK]
  ++$mic->{k};
}
#btest1b; btest2; btest2;
#btest1; btest2; btest2;
#if (defined($mic)) { btest1b; } else { btest1; } btest2; btest2;


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
