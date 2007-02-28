#!/usr/bin/perl -wd

use lib qw(..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use Benchmark qw(cmpthese timethese);

#use MUDL::Corpus::Buffer::Pdl;
#use MUDL::Cluster::Brown;

use MUDL::SVD;
use MUDL::PdlDist;
use MUDL::PdlDist::Sparse2d;

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
## Sparse 2d Pdl Dists

sub test_sparse2d {
  $a   = pdl(double,[[1,0,0,2],[0,3,4,0],[5,0,0,6]]);

  ##-- raw
  $ap  = MUDL::PdlDist->new(pdl=>pdl($a));
  $aps = MUDL::PdlDist::Sparse2d->new(dense=>pdl($a));
  print "raw: ", (all($ap->{pdl} == $aps->densePdl) ? "ok" : "NOT ok"), "\n";

  ##-- normalize
  $ap_norm  = $ap->clone->normalize();
  $aps_norm = $aps->clone->normalize();
  print "norm: ", (all($ap_norm->{pdl} == $aps_norm->densePdl) ? "ok" : "NOT ok"), "\n";

  ##-- conditionalize : p(d1|d0)
  $ap_g0  = $ap->clone->conditionalize([0]);
  $aps_g0 = $aps->clone->conditionalize([0]);
  print "cond(D1|D0): ", (all($ap_g0->{pdl} == $aps_g0->densePdl) ? "ok" : "NOT ok"), "\n";

  ##-- conditionalize : p(d0|d1)
  $ap_g1  = $ap->clone->conditionalize([1]);
  $aps_g1 = $aps->clone->conditionalize([1]);
  print "cond(D0|D1): ", (all($ap_g1->{pdl} == $aps_g1->densePdl) ? "ok" : "NOT ok"), "\n";

  ##-- to EDist
  $ap_ed  = $ap->toEDist();
  $aps_ed = $aps->toEDist();
  print "toEDist(): ", ($ap_ed->saveString eq $aps_ed->saveString ? "ok" : "NOT ok"), "\n";

  ##-- from EDist: Enum
  $e0   = MUDL::Enum->new();
  $e1   = MUDL::Enum->new();
  $enum = MUDL::Enum::Nary->new(nfields=>2,enums=>[$e0,$e1]);
  $e0->addIndexedSymbol($_,$_) foreach (0..($a->dim(0)-1));
  $e1->addIndexedSymbol($_,$_) foreach (0..($a->dim(1)-1));

  $ap_ed->{enum} = $aps_ed->{enum} = $enum;
  $ap_edp  = $ap_ed->toPdlDist();
  $aps_edp = $aps_ed->toPdlDistSparse();
  print "EDist->toPdlDist(): ", (all($ap_edp->{pdl}==$aps_edp->densePdl) ? "ok" : "NOT ok"), "\n";
}
test_sparse2d();

##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#ltest1;
foreach $i (0..3) {
  print "--dummy[$i]--\n";
}
