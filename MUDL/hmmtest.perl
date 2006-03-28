#!/usr/bin/perl -wd

use lib qw(..);
use MUDL;
use MUDL::CmdUtils;
use MUDL::HMM;
use PDL;
use PDL::HMM;
use MUDL::Corpus::Buffer::Pdl;

BEGIN { $, = ' '; }

##----------------------------------------------------------------------
## Memory usage:
##
##  + pdl B [double]      ($N=100,$K=100000       )  : rss= 95904  size=103868
##  + pdl B [float]       ($N=100,$K=100000       )  : rss= 56844  size= 64812
##  + hash {$k=>{$n=>$f}} ($N=100,$K=100000,$npk=4)  : rss= 46412  size= 55584
##  + hash {$k=>{$n=>$f}} ($N=100,$K=100000,$npk=8)  : rss= 57276  size= 66376
##  + hash {$k=>{$n=>$f}} ($N=100,$K=100000,$npk=16) : rss= 83004  size= 92112
##  + hash {$k=>{$n=>$f}} ($N=100,$K=100000,$npk=32) : rss=169988  size=179096

##----------------------------------------------------------------------
## load test
##----------------------------------------------------------------------
sub ltest1 {
  my $hmm = MUDL::HMM->new(type=>float);
  $hmm->loadModelFile('test.model');
}

##----------------------------------------------------------------------
## Test-series 1
##----------------------------------------------------------------------
use vars qw($hmm $oenum $qenum $N $M $a $a1 $b $b1 $pi $bos $eos $type);
sub test1 {
  $N0 = 2 if (!defined($N0));
  $M0 = 3 if (!defined($M0));
  $bos = $eos = '__$';
  $type = float if (!defined($type));
  $smooth = 'hapax' if (!defined($smooth));

  $oenum = MUDL::Enum->new();
  #$oenum->addSymbol($bos);
  $oenum->addSymbol(chr($_+97)) foreach (0..($M0-1));

  $qenum = MUDL::Enum->new();
  #$qenum->addSymbol($bos);
  $qenum->addSymbol(chr($_+65)) foreach (0..($N0-1));

  $hmm = MUDL::HMM->new(oenum=>$oenum,qenum=>$qenum,bos=>$bos,eos=>$eos,type=>$type);

  ($N,$M,$a,$a1,$b,$b1,$pi) = @$hmm{qw(N M a a1 b b1 pi)};

  use vars qw($ap $a1p $bp $b1p $pip);
  $ap=exp($a); $a1p=exp($a1); $bp=exp($b); $b1p=exp($b1); $pip=exp($pi);
}

sub test2 {
  ##-- SETUP: A() [from Pdl]
  use vars qw($ap $bp $alp $blp);
  $af = sequence(double, $a->dims);
  $ap = $af / $af->xchg(0,1)->sumover;
  $alp = log($ap);

  ##-- SETUP: B() [from Pdl]
  $bf = zeroes(double, $b->dims);
  $bf->set(0,0,1);
  $bf->slice("1:".($bf->dim(0)-1).",1:".($bf->dim(1)-1)) .= sequence($bf->dim(0)-1, $bf->dim(1)-1)+1;
  $bp = $bf / $bf->xchg(0,1)->sumover;
  $blp = log($bp);

  $hmm->compilePdls($af,$bf, $pif=ones($af->dim(0)), $omegaf=ones($af->dim(0)));

  ($N,$M,$a,$a1,$b,$b1,$pi,$omega) = @$hmm{qw(N M a a1 b b1 pi omega)};
  $ap=exp($a); $a1p=exp($a1); $bp=exp($b); $pip=exp($pi); $omegap=exp($omega);
}

sub test3 {
  ##-- SETUP: A() [from Pdl]
  use vars qw($ad $bd);

  our $ad = MUDL::Dist::Nary->new();
  our $bd = MUDL::Dist::Nary->new();

  %{$ad->{nz}} = ("$bos\tA"=>12, "$bos\tB"=>8,                ##-- f(bos->)=20
		  "A\tA"=>15,    "A\tB"=>10,   "A\t$eos"=>5,  ##-- f(A->)=30
		  "B\tA"=>3,     "B\tB"=>7,    "B\t$eos"=>15, ##-- f(B->)=25
		  ##                                          ##   sum(.->)=75
		  ##f(->A)=30    f(->B)=25     f(->eos)=20    ##   sum(->.)=75
		 );

  %{$bd->{nz}} = ("A\ta"=>15, "A\tb"=>0, "A\tc"=>4,  ## f(A@)=19
		  "B\ta"=>10, "B\tb"=>5, "B\tc"=>6,  ## f(B@)=21
		  ##                                 ## sum(.@)=40
		  ##f(@a)=25   f(@b)=5   f(@c)=10    ## sum(@.)=40
		 );

  ##-- separate
  our $ad0 = $ad->copy;
  our ($pid,$omegad);
  ($ad,$pid,$omegad) = $hmm->separateArcDist($ad);

  ##-- test ground
  $aed = $ad->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$qenum,$qenum]));
  $bed = $bd->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$qenum,$oenum]));
  $pied = $pid->toEDist($qenum);
  $omegaed = $omegad->toEDist($qenum);

  $af = $aed->toPDL;
  $bf = $bed->toPDL;
  $pif = $pied->toPDL;
  $omegaf = $omegaed->toPDL;

  $hmm->compileDists($ad,$bd,$pid,$omegad);

  ($N,$M,$a,$a1,$b,$b1,$pi,$omega) = @$hmm{qw(N M a a1 b b1 pi omega)};
  $ap=exp($a); $a1p=exp($a1); $bp=exp($b); $pip=exp($pi); $omegap=exp($omega);
}

##----------------------------------------------------------------------
## Restriction test
##----------------------------------------------------------------------
sub test3q {
  ##-- initialize
  test1;
  $qenum->addSymbol('C');
  $oenum->addSymbol('c');

  our $ad = MUDL::Dist::Nary->new();
  our $bd = MUDL::Dist::Nary->new();

  %{$ad->{nz}} = ("$bos\tA"=>12, "$bos\tB"=>8, "$bos\tC"=>1,             ##-- f(bos->)=21
		  "A\tA"=>15,    "A\tB"=>10,   "A\tC"=>1, "A\t$eos"=>5,  ##-- f(A->)=31
		  "B\tA"=>3,     "B\tB"=>7,    "B\tC"=>1, "B\t$eos"=>15, ##-- f(B->)=26
		  "C\tA"=>1,     "C\tB"=>1,    "C\tC"=>1, "C\t$eos"=>1,  ##-- f(C->)=4
		  ##                                                     ##   sum(.->)=82
		  ##f(->A)=31    f(->B)=26     f(->C)=4,  f(->eos)=21    ##   sum(->.)=82
		 );

  %{$bd->{nz}} = ("A\ta"=>15, "A\tb"=>0, "A\tc"=>1,  ## f(A@)=
		  "B\ta"=> 0, "B\tb"=>5, "B\tc"=>2,  ## f(B@)=
		  "C\ta"=> 0, "C\tb"=>0, "C\tc"=>3,  ## f(C@)=
		  ##                                 ## sum(.@)=
		  ##f(@a)=      f(@b)=     f(@c)=    ## sum(@.)=
		 );

  ##-- separate
  our $ad0 = $ad->copy;
  our ($pid,$omegad);
  ($ad,$pid,$omegad) = $hmm->separateArcDist($ad);

  ##-- test ground
  our $aed = $ad->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$qenum,$qenum]));
  our $bed = $bd->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$qenum,$oenum]));
  our $pied = $pid->toEDist($qenum);
  our $omegaed = $omegad->toEDist($qenum);

  our $af = $aed->toPDL;
  our $bf = $bed->toPDL;
  our $pif = $pied->toPDL;
  our $omegaf = $omegaed->toPDL;

  $hmm->compilePdls($af,$bf,$pif,$omegaf,
		    smoothb=>'none',
		    smoothu=>'types',
		   );
  $hmm->restrictByValue($hmm->bf(),zero=>1e-6,Q=>2);

  our ($N,$M,$a,$a1,$b,$b1,$pi,$omega) = @$hmm{qw(N M a a1 b b1 pi omega)};
  our $ap=exp($a);
  our $a1p=exp($a1);
  our $bp=exp($b);
  our $pip=exp($pi);
  our $omegap=exp($omega);

  ##-- oq mask
  our $oq   = pdl($hmm->{oq});
  our $oqok = $oq >= 0;
  our $oqmask = zeroes(byte,$bf->dims);
  our $oqyvals = $oq->yvals;
  our $oqwhich = scalar($oqok->whichND);
  $oqmask->indexND($oq->indexND($oqwhich)->cat($oqyvals->indexND($oqwhich))->xchg(0,1)) .= 1;

  print "test complete.\n";
}
test3q;

##----------------------------------------------------------------------
## Alternate model
##----------------------------------------------------------------------
## normalization
sub normpdl {
  my $ppdl = shift;
  return $ppdl /= $ppdl->xchg(0,1)->sumover;
}

sub test1b {
  $n = 3;
  $m = 3;

  ##-- a
  $ap = zeroes(double, $n,$n);
  $ap->slice("(0)") .= pdl([0,1,1]);  ## p(0-->0)=0   ; p(0-->1)=0.5 ; p(0-->2)=0.5
  $ap->slice("(1)") .= pdl([1,0,1]);  ## p(1-->0)=0.5 ; p(1-->1)=0.5 ; p(1-->2)=0.5
  $ap->slice("(2)") .= pdl([1,1,0]);  ## p(2-->0)=0.5 ; p(2-->1)=0.5 ; p(2-->2)=0
  $a = log(normpdl($ap));

  ##-- b
  $bp = zeroes(double, $n,$m);
  $bp->slice("(0)") .= pdl([1,0,0]);  ## p(0 @ 0)=1   ; p(1 @ 0)=0   ; p(2 @ 0)=0
  $bp->slice("(1)") .= pdl([0,1,0]);  ## p(0 @ 1)=0   ; p(1 @ 1)=1   ; p(2 @ 1)=0
  $bp->slice("(2)") .= pdl([0,0,1]);  ## p(0 @ 2)=0   ; p(1 @ 2)=0   ; p(2 @ 2)=1
  $b = log(normpdl($bp));

  ##-- pi
  $pip = pdl(double,[1,0,0]);      ## initial probability: force state-0
  $pi = log($pip/$pip->sum);

  ##-- omega
  $omegap = pdl(double,[1,1,1]); ## omega: unifrom
  $omegap /= $omegap->sum;
  $omega = log($omegap);

  ##-- o
  $o = pdl([0,1,2,0]);

  ##-- alpha
  $fw = hmmfw($a,$b,$pi, $o);
  $fwp = exp($fw);

  $fwp_expect  = pdl(double, [[1,0,0], [0,1/2,0], [0,0,1/4], [1/8,0,0]]);
  print "fw:  ", (all($fwp->approx($fwp_expect,1e-6))   ? "OK" : "NOT OK."), "\n";


  ##-- beta
  $bw  = hmmbw($a,$b,$omega, $o);
  $bwp = exp($bw);

  $bwp_expect  = pdl(double, [[1/8,0,1/8], [1/4,1/4,0], [0,1/2,1/2], [1,1,1]]);
  print "bw:  ", (all($bwp->approx($bwp_expect,1e-6))   ? "OK" : "NOT OK."), "\n";

  ##-- text prob
  $fwtp = sumover($omegap*$fwp->slice(",-1"));
  $bwtp = sumover($pip*$bwp->slice(",0"));
  print "fw==bw ? ", (all($fwtp->approx($bwtp),1e-6) ? "OK" : "NOT OK"), "\n";

  ##-- eos/bos/enum
  $qenum = MUDL::Enum->new(); $qenum->addSymbol($_) foreach (qw(__$ A B));
  $oenum = MUDL::Enum->new(); $oenum->addSymbol($_) foreach (qw(__$ a b));

  $hmm = MUDL::HMM->new(a=>$a,b=>$b,pi=>$pi,
			N=>$n,M=>$m,
			oenum=>$oenum,
			qenum=>$qenum,
			unknown=>'__$', ##-- hack
		       );
}


sub test1c {
  $n = 2;
  $m = 3;

  ##-- frequency pdls
  $af = pdl(double, [[1,2],[1,2]]);
  $bf = pdl(double, [[1,1],[2,1],[2,1]]);
  $pif = pdl(double,[1,1]);      ## initial probability: bos
  $omegaf = pdl(double,[1,1]);

  ##-- hmm, enums
  $hmm = MUDL::HMM->new();
  $qenum = $hmm->{qenum}; $qenum->addSymbol($_) foreach (qw(A B));
  $oenum = $hmm->{oenum}; $oenum->addSymbol($_) foreach (qw(a b));
  $hmm->compilePdls($af,$bf,$pif,$omegaf);

  ##-- params
  ($a,$b,$pi,$omega) = @$hmm{qw(a b pi omega)};
  ($ap,$bp,$pip,$omegap) = map { exp($_) } ($a,$b,$pi,$omega);

  return;
}


use vars qw($a1p $b1p $atotal $btotal);
sub test1d {
  $n = 3;
  $m = 4;

  ##-- frequency pdls
  $pif = pdl(double,[1,1,1]);

  $bf = pdl(double, [[0,0,0],
		     [0,0,1],
		     [0,1,1],
		     [1,1,1]]);

  $af = pdl(double,   [[0,1,0],
		       [1,1,1],
		       [3,1,4]]);
  $omegaf = pdl(double,[1,1,1]);
  ##-- total            5,4,6

  ##-- hmm, enums
  $hmm = MUDL::HMM->new();
  $qenum = $hmm->{qenum}; $qenum->addSymbol($_) foreach (qw(A B));
  $oenum = $hmm->{oenum}; $oenum->addSymbol($_) foreach (qw(a b c));

  $smoothb = 'none' if (!defined($smoothb));
  #$smootha = 'none' if (!defined($smootha));
  $smootha = 'uniform' if (!defined($smootha));

  $hmm->compilePdls($af,$bf,$pif,$omegaf, smootha=>$smootha,smoothb=>$smoothb);

  ##-- params
  ($a,$b,$pi,$omega) = @$hmm{qw(a b pi omega)};
  ($a1,$b1,$atotal,$btotal) = @$hmm{qw(a1 b1 atotal btotal)};
  ($ap,$bp,$pip,$omegap,$a1p,$b1p) = map { exp($_) } ($a,$b,$pi,$omega,$a1,$b1);

  ##-- save
  $label = 'default' if (!$label);
  $save{$label} = {};
  @{$save{$label}}{qw(a1 b1 atotal btotal ap bp pip omegap a1p b1p)}
    = ($a1,$b1,$atotal,$btotal,$ap,$bp,$pip,$omegap,$a1p,$b1p);

  return;
}
#test1d();


##----------------------------------------------------------------------
## test fw/bw
##----------------------------------------------------------------------
use vars qw($fwbwp);
sub testfb1 {
  $o = pdl(long, [0,1,1,0]) if (!defined($o));
  $fw = $hmm->alpha($o);
  $bw = $hmm->beta($o);
  $fwbw = $fw + $bw;
  $fwp = $fw->exp;
  $bwp = $bw->exp;
  $fwbwp = $fwbw->exp;
}

##----------------------------------------------------------------------
## test Viterbi
##----------------------------------------------------------------------
sub getcw {
  $cw = MUDL::CorpusWriter::TT->new();
  $cw->toFile('-');
}
sub testv {
  $s = bless([map { MUDL::Token->new(text=>$_) } qw(a @UNKNOWN b b @UNKNOWN a)], 'MUDL::Sentence');
  $hmm->viterbiTagSentence($s);
}
sub testfb {
  $s = bless([map { MUDL::Token->new(text=>$_) } qw(a @UNKNOWN b b @UNKNOWN a)], 'MUDL::Sentence');
  $hmm->fbTagSentence($s);
}


##----------------------------------------------------------------------
## Big test
##----------------------------------------------------------------------
use vars qw($tp);
sub tptestbig {
  $hmm = MUDL::HMM->loadFile('mudl.hmm.bin');
  $testfile = 'utest.ttt' if (!defined($testfile));
  $logtp = $hmm->fileProbability($testfile);
  $tp = $logtp->exp;
}

##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#ltest1;
#tptestbig();

foreach $i (0..10) {
  print "--dummy[$i]--\n";
}
