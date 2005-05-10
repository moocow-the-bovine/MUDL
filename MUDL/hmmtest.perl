#!/usr/bin/perl -wd

use lib qw(..);
use MUDL;
use MUDL::CmdUtils;
use MUDL::HMM;
use PDL;

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
use vars qw($hmm $oenum $qenum $N $K $A1 $A $B $a $a1 $b $bos $eos $type);
sub test1 {
  $N0 = 2 if (!defined($N0));
  $K0 = 3 if (!defined($K0));
  $bos = $eos = '__$';
  $type = float if (!defined($type));
  $smooth = 'hapax' if (!defined($smooth));

  $oenum = MUDL::Enum->new();
  $oenum->addSymbol($bos);
  $oenum->addSymbol(chr($_+97)) foreach (0..($K0-1));

  $qenum = MUDL::Enum->new();
  $qenum->addSymbol($bos);
  $qenum->addSymbol(chr($_+65)) foreach (0..($N0-1));

  $hmm = MUDL::HMM->new(oenum=>$oenum,qenum=>$qenum,bos=>$bos,eos=>$eos,type=>$type);

  ($N,$K,$A,$A1,$B) = @$hmm{qw(N K A A1 B)};
  $a=$A; $a1=$A1; $b=$B;

  use vars qw($ea $ea1 $eb);
  $ea=exp($a); $ea1=exp($a1); $eb=exp($b);
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

  $hmm->compilePdls($af,$bf);
}

sub test3 {
  ##-- SETUP: A() [from Pdl]
  use vars qw($Ad $Bd $ad $bd);

  $Ad = $ad = MUDL::Dist::Nary->new();
  $Bd = $bd = MUDL::Dist::Nary->new();

  %{$ad->{nz}} = ("$bos\tA"=>12, "$bos\tB"=>8,                ##-- f(bos->)=20
		  "A\tA"=>15,     "A\tB"=>10,   "A\t$eos"=>5, ##-- f(A->)=30
		  "B\tA"=>3,     "B\tB"=>7,    "B\t$eos"=>15, ##-- f(B->)=25
		  ##                                          ##   sum(.->)=75
		  ##f(->A)=30    f(->B)=25     f(->eos)=20    ##   sum(->.)=75
		 );

  %{$bd->{nz}} = ("A\ta"=>15, "A\tb"=>0, "A\tc"=>4,  ## f(A@)=19
		  "B\ta"=>10, "B\tb"=>5, "B\tc"=>6,  ## f(B@)=21
		  ##                                 ## sum(.@)=40
		  ##f(@a)=25   f(@b)=5   f(@c)=10    ## sum(@.)=40
		 );

  ##-- test ground
  use vars qw($Aed $Bed $aed $bed $af $bf);
  $Aed = $aed = $ad->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$qenum,$qenum]));
  $Bed = $bed = $bd->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$qenum,$oenum]));
  $af = $aed->toPDL;
  $bf = $bed->toPDL;

  $hmm->compileDists($ad,$bd);
}


##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#ltest1;
foreach $i (0..100) {
  print "--dummy[$i]--\n";
}
