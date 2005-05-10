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

BEGIN { $, = ' '; }

#test();
sub test1 {
  $ap = random(double,3,3) if (!defined($ap));
  $bp = random(double,3,4) if (!defined($bp));
  $ap->set(0,0,0);
  $ap->slice("0,2") .= 0;
  $bp->slice("0,1:3") .= 0;
  $bp->slice("1:2,0") .= 0;

  $ap /= $ap->xchg(0,1)->sumover;
  $bp /= $bp->xchg(0,1)->sumover;

  $a = log($ap);
  $b = log($bp);
  $o = pdl(long,[0,1,2,1,0]);
  $alpha = PDL::alpha2b($a,$b,$o);
  $ea = $ealpha = exp($alpha);

  print
    ("A=$a\n",
     "B=$b\n",
     "O=$o\n",
     "alpha=$alpha\n",
     "\n",
     "exp A = ", exp($a), "\n",
     "exp B = ", exp($b), "\n",
     "O=$o\n",
     "exp alpha = ", exp($alpha), "\n",
    );
}

##----------------------------------------------------------------------
## Display
##----------------------------------------------------------------------
sub showap {
  my $ap = shift;
  $ap = $main::ap if (!defined($ap));
  foreach $i (0..($ap->dim(0)-1)) {
    foreach $j (0..($ap->dim(1)-1)) {
      print "a_(i=$i --> j=$j) = ", $ap->at($i,$j), "\n";
    }
  }
}

sub showbp {
  my $bp = shift;
  $bp = $main::bp if (!defined($bp));
  foreach $i (0..($bp->dim(0)-1)) {
    foreach $k (0..($bp->dim(1)-1)) {
      print "b_(k=$k  @  i=$i) = ", $bp->at($i,$k), "\n";
    }
  }
}

##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------
foreach (0..100) {
  test1;
}


__DATA__

__Pdlpp__

pp_def('alpha2b',
       Pars => 'A(n,n); B(n,k); O(t); [o]alpha(n,t)',
       Code =>(''
	       ##-- initialize alpha(:,0)
	       .q(
	       int bos = $O(t=>0);
	       int tp;
               loop (n) %{
		 $alpha(n=>n, t=>0) = $B(n=>n, k=>bos);
	       %}
	       for (tp=0; tp < $SIZE(t)-1; tp++) {
		 int i, j, o = $O(t=>tp+1);
		 for (j=0; j < $SIZE(n); j++) {
		   double alpha_jt = 0;
                   double b_jk  = $B(n=>j,k=>o);
		   for (i=0; i < $SIZE(n); i++) {
		     alpha_jt += exp( $alpha(n=>i, t=>tp) + $A(n0=>i,n1=>j) + b_jk);
                     printf("i=%u,j=%u,t=%u,k=%d:  a_ij=%.2E  b_jk=%.2E  alpha_it-1=%.2E  prod=%.2E  sum=%.2e\n",
                            i,j,tp+1,o, exp($A(n0=>i,n1=>j)), exp(b_jk), exp($alpha(n=>i, t=>tp)),
                            exp( $alpha(n=>i, t=>tp) + $A(n0=>i,n1=>j) + b_jk ), alpha_jt);
		   }
		   $alpha(n=>j, t=>tp+1) = log(alpha_jt);
		 }
	       }
	       )
	      )
      );


pp_def('alpha2',
       Pars => 'A(n,n); B(n,k); O(t); [o]alpha(n,t)',
       Code =>(''
	       ##-- initialize alpha(:,0)
	       .q(
	       int ni;
               int bos = $O(t=>0);
	       for (ni=0; ni<$SIZE(n); ni++) {
		 $alpha(n=>ni, t=>0) = $B(n=>ni, k=>bos);
	       })
	      )
      );
