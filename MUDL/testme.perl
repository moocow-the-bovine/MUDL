#!/usr/bin/perl -wd

use lib qw(..);
use MUDL;
use MUDL::PDL;
use MUDL::Cluster::KMeans;
use MUDL::Cluster::Tree;
use PDL;
use Chart::Graph::Gnuplot qw(gnuplot);

BEGIN { $, = ' '; }

##----------------------------------------------------------------------
## Dataset generation
##----------------------------------------------------------------------
sub testdata {
  $n = 4 if (!$n);   # 4 training instances
  $d = 2 if (!$d);   # 2 features
  $k = 3 if (!$k);   # 2 clusters

  #$mat = $m = #MUDL::PDL->new(random($n,$d) * 10);
  $mat = $m = random($n,$d) * 10;

  ##-- centers
  ($min,$max) = $mat->minmaximum;
  $centers = $cs = random($k, $mat->dim(1)) * ($max-$min)->transpose + $min->transpose;

  ##-- indices
  $indices = $is = MUDL::PDL->new(zeroes($n));
  $indices_old = not($indices);

  ##-- distances
  $dists = zeroes($n, $k);

  ##-- temporaries
  $mtmp = zeroes($n,$d); ##-- for getdists0
}

##----------------------------------------------------------------------
## Native K-Means: Distance Measures
##----------------------------------------------------------------------
sub getdists0 {
  ##-- use temporary
  $mtmp = zeroes($n,$d) if (!defined($mtmp) || $iter==0);
  foreach $ki (0..$k-1) {
    ##-- difference
    $mat->minus($centers->slice($ki), $mtmp, 0);

    ##-- euclidean distance
    $mtmp->xchg(0,1)->inplace->pow(2)->sumover($stmp=$dists->slice(",($ki)"));
    $stmp->inplace->pow(0.5);
  }
  return $dists;
}

sub getdists1 {
  ##-- quite slow, not much difference for large datasets
  foreach $ki (0..$k-1) {
    $dists->slice(",($ki)") .= cdistance($mat, $centers->slice($ki));
  }
  return $dists;
}

sub getdists2 {
  ##-- fast (for small datasets), but gobbles memory
  return $dists = cdistance($mat->slice(",,*$k"),
			    $centers->xchg(0,1)->slice("*$n,,"),
			    \&euclid);
}
sub getdists2b {
  ##-- use temporary?
  $mtmp2 = zeroes($n,$d,$k) if (!defined($mtmp2) || $iter==0);

  ##-- difference
  $mat->slice(",,*$k")->minus($centers->xchg(0,1)->slice("*$n,,"), $mtmp2, 0);

  ##-- euclid
  $mtmp2->xchg(0,1)->inplace->pow(2)->sumover($dists);
  $dists->inplace->pow(0.5);

  return $dists;
}


sub getdists3 {
  ##-- slowest
  foreach $ni (0..$n-1) {
    $dists->slice("($ni)") .= cdistance($mat->slice("$ni"), $centers);
  }
  return $dists;
}
BEGIN{ *getdists = \&getdists1; }


##----------------------------------------------------------------------
## Native K-Means: Guts
##----------------------------------------------------------------------
sub realloc {
  $changed = 0;
  $indices_old = $indices;
  $indices     = $dists->xchg(0,1)->minimum_ind;
  $changed = 1 if (any $indices_old != $indices);
  return $indices;
}
sub recenter {
  foreach $ki (0..$k-1) {
    $centers->slice("($ki)") .= $m->dice(which $indices == $ki)->average;
  }
}

##----------------------------------------------------------------------
## Native K-Means: Iteration
##----------------------------------------------------------------------
sub kmiter {
  getdists();
  realloc;
  recenter;
  ++$iter;
}
$itermod = 10;
sub kmreset {
  $changed=1;
  $iter=0;
  $maxiter=0;
}

sub kmiterN {
  my $n = shift;
  my $max = $iter + $n;
  select STDERR; $| = 1; select STDOUT;
  while ($changed && $iter < $max) {
    print STDERR "$iter";
    kmiter;
    print STDERR "\[$changed] ";
  }
  print STDERR "\n";
}

##----------------------------------------------------------------------
## Test Configurations
##----------------------------------------------------------------------
sub bigtest {
  ($n,$d,$k) = (128,42,10);
  $iter = 0;
  testdata();
}
sub bigtest2d {
  ($n,$d,$k) = (128,2,10);
  $iter = 0;
  testdata();
}
sub ktest2d {
  ($n,$d,$k) = (1024,2,10);
  $iter = 0;
  testdata();
}
sub smalltest_random {
  ($n,$d,$k) = (4,2,3);
  $iter = 0;
  testdata;
}
sub smalltest {
  ($n,$d,$k) = (4,2,3);
  $iter = 0;

  $mat = $m = pdl([ [1,1], [2,8], [6,6], [7,5] ])->transpose;
  $centers = $cs = pdl([ [4,7], [4,5], [4,3] ])->transpose;
  $indices = $is = zeroes($n);
  $dists = zeroes($n,$k);
}



##----------------------------------------------------------------------
## Plots: guts
##----------------------------------------------------------------------
BEGIN {
  %plotm_global_options=
    (
     title => 'plotm',
     'output type'=>'x11',
     'xrange'=>'[0:10]',
     'yrange'=>'[0:10]',
    );

  %plotm_matrix_options=
    (
    );

  #testdata();
  smalltest();
}

##-- plotdata(\%global_opts, [$pdl,%options],...)
sub plotdata {
  gnuplot(plotargs_pdl(@_));
}
sub plotargs_pdl {
  my $global_args = shift;
  return (
	  {
	   %plotm_global_options,
	   ($global_args ? %$global_args : qw()),
	  },

	  (map {
	    [
	     {
	      %plotm_matrix_options,
	      @$_[1..$#$_],
	      type=>'columns',
	     },
	     (map { [$_->list] } $_->[0]->dog)
	    ]
	  } @_)
	 );
}


##-- plotadata(\%global_opts, [\@matrix,%options],...)
##    + plots a perl matrix
sub plotadata {
  gnuplot(plotargs_ary(@_));
}
sub plotargs_ary {
  my $global_args = shift;
  return ({
	   %plotm_global_options,
	   title=>'array',
	   ($global_args ? %$global_args : qw()),
	  },

	  (map {
	    [
	     {
	      %plotm_matrix_options,
	      @$_[1..$#$_],
	      type=>'matrix',
	     },
	     $_->[0]
	    ]
	  } @_));
}

##----------------------------------------------------------------------
## Plots: Aliases
##----------------------------------------------------------------------
sub plotmc {
  plotdata({title=>"K-Means Iter #$iter"}, [$mat, title=>'mat'], [$centers, title=>'centers']);
}

sub plotcl {
  plotdata({title=>"K-Means Iter #$iter"},
	   #[$mat, title=>'mat'],
	   [$centers, title=>'centers'],
	   map {
	     my $which = (which $indices == $_);
	     ($which->isempty
	      ? qw()
	      : [$m->dice($which), title=>"cluster $_"])
	   } (0..$k-1)
	  );
}


sub plotacdata {
  plotadata({title=>"Algorithm::Cluster Iter #$iter"}, [$adata, title=>'data']);
}
sub plotacl {
  my @pdata = map { [
		     [],
		     title=>"Cluster #$_",
		     type=>'matrix'
		    ]
		  } (0..($k-1));
  foreach $ni (0..($n-1)) {
    push(@{$pdata[$aclusters->[$ni]][0]}, $adata->[$ni]);
  }

  plotadata({title=>"Algorithm::Cluster Iter #$iter"}, @pdata);
}


##----------------------------------------------------------------------
## Algorithm::Cluster
##----------------------------------------------------------------------
use Algorithm::Cluster qw(kcluster);


##----------------------------------------------------------------------
## Dataset conversion
##----------------------------------------------------------------------

## $ary = pdl2ary($pdl)
sub pdl2ary {
  my $pdl = shift;
  my $data = undef;
  my @queue = ($pdl,\$data);
  my ($ref);
  while (($pdl,$ref) = splice(@queue,0,2)) {
    if ($pdl->ndims == 0) {
      $$ref = $pdl->sclr;
      next;
    }
    $$ref = [];
    push(@queue, $_, \$$ref->[@$$ref]) foreach ($pdl->dog);
  }
  return $data;
}

sub akprep {
  ##-- prepare Algorithm::Cluster params
  $amat = $adata = pdl2ary($mat->xchg(0,1));
  $amask = '';
  $aweight = '';
  $atranspose = 0;
  $anpass = 1 if (!defined($anpass));
  $amethod = 'a' if (!defined($amethod));
  $adist = 'e' if (!defined($adist));
  #$ainitialid = undef;

  return 1;
}

sub akmeans {
  return
    ($aclusters,$aerror,$anfound) =
      Algorithm::Cluster::kcluster( %{akparams(@_)} );
}

sub akparams {
  return
    $aparam = {
	       nclusters=>$k,
	       data=>$adata,
	       mask=>$amask,
	       weight=>$aweight,
	       transpose=>$atranspose,
	       npass=>$anpass,
	       method=>$amethod,
	       dist=>$adist,
	       #initialid=>$ainitialid,
	       @_,
	      };
}

sub akmiter {
  ;
}
sub akmreset {
  ;
}
sub akmiterN {
  ;
}


##----------------------------------------------------------------------
## tree clustering (hierarchical)
##----------------------------------------------------------------------

sub atcprep {
  ##-- prepare Algorithm::Cluster params
  akprep();

  ##-- method:
  # s : single-link
  # m : (maximum) complete-link
  # a : average-link
  # c : centroid-link
  $atcmethod = 'm';

  return 1;
}

##-- single iter
sub atreecluster {
  return
    ($atcresult, $atcdists) =
      Algorithm::Cluster::treecluster( %{atcparams(@_)} );
}

sub atcparams {
  return
    $aparam = {
	       data=>$adata,
	       mask=>$amask,
	       weight=>$aweight,
	       transpose=>$atranspose,
	       method=>$atcmethod,
	       dist=>$adist,
	       @_,
	      };
}


##----------------------------------------------------------------------
## Tree Clustering: dendograms
##----------------------------------------------------------------------
use MUDL::Cluster::Tree;
use MUDL::Tk::Dendogram;

sub dgprep {
  atcprep;
  $tc = MUDL::Cluster::Tree->new(%{atcparams(@_)});
  $tc->cluster();
  $tct = $tc->toTree(@_);
  $dg  = $tct->toDendogram(@_);
  return $dg;
}

##----------------------------------------------------------------------
## Generic Trees
##----------------------------------------------------------------------
use MUDL::Tree;
use MUDL::Tk::Tree;

##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

atcprep; ($tr,$td)=atreecluster();
$dg = MUDL::Tk::Dendogram->new(tree=>$tr,dists=>$td,xpad=>10,ypad=>5,dmult=>5);
$dg->view;

foreach $i (0..100) {
  print "--dummy[$i]--\n";
}


