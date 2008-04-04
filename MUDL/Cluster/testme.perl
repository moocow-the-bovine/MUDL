#!/usr/bin/perl -wd

use lib qw(../..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use PDL::Cluster;
use MUDL::PDL::Stats;
use MUDL::PDL::Ranks;
use MUDL::Cluster::Method;
use MUDL::Cluster::Tree;
use MUDL::Cluster::Buckshot;
use MUDL::Cluster::Distance;
use Benchmark qw(cmpthese timethese);

use MUDL::Corpus::MetaProfile::Attach;

BEGIN { $, = ' '; }

##----------------------------------------------------------------------
## test: perl distance func
##----------------------------------------------------------------------

## $ucmp = ucmp($cwhich,$cmpvec1,$cmpvec2,...)
sub ucmp {
  my $cw   = shift;
  my $cwi  = $cw->qsortveci;
  my $ucmp = $cw->dice_axis(1,$cwi)->glue(0,pdl(-1))->convert(double);
  foreach my $cv (@_) {
    $ucmp = $ucmp->glue(0,$cv->index($cwi)->slice("*1"));
  }
  return $ucmp;
}

#use MUDL::Cluster::Distance::L1;
#use MUDL::Cluster::Distance::L2;
use MUDL::Cluster::Distance::Pearson;
use MUDL::Cluster::Distance::Spearman;
sub test_perl_distance {
  my ($d,$n,$data,$mask,$wt);
  my $RANDOM_DATA = 0;
  #$RANDOM_DATA = 1;

  if (!$RANDOM_DATA) {
    ##-- literals
    #$data = pdl(double,[ [1,2,3,4],[1,3,3,0],[4,3,2,1] ]);
    $data = pdl(double, [[1,1,1,1],[2,2,2,2],[3,4,5,6],[2,3,3,2],[9,0,0,0]]);
    ##--
    ($d,$n) = $data->dims;
  } else {
    ##-- random data
    ($d,$n) = (200,256);
    $data = grandom(double,$d,$n);
  }
  $mask   = ones(long,$d,$n);
  $wt     = ones(double,$d);


  ##-- what to compare?
  my @compare = (
		 #['L1','b'], ##-- ok
		 #['L2','e'], ##-- ok (but PDL::Cluster 'e' is missing sqrt() step)
		 ['Pearson','c'], ##-- ok
		 #['Cosine','u'], ##-- ok
		 #[['Spearman',rank_min=>0],'s'], ##-- ok
		 #[['Spearman',rank_min=>1],'s'], ##-- ok
		);
  foreach my $cfg (@compare) {
    my ($class,$dflag) = @$cfg;

    my $cd = MUDL::Cluster::Distance->new(class=>$class);

    ##-- test: comparison vector
    #my ($rows1,$rows2) = $cd->cmp_pairs($n)->qsortvec->xchg(0,1)->dog;
    my $cwhich         = $cd->cmp_pairs($n)->qsortvec;
    my ($rows1,$rows2) = $cwhich->xchg(0,1)->dog;
    my $cmpvec         = $cd->compare(data=>$data, rows1=>$rows1,rows2=>$rows2);

    my $cdb = MUDL::Cluster::Distance->new(class=>$dflag);
    my $cmpvecb = $cdb->compare(data=>$data, rows1=>$rows1,rows2=>$rows2);
    print STDERR "cmpvec(class=$class)==cmpvec(flag=$dflag) ? ", (all($cmpvec->approx($cmpvecb)) ? "ok" : "NOT ok"), "\n";

    ##-- get data matrix using builtin funcs
    my $dmat  = $cd->distanceMatrix(data=>$data);
    my $dmatb = distancematrix($data,$mask,$wt, $dflag);
    print STDERR "dmat(class=$class)==dmat(flag=$dflag) ? ", (all($dmat->approx($dmatb)) ? "ok" : "NOT ok"), "\n";
  }

  print STDERR "$0: test_perl_distance() done: what now?\n";
}
#test_perl_distance();

##-- test: cross product
sub crossp1 {
  my ($n,$m) = @_;
  return xvals(long,$n,$m)->flat->cat(yvals(long,$n,$m)->flat)->xchg(0,1); #->qsortvec;
}
#sub crossp2 {
#  my ($n,$m) = @_;
#  return sequence(long,$n)->dummy(0,$m)->flat->cat(sequence(long,$m)->dummy(1,$n)->flat)->xchg(0,1);
#}
#sub crossp3 {
#  my ($n,$m) = @_;
#  return (sequence(long,$n*$m)->dummy(0,2) % pdl(long,$n,$m)); #->qsortvec;
#}


##----------------------------------------------------------------------
## test: perl link func
##----------------------------------------------------------------------
use MUDL::Cluster::Distance;
use MUDL::Cluster::Distance::Builtin;
sub test_perl_link {
  my ($d,$n,$k, $data, $cids);
  my $RANDOM_DATA = 0;
  $RANDOM_DATA = 1;
  if (!$RANDOM_DATA) {
    ##-- literals
    $data   = pdl(double,[ [1,2,3,4],[1,3,3,0],[4,3,2,1],[1,42,24,7],[10,12,14,16] ]);
    ($d,$n) = $data->dims;
    $k      = 3;
    $cids   = sequence(long,$n) % $k;
  } else {
    ##-- random data
    ($d,$n) = (200,256);
    $data   = grandom(double,$d,$n);
    $k      = 32;
    $cids   = (random($n)*$k)->long;
  }

  ##-- get target row-ids
  #my $rids = sequence(long,$n);

  my ($dclass,$dflag) = ('L1','b');
  #my ($lclass,$lflag) = ('min','s');
  #my ($lclass,$lflag) = ('max','x');
  my ($lclass,$lflag) = ('avg','v');

  my $cda = MUDL::Cluster::Distance->new(class=>$dclass, link=>$lclass);
  my $cdb = MUDL::Cluster::Distance->new(class=>'Builtin', distFlag=>$dflag, linkFlag=>$lflag);

  my $cdmata = $cda->clusterDistanceMatrix(data=>$data, cids=>$cids);
  my $cdmatb = $cdb->clusterDistanceMatrix(data=>$data, cids=>$cids);

  print "cdmata~cdmatb ? ", (all($cdmata->approx($cdmatb)) ? "ok" : "NOT ok"), "\n";

  print STDERR "$0: test_perl_link() done: what now?\n";
}
#test_perl_link();

##----------------------------------------------------------------------
## test native vs. builtin clustering
sub test_native_cluster {
  our ($data,$d,$n,$k, $adata,$gdata,$arows);
  my $RANDOM_DATA=0;
  #$RANDOM_DATA=1;
  if (!$RANDOM_DATA) {
    $data   = pdl(double, [[1,1,1],[2,2,2],[3,3,3],[4,5,6],[7,8,9],[10,11,12]]);
    ($d,$n) = $data->dims;
    $adata  = pdl(double,[[10,20,30],[16,8,4],[0,1,0]]);
    $k      = 2;
  } else {
    ($d,$n) = (32,256);
    $data   = (random(double,$d,$n)*100)->rint;
    $adata  = (random(double,$d,2)*100)->rint;
    $k = 16;
  }

  my ($dc1,$dc2);
  ($dc1,$dc2) = (['L1',link=>'max'],['b',linkFlag=>'x']);
  #($dc1,$dc2) = (['L1',link=>'avg'],['b',linkFlag=>'v']);
  #($dc1,$dc2) = (['Pearson',link=>'avg'],['c',linkFlag=>'v']);
  ($dc1,$dc2) = (['Pearson',link=>'max'],['c',linkFlag=>'x']);
  #($dc1,$dc2) = (['Cosine',link=>'min'],['u',linkFlag=>'s']);
  #($dc1,$dc2) = (['Spearman',rank_min=>1,link=>'max'],['s',linkFlag=>'x']);
  #($dc1,$dc2) = (['Spearman',rank_min=>0,link=>'max'],['s',linkFlag=>'x']);
  #($dc1,$dc2) = (['Spearman',rank_min=>0,link=>'avg'],['s',linkFlag=>'v']);

  my %opts = (data=>$data, nclusters=>$k, cdbonus=>0);
  my $cm1 = MUDL::Cluster::Tree->new(dclass=>$dc1, %opts);
  my $cm2 = MUDL::Cluster::Tree->new(dclass=>$dc2, %opts);

  ##-- dmat
  my ($df1,$df2) = ($cm1->distance,$cm2->distance);
  my $dmat1 = $df1->distanceMatrix(data=>$data);
  my $dmat2 = $df2->distanceMatrix(data=>$data);
  print "dmat1~=dmat2 ? ", (all($dmat1->approx($dmat2)) ? "ok" : "NOT ok"), "\n";

  $cm1->cluster;
  $cm2->cluster;
  $cm1->cut;
  $cm2->cut;

  my ($cids1,$cids2) = ($cm1->{clusterids},$cm2->{clusterids});
  print "cids1==cids2 ? ", (all($cids1==$cids2) ? "ok" : "NOT ok"), "\n";
  print "linkd1==linkd2 ? ", (all($cm1->{linkdist}->approx($cm2->{linkdist})) ? "ok" : "NOT ok"), "\n";
  print "ctree1==ctree2 ? ", (all($cm1->{ctree}==$cm2->{ctree}) ? "ok" : "NOT ok"), "\n";

  my $cdmat1 = $cm1->clusterDistanceMatrix;
  my $cdmat2 = $cm2->clusterDistanceMatrix;
  print "cdmat1==cdmat2 ? ", (all($cdmat1->approx($cdmat2)) ? "ok" : "NOT ok"), "\n";

  ##-- attach: BROKEN ?! (we're calling it wrong it seems, but this is how it *OUGHT* to work...)
  $arows = sequence(long,$adata->dim(1));
  ($acids1,$acdist1) = $cm1->attach(adata=>$adata, arows=>$arows);
  ($acids2,$acdist2) = $cm2->attach(adata=>$adata, arows=>$arows);
  print "attach: acids1==acids2 ? ", (all($acids1==$acids2) ? "ok" : "NOT ok"), "\n";
  print "attach: acdist1==acdist2 ? ", (all($acdist1->approx($acdist2)) ? "ok" : "NOT ok"), "\n";

  ##-- attach+adopt
  ($acids1,$acdist1) = $cm1->attach(adata=>$adata, arows=>$arows, adopt=>1);
  ($acids2,$acdist2) = $cm2->attach(adata=>$adata, arows=>$arows, adopt=>1);
  print "attach+adopt: acids1==acids2 ? ", (all($acids1==$acids2) ? "ok" : "NOT ok"), "\n";
  print "attach+adopt: acdist1==acdist2 ? ", (all($acdist1->approx($acdist2)) ? "ok" : "NOT ok"), "\n";
  print "attach+adopt: data1==data2 ? ", (all($cm1->{data}==$cm2->{data}) ? "ok" : "NOT ok"), "\n";
  print "attach+adopt: mask1==mask2 ? ", (all($cm1->{mask}==$cm2->{mask}) ? "ok" : "NOT ok"), "\n";
  $cdmat1 = $cm1->clusterDistanceMatrix;
  $cdmat2 = $cm2->clusterDistanceMatrix;
  print "attach+adopt: cdmat1==cdmat2 ? ", (all($cdmat1->approx($cdmat2)) ? "ok" : "NOT ok"), "\n";

  print STDERR "$0: test_native_cluster() done: what now?\n";
}
test_native_cluster;


##----------------------------------------------------------------------
## test data
use vars qw($k $n $d $data $mask $weight @dmw);
sub tdata {

  $data = pdl(double, [[1,1,1],[2,2,2],[3,3,3],[4,5,6],[7,8,9],[10,11,12]]) if (!defined($data));
  $d = $data->dim(0);
  $n = $data->dim(1);
  $k = 2 if (!defined($k));

  $mask = ones(long, $data->dims);
  $weight = ones(double, $d);
  @dmw = ($data,$mask,$weight);
}

##----------------------------------------------------------------------
## test random data
sub tdata_random {
  $n = 100 if (!defined($n));
  $d = 50  if (!defined($d));
  $k = 25  if (!defined($k));

  $data = random($d,$n);
  $mask = ones(long,$data->dims);
  $weight = ones(double,$d);
  @dmw = ($data,$mask,$weight);
}

##----------------------------------------------------------------------
## test cluster class
##  + requires: test data (tdata, tdata_random)
##  + usage: tcclass($class='Tree',%args)
sub tcclass {
  my $class = shift;
  $class = 'Tree' if (!defined($class));
  $niters = 0 if (!defined($niters));
  $cm = $tcclass{$class} = MUDL::Cluster::Method->new(class=>$class,
						      data=>$data,
						      mask=>$mask,
						      weight=>$weight,
						      niters=>$niters,
						      nclusters=>$k,
						      @_,
						     );
  $cm->cluster();
  $cm->cut();
}


##----------------------------------------------------------------------
## test prototypes
##   + requires: tdata()
use vars qw($np $protos $ptmp);
sub tprotos {
  $np = sclr(rint(sqrt($k*$n))) if (!$np);
  ($ptmp=random(float,$n))->minimum_n_ind($protos=zeroes(long,$np)-1);
  $protos .= qsort($protos);
}

##----------------------------------------------------------------------
## test prototypes: get data
##   + requires: tprotos()
use vars qw($pdata $pmask);
sub tpdata {
  $pdata = $data->dice_axis(1,$protos);
  $pmask = $mask->dice_axis(1,$protos);
}

##----------------------------------------------------------------------
## test prototypes: cluster 'em
##  + requires: tpdata()
use vars qw($ptree $plnkdist $pcids);
sub tpcluster {
  $pdist   = 'b' if (!defined($pdist));
  $pmethod = 'm' if (!defined($pmethod));

  ##-- cluster protos
  treecluster($pdata,$pmask,$weight,
	      ($ptree=zeroes(long,2,$np)),
	      ($plnkdist=zeroes(double,$np)),
	      $pdist, $pmethod);

  ##-- cut tree
  cuttree($ptree, $k, ($pcids=zeroes(long,$np)));
}


##----------------------------------------------------------------------
## test prototype centroid profiles
##  + requires: tpcluster()

##-- centroid profiles: means
use vars qw($pcmeans $pcmeansmask);
sub tpcmeans {
  getclustermean($pdata,$pmask,$pcids,
		 ($pcmeans=zeroes(double,$d,$k)),
		 ($pcmeansmask=zeroes(long,$d,$k)));

  ##-- centroid data: aliases
  $pcdata = $pcmeans;
  $pcmask = $pcmeansmask;
}

##-- centroid profiles: medians
use vars qw($pcmedians $pcmediansmask);
sub tpcmedians {
  getclustermedian($pdata,$pmask,$pcids,
		   ($pcmedians=zeroes(double,$d,$k)),
		   ($pcmediansmask=zeroes(long,$d,$k)));

  ##-- centroid data: aliases
  $pcdata = $pcmedians;
  $pcmask = $pcmediansmask;
}

##----------------------------------------------------------------------
## test prototype centroid profiles: weighted sum variants
##  + requires: tpcluster()

##-- get prototype cluster distance matrix
use vars qw($pcdm);
sub tpcmatrix {
  $cddist   = $pdist if (!defined($cddist));
  $cdmethod = 'x'    if (!defined($cdmethod));

  clustersizes($pcids, $pcsizes=zeroes(long,$k));
  clusterelements($pcids, $pcsizes, $pcelts=zeroes(long, $pcsizes->max, $k)-1);
  clusterdistancematrix($pdata,$pmask,$weight,
			sequence(long,$np), $pcsizes, $pcelts,
			$pcdm=zeroes(double,$k,$np),
			$cddist, $cdmethod);
}

##-- test m-best indices
## + requires: tpcmatrix
use vars qw($pcmbesti $pcmbestiND);
sub tpcmbesti {
 $m = 2 if (!defined($m));

 ##-- get minimum distance indices
 $pcmbesti = zeroes(long,$m,$k);
 $pcdm->xchg(0,1)->minimum_n_ind($pcmbesti);

 ##-- get values to keep
 $pcmbestiND = cat(yvals($pcmbesti)->flat, $pcmbesti->flat)->xchg(0,1);
}

##-- test m-best mask (soft)
## + requires: tpcmatrix
use vars qw($tpcmbestmask);
sub tpcmbestmask_soft {
  tpcmbesti;
  $pcmbestmask_soft = zeroes(byte, $pcdm->dims);
  $pcmbestmask_soft->indexND($pcmbestiND) .= 1;
  $pcmbestmask = pdl($pcmbestmask_soft);
}

##-- test m-best mask (hard)
## + requires: -
use vars qw($tpcmbestmask);
sub tpcmbestmask_hard {
  tpcmbestmask_soft;
  clusterelementmask($pcids, $pceltmask=zeroes(byte,$k,$np));
  $pcmbestmask_hard = $pcmbestmask_soft * $pceltmask;
  $pcmbestmask      = pdl($pcmbestmask_hard);
}

##-- test m-best mean (soft)
## + requires: tpcmatrix
sub tpcmbestmeans_soft {
  print STDERR "tpcmbestmeans_soft(): called.\n";

  tpcmbestmask_soft;
  $pcw  = zeroes(double, $pcdm->dims)+1/$m;
  $pcw *= $pcmbestmask;

  ##-- alt: given only tpcmbesti()
  #tpcmbesti;
  #$pcw = zeroes(double, $pcdm->dims); ##-- zero non-best values
  #$pcw->indexND($pcmbestiND) .= 1/$m; ##-- set weights for arithmetic mean

  #-- get centroid data
  getclusterwsum($pdata,$pmask, $pcw,
		 ($pcmbestmeans_soft_data=zeroes(double,$d,$k)),
		 ($pcmbestmeans_soft_mask=zeroes(long,$d,$k)));

  ##-- centroid data: aliases
  $pcdata = $pcmbestmeans_soft_data;
  $pcmask = $pcmbestmeans_soft_mask;
}


##-- test m-best mean (hard)
## + requires: tpcmatrix
sub tpcmbestmeans_hard {
  tpcmbestmask_hard;
  $pcw  = ones(double, $pcdm->dims);
  $pcw *= $pcmbestmask;
  $pcw /= $pcw->xchg(0,1)->sumover;

  #-- get centroid data
  getclusterwsum($pdata,$pmask, $pcw,
		 ($pcmbestmeans_hard_data=zeroes(double,$d,$k)),
		 ($pcmbestmeans_hard_mask=zeroes(long,$d,$k)),
		);

  ##-- centroid data: aliases
  $pcdata = $pcmbestmeans_hard_data;
  $pcmask = $pcmbestmeans_hard_mask;
}


##----------------------------------------------------------------------
## test prototype centroid profiles: weighted sum variants: inverse
##  + requires: tpcluster()

##-- test m-best inverse (soft)
## + requires: tpcmatrix
sub tpcmbestinverse_soft {
  tpcmbestmask_soft;

  $pcmimin = $pcdm->where($pcdm!=0)->flat->minimum if (!defined($pcmimin) || !sclr($pcmimin));
  $pcw     = $pcmimin / ($pcmimin+$pcdm);
  $pcw    *= $pcmbestmask;
  $pcw    /= $pcw->xchg(0,1)->sumover;

  #-- get centroid data
  getclusterwsum($pdata,$pmask, $pcw,
		 ($pcmbestinv_soft_data=zeroes(double,$d,$k)),
		 ($pcmbestinv_soft_mask=zeroes(long,$d,$k)));

  ##-- centroid data: aliases
  $pcdata = $pcmbestinv_soft_data;
  $pcmask = $pcmbestinv_soft_mask;
}

##-- test m-best inverse (hard)
## + requires: tpcmatrix
sub tpcmbestinverse_hard {
  tpcmbestmask_hard;

  $pcmimin = $pcdm->where($pcdm!=0)->flat->minimum if (!defined($pcmimin) || !sclr($pcmimin));
  $pcw     = $pcmimin / ($pcmimin+$pcdm);
  $pcw    *= $pcmbestmask;
  $pcw    /= $pcw->xchg(0,1)->sumover;

  #-- get centroid data
  getclusterwsum($pdata,$pmask, $pcw,
		 ($pcmbestinv_hard_data=zeroes(double,$d,$k)),
		 ($pcmbestinv_hard_mask=zeroes(long,$d,$k)));

  ##-- centroid data: aliases
  $pcdata = $pcmbestinv_hard_data;
  $pcmask = $pcmbestinv_hard_mask;
}


##----------------------------------------------------------------------
## test attachment
##  + requires: tpcluster(), $pcdata, $pcmask
##    i.e. tpc${method}(), e.g. tpcmeans(), tpcmbestinverse_hard(), ...
use vars qw($acdist $aceltmask);
sub tattach {
  ##-- dist? method?
  $adist   = $pdist if (!defined($adist));
  $amethod = 'x'    if (!defined($amethod));

  ##-- get attachment targets
  $apmask = zeroes(byte,$n);
  $apmask->index($protos) .= 1;
  $atmask = !$apmask;
  $atids  = $atmask->which;
  $na     = $atids->nelem;

  attachtonearest($data, $mask, $weight,
		  $atids,
		  $pcdata, $pcmask,
		  $acids=zeroes(long,$na),
		  $acdist=zeroes(double,$na),
		  $adist, $amethod);

  ##-- get grand total output
  $cids = zeroes(long,$n);
  $cids->index($protos) .= $pcids;
  $cids->index($atids)  .= $acids;

  ##-- ... and its mask
  clusterelementmask($cids, $aceltmask=zeroes(byte,$k,$n));
}

sub baddata {
  tdata;

  $protos = pdl(long,[2,3,4]);
  $np=$protos->nelem;
  $k=2;

  tpdata;
  tpcluster;
  tpcmeans;
  tattach;
}

sub itertest {
  $icsub = shift;
  $icsub = \&tpcmeans if (!defined($icsub));

  ##-- don't regen data
  tprotos;
  tpdata;
  tpcluster;
  tpcmatrix;
  clusterelementmask($pcids,$pceltmask=zeroes(long,$k,$np));

  &$icsub();
  tattach;

  print "pdata=$pdata, pcdata=$pcdata, pceltmask=$pceltmask, data=$data, aceltmask=$aceltmask\n";
}

##----------------------------------------------------------------------
## Buckshot
##----------------------------------------------------------------------
#use MUDL::Cluster::Method;
#use MUDL::Cluster::Buckshot;


##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#ltest1;
foreach $i (0..10) {
  print "--dummy[$i]--\n";
}
