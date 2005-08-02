#!/usr/bin/perl -wd

use lib qw(../..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use PDL::Cluster;
use MUDL::Cluster::Method;
use MUDL::Cluster::Tree;
use MUDL::Cluster::Buckshot;
use Benchmark qw(cmpthese timethese);

use MUDL::Corpus::MetaProfile::Attach;

BEGIN { $, = ' '; }

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
foreach $i (0..100) {
  print "--dummy[$i]--\n";
}
