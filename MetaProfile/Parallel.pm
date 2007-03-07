##-*- Mode: CPerl -*-

## File: MUDL::Corpus::MetaProfile::Parallel.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: multi-stage meta-profile
##    : all-new clusters induced at each stage
##======================================================================

package MUDL::Corpus::MetaProfile::Parallel;
use MUDL::Corpus::MetaProfile qw(:vlevels);
use MUDL::Corpus::MetaProfile::Attach;
use PDL;
use PDL::Cluster;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::MetaProfile::Attach);

##======================================================================
## Corpus::MetaProfile::Parallel: Constructor
## {
##  ##
##  ##-- global data
##  phat   => $phat,       ## dims: ($NBoundClusters, $NPrevTargets) : $phat->at($cid,$tid) = ^p_{<=$i}($cid|$tid)
##  phatm  => $mask,       ## type=byte,   dims as for $phat; $phatm ~ p(c|w) != 0
##
##  tenum => $enum,        ## previous+current targets (token) enum (T_{<=$i})
##  benum => $enum,        ## previous+current bounds (token) enum (B_{<=$i}) (targets + literals)
##  lbenum => $enum,       ## literal current bounds (type) enum (LB_{<=$i}) (e.g. bos,eos)
##  cbenum => $enum,       ## bound+class enum: includes literals (<=$i) [see {use_cbounds}]
##  cenum => $enum,        ## target-cluster enum: does NOT include literals
##  cm => $cluster_method, ## the initial MUDL::Cluster::Method subclass object used for clustering
##  tbeta => $tbeta,       ## pdl($n) : maps targets to their beta values as returned by $mp->d2pbeta()
##
##  use_cbounds=>$bool,    ## if true, stage<k clusters will be used as bounds in update(): default=true
##  svd_recompute=>$bool,  ## if true, SVD will be re-computed at each update() stage (also dep. on use_cbounds)
##  svd_reapply=>$bool,    ## if true, SVD will be applied at update() stages
##
##  nclusters => $n,       ## number of clusters to generate at (current) stage (default: $cm->{nclusters})
##
##  ##
##  ##-- previous data
##  #ctrprof => $ctr_prof,  ## ^f_{<k}($dir, $bound_cluster, $target_cluster{stage=>'<=k'})
##  curprof => $cur_prof,  ## ^f_{<k}($dir, $bound_cluster, $target_word   {stage=>'k'  })
##  ##
##  ##-- current iteration data
##  prof => $profile,      ## current profile: ^f_{$dir}($dir, $bound_word, $target_word{stage=>'<=k'})
##  stage => $i,           ## stage number
##  ##
##  ##-- more current iteration data
##  clusterids => $pdl,     ## pdl($nclusters_total) : best cluster-ids
##  tenum_k    => $enum,    ## current (token) targets only
##  tenum_ltk  => $enum,    ## previous (token) targets only
##  #tk2c => \%idmap,       ## maps current target-ids to previous cluster-ids: {$tid_k   => $cid_lek, ...}
##  #c2tk => \%idmap,       ## maps previous cluster-ids to current target-ids: {$cid_lek => $tid_k,   ...}
##  #tk2t => \%idmap,       ## maps current target-ids to global target-ids:    {$tid_k   => $tid_lek, ...}
##  #t2tk => \%idmap,       ## maps global target-ids to current target-ids:    {$tid_lek => $tid_k,   ...}
##  ugs_k => $pdl,          ## pdl($n): expected unigram pseudo-frequency (targets_k u clusters_{k-1})
##  ##
##  ##-- messages
##  verbose => $level
##  ##
##  ##-- info
##  stage_info => { ##-- stage unigram-frequency info for new targets
##           ntgs_k=>$n_new_targets,
##           avg=>$freq,
##           prms=>$stddev,
##           median=>$freq,
##           min=>$freq,
##           max=>$freq,
##           adev=>$abs_stddev,
##           rms=>$est_stddev,
##          },
## }
##
## $mp = MUDL::Corpus::MetaProfile::Attach->new(%args)
sub new {
  my ($that,%args) = @_;

  $args{method} = 'full';
  return $that->SUPER::new(
			   stage => 0,
			   verbose => $vl_default,
			   use_cbounds=>1,
			   svd_recompute=>0,
			   svd_reapply=>0,
			   nclusters=>undef,
			   %args,
			  );
}


##======================================================================
## Bootstrapping (stage = 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->bootstrap($profile1, $cm1, %args)
##  + initializes $mp->{phat}
##  + %args contains arguments for $cm1->membershipProbs()
##  + $profile1 and $cm1 may both be destructively altered!
sub bootstrap {
  my ($mp,$prof,$cm,%args) = @_;

  ##-- inherited bootstrap (::Attach)
  $mp->SUPER::bootstrap($prof,$cm,%args);

  ##-- ensure that $mp->{nclusters} is set
  if (!defined($mp->{nclusters})) {
    $mp->{nclusters} = $mp->{cm}{nclusters};
  }
  $mp->vmsg($vl_info, "bootstrap(): nclusters / stage = $mp->{nclusters}\n");

  return $mp;
}

## $beta = $mp->d2pbeta()
##  + get a value for beta constant for use with d2p_jaynes()-based methods
#(inherited)

##--------------------------------------------------------------
## $phatPdl = $mp->populatePhat(%args)
##  + populates $mp->{phat} ~ $phat->at($cid,$wid) = ^p($cid | $wid)
##    from $mp->{cm}
##  + %args:
##    - passed to $mp->{cm}->membershipProbPdl()
##  + requires data keys:
##     cbenum
##     cenum
##     cm
##  + (re-)sets data keys:
##     phat
##     phatm
##     tbeta
##
#(inherited)


##======================================================================
## Update (Stage > 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->update($profile)
##  + sets $mp->{prof} to $profile (a profile over bound- & target-words)
##  + updates $mp->{tenum} to include new targets
##  + updates $mp->{benum} to include new bounds (== old targets)
##  + updates $mp->{tenum_k} to new targets only
##  + $profile may be destructively altered!
sub update {
  my ($mp,$prof) = @_;

  ##-- update: stage-number
  ++$mp->{stage};

  ##-- update: profile
  my $lbenum = $mp->lbenum();
  $mp->vmsg($vl_info, "update(): profile ~ f_{<=k}(d, v_b, w)\n");
  $mp->vmsg($vl_info, "update(): |bounds|           = ", $prof->{bounds}->size, "\n");
  $mp->vmsg($vl_info, "update(): |literal bounds|   = ", $lbenum->size, "\n");
  $mp->vmsg($vl_info, "update(): |clustered bounds| = ", $prof->{bounds}->size - $lbenum->size, "\n");
  $mp->vmsg($vl_info, "update(): |targets|          = ", $prof->{targets}->size, "\n");
  $mp->vmsg($vl_info, "update(): |mp bounds|        = ", $mp->{cbenum}->size, "\n");
  $mp->{prof} = $prof;

  ##-- update: load modules
  MUDL::CmdUtils::loadModule(ref($prof))
      or confess(ref($mp), "::update(): could not load profile module '", ref($prof), "': $!");
  MUDL::CmdUtils::loadModule(ref($mp->{cm}))
      or confess(ref($mp), "::update(): could not load cluster module '", ref($mp->{cm}), "': $!");

  ##-- update: enums
  $mp->vmsg($vl_info, "update(): enums ~ B_<=k, T_<=k, T_k\n");
  my $tenum_ltk = $mp->{tenum_ltk} = $mp->{tenum}->copy;
  $mp->{tenum}->addEnum($prof->{targets});
  $mp->{benum}->addEnum($prof->{bounds});

  ##-- tenum_k: new targets
  my $tenum_k = $mp->{tenum_k} = MUDL::Enum->new();
  my $tk2t = $mp->{tk2t} = {};
  my $t2tk = $mp->{t2tk} = {};
  my ($tid,$tid_k);
  foreach $tid (grep {!exists($tenum_ltk->{sym2id}{$_})} (@{$prof->{targets}{id2sym}})) {
    $tid_k = $tenum_k->addSymbol($tid);
    $t2tk->{$tid}   = $tid_k;
    $tk2t->{$tid_k} = $tid;
  }
  $mp->vmsg($vl_info, "update(): ntargets_k   = ", $tenum_k->size, "\n");
  $mp->{stage_info}{ntgs_k} = $tenum_k->size;

  ##-- update: curprof
  $mp->vmsg($vl_info, "update(): curprof ~ f_{<=k}(d, c_b, t_k   )\n");
  #my ($ctrprof,$curprof) = $mp->populateProfiles($prof); ##-- attach
  my $curprof = $mp->populateProfile($prof);

  ##-- update: info & report
  my $ugs_tgs_k = $mp->{ugs_k}->slice("0:".($mp->{stage_info}{ntgs_k}-1));
  @{$mp->{stage_info}}{qw(avg prms median min max adev rms)} = map { $_->sclr } $ugs_tgs_k->stats;
  $mp->{stage_info}{logavg} = $ugs_tgs_k->log->average->exp->sclr;
  $mp->vmsg($vl_info, sprintf("update(): unigrams: min(f_k(w))=%.2f\n", $mp->{stage_info}{min}));
  $mp->vmsg($vl_info, sprintf("update(): unigrams: max(f_k(w))=%.2f\n", $mp->{stage_info}{max}));
  $mp->vmsg($vl_info, sprintf("update(): unigrams: avg(f_k(w))=%.2f\n", $mp->{stage_info}{avg}));
  $mp->vmsg($vl_info, sprintf("update(): unigrams: logavg(f_k(w))=%.2f\n", $mp->{stage_info}{logavg}));
  $mp->vmsg($vl_info, sprintf("update(): unigrams: median(f_k(w))=%.2f\n", $mp->{stage_info}{median}));


  ##-- update: cluster: T_k: toPDL
  $mp->vmsg($vl_info, "update(): T_k : toPDL()\n");
  my $data_k = $curprof->toPDL;
  #@$mp{qw(data_k mask_k weight_k tids_k)} = ($data_k, $mask_k, $weight_k, $tids_k); ##-- DEBUG

  ##-- update: cluster method
  $mp->vmsg($vl_info, "update(): cm_k()\n");
  my $cm   = $mp->{cm};
  my $cm_k = $mp->{cm_k} = $cm->shadow(class=>'MUDL::Cluster::Method',enum=>$mp->{tenum_k});

  ##-- update: cluster method: SVD
  $mp->vmsg($vl_info, "        : SVD: present   ? ", ($cm->{svd} ? 'yes' : 'no'), "\n");
  if ($cm->{svd}) {
    ##
    ##-- SVD: reapply?
    $mp->vmsg($vl_info, "        : SVD: reapply   ? ", ($mp->{svd_reapply} ? 'yes' : 'no'), "\n");
    if ($mp->{svd_reapply}) {
      $mp->vmsg($vl_info, "        : SVD: recompute ? ", ($mp->{svd_recompute} ? 'yes' : 'no'), "\n");
      ##
      ##-- SVD: recompute?
      my $svd_recompute = $mp->{svd_recompute};
      if (!$svd_recompute && $cm->{svd}{v}->dim(1) != $data_k->dim(0)) {
	$mp->vmsg($vl_info, "        : SVD: recompute: dimension mismatch:\n");
	$mp->vmsg($vl_info, "        :  data(d)=", $data_k->dim(0), " != v_svd(d)=", $cm->{svd}{v}->dim(1), "\n");
	$mp->vmsg($vl_info, "        : -> recompute forced!\n");
	$svd_recompute=1;
      }
      $cm_k->{svd_save} = !$svd_recompute;
      $mp->vmsg($vl_info, "        : SVD: r         = $cm_k->{svd}{r}\n");
    } else {
      delete($cm_k->{svd});
    }
  }

  ##-- update: cluster: data setup
  $mp->vmsg($vl_info, "update(): cluster: data()\n");
  $data_k = $cm_k->data($data_k); ##-- grab return value b/c $cm_k->data() might apply an SVD
  $cm_k->{mask}   = ones(long,   $data_k->dims  );
  $cm_k->{weight} = ones(double, $data_k->dim(0));

  ##-- update: cluster: cluster()
  $mp->vmsg($vl_info, "update(): cluster: cluster()\n");
  $mp->vmsg($vl_info, "                   + data dims = ", join(',', $data_k->dims), "\n");
  $cm_k->cluster();
  $mp->vmsg($vl_info, "update(): cluster: cut(k=$mp->{nclusters})\n");
  $cm_k->cut($mp->{nclusters});

  ##-- update: potentially apply hard-clustering bonus (?)
  $mp->vmsg($vl_info, "update(): cluster: clusterDistanceMatrix()\n");
  my $cdm_k = $cm_k->clusterDistanceMatrix();
  if ($cm_k->{cdmethod} =~ /\+bb/) {
    my $cemask = $cm_k->clusterElementMask();
    $cdm_k->where($cemask) .= 0;
  }

  #-- update: cm
  $mp->vmsg($vl_info, "update(): cm\n");
  $mp->updateCm();

  ##-- update: phat
  $mp->vmsg($vl_info, "update(): phat ~ ^p( c_{k} | w_{<=k} )\n");
  $mp->updatePhat();

  return $mp;
}

##--------------------------------------------------------------
## $curprof = $mp->populateProfile()
## $curprof = $mp->populateProfile($prof)
##  + populates
##      $mp->{curprof} ~ f_{k}(z, c_b \in C_{k-1}, w \in T_k     )
##    from
##      $prof          ~ f_{k}(z, v_b \in T_{<k} , w \in T_{<=k} )
##    and
##      $mp->{phat}    ~ ^p_{k}(c_b \in C_{k-1} | v_b \in T_{<k})
##  + requires:
##    - ???
##  + basically just calls addProfileDist() for each $dir qw(left right)
##  + $ctrprof may be undef, in which case it's ignored
*populateProfiles = \&populateProfile;
sub populateProfile {
  my ($mp,$prof) = @_;
  $prof = $mp->{prof} if (!defined($prof));

  ##-- profiles: step 0: copy profile (hack: shadow distributions)
  my $ctrprof = undef; ##-- we don't need to re-profile centers here
  my $curprof = $mp->{curprof} = $prof->shadow();

  ##-- profiles: step 0: allocate unigram dist
  $mp->{ugs_k} = zeroes(double, $mp->{tenum_k}->size);

  ##-- profiles: step 1: tweak profile neighbor-distributions
  $mp->updateProfileDists($prof->{left},  undef,  $curprof->{left});
  $mp->updateProfileDists($prof->{right}, undef,  $curprof->{right});
  $mp->{ugs_k}  /= 2; ##-- factor out l,r doubling

  ##-- profiles: step 1b: update unigram dist(s)
  $mp->updateProfileUnigramDists($prof, undef, $curprof);

  ##-- profiles: step 2a: replace enums: curprof
  foreach (@{$curprof->{enum}{enums}}) {
    $_ = $mp->{cbenum}  if ($_ eq $curprof->{bounds});
    $_ = $mp->{tenum_k} if ($_ eq $curprof->{targets});
  }
  $curprof->{bounds}  = $mp->{cbenum};
  $curprof->{targets} = $mp->{tenum_k};

  ##-- profiles: step 2b: replace enums: ctrprof
  # (not needed)

  return $curprof;
}


##--------------------------------------------------------------
## undef = $mp->updateProfileUnigramDists($rawProfile, $ctrProfile, $curProfile)
##  + updates 'tugs', 'bugs', and 'ftotal' for each of $ctrProfile, $curProfile
##  + uses $mp->{tenum_k}, $mp->{cbenum}, $mp->{cenum}
##  + $ctrProfile may be undef, in which case it's ignored
#
#(inherited)

##--------------------------------------------------------------
## undef = $mp->updateProfileDists($rawDistNaryDir, $ctrDistNaryDir, $curDistNaryDir)
##  + populates $ctrDistNaryDir and $curDistNaryDir from $rawDistNaryDir, where:
##    - $rawDistNaryDir : prof directed nary dist wrt targets (\in T_<=k) & bound-words   (\in     (LB u T_<k ))
##    - $ctrDistNaryDir : prof directed nary dist wrt classes (\in C_k-1) & bound-classes (\in B_k=(LB u C_k-1))
##    - $curDistNaryDir : prof directed nary dist wrt targets (\in T_k  ) & bound-classes (\in B_k=(LB u C_k-1))
##  + output distributions should be subsequently changed to use enums:
##    - $ctrDistNaryDir : targets=$mp->{cenum}   ; bounds=$mp->{cbenum}, lbounds=$mp->{lbenum}
##    - $curDistNaryDir : targets=$mp->{tenum_k} ; bounds=$mp->{cbenum}, lbounds=$mp->{lbenum}
##  + requires:
##    ~ $mp->{tenum_k}
##    ~ $mp->{clusterids}
##    ###~ $mp->{c2tk}
##  + $ctrDistNaryDir may be undef, in which case it's ignored
#
#(inherited)

##--------------------------------------------------------------
## $beta_pdl = $mp->beta()
##  + for pinsker dist-to-probability conversion
#(inherited)

##--------------------------------------------------------------
## $phatPdl = $mp->updatePhat(%args)
##  + populates $mp->{phat} ~ $phat->at($cid,$wid) = ^p($cid | $wid)
##    from $mp->{cm_k} and $mp->{phat} (last iteration)
##  + %args:
##    - passed to $mp->{cm_k}->membershipProbPdl()
##  + needs:
##    $mp->{phat}        ## ^p_{<k}
##    $mp->{tenum_ltk}   ## T_{<k}
sub updatePhat {
  my $mp = shift;

  ##-- get ^p_{<k}( c_{k-1} | t_{<k} )  ~ previous iteration
  $mp->vmsg($vl_info, "updatePhat(): ^p_{<k}( c_{k-1} | t_{<k} )\n");
  my $phat_ltk  = $mp->{phat};
  my $phatm_ltk = $mp->{phatm};

  ##-- get id-translation pdl: [$id_Tk] = $id_Tlek
  my $tk2t = pdl(long, [ @{$mp->{tenum}{sym2id}}{ @{$mp->{tenum_k}{id2sym}} } ]);

  ##-- get ^p_k( c_k | t_k + c_{k-1} )  ~ current iteration
  my $beta = $mp->beta;
  $mp->vmsg($vl_info, "updatePhat(): avg(beta) = ", $beta->avg, "\n");

  $mp->vmsg($vl_info, "updatePhat(): membershipProbPdl ~ ^p_k( c_k | t_k + c_{k-1} )\n");
  $mp->vmsg($vl_info, "            :         betamode  = ", $mp->{betamode}, "\n");
  my $cm_k   = $mp->{cm_k};
  my $phat_k = zeroes(double, $cm_k->{nclusters}, $mp->{tenum_k}->size);
  my $d2pbeta = $mp->d2pbeta();
  $cm_k->membershipProbPdl(@_,
			   d2pbeta=>$d2pbeta,
			   r2cprobs=>$phat_k,
			  );


  ##-- get stage-k n-best membership mask
  my $phatm_k = $cm_k->membershipSimMask($phat_k, d2pbeta=>$d2pbeta);

  ##-- save stage-k betas
  $mp->{tbeta}->reshape($mp->{tbeta}->dim(0) + $phat_k->dim(1));
  $mp->{tbeta}->slice("-".$phat_k->dim(1).":-1") .= $d2pbeta;

  ##-- allocate new ^p_{<=k}()
  $mp->vmsg($vl_info, "updatePhat(): adjust ~ ^p_{<=k}( c_k | t_{<=k} )\n");
  my $phat  = $mp->{phat}  = zeroes(double, $mp->{cenum}->size, $mp->{tenum}->size);
  my $phatm = $mp->{phatm} = zeroes(byte,   $phat->dims);

  ##----------------------------------------------
  ## phat: create: ^p_{<=k} = p_k u p_{<k}

  ##-- phat: create: adopt old targets
  $mp->vmsg($vl_info, "updatePhat(): add<k ~ ^p_{<=k} u= ^p_{<k}\n");
  my $tenum     = $mp->{tenum};
  my $tenum_ltk = $mp->{tenum_ltk};
  my $tenum_k   = $mp->{tenum_k};
  my $nC_ltk    = $phat_ltk->dim(0);
  $phat->slice ("0:".($phat_ltk->dim(0)-1).",0:".($phat_ltk->dim(1)-1)) .= $phat_ltk;
  $phatm->slice("0:".($phat_ltk->dim(0)-1).",0:".($phat_ltk->dim(1)-1)) .= $phatm_ltk;

  ##-- phat: create: add new targets
  $mp->vmsg($vl_info, "updatePhat(): add=k ~ ^p_{<=k} u= t_k\n");
  $phat->slice ("-".($phat_k->dim(0)).":-1,:")->dice_axis(1,$tk2t) .= $phat_k;
  $phatm->slice("-".($phat_k->dim(0)).":-1,:")->dice_axis(1,$tk2t) .= $phatm_k;

  $mp->{phat}  = $phat;
  $mp->{phatm} = $phatm;
  return $phat;
}


##--------------------------------------------------------------
## $cm = $mp-updateCm(%args)
##  + updates $mp->{cm} from $mp->{cm_k} and $mp->{cm_ltk}
##  + %args:
##    - tk2t => $t2tk_pdl # $tk2t_pdl->index($id_tenum_k) = $id_tenum
##  + needs:
##    $mp->{cm_k}
##    $mp->{cm_ltk}
sub updateCm {
  my ($mp,%args) = @_;
  my $tk2t = $args{tk2t};

  ##-- report
  $mp->vmsg($vl_info, "updateCm()\n");

  ##-- enums
  my $tenum   = $mp->{tenum};
  my $tenum_k = $mp->{tenum_k};
  my $cenum   = $mp->{cenum};
  my $cbenum  = $mp->{cbenum};

  ##-- id-translation pdl: [$id_Tk] = $id_Tlek
  $tk2t = pdl(long, [ @{$mp->{tenum}{sym2id}}{ @{$mp->{tenum_k}{id2sym}} } ])
    if (!defined($tk2t));

  ##-- update: cm: create new cm
  $mp->vmsg($vl_info, "updateCm(): cm\n");
  my $cm_ltk = $mp->{cm_ltk} = $mp->{cm};
  my $cm_k   = $mp->{cm_k};
  my $cm     = $mp->{cm} = $cm_ltk->shadow(class=>'MUDL::Cluster::Method',enum=>$mp->{tenum});

  ##-- update: cm: clusterids
  $mp->vmsg($vl_info, "updateCm(): clusterids\n");
  my $nC_ltk   = $cenum->size;
  my $cids_ltk = $cm_ltk->{clusterids};
  my $cids_k   = $cm_k->{clusterids};
  my $cids     = zeroes(long, $cids_ltk->nelem + $cids_k->nelem) -1; ##-- (-1) for sanity check

  $cids->slice("0:".($nC_ltk-1)) .= $cids_ltk;
  $cids->index($tk2t)            .= $cids_k + $nC_ltk;

  ##-- update: cluster enum
  $mp->vmsg($vl_info, "updateCm(): cluster enum(s)\n");
  delete($cm_k->{cenum});
  my $cenum_k = $cm_k->clusterEnum(offset=>$nC_ltk);
  my ($cid,$cstr);
  foreach $cid (0..($cenum_k->size-1)) {
    $cstr = $cenum_k->{id2sym}[$cid];
    $cenum->addIndexedSymbol($cstr,$cid);
    $cbenum->addIndexedSymbol($cstr,$cid) if ($mp->{use_cbounds});
  }

  ##-- populate cm: dimensions
  $cm->{nfeatures} = $mp->{cbenum}->size;
  $cm->{ndata}     = $cids->nelem;
  $cm->{nclusters} = $cenum->size;

  ##-- populate cm: cluster distance matrix
  ## + remind me: why do we need to maintain this? --> may be related to toHMM() method...
  $mp->vmsg($vl_info, "updateCm(): cluster distance matrix\n");
  if (defined($cm_ltk->{cdmatrix})) {
    ##-- setup new $cm->{cdmatrix}
    my $cdm = $cm->{cdmatrix} = zeroes(double, $cm->{nclusters}, $cm->{ndata});
    $cdm
      .= pdl(double,0)->setvaltobad(0);               ##-- use PDL 'BAD' for missing values
      #.= pdl(double(0))->setvaltobad(0)->setvaltonan; ##-- use PDL 'nan' for missing values
      #.= pdl(double(0))**-1;                          ##-- use PDL 'inf' for missing values

    ##-- copy values: d_{<=k}(c,w)
    $cdm->slice("0:".($nC_ltk-1).",0:".($cm_ltk->{cdmatrix}->dim(1)-1)) .= $cm_ltk->{cdmatrix};

    ##-- copy values. d_k(c,w)
    my $cdm_k = $cm_k->clusterDistanceMatrix();
    $cdm->slice($nC_ltk.":-1,")->dice_axis(1,$tk2t) .= $cdm_k;
  }

  ##-- return
  return $mp->{cm} = $cm;
}


##======================================================================
## Re-clustering (Stage >= 1)
##======================================================================

## $mp = $mp->recluster()
## $mp = $mp->recluster($prof)
#
#(not yet implemented)


##======================================================================
## Useful Summary Information
##======================================================================
## \%infoHash = $mp->getSummaryInfo()
sub getSummaryInfo {
  my $mp = shift;
  my $info = {};

  $info->{stage} = $mp->{stage};

  $info->{d2p_n} = $mp->{cm}{d2pn};
  $info->{d2p_method} = $mp->{cm}{d2pmethod};
  $info->{cddist} = $mp->{cm}->cddist;
  $info->{cdmethod} = $mp->{cm}->cdmethod;

  $info->{prfType} = ref($mp->{prof});
  $info->{nTargets} = $mp->{tenum}->size;
  $info->{nBounds} = $mp->{benum}->size;
  $info->{nBoundsLiteral} = $mp->lbenum()->size;
  $info->{nClusters} = $mp->{cenum}->size;
  $info->{nClustersAndBounds} = $mp->{cbenum}->size;
  $info->{nTargets_k} = (defined($mp->{tenum_k})
			 #? ($mp->{tenum_k}->size - $info->{nClusters})
			 ? ($mp->{tenum_k}->size)
			 : $info->{nTargets});

  ##-- unigram freq info
  my $ugs = $mp->{ugs_k}->slice("0:".($info->{nTargets_k}-1));
  @$info{qw(ugk_avg ugk_prms ugk_median ugk_min ugk_max ugk_adev ugk_rms)} = map { $_->sclr } $ugs->stats;

  return bless($info,'MUDL::Object');
}



##======================================================================
## Viewing: Tree
##======================================================================

##-- inherited

1;

##======================================================================
## Docs
=pod

=head1 NAME

MUDL - MUDL Unsupervised Dependency Learner

=head1 SYNOPSIS

 use MUDL;

=cut

##======================================================================
## Description
=pod

=head1 DESCRIPTION

...

=cut

##======================================================================
## Footer
=pod

=head1 ACKNOWLEDGEMENTS

perl by Larry Wall.

=head1 AUTHOR

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
