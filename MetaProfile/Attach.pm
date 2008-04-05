##-*- Mode: CPerl -*-

## File: MUDL::Corpus::MetaProfile::Attach.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: multi-stage meta-profile:
##    : attach new targets to old clusters
##======================================================================

package MUDL::Corpus::MetaProfile::Attach;
use MUDL::Corpus::MetaProfile qw(:vlevels);
use PDL;
use PDL::Cluster;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::MetaProfile);

#our $LONG_CLUSTER_LABELS = 1; ##-- provide full cluster labels on itermediate tree

##======================================================================
## Corpus::MetaProfile::Attach: Constructor
## {
##  ##
##  ##-- global data
##  phat   => $phat,        ## dims: ($NClusters, $NPrevTargets) : $phat->at($cid,$tid) = ^p_{<=$i}($cid|$tid)
##  phatm  => $mask,        ## type=byte,   dims as for $phat; $phatm ~ p(c|w) != 0
##  stg2ntgs => $stg2ntgs,  ## pdl(byte,$nstages) : $stg2ntgs->at($stg) = $number_of_targets_at_stage
##                          ##  + s.t. $tgs2stg == rld($stg2ntgs, 1+$stg2ntgs->sequence)
##                          ##    since $tid_i<=$tid_j implies firstStage($tid_i)<=firstStage($tid_j)
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
##  ##
##  ##-- previous data
##  ctrprof => $ctr_prof,  ## ^f_{<k}($dir, $bound_cluster, $target_cluster{stage=>'<=k'})
##  curprof => $cur_prof,  ## ^f_{<k}($dir, $bound_cluster, $target_word   {stage=>'k'  })
##  ##
##  ##-- current iteration data
##  prof => $profile,      ## current profile: ^f_{$dir}($dir, $bound_word, $target_word{stage=>'<=k'})
##  stage => $i,           ## stage number
##  ##
##  ##-- more current iteration data
##  clusterids => $pdl,    ## pdl($n) : best cluster-ids
##  tenum_k => $enum,      ## current (token) targets only
##  tenum_ltk => $enum,    ## previous (token) targets only
##  #tk2c => \%idmap,       ## maps current target-ids to previous cluster-ids: {$tid_k   => $cid_lek, ...}
##  #c2tk => \%idmap,       ## maps previous cluster-ids to current target-ids: {$cid_lek => $tid_k,   ...}
##  tk2t => $xlpdl,        ## maps current target-ids to global target-ids:    $tid_k   => $tid_lek
##  #t2tk => $xlpdl,        ## maps global target-ids to current target-ids:    $tid_lek => $tid_k_or_-1
##  ugs_k  => $pdl,        ## pdl($ntgs_k): empirical unigram frequency for T_k
##  ugs_kz => $pdl,        ## pdl($ntgs_k): expected co-occurrence frequency for T_k
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
  $mp->vmsg($vl_info, "bootstrap().\n");

  ##-- set base data
  $mp->{prof}   = $prof;
  $mp->{cm}     = $cm;
  $mp->{stage}  = 1;

  ##-- load modules
  MUDL::CmdUtils::loadModule(ref($prof))
      or confess(ref($mp), "::bootstrap(): could not load profile module '", ref($prof), "': $!");
  MUDL::CmdUtils::loadModule(ref($cm))
      or confess(ref($mp), "::bootstrap(): could not load cluster module '", ref($cm), "': $!");

  ##-- initialize: bos,eos
  @$mp{qw(bos eos)} = @$prof{qw(bos eos)};

  ##-- sanitize 'use_cbounds' (default)
  $mp->{use_cbounds} = 1 if (!defined($mp->{use_cbounds}));

  ##-- initialize enums
  $mp->{tenum}  = $prof->{targets};
  $mp->{benum}  = $prof->{bounds}->copy;
  $mp->{cenum}  = $cm->clusterEnum()->copy;
  $mp->{cbenum} = ($mp->{use_cbounds} ? $mp->{cenum}->copy : MUDL::Enum->new());
  ##
  ##-- literal bounds: add to {cbenum}, {benum}
  $mp->{lbenum} = $mp->lbenum();             ##-- auto-generates if undefined
  $mp->{cbenum}->addEnum($mp->{lbenum});
  $mp->{benum }->addEnum($mp->{lbenum});

  ##-- initialize: stg2ntgs
  $mp->{stg2ntgs} = pdl(long,[$mp->{tenum}->size]);

  ##-- info: enums
  $mp->vmsg($vl_info, sprintf("bootstrap(): |targets|     = %6d\n", $mp->{tenum}->size));
  $mp->vmsg($vl_info, sprintf("bootstrap(): |bounds|      = %6d\n", $mp->{benum}->size));
  $mp->vmsg($vl_info, sprintf("bootstrap(): |lit. bounds| = %6d\n", $mp->{lbenum}->size));
  $mp->vmsg($vl_info, sprintf("bootstrap(): |clusters|    = %6d\n", $mp->{cenum}->size));
  #$mp->vmsg($vl_info, sprintf("bootstrap(): |any bounds|  = %6d\n", $mp->{cbenum}->size)); ##--n/a here

  ##-- populate {clusterids}: word => argmax_class ( p(class|word) )
  $mp->vmsg($vl_info, "bootstrap(): clusterids ~ w => argmax_c ( p(c|w) )\n");
  $mp->{clusterids} = $cm->{clusterids};

  ##-- populate ugs_k, ugs_kz
  $mp->vmsg($vl_info, "bootstrap(): unigrams ~ f_0(w)\n");
  $mp->{ugs_k}  = $prof->targetUgPdl();
  $mp->{ugs_kz} = ($prof->{pleft}{pdl}->xchg(0,1)->sumover->todense
		   + $prof->{pright}{pdl}->xchg(0,1)->sumover->todense)->double / 2;

  ##-- populate {stage_info}
  $mp->{tenum_k}    = $mp->{tenum};
  $mp->{stage_info} = $mp->getStageInfo($mp->{stage_info});

  ##-- populate {phat}: p(class|word)
  $mp->vmsg($vl_info, "bootstrap(): phat ~ ^p(c|w)\n");
  $mp->populatePhat(%args);

  ##-- initialize $mp->{cm_lek} (dummy)
  #$mp->{cm_lek} = $cm->shadow(class=>'MUDL::Cluster::Method');

  return $mp;
}

## $beta = $mp->d2pbeta()
##  + get a value for beta constant for use with d2p_jaynes()-based methods
sub d2pbeta {
  my $mp = shift;
  $mp->{betamode} = 1 if (!defined($mp->{betamode}));

  if ($mp->{betamode} =~ /^[\+\-]?[\d+\.]+$/) {
    return pdl(double,$mp->{betamode});
  }
  elsif ($mp->{betamode} eq 'freq') {
    return $mp->beta;
  }
  elsif ($mp->{betamode} eq 'avg') {
    return $mp->beta->flat->average;
  }
  elsif ($mp->{betamode} eq 'stage') {
    return pdl(double, $mp->{stage});
  }
  elsif ($mp->{betamode} eq 'bstage') {
    return pdl(double, $mp->{stage})*$mp->{cm}{d2pb};
  }
  elsif ($mp->{betamode} eq 'istage') {
    return pdl(double, $mp->{stage})**-1;
  }
  elsif ($mp->{betamode} eq 'bistage') {
    return pdl(double, $mp->{cm}{d2pb}*$mp->{stage})**-1;
  }
  ##... ?
  ##-- default
  return 1.0;
}

##--------------------------------------------------------------
## \%stage_info = $mp->getStageInfo()
## \%stage_info = $mp->getStageInfo(\%stage_info)
##  + requires: $mp->{tenum_k}, $mp->{ugs_k}, $mp->{ugs_kz}
##  + can be used to populate $mp->{stage_info}
sub populateStageInfo { $_[0]{stage_info} = $_[0]->getStageInfo(); }
sub getStageInfo {
  my ($mp,$stage_info) = @_;
  $stage_info = {} if (!defined($stage_info));
  $stage_info->{ntgs_k} = $mp->{tenum_k}->size;

  $mp->vmsg($vl_info, "populateStageInfo(): unigrams / co-occs ~ f_z(w) / f_0(w) : w \\in T_k\n");
  @$stage_info{qw(avg prms median min max adev rms)}        = map { $_->sclr } $mp->{ugs_k}->stats;
  @$stage_info{qw(zavg zprms zmedian zmin zmax zadev zrms)} = map { $_->sclr } $mp->{ugs_kz}->stats;
  $stage_info->{logavg}  = $mp->{ugs_k}->log->average->exp->sclr;
  $stage_info->{zlogavg} = $mp->{ugs_kz}->log->average->exp->sclr;

  $mp->vmsg($vl_info, sprintf("       min = %7.2f / %7.2f (%6.2f %%)\n", stageInfoValues($stage_info,'min')));
  $mp->vmsg($vl_info, sprintf("       max = %7.2f / %7.2f (%6.2f %%)\n", stageInfoValues($stage_info,'max')));
  $mp->vmsg($vl_info, sprintf("       avg = %7.2f / %7.2f (%6.2f %%)\n", stageInfoValues($stage_info,'avg')));
  $mp->vmsg($vl_info, sprintf("    logavg = %7.2f / %7.2f (%6.2f %%)\n", stageInfoValues($stage_info,'logavg')));
  $mp->vmsg($vl_info, sprintf("    median = %7.2f / %7.2f (%6.2f %%)\n", stageInfoValues($stage_info,'median')));

  return $stage_info;
}
sub stageInfoValues {
  my ($info,$key) = @_;
  return ($info->{"z$key"}, $info->{"$key"}, ($info->{$key}==0 ? 0 : (100*$info->{"z$key"}/$info->{$key})));
}
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
sub populatePhat {
  my $mp = shift;

  my $cbenum  = $mp->{cbenum};
  my $cenum   = $mp->{cenum};
  #my $phat    = zeroes(double, $cbenum->size, $mp->{tenum}->size);
  my $phat    = zeroes(double, $cenum->size, $mp->{tenum}->size);
  my $beta    = $mp->beta;
  my $d2pbeta = $mp->d2pbeta();
  $mp->vmsg($vl_info, "populatePhat(): avg(beta) = ", $beta->davg, "\n");
  $mp->vmsg($vl_info, "populatePhat(): betamode  = ", $mp->{betamode}, "\n");

  $mp->{cm}->membershipProbPdl(@_,
			       ##-- NOTE: this key should be 'd2pbeta', but
			       ##   the default (beta=ones(double,$cdm->dim(1)))
			       ##   seems to work quite well, giving us:
			       ##     p(c|w) = b^(-1 * dist(c,w)) / sum ...
			       ##   TODO:
			       ##     try varying beta with STAGE:
			       ##     - low  beta [<1] --> high-entropy result dist (very uniform) ~ early targets
			       ##     - high beta [>1] --> low-entropy  result dist (very certain) ~ later targets
			       ##   BUT:
			       ##     - combination 
			       ##         high beta ~ low entropy  @ EARLY targets
			       ##         low beta  ~ high entropy @ LATE targets
			       ##       seems to work BETTER!
			       ##   DIFFERENCE between 'pinskerL1' and 'pinskerD'='jaynes' methods:
			       ##     - 'pinskerL1' is lower-entropy for the same beta
			       ##       --> b/c it squares distance
			       #beta=>$beta,
			       ##--
			       d2pbeta=>$d2pbeta,
			       ##--
			       r2cprobs=>$phat,
			       ##
			       ##-- for hard-clustering bonus
			       cdbonus=>1, ##-- bonus is available here
			      );

  delete(@{$mp->{cm}}{qw(d2pbeta)}); ##-- cleanup

  ##-- no bonus after initial stage (?)
  $mp->{cm}{cdbonus} = 0 if ($mp->{cm}{cdmethod} !~ /\+bb/);

  ##-- save n-best membership mask
  $mp->{phatm} = $mp->{cm}->membershipSimMask($phat, d2pbeta=>$d2pbeta);

  ##-- save beta (initial)
  $mp->{tbeta}  = zeroes(double, $phat->dim(1));
  $mp->{tbeta} .= $d2pbeta;

  ##-- class-probability membership hack: literals
  #my $lbenum = $mp->lbenum();
  #foreach (@{$lbenum->{id2sym}}) {
  #  $phat->slice($cbenum->{sym2id}{$_}.",") .= 0;
  #}

  #@$mp{qw(phat shatr)} = ($phat,$shatr);
  $mp->{phat} = $phat;
  return $phat;
}


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
  @{$tenum_k->{id2sym}}  = grep {!exists($tenum_ltk->{sym2id}{$_})} @{$prof->{targets}{id2sym}};
  $tenum_k->{sym2id}{ $tenum_k->{id2sym}[$_] } = $_ foreach (0..$#{$tenum_k->{id2sym}});

  my $tk2t = $mp->{tk2t} = $tenum_k->xlatePdlTo($mp->{tenum},badval=>-1); ##-- $tk2t->at($tid_k  ) = $tid_lek
  #my $t2tk = $mp->{t2tk} = $mp->{tenum}->xlatePdlTo($tenum_k,badval=>-1); ##-- $t2tk->at($tid_lek) = $tid_k_or_-1
  ##-- it should be the case that: all( $tk2t == sequence($tk2t->nelem)+256 )

  ##-- update: stg2ntgs
  $mp->{stg2ntgs} = $mp->{stg2ntgs}->reshape($mp->{stage});
  $mp->{stg2ntgs}->set($mp->{stage}-1, $tenum_k->size);

  ##-- update: pprof (pdl-ized)
  ##  + also updates $mp->{ugs_k}, $mp->{ugs_kz}
  $mp->vmsg($vl_info, "update(): ctrprof ~ f_{<=k}(d, c_b, c_{<k})\n");
  $mp->vmsg($vl_info, "        : curprof ~ f_{<=k}(d, c_b, t_k   )\n");
  my ($ctrprof,$curprof) = $mp->populateProfiles($prof);

  ##-- update: info & report
  $mp->{stage_info} = $mp->getStageInfo({});

  ##-- update: cluster: C_{k-1}: toPDL
  $mp->vmsg($vl_info, "update(): C_{k-1} : toPDL\n");
  my $cm    = $mp->{cm};
  my $cdata = $ctrprof->toPDL;
  my $cmask = ones(long, $cdata->dims);
  @$mp{qw(cdata cmask)} = ($cdata,$cmask);  ##-- DEBUG

  ##-- update: cluster: T_k: toPDL
  $mp->vmsg($vl_info, "update(): T_k : toPDL()\n");
  my $data_k   = $curprof->toPDL;
  #@$mp{qw(data_k mask_k weight_k tids_k)} = ($data_k, $mask_k, $weight_k, $tids_k); ##-- DEBUG

  ##-- update: cluster method
  $mp->vmsg($vl_info, "update(): cm_k()\n");
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

  ##-- construct targets+centers data pdl: NEW_TARGETS . OLD_CLUSTERS
  my $cmdata_k = zeroes(double, $data_k->dim(0), $data_k->dim(1)+$cdata->dim(1));
  my $trowids  = sequence(long, $data_k->dim(1));
  my $crowids  = sequence(long, $cdata->dim(1)) + $trowids->nelem;
  $cmdata_k->dice_axis(1, $trowids) .= $data_k;
  $cmdata_k->dice_axis(1, $crowids) .= $cdata;

  ##-- data/SVD stuff
  $cmdata_k = $cm_k->data($cmdata_k); ##-- grab return value b/c $cm_k->data() might apply SVD
  $cm_k->{mask}   = $cmdata_k->isgood;
  $cm_k->{weight} = ones(double, $cmdata_k->dim(0));

  ##-- update: cluster: cm_k: row-probabilities
  if ($ctrprof->can('targetUgPdl') && $curprof->can('targetUgPdl')) {
    $mp->vmsg($vl_info, "update(): cluster: data (row) weights()\n");
    $cm_k->{dataweights} = zeroes(double, $cmdata_k->dim(1));
    $cm_k->{dataweights}->index($crowids) .= $ctrprof->targetUgPdl();
    $cm_k->{dataweights}->index($trowids) .= $curprof->targetUgPdl();
  }

  my $UPDATE_ATTACH_OLD=0;
  if ($UPDATE_ATTACH_OLD) {
    ##-- update:attach:old: cluster: clusterDistanceMatrix()
    $mp->vmsg($vl_info, "update(): clusterDistanceMatrix()\n");
    $mp->vmsg($vl_info, "        : dims = ", join(' ', $cmdata_k->dims), "\n");
    my ($csizes,$celtids,$cdm);
    PDL::Cluster::clusterdistancematrix(@$cm_k{qw(data mask weight)},
					$trowids,
					($csizes=ones(long, $cdata->dim(1))),
					($celtids=$crowids->slice("*1,")),
					($cdm=$cm_k->{cdmatrix}=zeroes(double, $cdata->dim(1), $trowids->dim(0))),
					$cm_k->cddist, $cm_k->cdmethod);

    ##-- update:attach:old: cluster: attachtonearest()
    $mp->vmsg($vl_info, "update(): attachtonearestd()\n");
    my $cids_k  = $cm_k->{clusterids} = zeroes(long, $trowids->nelem);
    my $cdist_k = zeroes(double, $trowids->nelem);
    PDL::Cluster::attachtonearestd($cdm, $trowids, $cids_k, $cdist_k);
  }
  else {
    ##-- update:attach:new: cluster: clusterDistanceMatrix()
    $mp->vmsg($vl_info, "update(): attach()\n");
    $mp->vmsg($vl_info, "        : GU data dims = ", join(' ', $cmdata_k->dims), "\n");

    my ($cids_k,$cdist_k,$cdmat_k) = $cm_k->attach
      (
       tpdata => $cdata,
       tpmask => $cdata->isgood,
       tpcids => sequence(long,$cdata->dim(1)), ##-- $crowids-$trowids->nelem
       adata  => $data_k,
       amask  => $data_k->isgood,
       arows  => $trowids,
       adopt  => 0,
      );

    ##-- update:attach:new: cluster: cluster ids() [HACK]
    $cm_k->{clusterids} = $cids_k;
    $cm_k->{cdmatrix}   = $cdmat_k;
  }

  ##-- update:attach:common: apply hard-clustering bonus (?)
  if ($cm_k->{cdbonus}) {
    my $cemask = $cm_k->clusterElementMask;
    $cdm->where($cemask) .= 0;
  }

  ##-- update:attach:common: cm
  $mp->vmsg($vl_info, "update(): cm\n");
  $mp->updateCm();


  ##-- update: phat (pdl-ized)
  $mp->vmsg($vl_info, "update(): phat ~ ^p( c_{k} | w_{<=k} )\n");
  $mp->updatePhat();

  return $mp;
}

##--------------------------------------------------------------
## ($ctrprof,$curprof) = $mp->populateProfiles()
## ($ctrprof,$curprof) = $mp->populateProfiles($prof)
##  + populate
##      $mp->{ctrprof} ~ f_{k}(z, c_b \in C_{k-1}, c \in C_{k-1} )
##    and
##      $mp->{curprof} ~ f_{k}(z, c_b \in C_{k-1}, w \in T_k     )
##    from
##      $prof          ~ f_{k}(z, v_b \in T_{<k} , w \in T_{<=k} )
##    and
##      $mp->{phat}   ~ ^p_{k}(c_b \in C_{k-1} | v_b \in T_{<k})
##  + requires:
##    ~ Bounds_{stage=$i} \subseteq Bounds_{stage=$i+1}, \forall i, 1 <= $i <= $mp->{stage}
##    ~ $mp->{tenum_k} ????
##    ~ anything required by xlateBoundsPdl,xlateTargetPdls,updateProfileDists
##  + basically just calls addProfileDist() for each $dir qw(left right)
sub populateProfiles {
  my ($mp,$prof) = @_;
  $prof = $mp->{prof} if (!defined($prof));

  ##-- profiles: step 0a: copy source profile (hack: shadow distributions)
  my $ctrprof = $mp->{ctrprof} = $prof->shadow();
  my $curprof = $mp->{curprof} = $prof->shadow();

  ##-- map ids (profile->mp)
  ##  + $pb2cb : ($NProfileBounds , $NMetaProfileBounds)
  ##  + $pt2c  : ($NProfileTargets, $NMetaProfileClusters)
  ##  + $pt2tk : ($NProfileTargets, $NTargets_k)
  my $pb2cb         = $mp->{pb2cb}        = $mp->xlateBoundsPdl($prof->{bounds});
  my ($pt2c,$pt2tk) = @$mp{qw(pt2c pt2tk)} = $mp->xlateTargetPdls($prof->{targets});

  ##-- update underlying profile (sparse) pdls
  $mp->updateProfileDists($prof->{pleft},  $ctrprof->{pleft},  $curprof->{pleft});
  $mp->updateProfileDists($prof->{pright}, $ctrprof->{pright}, $curprof->{pright});

  ##-- update ugs_k, ugs_kz
  my $pt2tk_xi = $mp->{pt2tk_xi};
  my $pt2tk_xw = $mp->{pt2tk_xw};
  my $ugs_p = $prof->targetUgPdl;
  my $ugs_k = $mp->{ugs_k} = zeroes(double,$mp->{tenum_k}->size);
  $ugs_k->index($pt2tk_xi->index($pt2tk_xw)) .= $ugs_p->index($pt2tk_xw);
  ##
  my $ugs_kz = $curprof->{pleft}{pdl}->xchg(0,1)->sumover->todense;
  $ugs_kz   += $curprof->{pright}{pdl}->xchg(0,1)->sumover->todense;
  $mp->{ugs_kz} = $ugs_kz->float / 2;

  ##-- profiles: update unigram dists
  $mp->updateProfileUnigramDists($prof, $ctrprof, $curprof);

  ##-- profiles: replace enums: curprof
  foreach (@{$curprof->{enum}{enums}}) {
    $_ = $mp->{cbenum}  if ($_ eq $curprof->{bounds});
    $_ = $mp->{tenum_k} if ($_ eq $curprof->{targets});
  }
  $curprof->{bounds}  = $mp->{cbenum};
  $curprof->{targets} = $mp->{tenum_k};

  ##-- profiles: replace enums: ctrprof
  foreach (@{$ctrprof->{enum}{enums}}) {
    $_ = $mp->{cbenum} if ($_ eq $ctrprof->{bounds});
    $_ = $mp->{cenum}  if ($_ eq $ctrprof->{targets});
  }
  $ctrprof->{bounds}  = $mp->{cbenum};
  $ctrprof->{targets} = $mp->{cenum};

  ##-- cleanup
  delete(@$mp{qw(pb2cb pt2c pt2tk pt2tk_xi pt2tk_xw pt2c_xi pt2c_xw)});

  return ($ctrprof,$curprof);
}

##--------------------------------------------------------------
## $pb2cb = $mp->xlateBoundsPdl($profile_bounds_enum)
##   + returns a CCS::Nd-encoded translation matrix
##       $pb2cb : pdl(double, $NProfileBounds,$NMetaProfileBounds)
##     suitable for bounds-translation via CCS::Nd::matmult() on a literal sub-profile
##     encoded in terms of $profile_bounds
##   + $profile_bounds_enum defaults to $mp->{prof}{bounds}
##   + requires:
sub xlateBoundsPdl {
  my ($mp,$pbenum) = @_;
  $pbenum = $mp->{prof}{bounds} if (!defined($pbenum)); ##-- profile bounds

  ##-- map bounds of current profile to mp-bounds
  my $lbenum  = $mp->lbenum();   ##-- mp-bounds: literals
  my $cenum   = $mp->{cenum};    ##-- mp clusters
  my $cbenum  = $mp->{cbenum};   ##-- all mp-bounds

  ##-- map bounds: literals
  my $pb2cb_l       = $pbenum->xlatePdlTo($cbenum,badval=>-1);          ## $pb2cb_l : $pbid => $cbid  (literals)
  my $pb_which_l    = which($pb2cb_l>=0);
  my $pb2cb_whichND = $pb_which_l->cat($pb2cb_l->index($pb_which_l)     ## $pb2cb_whichND : [[$pbid,$cbid],...]
				      )->xchg(0,1);

  ##-- map bounds: CCS::Nd matrix $upd_b2cb
  my $pb2cb = PDL::CCS::Nd->newFromWhich($pb2cb_whichND, ones(double,$pb2cb_whichND->dim(1)),
					 pdims  =>pdl(long, [$pbenum->size,$cbenum->size]),
					 missing=>0,
					)->badmissing->nanmissing;

  ##-- map bounds: clusters
  if ($mp->{use_cbounds}) {
    my $tenum_ltk  = $mp->{tenum_ltk};
    my $pb2t       = $pbenum->xlatePdlTo($tenum_ltk, badval=>-1);  ## $pb2t : $pbid => $tid_ltk
    my $pb2t_mask  = ($pb2t>=0);
    $pb2t_mask->index($pb_which_l) .= 0;
    my $pb_which_c = which($pb2t_mask);

    my $t2pb = zeroes(long,$tenum_ltk->size)-1;                    ## $t2pb : $tid_ltk => $pbid
    $t2pb->index($pb2t->index($pb_which_c)) .= $pb_which_c;
    #my $t_which = which($t2pb>=0);
    ##--
    #my $t_which = $pb2t->index($pb_which_c);
    #$tc_mask->index($pb2t->index($pb_which_c)) .= 1;

    my $c2cb = $cenum->xlatePdlTo($cbenum, badval=>-1);            ## $c2cb : $cid => $cbid
    #my $c_which = which($c2cb>=0);

    ##-- phat: CCS-ifiy
    my $phat = $mp->{phat} * $mp->{phatm};                         ## $phat : ($NClusters, $NPrevTargets)
    $phat    = $phat->toccs->badmissing->nanmissing;
    $phat   /= $phat->sumover->dummy(0,1);
    $phat->missing(0);

    ##-- phat: translate
    my $phat_which = $phat->whichND;
    my $phat_vals  = $phat->_nzvals;

    my $phat_t     = $t2pb->index($phat_which->slice("(1),"));     ## $NPrevTargets ~~> $NProfileBounds
    my $phat_c     = $c2cb->index($phat_which->slice("(0),"));     ## $NClusters    ~~> $NClusterBounds
    my $phat_tci   = which( ($phat_t >= 0) & ($phat_c >= 0) );

    ##-- phat: append to bounds-map
    my $phatx_which = $phat_t->index($phat_tci)->cat($phat_c->index($phat_tci))->xchg(0,1);
    my $phatx_vals  = $phat_vals->index($phat_tci);

    $pb2cb->appendWhich($phatx_which, $phatx_vals);
  }

  return $pb2cb;
}

##--------------------------------------------------------------
## ($pt2c,$pt2tk) = $mp->xlateTargetPdls($profile_targets_enum)
##   + returns a pair of CCS::Nd-encoded translation matrices
##       $pt2c  : ($NProfileTargets, $NMetaProfileClusters)
##       $pt2tk : ($NProfileTargets, $NTargets_k)
##     suitable for target-translation via CCS::Nd::matmult() on a literal sub-profile
##     encoded in terms of $profile_targets_enum
##   + caches $mp keys:
##       $pt2tk_xi : pdl(long,$NProfileTargets) ~ $tgs_prof->xlatePdlTo($tgs_k,badval=>-1)
##       $pt2tk_xw : pdl(?,   $NTgsKInProf)     ~ which($pt2tk_xi>=0)
##       $pt2c_xi  : pdl(long,$NProfileTargets) ~ $cids->index( $tgs_prof->xlatePdlTo($tgs_ltk,badval=>-1) )
##       $pt2c_xw  : pdl(?,   $NOldTargets)     ~ which($pt2c_xi>=0)
##   + $profile_targets_enum defaults to $mp->{prof}{targets}
##   + requires:
##      $mp->{tk2t} : pdl(long, $Ntgs_k  ) : $tid_k   => $tid_lek
##      #$mp->{t2tk} : pdl(long, $Ntgs_lek) : $tid_lek => $tid_k_or_-1
##   + new targets ($mp->{tk2t}) override old targets
sub xlateTargetPdls {
  my ($mp,$ptenum) = @_;
  $ptenum = $mp->{prof}{targets} if (!defined($ptenum)); ##-- profile targets

  ##-- map targets: new
  my $tenum_lek = $mp->{tenum};
  my $tenum_k   = $mp->{tenum_k};
  my $tenum_ltk = $mp->{tenum_ltk};
  my $tk2t      = $mp->{tk2t};

  ##-- map T_{prof} ~~> T_k
  my $pt2tk      = $ptenum->xlatePdlTo($tenum_k, badval=>-1);
  my $pt_which_k = which($pt2tk>=0);
  @$mp{qw(pt2tk_xi pt2tk_xw)} = ($pt2tk,$pt_which_k);  ##-- cache

  ##-- map T_{prof} ~~> T_{<k} [later ~~> C_{<k}]
  my $pt2t_ltk    = $ptenum->xlatePdlTo($tenum_ltk, badval=>-1);
  my $pt_mask_ltk = ($pt2t_ltk>=0);
  $pt_mask_ltk->index($pt_which_k) .= 0;  ##-- allow T_k to override T_{<k}
  my $pt_which_ltk = which($pt_mask_ltk);

  ##-- xlate: T_{prof} ~~> T_k
  my $pt2t_whichND = $pt_which_k->cat($pt2tk->index($pt_which_k)      ## $pt2t_whichND : [[$ptid,$tid_k],...]
				     )->xchg(0,1);
  my $pt2t = PDL::CCS::Nd->newFromWhich($pt2t_whichND, ones(byte,$pt2t_whichND->dim(1)),
					pdims   => pdl(long, [$ptenum->size,$tenum_k->size]),
					missing => 0,
				       )->badmissing->nanmissing;

  ##-- xlate: T_{prof} ~~> C_{<=k} @ k
  my $cids    = $mp->{clusterids};
  my $pt2c_xi = $pt2t_ltk->pdl;
  my $pt2c_xw = $pt_which_ltk;
  $pt2c_xi->index($pt_which_ltk) .= $cids->index($pt2t_ltk->index($pt_which_ltk));
  @$mp{qw(pt2c_xi pt2c_xw)} = ($pt2c_xi,$pt2c_xw); ##-- cache

  my $pt2c_whichND = $pt_which_ltk->cat($pt2c_xi->index($pt_which_ltk) ## [[$ptid,$cid],...]
				       )->xchg(0,1);
  my $pt2c = PDL::CCS::Nd->newFromWhich($pt2c_whichND, ones(byte,$pt2c_whichND->dim(1)),
					pdims   => pdl(long, [$ptenum->size,$mp->{cenum}->size]),
					missing => 0,
				       )->badmissing->nanmissing;

  return ($pt2c,$pt2t);
}


##--------------------------------------------------------------
## undef = $mp->updateProfileUnigramDists($rawProfile, $ctrProfile, $curProfile)
##  + updates 'ptugs', 'pbugs', and 'ftotal' for each of $ctrProfile, $curProfile
##  + uses $mp->{tenum_k}, $mp->{cbenum}, $mp->{cenum}
##  + $ctrProfile may be undef, in which case it's ignored
##  + requires:
##      $mp->{pb2cb} : PDL::CCS::Nd ($NProfileBounds , $NMetaProfileBounds)
##      $mp->{pt2c}  : PDL::CCS::Nd ($NProfileTargets, $NMetaProfileClusters)
##      $mp->{pt2tk} : PDL::CCS::Nd ($NProfileTargets, $NTargets_k)
##  + FIXME: pdl-ize
sub updateProfileUnigramDists {
  my ($mp, $prf_wv_raw, $prf_cb_new, $prf_wb_new) = @_;

  ##-- update: target unigrams
  if (defined($prf_wv_raw->{ptugs})) {
    my $ptugs_raw = $prf_wv_raw->{ptugs}{pdl};
    ##
    ##-- target unigrams: w,b
    my $pt2tk_xi  = $mp->{pt2tk_xi};
    my $pt2tk_xw  = $mp->{pt2tk_xw};
    my $ptugs_wb_new = zeroes($ptugs_raw->type, $mp->{tenum_k}->size);
    $ptugs_wb_new->index($pt2tk_xi->index($pt2tk_xw)) .= $ptugs_raw->index($pt2tk_xw);
    $prf_wb_new->{ptugs}{pdl}  = $ptugs_wb_new;
    $prf_wb_new->{ptugs}{enum} = $mp->{tenum_k};
    ##
    ##-- target unigrams: c,b
    ##   +  use sparse matrix multiplication here, since we need to sum over word-to-cluster mapping:
    ##      (pt2c(T_p,C) x ptugs_raw(1,T_p)) --> ptugs_c(1,C)
    if (defined($prf_cb_new)) {
      my $pt2c = $mp->{pt2c};
      my $ptugs_cb_new = ($pt2c x $ptugs_raw->toccs->dummy(0,1))->todense->flat;
      $prf_cb_new->{ptugs}{pdl}  = $ptugs_cb_new;
      $prf_cb_new->{ptugs}{enum} = $mp->{cenum};
    }
  }

  ##-- update: bound unigrams
  ##   +  use sparse matrix multiplication here, since we might need to sum over word-to-bound mapping
  ##      (pb2cb(B_p,B_m) x pbugs_raw(1,B_p)) --> pbugs_m(1,B_m)
  if (defined($prf_wv_raw->{pbugs})) {
    my $pbugs_raw  = $prf_wv_raw->{pbugs}{pdl};
    my $pb2cb      = $mp->{pb2cb};
    my $pbugs_meta = ($pb2cb x $pbugs_raw->toccs->dummy(0,1))->todense->flat;
    $prf_wb_new->{pbugs}{pdl}  = $pbugs_meta;
    $prf_wb_new->{pbugs}{enum} = $mp->{cbenum};
    if (defined($prf_cb_new)) {
      $prf_cb_new->{pbugs}{pdl}  = $pbugs_meta->pdl;
      $prf_cb_new->{pbugs}{enum} = $mp->{cbenum};
    }
  }

  ##-- update: frequency totals
  if (defined($prf_wv_raw->{ftotal})) {
    $prf_wb_new->{ftotal} = $prf_wv_raw->{ftotal};
    $prf_cb_new->{ftotal} = $prf_wv_raw->{ftotal} if (defined($prf_cb_new));
  }

  return $mp;
}

sub updateProfileUnigramDists_OLD {
  my ($mp, $prf_wv_raw, $prf_cb_new, $prf_wb_new) = @_;

  my $tenum_w_raw = $prf_wv_raw->{targets};
  my $benum_v_raw = $prf_wv_raw->{bounds};
  my $lbenum      = $mp->lbenum();
  my ($tenum,$tenum_k,$cenum,$cbenum) = @$mp{qw(tenum tenum_k cenum cbenum)};
  my $cids_ltk = $mp->{clusterids};

  ##-- update: target unigrams
  my ($id_w_raw,$f,$w,$id_w_k,$id_c);
  if (defined($prf_wv_raw->{tugs})) {
    while (($id_w_raw,$f)=each(%{$prf_wv_raw->{tugs}{nz}})) {
      $w = $tenum_w_raw->{id2sym}[$id_w_raw];

      if (defined($id_w_k = $tenum_k->{sym2id}{$w})) {
	##-- target is new: add data to $prf_wv_raw->{tugs}
	$prf_wb_new->{tugs}{nz}{$id_w_k} += $f;
      }
      elsif (defined($prf_cb_new)) {
	##-- target is old: add data to $prf_cb->{tugs} (no smearing)
	$id_c = $cids_ltk->at($tenum->{sym2id}{$w});
	$prf_cb_new->{tugs}{nz}{$id_c} += $f;
      }
    }
#    ##-- totals
#    if (defined($prf_wv_raw->{ftotal})) {
#      $prf_wb_new->{ftotal} = $prf_wv_raw->{ftotal};
#      $prf_cb_new->{ftotal} = $prf_wv_raw->{ftotal};
#    }
  }

  ##-- update: bound unigrams
  my ($id_v_raw,$v,$id_v_ltk,$id_v_lb);
  my ($Ncb,$bugs_pdl,$phat);
  if (defined($prf_wv_raw->{bugs})) {
    $Ncb = $cbenum->size();
    $bugs_pdl = zeroes(double, $Ncb);

    $phat = $mp->{phat} * $mp->{phatm};
    $phat /= $phat->sumover->slice("*1,");
    $phat->inplace->setnantobad->inplace->setbadtoval(0); ##-- hack

    ##-- $c2cb->at($ci) = $cbenum->index( $cenum->symbol($ci) )
    ##   + used to index $cbenum distribution PDLs for non-literal class-bounds
    my $c2cb = pdl(long, [ @{$cbenum->{sym2id}}{ @{$cenum->{id2sym}} } ]);

    while (($id_v_raw,$f)=each(%{$prf_wv_raw->{bugs}{nz}})) {

      ##-- get bound-index ($id_v_ltk)
      $v        = $benum_v_raw->{id2sym}[$id_v_raw];

      if (exists($lbenum->{sym2id}{$v})) {
	##-- whoa: bound-word is a literal singleton class-bound -- i.e. {BOS},{EOS}
	$id_v_ltk = $cbenum->{sym2id}{$v};
	$bugs_pdl->slice("$id_v_ltk") += $f;
      } elsif ($mp->{use_cbounds}) {
	##-- usual case: bound-word is a previous target: respect $mp->{phat}
	$id_v_ltk                = $tenum->{sym2id}{$v};
	$bugs_pdl->index($c2cb) += $phat->slice(",($id_v_ltk)") * $f;
      }
    }
    ##-- PDL-to-EDist
    $prf_wb_new->{bugs}->fromPDLnz($bugs_pdl);
    %{$prf_cb_new->{bugs}{nz}} = %{$prf_wb_new->{bugs}{nz}} if (defined($prf_cb_new));
  }

  ##-- totals
  if (defined($prf_wv_raw->{ftotal})) {
    $prf_wb_new->{ftotal} = $prf_wv_raw->{ftotal};
    $prf_cb_new->{ftotal} = $prf_wv_raw->{ftotal} if (defined($prf_cb_new));
  }

  return $mp;
}

##--------------------------------------------------------------
## undef = $mp->updateProfileDists($rawSparsePdlDistNd, $ctrSparsePdlDistNd, $curSparsePdlDistNd)
##  + populates $ctrSparsePdlDistNd and $curSparsePdlDistNd from $rawSparsePdlDistNd, where:
##    - $rawSparsePdlDistNd : prof directed nary dist wrt targets (\in T_<=k) & bound-words   (\in     (LB u T_<k ))
##    - $ctrSparsePdlDistNd : prof directed nary dist wrt classes (\in C_k-1) & bound-classes (\in B_k=(LB u C_k-1))
##    - $curSparsePdlDistNd : prof directed nary dist wrt targets (\in T_k  ) & bound-classes (\in B_k=(LB u C_k-1))
##  + output distributions should be subsequently changed to use enums:
##    - $ctrSparsePdlDistNd : targets=$mp->{cenum}   ; bounds=$mp->{cbenum}, lbounds=$mp->{lbenum}
##    - $curSparsePdlDistNd : targets=$mp->{tenum_k} ; bounds=$mp->{cbenum}, lbounds=$mp->{lbenum}
##  + requires:
##    ~ $mp->{pb2cb}        : CCS::Nd ($NProfileBounds,  $NMetaProfileBounds)
##    ~ $mp->{pt2c}         : CCS::Nd ($NProfileTargets, $NMetaProfileClusters)
##    ~ $mp->{pt2tk}        : CCS::Nd ($NProfileTargets, $NTargets_k)
##  + $ctrSparsePdlDistNd may be undef, in which case it's ignored
##  + TODO: test me!
sub updateProfileDists {
  my ($mp,$wvdist,$cbdist,$wbdist) = @_;

  my $wvpdl = $wvdist->{pdl};                   ##-- wvpdl(T_p,B_p)
  my $pb2cb = $mp->{pb2cb};                     ##-- pb2cb(B_p,B_m)
  my $pt2tk = $mp->{pt2tk};                     ##-- pt2tk(T_p,T_k)
  my $pt2c  = $mp->{pt2c};                      ##-- pt2c (T_p, C )
  my $xbpdl = $pb2cb x $wvpdl;                  ##-- (pb2cb(B_p,B_m) x wvpdl  (T_p,B_p)) --> xbpdl(T_p,B_m)

  ##-- create: wbdist
  if (defined($wbdist)) {
    my $wbpdl = $xbpdl x $pt2tk->xchg(0,1);     ##-- (xbpdl(T_p,B_m) x pt2tk^T(T_k,T_p)) --> tbpdl(T_k,B_m)
    $wbdist->{pdl}  = $wbpdl;
    $wbdist->{pdl}->missing($wvpdl->missing->sclr);
    $wbdist->{enum} = MUDL::Enum::Nary->new(nfields=>2, #$wvdist->{enum}{nfields},
					    enums  =>[$mp->{tenum_k},$mp->{cbenum}],
					   );
  }

  ##-- create: cbdist
  if (defined($cbdist)) {
    my $cbpdl = $xbpdl x $pt2c->xchg(0,1);      ##-- (xbpdl(T_p,B_m) x pt2c^T ( C, T_p)) --> cbpdl( C ,B_m)

    $cbdist->{pdl}  = $cbpdl;
    $cbdist->{pdl}->missing($wvpdl->missing->sclr);
    $cbdist->{enum} = MUDL::Enum::Nary->new(nfields=>2, #$wvdist->{enum}{nfields},
					    enums  =>[$mp->{cenum},$mp->{cbenum}],
					   );
  }

  return;
}

sub updateProfileDists_OLD {
  my ($mp,$wvdist,$cbdist,$wbdist) = @_;

  my $wenum   = $wvdist->{enum}{enums}[0];
  my $venum   = $wvdist->{enum}{enums}[1];
  my $tenum   = $mp->{tenum};
  my $cbenum  = $mp->{cbenum};
  my $cenum   = $mp->{cenum};
  my $lbenum  = $mp->lbenum();

  my $tenum_k  = $mp->{tenum_k};
  my $cids_ltk = $mp->{clusterids};
  #my $c2tk     = $mp->{c2tk};

  my $Nc      = $cenum->size;
  my $Ncb     = $cbenum->size;
  my $Nt      = $tenum->size;
  my $Ntk     = $tenum_k->size;

  my $fwb_pdl = zeroes(double, $Ntk, $Ncb);
  my $fcb_pdl = (defined($cbdist) ? zeroes(double, $Nc,  $Ncb) : undef);
  my $phat    = $mp->{phat} * $mp->{phatm};
  $phat      /= $phat->sumover->slice("*1,");
  $phat->inplace->setnantobad->inplace->setbadtoval(0); ##-- hack

  ##-- $c2cb->at($ci) = $cbenum->index( $cenum->symbol($ci) )
  ##   + used to index {cbenum} distribution PDLs for non-literal class-bounds
  my $c2cb = pdl(long, [ @{$cbenum->{sym2id}}{ @{$cenum->{id2sym}} } ]);

  my ($key,$f, $w,$v, $ws,$wi, $vs,$vi, $Pbv, $opdl);
  while (($key,$f)=each(%{$wvdist->{nz}})) {
    ($w,$v) = $wvdist->split($key);

    ##-- get target-index ($wi)
    $ws = $wenum->{id2sym}[$w];
    $wi = $tenum_k->{sym2id}{$ws};
    if (defined($wi)) {
      ##-- target is new: add data to $fwb_pdl
      $opdl = $fwb_pdl;
    } elsif (defined($fcb_pdl)) {
      ##-- target is old: add associated class-data to $fcb_pdl
      $opdl = $fcb_pdl;
      $wi   = $cids_ltk->at($tenum->{sym2id}{$ws}); ##-- HACK: no smearing
    } else {
      ##-- totally bizarre target, or $ctrDistNaryDir was undef
      next;
    }

    ##-- get bound-index ($vi)
    $vs  = $venum->{id2sym}[$v];

    if (exists($lbenum->{sym2id}{$vs})) {
      ##-- whoa: bound-word is a literal singleton class-bound -- i.e. {BOS},{EOS}
      $vi = $cbenum->{sym2id}{$vs};
      $opdl->slice("$wi,$vi") += $f;
    } elsif ($mp->{use_cbounds}) {
      ##-- bound-word is a previous target
      $vi = $tenum->{sym2id}{$vs};
      $Pbv                                 = $phat->slice(",($vi)");
      $opdl->slice("($wi)")->index($c2cb) += $Pbv * $f;
    }
  }

  $mp->{ugs_k} += $fwb_pdl->xchg(0,1)->sumover; ##-- new stage-local target unigrams, may be needed later

  ##-- output: {ctrdist}
  if (defined($cbdist)) {
    @$cbdist{qw(nfields sep enum)} = (@$wvdist{qw(nfields sep)},
				      MUDL::Enum::Nary->new(nfields=>$wvdist->{enum}{nfields},
							    enums=>[$cenum, $cbenum]));
    MUDL::PdlDist->new(pdl=>$fcb_pdl, enum=>$cbdist->{enum})->toEDist($cbdist);
  }

  @$wbdist{qw(nfields sep enum)} = (@$wvdist{qw(nfields sep)},
				    MUDL::Enum::Nary->new(nfields=>$wvdist->{enum}{nfields},
							  enums=>[$tenum_k, $cbenum]));
  MUDL::PdlDist->new(pdl=>$fwb_pdl, enum=>$wbdist->{enum})->toEDist($wbdist);

  return;
}

##--------------------------------------------------------------
## $beta_pdl = $mp->beta()
##  + for pinsker dist-to-probability conversion
sub beta {
  my $mp = shift;
  return
    $mp->{ugs_k};
    #$mp->{ugs_k}->minimum;
    #$mp->{ugs_k}->average;
    #$mp->{ugs_k}->sum/($mp->{tenum}->size);
}


##--------------------------------------------------------------
## $phatPdl = $mp->updatePhat(%args)
##  + FIXME: pdl-ize
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
  #$mp->vmsg($vl_info, "updatePhat(): ^p_{<k}( c_{k-1} | t_{<k} )\n");
  my $phat_ltk  = $mp->{phat};
  my $phatm_ltk = $mp->{phatm};

  ##-- get ^p_k( c_k | t_k + c_{k-1} )  ~ current iteration
  #my $beta = $mp->beta;
  #$mp->vmsg($vl_info, "updatePhat(): avg(beta) = ", $beta->avg, "\n");

  #$mp->vmsg($vl_info, "updatePhat(): membershipProbPdl ~ ^p_k( c_k | t_k + c_{k-1} )\n");
  #$mp->vmsg($vl_info, "            :         betamode  = ", $mp->{betamode}, "\n");
  my $phat_k = zeroes(double, $mp->{cenum}->size, $mp->{tenum_k}->size);
  my $cm_k   = $mp->{cm_k};

  my $d2pbeta = $mp->d2pbeta();
  $cm_k->membershipProbPdl(@_,
			   ##-- NOTE: this key should be 'd2pbeta', but
			   ##   the default (beta=ones(double,$cdm->dim(1)))
			   ##   seems to work quite well, giving us:
			   ##     p(c|w) = b^(-1 * dist(c,w)) / sum ...
			   ##   TODO:
			   ##     try varying beta with STAGE:
			   ##     - low  beta [<1] --> high-entropy result dist (very uniform) ~ early stages
			   ##     - high beta [>1] --> low-entropy  result dist (very certain) ~ later stages
			   ##   DIFFERENCE between 'pinskerL1' and 'pinskerD' methods:
			   ##     - 'pinskerL1' is lower-entropy for the same beta
			   ##       --> b/c it squares distance
			   #beta=>$beta,
			   ##--
			   d2pbeta=>$d2pbeta,
			   ##--
			   r2cprobs=>$phat_k,
			   ##
			   ##-- for hard-clustering bonus
			   #cdbonus=>0, ##-- no bonus for update() ? [see above]
			  );


  ##-- get stage-k n-best membership mask
  my $phatm_k = $cm_k->membershipSimMask($phat_k, d2pbeta=>$d2pbeta);

  ##-- save stage-k betas
  $mp->{tbeta}->reshape($mp->{tbeta}->dim(0) + $phat_k->dim(1));
  $mp->{tbeta}->slice("-".$phat_k->dim(1).":-1") .= $d2pbeta;

  ##-- allocate new ^p_{<=k}()
  #$mp->vmsg($vl_info, "updatePhat(): adjust ~ ^p_{<=k}( c_k | t_{<=k} )\n");
  #my $phat  = $mp->{phat}  = zeroes(double, $mp->{cbenum}->size, $mp->{tenum}->size);
  my $phat  = $mp->{phat}  = zeroes(double, $mp->{cenum}->size, $mp->{tenum}->size);
  my $phatm = $mp->{phatm} = zeroes(byte,   $phat->dims);

  ##----------------------------------------------
  ## phat: create: ^p_{<=k} = p_k u p_{<k}

  my $tk2t = $mp->{tk2t};
  $phat ->slice(",0:".($phat_ltk->dim(1)-1))  .= $phat_ltk;
  $phatm->slice(",0:".($phatm_ltk->dim(1)-1)) .= $phatm_ltk;

  $phat ->dice_axis(1,$tk2t) .= $phat_k;
  $phatm->dice_axis(1,$tk2t) .= $phatm_k;

  ##-- HACK: class-membership probability: bos,eos: ---> should be LEFT OUT of phat ENTIRELY!
  #foreach (@$mp{qw(bos eos)}) {
  #  $phat->slice($mp->{cbenum}->index($_).",") .= 0;
  #}

  $mp->{phat}  = $phat;
  $mp->{phatm} = $phatm;
  return $phat;
}


##--------------------------------------------------------------
## $cm = $mp-updateCm(%args)
##  + FIXME: pdl-ize
##  + updates $mp->{cm} from $mp->{cm_k} and $mp->{cm_ltk}
##  + %args:
##    - ignored
##  + needs:
##    $mp->{cm_k}
##    $mp->{cm_ltk}
##    $mp->{tk2t}
sub updateCm {
  my $mp = shift;

  ##-- enums
  my $tenum   = $mp->{tenum};
  my $tenum_k = $mp->{tenum_k};

  ##-- id-translation pdl: [$id_Tk] = $id_Tlek
  #my $tk2t = pdl(long, [ @{$tenum->{sym2id}}{ @{$tenum_k->{id2sym}} } ]);
  ##--
  my $tk2t = $mp->{tk2t};

  ##-- create new cm
  #$mp->vmsg($vl_debug, "updateCm(): cm\n");
  my $cm_ltk = $mp->{cm_ltk} = $mp->{cm};
  my $cm_k   = $mp->{cm_k};
  my $cm     = $mp->{cm} = $cm_ltk->shadow(class=>'MUDL::Cluster::Method',enum=>$mp->{tenum});

  ##-- populate cm: {clusterids}
  my $cids_ltk = $cm_ltk->{clusterids};
  my $cids_k   = $cm_k->{clusterids};
  my $cids     = zeroes(long, $tenum->size);

  ##-- add cluster-ids
  $cids->slice("0:".($cids_ltk->dim(0)-1)) .= $cids_ltk;
  $cids->index($tk2t)                      .= $cids_k;
  $mp->{clusterids} = $cm->{clusterids} = $cids;

  ##-- populate cm: dimensions
  $cm->{nfeatures} = $mp->{cbenum}->size;
  $cm->{ndata}     = $cids->nelem;

  ##-- populate cm: cluster distance matrix
  if (defined($cm_ltk->{cdmatrix}) && defined($cm_k->{cdmatrix})) {
    my $cdm = $cm->{cdmatrix} = zeroes(double, $cm->{nclusters}, $cm->{ndata});

    ##-- copy old values: d_{<=k}(c,w)
    $cdm->slice(",0:".($cm_ltk->{cdmatrix}->dim(1)-1))   .= $cm_ltk->{cdmatrix};

    ##-- copy new values. d_k(c,w)
    #$cdm->slice(",".($cm_ltk->{cdmatrix}->dim(1)).":-1") .= $cm_k->{cdmatrix};
    ##--
    #my $cdm_k = $cm_k->clusterDistanceMatrix();
    my $cdm_k = $cm_k->{cdmatrix};
    $cdm->dice_axis(1,$tk2t) .= $cdm_k;
  } else {
    delete($cm->{cdmatrix});
  }

  ##-- populate cm: data (row) weights
  if (defined($cm_ltk->{dataweights})) {
    my $dweights = $cm->{dataweights} = zeroes(double,$cm->{ndata});
    $dweights->slice("0:".($cm_ltk->{dataweights}->nelem-1)) .= $cm_ltk->{dataweights};
    $dweights->index($tk2t) .= (defined($cm_k->{dataweights})
				? $cm_k->{dataweights}->slice("0:".($tk2t->nelem-1))
				: 1);
  }

  return $mp->{cm} = $cm;
}


##======================================================================
## Re-clustering (Stage >= 1)
##======================================================================

## $mp = $mp->recluster()
## $mp = $mp->recluster($prof)
##  + FIXME: (TODO): implement!
sub recluster {
  my ($mp,$prof) = @_;
  confess(ref($mp), ": recluster() method not implemented!");
}


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
