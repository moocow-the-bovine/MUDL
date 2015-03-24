##-*- Mode: Perl -*-

## File: MUDL::Corpus::MetaProfile::Deep.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: deep meta-profiles (stage-(k>1))
##  + recluster only old centroids on update()
##======================================================================

package MUDL::Corpus::MetaProfile::Deep;
use MUDL::Corpus::MetaProfile qw(:vlevels);
use PDL;
use PDL::Cluster;
use Carp;
our @ISA = qw(MUDL::Corpus::MetaProfile);

use strict;
our $LONG_CLUSTER_LABELS = 1; ##-- provide full cluster labels on itermediate tree

##======================================================================
## Corpus::MetaProfile::Deep: Constructor
## {
##  ##
##  ##-- global data
##  phat => $pdlDist,     ## dims: ($NBoundClusters, $NPrevTargets) : $phat->at($cid,$tid) = ^p_{<=$i}($cid|$tid)
##
##  tenum => $enum,        ## previous+current targets (token) enum (T_{<=$i})
##  benum => $enum,        ## previous+current bounds (token) enum (B_{<=$i}) (targets + bos,eos)
##  cbenum => $enum,       ## (bound-)class enum: includes bos,eos (<=$i)
##  cm => $cluster_method, ## MUDL::Cluster::Method
##  ##
##  ##-- previous data
##  pprof => $prev_prof,   ## ^f_{<=k}($dir, $bound_cluster, $target_word)
##  ##
##  ##-- current iteration data
##  prof => $profile,      ## current profile: ^f_{$dir}($dir, $bound_word, $target_word)
##  stage => $i,           ## stage number
##  #d2p => {method=>'nbest_base',n=>4,b=>2},  ## distance-to-probability arguments: now in {cm}{cd...}
##  ##
##  ##-- more current iteration data
##  clusterids => $pdl,    ## pdl($n) : best cluster-ids
##  tenum_k => $enum,      ## current (token) targets and previous (cluster) targets
##  tenum_ltk => $enum,    ## previous (token) targets only
##  tk2c => \%idmap,       ## maps current target-ids to previous cluster-ids: {$tid_k   => $cid_lek, ...}
##  c2tk => \%idmap,       ## maps previous cluster-ids to current target-ids: {$cid_lek => $tid_k,   ...}
##  tk2t => \%idmap,       ## maps current target-ids to global target-ids:    {$tid_k   => $tid_lek, ...}
##  t2tk => \%idmap,       ## maps global target-ids to current target-ids:    {$tid_lek => $tid_k,   ...}
##  ugs_k => $pdl,         ## pdl($n): expected unigram pseudo-frequency (targets_k u clusters_{k-1})
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
## $mp = MUDL::Corpus::MetaProfile::Deep->new(%args)
sub new {
  my ($that,%args) = @_;
  return $that->SUPER::new(stage => 0,
			   verbose => $vl_default,
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
  $mp->{prof}  = $prof;
  $mp->{cm}    = $cm;
  $mp->{stage} = 1;

  ##-- load modules
  MUDL::CmdUtils::loadModule(ref($prof))
      or confess(ref($mp), "::bootstrap(): could not load profile module '", ref($prof), "': $!");
  MUDL::CmdUtils::loadModule(ref($cm))
      or confess(ref($mp), "::bootstrap(): could not load cluster module '", ref($cm), "': $!");

  ##-- initialize enums
  $mp->{tenum}  = $prof->{targets};
  $mp->{benum}  = $prof->{targets}->copy;
  $mp->{cbenum} = $cm->clusterEnum()->copy;

  ##-- bound-class-hack: bos,eos
  foreach (@$mp{qw(bos eos)} = @$prof{qw(bos eos)}) {
    $mp->{cbenum}->addSymbol($_);
  }

  ##-- populate {clusterids}: word => argmax_class ( p(class|word) )
  $mp->vmsg($vl_info, "bootstrap(): clusterids ~ w => argmax_c ( p(c|w) )\n");
  $mp->{clusterids} = $cm->{clusterids};

  ##-- populate ugs_k
  $mp->vmsg($vl_info, "bootstrap(): unigrams ~ f_0(w)\n");
  $mp->{ugs_k} = zeroes(double, $mp->{tenum}->size);
  foreach my $dir (qw(right left)) {
    my $d = $mp->{prof}{$dir};
    my ($k,$t,$b,$f,$w);
    while (($k,$f)=each(%{$d->{nz}})) {
      ($w,$b) = $d->split($k);
      $mp->{ugs_k}->slice("$w") += $f;
    }
  }
  $mp->{ugs_k} /= 2;

  ##-- update info
  $mp->{stage_info} = {} if (!$mp->{stage_info});
  @{$mp->{stage_info}}{qw(avg prms median min max adev rms)} = map { $_->sclr } $mp->{ugs_k}->stats;
  $mp->{ntgs_k} = $mp->{tenum}->size;

  $mp->vmsg($vl_info, sprintf("bootstrap(): unigrams: min(f_0(w))=%.2f\n", $mp->{stage_info}{min}));
  $mp->vmsg($vl_info, sprintf("bootstrap(): unigrams: max(f_0(w))=%.2f\n", $mp->{stage_info}{max}));
  $mp->vmsg($vl_info, sprintf("bootstrap(): unigrams: avg(f_0(w))=%.2f\n", $mp->{stage_info}{avg}));

  ##-- populate {phat}: p(class|word)
  $mp->vmsg($vl_info, "bootstrap(): phat ~ ^p(c|w)\n");
  $mp->populatePhat(%args);

  return $mp;
}

##--------------------------------------------------------------
## $phatPdl = $mp->populatePhat(%args)
##  + populates $mp->{phat} ~ $phat->at($cid,$wid) = ^p($cid | $wid)
##    from $mp->{cm}
##  + %args:
##    - passed to $mp->{cm}->membershipProbPdl()
sub populatePhat {
  my $mp = shift;

  my $phat = zeroes(double, $mp->{cbenum}->size, $mp->{tenum}->size);
  my $beta = $mp->beta;
  $mp->vmsg($vl_info, "populatePhat(): avg(beta) = ", $beta->avg, "\n");
  $mp->{cm}->membershipProbPdl(@_,d2pbeta=>$beta,r2cprobs=>$phat);

  ##-- class-probability membership hack: bos,eos
  foreach (@$mp{qw(bos eos)}) {
    $phat->slice($mp->{cbenum}->index($_).",") .= 0;
  }

  ##-- sanity check
  $phat->inplace->setnantobad();
  confess(ref($mp), "::populatePhat(): bad values in {phat} bode ill!")
    if ($phat->nbad > 0);
  confess(ref($mp), "::populatePhat(): no nonzero values in {phat}!")
    if (all($phat==0));


  return $mp->{phat} = $phat;
}


##======================================================================
## Update (Stage > 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->update($profile)
##  + sets $mp->{prof} to $profile (a profile over bound- & target-words)
##  + updates $mp->{tenum} to include new targets
##  + updates $mp->{benum} to include new bounds
##  + updates $mp->{tenum_k} to new targets only
##  + $profile may be destructively altered!
sub update {
  my ($mp,$prof) = @_;

  ##-- update: stage-number
  ++$mp->{stage};

  ##-- update: profile
  $mp->vmsg($vl_info, "update(): profile ~ f_{<=k}(d, v_b, w)\n");
  $mp->vmsg($vl_info, "update(): nbounds      = ", $prof->{bounds}->size, "\n");
  $mp->vmsg($vl_info, "update(): ntargets     = ", $prof->{targets}->size, "\n");
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

  ##-- tenum_k: old clusters
  my $c2tk = $mp->{c2tk} = {};
  my $tk2c = $mp->{tk2c} = {};
  my $Nc = $mp->{cm}{nclusters};

  my $km1 = $mp->{stage}-1;
  my @clabs = map { "~c${_}_${km1}:" } (0..($Nc-1)); ##-- $clabs[$cluster_id] => $cluster_label
  my $cids  = $mp->{clusterids};

  my ($cid);
  if ($LONG_CLUSTER_LABELS) {
    foreach $tid (0..($tenum_ltk->size-1)) {
      $cid = $cids->at($tid);
      $clabs[$cid] .= ' '.($tenum_ltk->symbol($tid));
    }
  }

  foreach $cid (0..($Nc-1)) {
    ##-- string hack: symbols must be unique
    $tid_k = $tenum_k->addSymbol($clabs[$cid]);
    $c2tk->{$cid}   = $tid_k;
    $tk2c->{$tid_k} = $cid;
  }
  $mp->vmsg($vl_info, "update(): nclusters_<k = ", $Nc, "\n");

  ##-- update: pprof
  $mp->vmsg($vl_info, "update(): pprof ~ f_{<=k}(d, c_b, t_k + c_<k)\n");
  my $pprof = $mp->populatePprof($prof);

  ##-- update info
  my $ugs_tgs_k = $mp->{ugs_k}->slice("0:".($mp->{stage_info}{ntgs_k}-1));
  @{$mp->{stage_info}}{qw(avg prms median min max adev rms)} = map { $_->sclr } $ugs_tgs_k->stats;

  $mp->vmsg($vl_info, sprintf("update(): unigrams: min(f_k(w))=%.2f\n", $mp->{stage_info}{min}));
  $mp->vmsg($vl_info, sprintf("update(): unigrams: max(f_k(w))=%.2f\n", $mp->{stage_info}{max}));
  $mp->vmsg($vl_info, sprintf("update(): unigrams: avg(f_k(w))=%.2f\n", $mp->{stage_info}{avg}));


  ##-- update: cluster: toPDL
  $mp->vmsg($vl_info, "update(): toPDL()\n");
  my $cm = $mp->{cm_k} = $mp->{cm}->shadow(enum=>$pprof->{targets});
  $cm->data($pprof->toPDL);

  ##-- update: cluster: cluster()
  $mp->vmsg($vl_info, "update(): cluster()\n");
  $cm->cluster();

  ##-- update: cm
  $mp->vmsg($vl_info, "update(): cm\n");
  $mp->updateCm();

  ##-- update: phat
  $mp->vmsg($vl_info, "update(): phat ~ ^p( c_{k} | w_{<=k} )\n");
  $mp->updatePhat();  ##-- hacked: weirdness

  return $mp;
}

##--------------------------------------------------------------
## $pprof = $mp->populatePprof)
## $pprof = $mp->populatePprof($prof)
##  + populate
##      $mp->{pprof} ~ f_{k}(d, c_b, w_k + c_<k)
##    from
##      $prof        ~ f_{k}(d, v_b, w_k + c_<k)
##    and
##      $mp->{phat}  ~ ^p_{k}(c_b | v_b)
##  + requires:
##    ~ Bounds_{stage=$i} \subseteq Bounds_{stage=$i+1}, \forall i, 1 <= $i <= $mp->{stage}
##    ~ $mp->{tenum_k}
##  + basically just calls addProfileDist() for each $dir qw(left right)
sub populatePprof {
  my ($mp,$prof) = @_;
  $prof = $mp->{prof} if (!defined($prof));

  ##-- pprof: step 0: copy profile (hack: shadow distributions)
  my $pprof = $mp->{pprof} = $prof->shadow();

  #return $mp; ##-- DEBUG

  ##-- pprof: step 0: allocate unigram dist
  $mp->{ugs_k} = zeroes(double, $mp->{tenum_k}->size);

  ##-- pprof: step 1: tweak profile distributions
  $pprof->{left}   = $mp->addProfileDist($prof->{left});
  $pprof->{right}  = $mp->addProfileDist($prof->{right});
  $mp->{ugs_k}    /= 2; ##-- factor out l,r doubling

  ##-- pprof: step 2: replace enums
  foreach (@{$pprof->{enum}{enums}}) {
    $_ = $mp->{cbenum}  if ($_ eq $pprof->{bounds});
    $_ = $mp->{tenum_k} if ($_ eq $pprof->{targets});
  }
  $pprof->{bounds}  = $mp->{cbenum};
  $pprof->{targets} = $mp->{tenum_k};


  return $pprof;
}

##--------------------------------------------------------------
## $tweakedDist = $mp->addProfileDist($profileDistNary)
##  + generated $tweakedDist, a distribution over
##    OVER target-(words||classes) & bound-classes
##    FROM $profileDistNary, over target- and bound-words.
##  + ouptut distribution uses $mp->{tenum_k}, input distribution uses
##    its own target enum
##  + requires:
##    ~ $mp->{tenum_k}
##    ~ $mp->{clusterids}
##    ~ $mp->{c2tk}
sub addProfileDist {
  my ($mp,$wvdist) = @_;

  my $wenum   = $wvdist->{enum}{enums}[0];
  my $venum   = $wvdist->{enum}{enums}[1];
  my $tenum   = $mp->{tenum};
  my $cbenum  = $mp->{cbenum};

  my $tenum_k  = $mp->{tenum_k};
  my $cids_ltk = $mp->{clusterids};
  my $c2tk     = $mp->{c2tk};

  my $Ncb     = $cbenum->size;
  my $Nt      = $tenum->size;
  my $Ntk     = $tenum_k->size;

  my $fwb_pdl = zeroes(double, $Ntk, $Ncb);
  my $phat    = $mp->{phat};

  my ($key,$f, $w,$v, $ws,$wi, $vs,$vi, $Pbv);
  while (($key,$f)=each(%{$wvdist->{nz}})) {
    ($w,$v) = $wvdist->split($key);

    ##-- get target-index ($wi)
    $ws = $wenum->symbol($w);
    $wi = $tenum_k->index($ws);
    if (!defined($wi)) {
      ##-- whoa: target looks like a pseudo-class  (hack: no smearing) : FIX THIS?
      $wi = $c2tk->{ $cids_ltk->at($tenum->index($ws)) };
    }

    ##-- get bound-index ($vi)
    $vs = $venum->symbol($v);
    $vi = $tenum->index($vs);
    if (defined($vi)) {
      ##-- bound-word, previous target
      $Pbv                      = $phat->slice(",($vi)");
      $fwb_pdl->slice("($wi)") += $Pbv * $f;
    }
    else {
      ##-- whoa, it's a literal singleton class-bound -- i.e. {BOS},{EOS}
      $vi = $cbenum->index($vs);
      $fwb_pdl->slice("$wi,$vi") += $f;
    }
  }

  $mp->{ugs_k} += $fwb_pdl->xchg(0,1)->sumover; ##-- new target unigrams, may be needed later

  my $fwb_ed = MUDL::EDist::Nary->new(nfields=>$wvdist->{nfields},
				      sep=>$wvdist->{sep},
				      enum=>MUDL::Enum::Nary->new(nfields=>$wvdist->{enum}{nfields},
								  enums=>[$tenum_k, $cbenum]),
				     );

  return MUDL::PdlDist->new(pdl=>$fwb_pdl, enum=>$fwb_ed->{enum})->toEDist($fwb_ed);
}

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
  my $phat_ltk = $mp->{phat};

  ##-- get ^p_k( c_k | t_k + c_{k-1} )  ~ current iteration
  my $beta = $mp->beta;
  $mp->vmsg($vl_info, "updatePhat(): avg(beta) = ", $beta->avg, "\n");

  $mp->vmsg($vl_info, "updatePhat(): membershipProbPdl ~ ^p_k( c_k | t_k + c_{k-1} )\n");
  my $phat_k   = zeroes(double, $mp->{cbenum}->size, $mp->{tenum_k}->size);
  $mp->{cm_k}->membershipProbPdl(@_, d2pbeta=>$beta, r2cprobs=>$phat_k);

  ##-- allocate new ^p_{<=k}()
  $mp->vmsg($vl_info, "updatePhat(): adjust ~ ^p_{<=k}( c_k | t_{<=k} )\n");
  my $phat = $mp->{phat} = zeroes(double, $mp->{cbenum}->size, $mp->{tenum}->size);

  ##----------------------------------------------
  ## phat: create: ^p_{<=k} = p_k u p_{<k}

  ## phat: create: add old
  my $tenum     = $mp->{tenum};
  my $tenum_ltk = $mp->{tenum_ltk};
  my $tenum_k   = $mp->{tenum_k};
  my $cids_ltk  = $mp->{cm_ltk}{clusterids};

#=begin comment

  $mp->vmsg($vl_info, "updatePhat(): add<k ~ ^p_{<=k} += t_{<k} [HARD]\n");
  my ($tid_ltk,$tid_lek,$cid_ltk);
  my $c2tk = $mp->{c2tk};
  ##-- OLD
  foreach $tid_ltk (0..($tenum_ltk->size-1)) {
    $cid_ltk = $cids_ltk->at($tid_ltk);
    $phat->slice(",($tid_ltk)") .= $phat_k->slice(",($c2tk->{$cid_ltk})"); ##-- NO SMEARING: FIX!
  }

#=end comment
#
#=cut

=begin comment

  ##-- NEW
  $mp->vmsg($vl_info, "updatePhat(): add<k ~ ^p_{<=k} += t_{<k} [SOFT]\n");
  my $phat_cltk2ck = $phat_k->dice_axis(1, pdl(long, [@$c2tk{(0..(scalar(keys(%{$mp->{c2tk}}))-1))}]));
  my $phat_t_lek = zeroes(double, $phat_k->dim(0));
  my $d2p_n = $mp->{d2p}{n} ? $mp->{d2p}{n} : 1;
  my $tcids_ltk = zeroes(long, $d2p_n);
  my ($phat_t_ltk, $phat_t);
  foreach $tid_ltk (0..($tenum_ltk->size-1)) {
    #$cid_ltk = $cids_ltk->at($tid_ltk);

    ##-- get p(c_{k-1} | t)
    $phat_t_ltk = $phat_ltk->slice(",($tid_ltk)");
    $phat_t_ltk->which($tcids_ltk);

    ##-- compute p(c_k|t_{k-1}) = \sum_{c_{k-1}} p(c_k|c_{k-1}) * p(c_{k-1}|t_{k-1})
    sumover($phat_t_ltk->dice($tcids_ltk) * $phat_cltk2ck->dice_axis(1,$tcids_ltk)->xchg(0,1), $phat_t_lek);

    ##-- restrict to n-best
    $phat_t_lek->maximum_n_ind($tcids_ltk);
    $phat_t = $phat->slice(",($tid_ltk)");
    $phat_t->index($tcids_ltk) .= $phat_t_lek->index($tcids_ltk);
    $phat_t /= $phat_t->sum;
  }

=end comment

=cut

  ##-- phat: create: add new targets
  $mp->vmsg($vl_info, "updatePhat(): add+k ~ ^p_{<=k} += t_k\n");
  my ($tid_k);
  foreach $tid_k (0..($tenum_k->size-1)) {
    $tid_lek = $tenum->index($tenum_k->symbol($tid_k));
    next if (!defined($tid_lek));  ##-- ignore old clusters
    $phat->slice(",($tid_lek)") .= $phat_k->slice(",($tid_k)");
  }


  ##-- HACK: class-membership probability: bos,eos
  foreach (@$mp{qw(bos eos)}) {
    $phat->slice($mp->{cbenum}->index($_).",") .= 0;
  }

  return $mp->{phat} = $phat;
}


##--------------------------------------------------------------
## $cm = $mp-updateCm(%args)
##  + updates $mp->{cm} from $mp->{cm_k} and $mp->{cm_ltk}
##  + %args:
##    - ignored
##  + needs:
##    $mp->{cm_k}
##    $mp->{cm_ltk}
sub updateTree { return shift->updateCm(@_); }
sub updateCm {
  my $mp = shift;

  ##-- update cluster-ids
  my $tenum   = $mp->{tenum};
  my $tenum_k = $mp->{tenum_k};

  ##-- create new cm
  $mp->vmsg($vl_debug, "updateCm(): cm\n");
  my $cm_k   = $mp->{cm_k};
  my $cm_ltk = $mp->{cm_ltk} = $mp->{cm};
  my $cm     = $mp->{cm} = $cm_ltk->shadow(enum=>$mp->{tenum});

  if (UNIVERSAL::isa(ref($cm), 'MUDL::Cluster::Tree')) {
    ##-- populate new cm: Tree
    $mp->vmsg($vl_debug, "updateCm(): cm: index (<k)\n");
    my $ct_k   = $cm_k->{ctree};
    my $ct_ltk = $cm_ltk->{ctree};
    my $ct     = $cm->{ctree} = zeroes(long, 2, $tenum->size);

    ##-- populate: Tree: Tree: Tree: find node-ids of old cluster nodes
    ##   + code depends heavily on cutcm() from cluster.c
    my $nelts_ltk = $ct_ltk->dim(1);
    my $njoin_ltk = $nelts_ltk - $cm_ltk->{nclusters};

    ##-- populate: Tree: map old node-ids to cluster-ids
    my $cids_ltk = $cm_ltk->{clusterids};
    my %nid2cid = qw();
    my ($did,$cid,$i);
    foreach $i (0..($njoin_ltk-1)) {
      if ( ($did=$ct_ltk->at(0,$i)) >= 0) {
	##-- left-leaf
	$cid = $cids_ltk->at($did);
	$nid2cid{-$i-1} = $cid;
      } elsif ( ($did=$ct_ltk->at(1,$i)) >= 0) {
	##-- right-leaf
	$cid = $cids_ltk->at($did);
	$nid2cid{-$i-1} = $cid;
      } else {
	##-- internal node: we should have registered both left and right daughters
	$did = $ct_ltk->at(0,$i);
	$cid = $nid2cid{$did};
	$nid2cid{-$i-1} = $cid;
      }
    }

    ##-- populate: Tree: map old cluster-ids to old node-ids
    my %cid2nid = map { $nid2cid{$_}=>$_ } sort {$b<=>$a} keys(%nid2cid);

    ##-- populate: Tree: map old singleton-clusterids to old leafnode-ids
    foreach $i ($njoin_ltk..($nelts_ltk-2)) {
      if ( ($did=$ct_ltk->at(0,$i)) >= 0) {
	##-- left-leaf
	$cid = $cids_ltk->at($did);
	$cid2nid{$cid} = $did;
      }
      if ( ($did=$ct_ltk->at(1,$i)) >= 0) {
	##-- right-leaf
	$cid = $cids_ltk->at($did);
	$cid2nid{$cid} = $did;
      }
    }

    ##-- populate: Tree: add old nodes
    $mp->vmsg($vl_debug, "updateCm(): cm: adopt (<k)\n");
    if ($njoin_ltk > 0) {
      $ct->slice(",0:".($njoin_ltk-1)) .= $ct_ltk->slice(",0:".($njoin_ltk-1));
    } else {
      ##-- hack for nclusters==ntargets
      $ct->slice(",0:".($ct_ltk->dim(1)-1)) .= $ct_ltk;
    }


    ##-- populate: Tree: add new nodes
    $mp->vmsg($vl_debug, "updateCm(): cm: adopt (=k)\n");
    my ($ii,$tid,$j);
    my $tk2c = $mp->{tk2c};
    foreach $i (0..($ct_k->dim(1)-2)) {
      $ii = $njoin_ltk + $i;
      foreach $j (0,1) {
	$did = $ct_k->at($j,$i);
	if ($did >= 0) {
	  ##-- leaf daughter
	  if (defined($cid=$tk2c->{$did})) {
	    ##-- placeholder for old cluster
	    $ct->set($j,$ii, $cid2nid{$cid});
	  } else {
	    ##-- real (new) leaf: must translate
	    $ct->set($j,$ii, $tenum->index($tenum_k->symbol($did)));
	  }
	} else {
	  ##-- non-leaf daughter
	  $ct->set($j,$ii, $did-$njoin_ltk);
	}
      }
    }

    ##-- populate: Tree: link distances
    $mp->vmsg($vl_debug, "updateCm(): cm: link distances\n");
    my $linkdist = zeroes(double, $ct->dim(1));

    if ($njoin_ltk > 0) {
      $linkdist->slice("0:".($njoin_ltk-1))
	.= $cm_ltk->{linkdist}->slice("0:".($njoin_ltk-1));
    } else {
      ##-- hack for nclusters==ntargets
      $linkdist->slice("0:".($cm_ltk->{linkdist}->dim(0)-1)) .= $cm_ltk->{linkdist};
    }

    $linkdist->slice("$njoin_ltk:".($njoin_ltk + $cm_k->{linkdist}->dim(0) - 1))
      .= $cm_k->{linkdist}->slice("0:".($cm_k->{linkdist}->dim(0)-1));


    ##-- update: finalize
    $mp->vmsg($vl_debug, "updateCm(): cut()\n");
    $cm->{ctree} = $ct;
    $cm->{linkdist} = $linkdist;
    $cm->cut();
    $mp->{clusterids} = $cm->{clusterids};
  }
  else {
    ##-- populate: non-tree: {clusterids} only
    my $cids_k   = $cm_k->{clusterids};
    my $cids_ltk = $cm_ltk->{clusterids};
    my $cids   = zeroes(long, $tenum->size);
    my ($tid,$cid);
    foreach $tid (0..($tenum->size-1)) {
      if (defined($tenum_k->index($tenum->symbol($tid)))) {
	##-- new target
	$cids->set($tid, $cids_k->at($tid));
      } else {
	##-- old target: use cluster
	$cids->set($tid, $cids_k->at($cids_ltk->at($tid)));
      }
    }
    $mp->{clusterids} = $cm->{clusterids} = $cids;
  }

  return $mp->{cm} = $cm;
}


##======================================================================
## Re-clustering (Stage >= 1)
##======================================================================

## $mp = $mp->recluster()
## $mp = $mp->recluster($prof)
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

  @$info{qw(d2p_n d2p_method)} = @{$mp->{cm}}{qw(d2pn d2pmethod)};
  $info->{prfType} = ref($mp->{prof});
  $info->{nTargets} = $mp->{tenum}->size;
  $info->{nBounds} = $mp->{benum}->size;
  $info->{nClusters} = $mp->{cbenum}->size;
  $info->{nTargets_k} = (defined($mp->{tenum_k})
			 ? ($mp->{tenum_k}->size - $info->{nClusters})
			 : $info->{nTargets});

  ##-- unigram freq info
  my $ugs = $mp->{ugs_k}->slice("0:".($info->{nTargets_k}-1));
  @$info{qw(ugk_avg ugk_prms ugk_median ugk_min ugk_max ugk_adev ugk_rms)} = map { $_->sclr } $ugs->stats;

  return $info;
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
