##-*- Mode: Perl -*-

## File: MUDL::Corpus::MetaProfile::Deep.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
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
##  tree => $cluster_tree, ## MUDL::Cluster::Tree
##  ##
##  ##-- previous data
##  pprof => $prev_prof,   ## ^f_{<=k}($dir, $bound_cluster, $target_word)
##  ##
##  ##-- current iteration data
##  prof => $profile,      ## current profile: ^f_{$dir}($dir, $bound_word, $target_word)
##  stage => $i,           ## stage number
##  d2p => {method=>'nbest_base',n=>4,b=>2},  ## distance-to-probability arguments
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
## }
##
## $mp = MUDL::Corpus::MetaProfile::Deep->new(%args)
sub new {
  my ($that,%args) = @_;

  $args{method} = 'full';
  return $that->SUPER::new(
			   stage => 0,
			   d2p => {method=>'nbest_inverse',n=>8,b=>2},
			   verbose => $vl_default,
			   %args,
			  );
}

##======================================================================
## Bootstrapping (stage = 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->bootstrap($profile1, $tree1, %args)
##  + initializes $mp->{phat}
##  + %args contains arguments for $tree1->membershipProbs()
##  + $profile1 and $tree1 may both be destructively altered!
sub bootstrap {
  my ($mp,$prof,$tree,%args) = @_;
  $mp->vmsg($vl_info, "bootstrap().\n");

  $mp->{prof} = $prof;
  $mp->{tree} = $tree;
  $mp->{stage} = 1;

  ##-- initialize enums
  $mp->{tenum}  = $prof->{targets};
  $mp->{benum}  = $prof->{targets}->copy;

  ##--
  $mp->{cbenum} = $tree->clusterEnum()->copy;

  ##-- bound-class-hack: bos,eos
  foreach (@$mp{qw(bos eos)} = @$prof{qw(bos eos)}) {
    $mp->{cbenum}->addSymbol($_);
  }

  ##-- populate {clusterids}: word => argmax_class ( p(class|word) )
  $mp->vmsg($vl_info, "bootstrap(): clusterids ~ w => argmax_c ( p(c|w) )\n");
  $mp->{clusterids} = $tree->{clusterids};

  ##-- populate ugs_k
  $mp->vmsg($vl_info, "bootstrap(): unigrams ~ f_0(w)\n");
  $mp->{ugs_k} = zeroes(double, $mp->{tenum}->size);
  foreach $dir (qw(right left)) {
    my $d = $mp->{prof}{$dir};
    my ($k,$t,$b,$f);
    while (($k,$f)=each(%{$d->{nz}})) {
      ($w,$b) = $d->split($k);
      $mp->{ugs_k}->slice("$w") += $f;
    }
  }
  $mp->{ugs_k} /= 2;
  $mp->vmsg($vl_info, sprintf("bootstrap(): unigrams: min(f_0(w))=%.2f\n", $mp->{ugs_k}->min));

  ##-- populate {phat}: p(class|word)
  $mp->vmsg($vl_info, "bootstrap(): phat ~ ^p(c|w)\n");
  $mp->populatePhat(%args);

  return $mp;
}

##--------------------------------------------------------------
## $phatPdl = $mp->populatePhat(%args)
##  + populates $mp->{phat} ~ $phat->at($cid,$wid) = ^p($cid | $wid)
##    from $mp->{tree}
##  + %args:
##    - passed to $mp->{tree}->membershipProbPdl()
sub populatePhat {
  my $mp = shift;

  my $phat = zeroes(double, $mp->{cbenum}->size, $mp->{tenum}->size);
  my $beta = $mp->beta;
  $mp->vmsg($vl_info, "populatePhat(): avg(beta) = ", $beta->avg, "\n");
  $mp->{tree}->membershipProbPdl( %{$mp->{d2p}},
				  beta=>$beta,
				  @_,
				  pdl=>$phat );

  ##-- class-probability membership hack: bos,eos
  foreach (@$mp{qw(bos eos)}) {
    $phat->slice($mp->{cbenum}->index($_).",") .= 0;
  }

  return $mp->{phat} = $phat;
}

##--------------------------------------------------------------
## $tweakedDist = $mp->bootstrapProfileDist($profileDistNary)
##  + generated $tweakedDist, a distribution over target-words & bound-classes
##    from $profileDistNary, over target- and bound-words.
*boundWords2Clusters = \&bootstrapProfileDist;
sub bootstrapProfileDist {
  my ($mp,$wvdist) = @_;

  my $wenum   = $wvdist->{enum}{enums}[0];
  my $venum   = $wvdist->{enum}{enums}[1];
  my $tenum   = $mp->{tenum};
  my $cbenum  = $mp->{cbenum};

  my $Ncb     = $cbenum->size;
  my $Nt      = $tenum->size;
  my $fwb_pdl = zeroes(double, $Nt, $Ncb);
  my $phat    = $mp->{phat};

  my ($key,$f, $w,$v, $wi, $vs,$vi, $Pbv);
  while (($key,$f)=each(%{$wvdist->{nz}})) {
    ($w,$v) = $wvdist->split($key);

    $wi = $tenum->index($wenum->symbol($w)); ##-- this should never fail

    $vs = $venum->symbol($v);                ##-- nor should this

    if (defined($vi=$tenum->index($vs))) {
      ##-- bound-word, previous target
      $Pbv                      = $phat->slice(",($vi)");
      $fwb_pdl->slice("($wi)") += $Pbv * $f;
    }
    else {
      ##-- whoa, it's a literal singleton class-bound!
      $vi = $cbenum->index($vs);
      $fwb_pdl->slice("$wi,$vi") += $f;
    }
  }

  my $fwb_ed = MUDL::EDist::Nary->new(nfields=>$wvdist->{nfields},
				      sep=>$wvdist->{sep},
				      enum=>MUDL::Enum::Nary->new(nfields=>$wvdist->{enum}{nfields},
								  enums=>[$tenum, $cbenum]),
				     );

  return MUDL::PdlDist->new(pdl=>$fwb_pdl, enum=>$fwb_ed->{enum})->toEDist($fwb_ed);
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

  ##-- tenum_k: old clusters
  my $c2tk = $mp->{c2tk} = {};
  my $tk2c = $mp->{tk2c} = {};
  my $Nc = $mp->{tree}{nclusters};

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

  $mp->vmsg($vl_info, sprintf("update(): unigrams: min(f_{<=k}(w))=%.2f\n", $mp->{ugs_k}->min));

  ##-- update: cluster: toPDL
  $mp->vmsg($vl_info, "update(): toPDL()\n");
  my $tree = $mp->{tree_k} = $mp->{tree}->shadow(enum=>$pprof->{targets});
  $tree->data($pprof->toPDL);

  ##-- update: cluster: cluster()
  $mp->vmsg($vl_info, "update(): cluster()\n");
  $tree->cluster();

  ##-- update: cluster: cut()
  #$mp->vmsg($vl_info, "update(): cut()\n");
  #$tree->cut();

  #return $mp; ##-- DEBUG

  ##-- update: tree
  $mp->vmsg($vl_info, "update(): tree\n");
  $mp->updateTree();

  ##-- update: phat
  #$mp->vmsg($vl_info, "update(): phat ~ ^p( c_{k} | w_{<=k} )\n");
  #$mp->populatePhat();

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
      ##-- whoa: target looks like a pseudo-class  (hack: no smearing)
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
      ##-- whoa, it's a literal singleton class-bound!
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
##    from $mp->{tree_k} and $mp->{phat} (last iteration)
##  + %args:
##    - passed to $mp->{tree_k}->membershipProbPdl()
##  + needs:
##    $mp->{phat}        ## ^p_{<k}
##    $mp->{tenum_ltk}   ## T_{<k}
sub updatePhat {
  my $mp = shift;

  ##-- get ^p_{<k}( c_{k-1} | t_{<k} )  ~ previous iteration
  $mp->vmsg($vl_info, "updatePhat(): ^p_{<k}( c_{k-1} | t_{<k} )\n");
  my $phat_ltk = $mp->{phat};

  ##-- get ^p_k( c_k | t_k + c_{k-1} )  ~ current iteration
  $mp->vmsg($vl_info, "updatePhat(): membershipProbPdl ~ ^p_k( c_k | t_k + c_{k-1} )\n");
  my $beta = $mp->beta;
  $mp->vmsg($vl_info, "updatePhat(): avg(beta) = ", $beta->avg, "\n");
  my $phat_k   = zeroes(double, $mp->{cbenum}->size, $mp->{tenum_k}->size);
  $mp->{tree_k}->membershipProbPdl( %{$mp->{d2p}},
				    @_,
				    beta=>$beta,
				    pdl=>$phat_k );

  ##-- allocate new ^p_{<=k}()
  $mp->vmsg($vl_info, "updatePhat(): adjust ~ ^p_{<=k}( c_k | t_{<=k} )\n");
  my $phat = $mp->{phat} = zeroes(double, $mp->{cbenum}->size, $mp->{tenum}->size);

  ##----------------------------------------------
  ## phat: create: ^p_{<=k} = p_k u p_{<k}

  ## phat: create: add old
  my $tenum     = $mp->{tenum};
  my $tenum_ltk = $mp->{tenum_ltk};
  my $tenum_k   = $mp->{tenum_k};
  my $cids_ltk  = $mp->{tree_ltk}{clusterids};

  $mp->vmsg($vl_info, "updatePhat(): add<k ~ ^p_{<=k} += t_{<k}\n");
  my ($tid_ltk,$tid_lek,$cid_ltk);
  my $c2tk = $mp->{c2tk};
  foreach $tid_ltk (0..($tenum_ltk->size-1)) {
    $cid_ltk = $cids_ltk->at($tid_ltk);
    $phat->slice(",($tid_ltk)") .= $phat_k->slice(",($c2tk->{$cid_ltk})");
  }

  ## phat: create: add new
  $mp->vmsg($vl_info, "updatePhat(): add+k ~ ^p_{<=k} += t_k\n");
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
## $tree = $mp-updateTree()
##  + updates $mp->{tree} from $mp->{tree_k} and $mp->{tree_ltk}
##  + %args:
##    - passed to $mp->{tree}->membershipProbPdl()
##  + needs:
##    $mp->{tree_k}
##    $mp->{tree_ltk}
sub updateTree {
  my $mp = shift;

  ##-- update cluster-ids
  #$mp->vmsg($vl_info, "updateTree(): clusterids ~ w => arg max_c ( p(c|w) )\n");
  my $tenum   = $mp->{tenum};
  my $tenum_k = $mp->{tenum_k};
  my $cids  = zeroes(long, $tenum->size);
  #my $phat  = $mp->{phat};
  #foreach $tid_lek (0..($tenum->size-1)) {
  #  $cids->set($tid_lek, $phat->slice(",($tid_lek)")->maximum_ind);
  #}

  ##-- create new tree
  $mp->vmsg($vl_debug, "updateTree(): tree\n");
  my $tree_k   = $mp->{tree_k};
  my $tree_ltk = $mp->{tree_ltk} = $mp->{tree};
  my $tree     = $mp->{tree} = $tree_ltk->shadow(
						 enum=>$mp->{tenum},
						 #ctree=>$tree_ltk->{ctree},
						 #linkdist=>$tree_ltk->{linkdist},
						 #clusterids=>$cids,
						);

  ##-- populate new tree
  $mp->vmsg($vl_debug, "updateTree(): tree: index (<k)\n");
  my $ct_k   = $tree_k->{ctree};
  my $ct_ltk = $tree_ltk->{ctree};
  my $ct     = $tree->{ctree} = zeroes(long, 2, $tenum->size);

  ##-- populate: find node-ids of old cluster nodes
  ##   + code depends heavily on cuttree() from cluster.c
  my $nelts_ltk = $ct_ltk->dim(1);
  my $njoin_ltk = $nelts_ltk - $tree_ltk->{nclusters};

  ##-- populate: map old node-ids to cluster-ids
  my $cids_ltk = $tree_ltk->{clusterids};
  my %nid2cid = qw();
  my ($did,$cid);
  foreach $i (0..($njoin_ltk-1)) {
    if    ( ($did=$ct_ltk->at(0,$i)) >= 0) {
      ##-- left-leaf
      $cid = $cids_ltk->at($did);
      $nid2cid{-$i-1} = $cid;
    }
    elsif ( ($did=$ct_ltk->at(1,$i)) >= 0) {
      ##-- right-leaf
      $cid = $cids_ltk->at($did);
      $nid2cid{-$i-1} = $cid;
    }
    else {
      ##-- internal node: we should have registered both left and right daughters
      $did = $ct_ltk->at(0,$i);
      $cid = $nid2cid{$did};
      $nid2cid{-$i-1} = $cid;
    }
  }

  ##-- populate: map old cluster-ids to old node-ids
  my %cid2nid = map { $nid2cid{$_}=>$_ } sort {$b<=>$a} keys(%nid2cid);

  ##-- populate: map old singleton-clusterids to old leafnode-ids
  foreach $i ($njoin_ltk..($nelts_ltk-2)) {
    if  ( ($did=$ct_ltk->at(0,$i)) >= 0) {
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

  ##-- populate: add old nodes
  $mp->vmsg($vl_debug, "updateTree(): tree: adopt (<k)\n");
  $ct->slice(",0:".($njoin_ltk-1)) .= $ct_ltk->slice(",0:".($njoin_ltk-1));


  ##-- populate: add new nodes
  $mp->vmsg($vl_debug, "updateTree(): tree: adopt (=k)\n");
  my ($ii,$tid);
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
	}
	else {
	  ##-- real (new) leaf: must translate
	  $ct->set($j,$ii, $tenum->index($tenum_k->symbol($did)));
	}
      }
      else {
	##-- non-leaf daughter
	$ct->set($j,$ii, $did-$njoin_ltk);
      }
    }
  }

  ##-- populate: link distances
  $mp->vmsg($vl_debug, "updateTree(): tree: link distances\n");
  my $linkdist = zeroes(double, $ct->dim(1));

  $linkdist->slice("0:".($njoin_ltk-1))
    .= $tree_ltk->{linkdist}->slice("0:".($njoin_ltk-1));

  $linkdist->slice("$njoin_ltk:".($njoin_ltk + $tree_k->{linkdist}->dim(0) - 1))
    .= $tree_k->{linkdist}->slice("0:".($tree_k->{linkdist}->dim(0)-1));

  ##-- update: finalize
  $mp->vmsg($vl_debug, "updateTree(): cut()\n");
  $tree->{ctree} = $ct;
  $tree->{linkdist} = $linkdist;
  $tree->cut();
  $mp->{clusterids} = $tree->{clusterids};

  return $mp->{tree} = $tree;
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
