##-*- Mode: Perl -*-

## File: MUDL::Corpus::MetaProfile.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus meta-profiles (stage-(k>1))
##======================================================================

package MUDL::Corpus::MetaProfile;
use MUDL::Corpus::Profile;
use MUDL::Cluster::Tree;
use PDL;
use PDL::Cluster;
use Carp;
our @ISA = qw(MUDL::Object Exporter);

##======================================================================
## Corpus::MetaProfile: Globals

our @EXPORT_OK = (map { $@_ } values(%EXPORT_TAGS));
our %EXPORT_TAGS=
  (
   vlevels=>[qw($vl_none $vl_error $vl_warn $vl_debug $vl_full $vl_default)],
  );

##-- verbosity levels
our $vl_none  = 0;
our $vl_error = 1;
our $vl_warn  = 2;
our $vl_info  = 3;
our $vl_debug = 10;
our $vl_full  = 255;

our $vl_default = $vl_debug;

##-- methods (subclass aliases)
our %methods = {
		full => __PACKAGE__ ,
		DEFAULT => __PACKAGE__ ,
		deep => MUDL::Corpus::MetaProfile::Deep,
	       };

##======================================================================
## Corpus::MetaProfile: Constructor
##   + special %args:
##     method => $reclusterMethod,  ##-- subclass flag: see %methods
##   + Object structure:
## {
##  ##
##  ##-- global data
##  phat => $pdlDist,     ## dims: ($NBoundClusters, $NPrevTargets) : $phat->at($cid,$tid) = ^p_{<=$i}($cid|$tid)
##  #phatd => $lexDist,     ## $phatd->{$tid}{$cid} = ^p_{<=$i}($cid|$tid)
##
##  tenum => $enum,        ## previous+current targets (token) enum (T_{<=$i})
##  benum => $enum,        ## previous+current bounds (token) enum (B_{<=$i}) (targets + bos,eos)
##  #ctenum => $enum,       ## (target-)class enum
##  cbenum => $enum,       ## (bound-)class enum: includes bos,eos
##  #cttenum => $enum_nary, ## ($target_class,$target_word) enum
##  cbtenum => $enum_nary, ## ($bound_class,$target_word) enum
##  tree => $cluster_tree, ## MUDL::Cluster::Tree
##  vtree => $viewing_tree, ## viewable tree
##  ##
##  ##-- previous data
##  pprof => $prev_prof,   ## ^f_{<=k}($dir, $bound_cluster, $target_word)
##  #Mprev => $pdl2d,      ## dims: 2*$n_clusters_bounds, $n_clusters_targets
##  #Mhat  => $pdl2d,      ## dims: 2*$n_clusters_bounds, $n_current_word_targets + $n_clusters_targets_old
##  ##
##  ##-- current iteration data
##  #fhat => $edist_nary,  ## $fhat($dir,$classid,$tokid) = ^f_${dir}($tokid,$bound_classid)
##  prof => $profile,      ## current profile: ^f_{$dir}($dir, $bound_word, $target_word)
##  stage => $i,           ## stage number
##  d2p => {method=>'nbest_inverse',n=>8,b=>2},  ## distance-to-probability arguments
##  ##-- messages
##  verbose => $level
## }
## $mp = MUDL::Corpus::MetaProfile->new(%args)
sub new {
  my ($that,%args) = @_;

  my $method = defined($args{method}) ? $args{method} : 'DEFAULT';
  my $class  = $methods{$method};
  if (!defined($class)) {
    confess(ref($that)||$that, "::new(): no class for method '$method'!");
    return undef;
  }

  if ($class eq __PACKAGE__) {
    return $that->SUPER::new(stage => 0,
			     phat  => undef,
			     prof  => undef,
			     d2p => {method=>'nbest_inverse',n=>8,b=>2},
			     verbose => $vl_default,
			     %args);
  } else {
    if (!eval qq(require $class qw();)) {
      confess(__PACKAGE__, "::new(): 'require $class' failed: $@");
      return undef;
    }
    return $class->new(%args);
  }
}

##======================================================================
## Corpus::MetaProfile: Messages

## $mp->vmsg($level, @msg)
sub vmsg {
  my ($mp,$level,@msg) = @_;
  $mp->{verbose} = $vl_default if (!defined($mp->{verbose}));
  return if (!defined($mp->{verbose}) || $mp->{verbose} < $level);
  print STDERR ref($mp), "[stage=$mp->{stage}]: ", @msg;
}

##======================================================================
## Bootstrapping (stage = 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->bootstrap($profile1, $tree1, %args)
##  + initializes $mp->{phat}
##  + ###intitializes $mp->{pprof}
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
  #$mp->{cbenum} = $mp->{cenum}->copy;
  $mp->{cbenum} = $tree->clusterEnum()->copy;
  $mp->{cbtenum} = MUDL::Enum::Nary->new(nfields=>2, enums=>[@$mp{qw(cbenum tenum)}]);
  ##--
  #$mp->{ctenum} = $tree->clusterEnum();
  #$mp->{cttenum} = MUDL::Enum::Nary->new(nfields=>2, enums=>[@$mp{qw(cenum tenum)}]);

  ##-- target-hack: bos,eos
  foreach (@$mp{qw(bos eos)} = @$prof{qw(bos eos)}) {
    #$mp->{cenum}->addSymbol($_);
    #$mp->{tenum}->addSymbol($_);
    $mp->{cbenum}->addSymbol($_);
  }

  ##-- populate {phat}: p(class|word)
  $mp->vmsg($vl_info, "bootstrap(): phat ~ ^p(c|w)\n");
  $mp->populatePhat(%args);

  #return $mp; ##-- DEBUG

  ##-------- populate pprof: ($bcluster, $tcluster)

  ##-- pprof: step 0: create pseudo-profile
  #$mp->vmsg($vl_info, "bootstrap(): pprof ~ f_{<=k}(d, c_b, w)\n");
  #$mp->populatePprof($prof);

  ##-------- populate Mprev: ($ndirs, $bcluster, $tcluster)
  #$mp->vmsg($vl_info, "bootstrap(): Mprev ~ ^M_{<=k}(d, c_b, c_t)\n");
  #
  ##-- Mprev: generate pdl (using tweaked profile)
  #$mp->{Mprev} = $pprof->toPDL;

  return $mp;
}

##--------------------------------------------------------------
## $pprof = $mp->populatePprof()
## $pprof = $mp->populatePprof($prof)
##  + populate $mp->{pprof} ~ f_{<=k}(d, c_b, w) from $prof ~ f_{<=k}(d,v_b,w) and $mp->{phat} ~ ^p_{<=k}(c_b|v_b)
sub populatePprof {
  my ($mp,$prof) = @_;
  $prof = $mp->{prof} if (!defined($prof));

  ##-- copy profile
  my (%nztmp);
  foreach $dir (qw(left right)) {
    ##-- save temp
    $nztmp{$dir} = $prof->{$dir}{nz};
    $prof->{$dir}{nz} = ref($nztmp{$dir})->new();
  }
  my $pprof = $mp->{pprof} = $prof->copy;
  $prof->{$_}{nz} = $nztmp{$_} foreach (qw(left right));

  ##-- pprof: step 1: tweak profile distributions
  $pprof->{left}  = $mp->bootstrapProfileDist($prof->{left});
  $pprof->{right} = $mp->bootstrapProfileDist($prof->{right});

  ##-- pprof: step 2: replace enums
  foreach (@{$pprof->{enum}{enums}}) {
    $_ = $mp->{cbenum} if ($_ eq $pprof->{bounds});
    $_ = $mp->{tenum}  if ($_ eq $pprof->{targets});
  }
  $pprof->{bounds}  = $mp->{cbenum};
  $pprof->{targets} = $mp->{tenum};

  return $pprof;
}

##--------------------------------------------------------------
## $phatPdl = $mp->populatePhat(%args)
##  + populates $mp->{phat} ~ $phat->at($cid,$wid) = ^p($cid | $wid)
##    from $mp->{tree}
##  + %args:
##    - passed to $mp->{tree}->membershipProbPdl()
sub populatePhat {
  my $mp = shift;

  #my $phat = zeroes(double, $mp->{ctenum}->size, $mp->{tenum}->size);
  my $phat = zeroes(double, $mp->{cbenum}->size, $mp->{tenum}->size);
  $mp->{tree}->membershipProbPdl( %{$mp->{d2p}}, @_, pdl=>$phat );
  #$mp->{phatd} = MUDL::PdlDist->new(pdl=>$phat, enum=>$mp->{cttenum})->toEDist();

  ##-- class-probability membership hack: bos,eos
  foreach (@$mp{qw(bos eos)}) {
    $phat->slice($mp->{cbenum}->index($_).",") .= 0;
    #$phat->slice(",".$mp->{tenum}->index($_)) .= 0;
    #$phat->set($mp->{ctenum}->indices($_,$_), 1.0);
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
## Re-clustering (Stage >= 1)
##======================================================================

## $mp = $mp->recluster()
## $mp = $mp->recluster($prof)
##  + updates $mp->{pprof} based on $prof (default=$mp->{prof}) and $mp->{phat}
##  + updates $mp->{tree} (reclusters data) based on $mp->{pprof}
##  + updates $mp->{phat} based on reclustering
sub recluster {
  my ($mp,$prof) = @_;
  $prof = $mp->{prof} if (!defined($prof));
  my $tree  = $mp->{tree};

  ##-- recluster: pprof
  $mp->vmsg($vl_info, "recluster(): pprof ~ f_{<=k}(d, c_b, w)\n");
  my $pprof = $mp->populatePprof($prof);

  ##-- recluster: data
  $mp->vmsg($vl_info, "recluster(): toPDL()\n");
  $tree->data($pprof->toPDL);
  $tree->{enum} = $mp->{tenum};

  $mp->vmsg($vl_info, "recluster(): cluster()\n");
  $tree->cluster();

  $mp->vmsg($vl_info, "recluster(): cut()\n");
  $tree->cut();

  #return $mp; ##-- DEBUG

  ##-- update: phat
  $mp->vmsg($vl_info, "recluster(): phat ~ ^p(c|w)\n");
  $mp->populatePhat();

  ##-- update: pprof
  #$mp->vmsg($vl_info, "recluster(): pprof ~ f_{<=k}(d, c_b, w)\n");
  #$mp->populatePprof($prof);

  return $mp;
}


##======================================================================
## Update (Stage > 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->update($profile)
##  + sets $mp->{prof} to $profile (a profile over bound- & target-words)
##  + updates $mp->{tenum} to include new targets
##  + updates $mp->{benum} to include new bounds
##  + updates $mp->{pprof} to include new profile data
##  + calls recluster()
##  + $profile may be destructively altered!
sub update {
  my ($mp,$prof) = @_;

  ##-- update: stage-number
  ++$mp->{stage};

  ##-- update: profile
  $mp->vmsg($vl_info, "update(): nbounds  = ", $prof->{bounds}->size, "\n");
  $mp->vmsg($vl_info, "update(): ntargets = ", $prof->{targets}->size, "\n");
  $mp->vmsg($vl_info, "update(): profile ~ f_{<=k}(d, v_b, w)\n");
  $mp->{prof} = $prof;

  ##-- update: enums
  $mp->vmsg($vl_info, "update(): enums ~ T_k, B_k\n");
  $mp->{tenum}->addEnum($prof->{targets});
  $mp->{benum}->addEnum($prof->{bounds});

  ##-- update: recluster
  $mp->recluster($prof);

  return $mp;
}

##======================================================================
## ---OLD---
##======================================================================


##--------------------------------------------------------------
## $tweakedDist = $mp->boundWords2Clusters($profileDistNary)
##  + generated $tweakedDist, a distribution over target-words & bound-clusters
##    from $profileDistNary, over target- and bound-words.
##  + still pretty slow, but the best we've got yet [ca. 10s on test 200x200 / nbest=4]
sub boundWords2ClustersOld {
  my ($mp,$wvdist) = @_;

  my $Nc      = $mp->{cenum}->size;
  my $Nt      = $wvdist->{enum}{enums}[0]->size;
  my $fwb_pdl = zeroes(double, $Nt, $Nc);
  my $phat    = $mp->{phat};

  my ($key,$f, $w,$v, $Pbv);
  while (($key,$f)=each(%{$wvdist->{nz}})) {
    ($w,$v) = $wvdist->split($key);

    $Pbv                     = $phat->slice(",($v)"); ##-- implicit bound-reuse!
    $fwb_pdl->slice("($w)") += $Pbv * $f;
  }

  my $fwb_ed = MUDL::EDist::Nary->new(nfields=>$wvdist->{nfields},
				      sep=>$wvdist->{sep},
				      enum=>MUDL::Enum::Nary->new(nfields=>$wvdist->{enum}{nfields},
								  enums=>[
									  $wvdist->{enum}{enums}[0],
									  $mp->{cenum}
									 ]),
				     );

  return MUDL::PdlDist->new(pdl=>$fwb_pdl, enum=>$fwb_ed->{enum})->toEDist($fwb_ed);
}


##--------------------------------------------------------------
## $tweakedDist = $mp->targetWords2Clusters($profileDistNary)
##  + generates $tweakedDist, a distribution over target-clusters & bound-clusters
##    from $profileDistNary, over target-words and bound-clusters.
sub targetWords2Clusters {
  my ($mp,$wbdist) = @_;

  my $Nc      = $mp->{cenum}->size;
  my $fcb_pdl = zeroes(double, $Nc, $Nc);
  my $phat    = $mp->{phat};

  my ($key,$f, $w,$b, $Pcw);
  while (($key,$f)=each(%{$wbdist->{nz}})) {
    ($w,$b) = $wbdist->split($key);

    $Pcw                      = $phat->slice(",($w)");
    $fcb_pdl->slice(",($b)") += $Pcw * $f;
  }

  my $fcb_ed = MUDL::EDist::Nary->new(nfields=>$wbdist->{nfields},
				      sep=>$wbdist->{sep},
				      enum=>MUDL::Enum::Nary->new(nfields=>$wbdist->{enum}{nfields},
								  enums=>[$mp->{cenum},$mp->{cenum}]),
				     );

  return MUDL::PdlDist->new(pdl=>$fcb_pdl, enum=>$fcb_ed->{enum})->toEDist($fcb_ed);
}


##======================================================================
## Attachment (Stage > 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->attach($profileK)
##  + extends @$profileK{'left','right'} to ^f_${dir}($tokid_K, $bound_classid)
##  + computes $mp->{Mhat} ~ \phi(^f) ~ (clustering data for current stage)
##  + updates $mp->{phat}  ~ ^p(c|w)  ~ (by comparing new targets to old cluster assignment)
##  + extends $mp->{tenum} to include $profileK->{targets} strings
##  + updates $mp->{Mprev} to include newly acquired targets
##  + $profileK is destructively altered!
sub attach {
  my ($mp,$prof) = @_;

  $mp->vmsg($vl_info, "attach().\n");

  ##----------------------------
  ## Dummy
  #carp(ref($mp),"::attach(): WARNING: not yet fully implemented!");
  #croak(ref($mp),"::attach(): not yet implemented!");

  ##----------------------------
  ## Assign new profile (loses old literal profile)
  $mp->{prof}  = $prof;
  ++$mp->{stage};

  ##----------------------------
  ## Tweak profile: f_k(d,w,v) ---> f'_k(d,w,b)
  $mp->vmsg($vl_info, "attach(): f_k(d,w,v) --> f'_k(d,w,c_b)\n");
  $prof->{left}  = $mp->boundWords2Clusters($prof->{left});
  $prof->{right} = $mp->boundWords2Clusters($prof->{right});

  #return $mp; ##-- DEBUG

  ##----------------------------
  ## Tweak profile: replace literal bounds with cluster-enum
  $_ = $mp->{cenum} foreach (grep { $_ eq $prof->{bounds} } @{$prof->{enum}{enums}});
  $prof->{bounds} = $mp->{cenum};

  ##----------------------------
  ## Tweak profile: generate new data PDL-2d: ^M(d*Nb+b,w) ~ $mp->{Mhat}
  ##
  ##                                 0..(N_t-1)                (N_t..(N_t+N_c-1))
  ##  + Mhat : (2*$nclusters_bounds, $ntargets_current_words + $ntargets_clusters)
  $mp->vmsg($vl_info, "attach(): Mhat ~ ^M_k(d,c_b,c_t)\n");
  my $Mhat  = $prof->toPDL;
  my $Mprev = $mp->{Mprev};
  my $Nt    = $mp->{prof}{targets}->size;     ## == $Mhat->dim(1) 
  my $Nc    = $mp->{cenum}->size;             ## == $Mprev->dim(1)
  $Mhat->reshape($Mhat->dim(0), $Nt + $Nc);
  $Mhat->slice(",$Nt:".($Nt+$Nc-1)) .= $Mprev;

  ##-- save / debug
  $mp->{Mhat} = $Mhat;
  #$mp->vmsg($vl_debug, "Nt=$Nt ; Nc=$Nc\n");

  #return $mp; ##-- DEBUG

  ##----------------------------
  ## Attach Data: get new distances ~~ add this to Tree() ?!
  $mp->vmsg($vl_info, "attach(): rowdistances()\n");
  my $Dists   = zeroes(double, $Nc, $Nt);
  my $rowids  = sequence(long, $Nt);
  my $mask    = ones(long, $Mhat->dims);
  my $weights = ones(double, $Mhat->dim(0)); # 2*$Nc
  my @eosids  = map { $mp->{cenum}->index($_) } @$mp{qw(bos eos)};
  my %eosids  = map { $_=>1 } @eosids;
  foreach $cid (0..($Nc-1)) {
    next if (exists($eosids{$cid}));
    rowdistances($Mhat,
		 $mask,
		 $weights,
		 $rowids,
		 $cid+$Nt,
		 $Dists->slice("($cid)"),
		 $mp->{tree}{dist},
		 $mp->{tree}{method});
  }
  #$Dists->inplace->setnantobad->inplace->setbadtoval(-1); ##-- eos,bos get 'inf'

  ##-- HACK: eos/bos
  my $dmax = $Dists->max;
  foreach $cid (@eosids) {
    $Dists->slice("($cid)") .= 1000*$dmax; ##-- HACK HACK HACK -- maybe use mask?  bad-flag?
  }

  ##-- save/debug
  $mp->{Dists} = $Dists;
  #return $mp; ##-- DEBUG


  ##----------------------------
  ## Attach Data: update target-enum $mp->{tenum}
  $mp->vmsg($vl_info, "attach(): targets_{<=k} u= targets_k\n");
  my %tk2tlek = qw();
  my $tenum = $mp->{tenum};
  my ($oldid,$newid,$tok);
  while (($tok,$oldid)=each(%{$prof->{targets}{sym2id}})) {
    $tk2tlek{$oldid} = $tenum->addSymbol($tok);
  }

  $mp->{tk2tlek} = \%tk2tlek; ##-- DEBUG
  #return $mp; ##-- DEBUG

  ##----------------------------
  ## Attach Data: convert distances to probabilities: ^p(c|w)
  $mp->vmsg($vl_info, "attach(): phat ~ ^p_k(c|w)\n");
  my $phatk = $mp->{tree}->membershipProbPdl(leafdist=>$Dists,
					     #pdl=>zeroes(double, $Nc, $Nt),
					     %{$mp->{d2p}});
  $mp->{phat}->reshape($mp->{phat}->dim(0), $tenum->size);
  my $phatlek = $mp->{phat};
  while (($oldid,$newid)=each(%tk2tlek)) {
    $phatlek->slice(",($newid)") += $phatk->slice(",($oldid)");
  }

  #return $mp; ##-- DEBUG

  ##----------------------------
  ## Bootstrap: update ^f_{<=k}(d, c_t, c_b) == $mp->{pprof}
  $mp->vmsg($vl_info, "attach(): pprof ~ ^f_{<=k}(d, c_t, c_b)\n");
  foreach $dir (qw(left right)) {
    $mp->addProfileDist($prof->{$dir}, $mp->{pprof}{$dir}, \%tk2tlek);
  }

  ##----------------------------
  ## Update Bootstrapper: refine Mprev == ^M_{<=k}(d, c_b, c_t) == \phi(^f_{<=k}(d, c_b, c_t))

  $mp->vmsg($vl_info, "attach(): Mprev ~ ^M_{<=k}(d, b_c, b_t)\n");
  $mp->{Mprev} = $mp->{pprof}->toPDL;

  ##-- cleanup: delete temporaries
  #delete(@$mp{qw(tk2tlek Mhat Dists)};

  return $mp;
}


##--------------------------------------------------------------
## $mp = $mp->addProfileDist($profDirDist_k, $profDirDist_lek, \%id2id)
##  + extends $profDirDist_lek by $profDirDist_k
##  + %id2id maps old IDs (in $profDirDist_k) to new IDs (for $profDirDist_lek)
##  + pretty ugly, really.
sub addProfileDist {
  my ($mp,$wbdist,$pcbdist,$id2id) = @_;

  my $fcb_pdl = $pcbdist->toPDL();
  my $phat    = $mp->{phat};

  my ($key,$f,$w,$b,$Pcw);
  while (($key,$f)=each(%{$wbdist->{nz}})) {
    ($w,$b) = $wbdist->split($key);

    $Pcw                      = $phat->slice(",($id2id->{$w})");
    $fcb_pdl->slice(",($b)") += $Pcw * $f;
  }

  MUDL::PdlDist->new(pdl=>$fcb_pdl, enum=>$pcbdist->{enum})->toEDist($pcbdist);
  return $pcbdist;
}


##--------------------------------------------------------------
## $tweakedDist = $mp->attachProfileDist($profileDistNary)
##  + generated $tweakedDist, a distribution over target-WORDS & bound-CLASSES
##    from $profileDistNary, over target-words and bound-words.
##
## --> OBSOLETE
*attachProfileDist = \&attachProfileDist1;
sub attachProfileDist1 {
  my ($mp,$wvdist) = @_;

  my $Nc       = $mp->{cenum}->size;
  my $Nt       = $wvdist->{enum}{enums}[0]->size;
  my $fwb_pdl  = zeroes(double, $Nt, $Nc);
  my $phat     = $mp->{phat};

  my ($key,$f, $w,$v, $Pbv);
  while (($key,$f)=each(%{$wvdist->{nz}})) {
    ($w,$v) = $wvdist->split($key);

    $Pbv                     = $phat->slice(",($v)"); ##-- implicit bound-reuse!
    $fwb_pdl->slice("($w)") += $Pbv * $f;
  }

  my $fwb_ed = MUDL::EDist::Nary->new(nfields=>$wvdist->{nfields},
				      sep=>$wvdist->{sep},
				      enum=>MUDL::Enum::Nary->new(nfields=>$wvdist->{enum}{nfields},
								  enums=>[
									  $wvdist->{enum}{enums}[0],
									  $mp->{cenum}
									 ]),
				     );

  return MUDL::PdlDist->new(pdl=>$fwb_pdl, enum=>$fwb_ed->{enum})->toEDist($fwb_ed);
}


##  -> (old version): pretty darn slow [ca. 18s on test 200x200 / nbest=4]
sub attachProfileDist0 {
  my ($mp,$wvdist) = @_;
  my $wbdist = MUDL::EDist::Nary->new(nfields=>$wvdist->{nfields},
				      sep=>$wvdist->{sep},
				      enum=>MUDL::EDist::Nary->new(nfields=>$wvdist->{enum}{nfields},
								   enums=>[
									   $wvdist->{enum}{enums}[0],
									   $mp->{cenum}
									  ]),
				     );
  my $phat = $mp->{phat};
  my ($key,$f, $w,$v, $b, $Pbv);
  while (($key,$f)=each(%{$wvdist->{nz}})) {
    ($w,$v) = $wvdist->split($key);
    $Pbv = $phat->slice(",($v)");

    ##-- actual frequency-smearing: dog slow!
    foreach $b ($Pbv->which->list) {
      $wbdist->{nz}{$w.$wbdist->{sep}.$b} += $Pcv->at($b) * $f;
    }
  }
  return $wbdist;
}


##======================================================================
## Viewing: Tree
##======================================================================

##------------------------------------------------------
## $tree = $mp->toTree(%args)
##   + %args : passed to $mp->{tree}->toTree();
sub toTree {
  my $mp=shift;
  return $mp->{tree}->toTree(@_);
}

##------------------------------------------------------
## $dg = $mp->toDendogram(%args)
##   + %args : passed to $mp->toTree(@_)->toDendogram(@_)
sub toDendogram {
  my $mp = shift;
  return $mp->toTree(@_)->toDendogram(@_);
}

##------------------------------------------------------
## ? = $mp->view(%args)
##   + %args (?)
sub view {
  my $mp = shift;
  return $mp->toDendogram(@_)->view;
}


##------------------------------------------------------
## $tree = $mp->toTree(%args)
##   + %args (?)
sub toTreeOld {
  my $mp = shift;
  require MUDL::Tree;

  ##-- base tree
  my $ct = $mp->{tree};
  my $vt = MUDL::Tree->new();

  ##-- tree properties
  my $Nc = $mp->{cenum}->size;
  $vt->{enum}   = $mp->{tenum};
  #$vt->{dists}  = pdl(1)/($mp->{phat}+1);
  $vt->{dists}  = {};
  $vt->{groups} = {};

  my $tid2cid = $mp->{phat}->maximum_ind;

  ##-- root
  $root = $vt->root('');

  ##-- add clusters
  my %cid2node = qw();
  my %cid2newnode = qw();
  my %cid2oldnode = qw();
  my ($cid,$cstr,$nid);
  foreach $cid (0..($mp->{cenum}->size-1)) {
    $cstr = $mp->{cenum}->symbol($cid);
    $nid = $cid2node{$cid} = $vt->addDaughter($root, $cid);
    $cid2oldnode{$cid}     = $vt->addDaughter($nid, $cid+$Nc);
    $cid2newnode{$cid}     = $vt->addDaughter($nid, $cid+2*$Nc);
  }

  ##-- add targets
  my $ctargets = $mp->{prof}{targets};
  my %tid2node = qw();
  my ($tid,$tstr,$ttid,$cnid);
  while (($tstr,$tid)=each(%{$mp->{tenum}{sym2id}})) {
    $cid = $tid2cid->at($tid);

    $cnid = defined($ctargets->index($tstr)) ? $cid2newnode{$cid} : $cid2oldnode{$cid};
    #$ttid = $tid2node{$tid} = $vt->addDaughter($cnid, $tid);
    $ttid = $tid2node{$tid} = $vt->addDaughter($cnid, $tstr);
    $vt->{groups}{$ttid} = $cid;
  }

  ##-- cleanup: remove empty cluster-nodes
  foreach $cid (0..($mp->{cenum}->size-1)) {
    $nid = $cid2newnode{$cid};
    $vt->addDaughter($nid,"-X-") if ($vt->isLeafNode($nid));

    $nid = $cid2oldnode{$cid};
    $vt->addDaughter($nid,"-X-") if ($vt->isLeafNode($nid));
  }

  return $vt;
}

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
