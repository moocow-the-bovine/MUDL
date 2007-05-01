##-*- Mode: CPerl -*-

## File: MUDL::Corpus::MetaProfile.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus meta-profiles (stage-(k>1))
##======================================================================

package MUDL::Corpus::MetaProfile;
use MUDL::Corpus::Profile;
use MUDL::Cluster::Method;
use MUDL::Corpus::Profile::PdlProfile::Bigrams;
use MUDL::PdlDist::SparseNd;
use PDL;
use PDL::Cluster;
use PDL::CCS::Nd;
use MUDL::CmdUtils qw();
use Carp;

use strict;
our @ISA = qw(MUDL::Object Exporter);

##======================================================================
## Corpus::MetaProfile: Globals

our %EXPORT_TAGS=
  (
   vlevels=>[qw($vl_none $vl_error $vl_warn $vl_info $vl_debug $vl_full $vl_default)],
  );
our @EXPORT_OK = (map { @$_ } values(%EXPORT_TAGS));

##-- verbosity levels
our $vl_none  = 0;
our $vl_error = 1;
our $vl_warn  = 2;
our $vl_info  = 3;
our $vl_debug = 10;
our $vl_full  = 255;

our $vl_default = $vl_debug;

##-- %subclasses: (subclass aliases)
our %subclasses = (
		   full => 'MUDL::Corpus::MetaProfile::Full',
		   deep => 'MUDL::Corpus::MetaProfile::Deep',
		   attach => 'MUDL::Corpus::MetaProfile::Attach',
		   parallel => 'MUDL::Corpus::MetaProfile::Parallel',
		   para     => 'MUDL::Corpus::MetaProfile::Parallel',
		   DEFAULT => 'MUDL::Corpus::MetaProfile::Attach',
		  );

##======================================================================
## Corpus::MetaProfile: Constructor
##   + special %args:
##     class => $subclass_alias,  ##-- subclass flag: see %subclasses
##   + Object structure:
## {
##  ##
##  ##-- global data
##  phat => $pdlDist,     ## dims: ($NBoundClusters, $NPrevTargets) : $phat->at($cid,$tid) = ^p_{<=$i}($cid|$tid)
##  #phatd => $lexDist,     ## $phatd->{$tid}{$cid} = ^p_{<=$i}($cid|$tid)
##
##  tenum => $enum,        ## previous+current targets (type) enum (T_{<=$i})
##  benum => $enum,        ## previous+current bounds (type) enum (B_{<=$i}) (targets + literals)
##  lbenum => $enum,       ## literal current bounds (type) enum (LB_{<=$i}) (e.g. bos,eos)
##  #ctenum => $enum,       ## (target-)class enum
##  cbenum => $enum,       ## (bound-)class enum: includes literals
##  #cttenum => $enum_nary, ## ($target_class,$target_word) enum
##  #cbtenum => $enum_nary, ## ($bound_class,$target_word) enum
##  #--
##  #tree => $cluster_tree, ## MUDL::Cluster::Tree
##  cm => $cluster_method,  ## a MUDL::Cluster::Method object
##  #--
##  bos=>$bos,eos=>$eos,    ## as for Profile
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
##  #d2p => {method=>'nbest_inverse',n=>8,b=>2},  ## distance-to-probability arguments: now in MUDL::Cluster::Method
##  ##
##  ##-- messages
##  verbose => $level
##  ##
##  ##-- viewing
##  encoding => $encoding, ##-- for viewing
## }
## $mp = MUDL::Corpus::MetaProfile->new(%args)
sub new {
  my ($that,%args) = @_;

  ##-- dispatch on optional 'class' argument
  if (!ref($that) && exists($args{class})) {
    $that = $args{class};
    delete $args{class};
    $that = $subclasses{$that} if (exists($subclasses{$that}));
    $that = "MUDL::Corpus::MetaProfile::$that" if ($that !~ /::/);
    MUDL::CmdUtils::loadModule($that);
    return $that->new(%args);
  }

  return $that->SUPER::new(stage => 0,
			   phat  => undef,
			   prof  => undef,
			   #d2p => {method=>'nbest_inverse',n=>8,b=>2},
			   verbose => $vl_default,
			   %args);
}

########################################################################
## Utilities: Messages
########################################################################

## $mp->vmsg($level, @msg)
sub vmsg {
  my ($mp,$level,@msg) = @_;
  $mp->{verbose} = $vl_default if (!defined($mp->{verbose}));
  return if (!defined($mp->{verbose}) || $mp->{verbose} < $level);
  print STDERR ref($mp), "[stage=$mp->{stage}]: ", @msg;
}

########################################################################
## Utiltiies: squeezing
########################################################################

## $mp = $mp->squish()
##  + reduce memory usage, assuming the object will no
##    longer be used to profile and/or cluster
##  + good idea as a prelude to $mp->toHMM()
sub squish {
  my $mp=shift;
  delete(@$mp{
	      qw(prof pprof ctrprof curprof),
	      qw(tenum_k tenum_ltk),
	      #qw(cm cm_k),
	      qw(cm_k),
	      qw(t2tk tk2t ugs_k),
	      qw(cdata cmask),
	     });
  return $mp;
}

########################################################################
## API: specification
########################################################################

##======================================================================
## API: Bootstrapping (stage = 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->bootstrap($profile1, $cm1, %args)
##  + initializes $mp->{phat}
##  + %args contains additional arguments for $cm1->membershipProbPdl()
##  + $profile1 and $cm1 may both be destructively altered!
*bootstrap = MUDL::Object::dummy('bootstrap');


##======================================================================
## API: Bootstrapped (stage > 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->update($profile)
## $mp = $mp->update($profile,%args)
##   + must update object based on (new) profile $profile
##   + must update $mp->{phat} to include new data
##   + %args are any additional arguments
*update = MUDL::Object::dummy('update');


##======================================================================
## API: literal bounds enum
##======================================================================

## $lbenum = $mp->lbenum()
##  + for compatibility; if no {lbenum} is defined, instantiates one with {bos,eos}
sub lbenum {
  return $_[0]{lbenum} if (defined($_[0]{lbenum}));
  my $mp = shift;
  my $lbenum = $mp->{lbenum} = MUDL::Enum->new();
  $lbenum->addSymbol($mp->{bos}) if (defined($mp->{bos}));
  $lbenum->addSymbol($mp->{eos}) if (defined($mp->{eos}));
  return $lbenum;
}

########################################################################
## Export & Conversion
########################################################################

##======================================================================
## Export: to LR3
##======================================================================

## $lr3 = $mp->toLR3(%args)
##  + creates & returns a new MUDL::Corpus::MetaProfile::LR3 from this object
##  + %args are passed to MUDL::Corpus::MetaProfile::LR3->new()
sub toLR3 {
  my ($mp,%args) = @_;
  require MUDL::Corpus::MetaProfile::LR3;
  MUDL::CmdUtils::loadModule($mp->{prof});

  my $lr3 = MUDL::Corpus::MetaProfile::LR3->new(%args);
  $lr3->{tenum1} = $mp->{tenum};
  $lr3->{cenum}  = $mp->{cenum};
  $lr3->{cbenum} = $mp->{cbenum};
  if (defined($mp->{phatm})) {
    $lr3->{phat}  = $mp->{phat} * $mp->{phatm};
    $lr3->{phat} /= $lr3->{phat}->sumover->slice("*1,");
  } else {
    $lr3->{phat} = $mp->{phat};
  }
  $lr3->{cids}   = $mp->{clusterids};
  $lr3->{uprof}  = $mp->{prof}->shadow;
  $lr3->{uprof}->setEnums(MUDL::Enum->new,MUDL::Enum->new);
  $lr3->reset();

  return $lr3;
}

##======================================================================
## Export: to Map
##======================================================================
sub toMap {
  my $mp = shift;
  my $cm = $mp->{cm};
  $mp->{cm}{cdmatrix} = -($mp->{phat}*$mp->{phatm}) if (!defined($mp->{cm}{cdmatrix}));
  return $cm->toMap(@_);
}

##======================================================================
## Export: to HMM
##======================================================================

## $hmm = $mp->toHMM($bigrams_pdldist_sparsend,%args)
##   + %args are passed to MUDL::HMM->New(),
##     execpt for:
##      arcmode => $mode, ##-- either 'uniform' (default) or 'estimate'
##   + returns a new HMM with uniform arc transition probabilities,
##     and observation probabilities initialized by Bayesian inversion
##     of $mp->{phat}
##   + $unigrams is a MUDL::Dist (or similar) representing word-unigrams,
##     used for inversion
sub toHMM {
  my ($mp,$bgpd,%args) = @_;
  require MUDL::HMM;

  $mp->vmsg($vl_info, "toHMM(): new HMM\n");
  $args{type} = double if (!defined($args{type}));

  my $arcmode = defined($args{arcmode}) ? $args{arcmode} : 'uniform';
  $args{arcmode} = 'uniform' if (!defined($args{arcmode}));
  $args{smoothb} = $mp->{smoothb} if (!defined($args{smoothb}) && defined($mp->{smoothb}));

  my $phat = $mp->{phat};
  my $N    = $phat->dim(0);
  my $M    = $phat->dim(1);

  my $hmm   = MUDL::HMM->new(bos=>$mp->{bos},eos=>$mp->{eos},%args);

  ##-- add enums
  $mp->vmsg($vl_info, "toHMM(): enums\n");
  my $tenum = $mp->{tenum};
  #my $cenum = $mp->{cm}->clusterEnum;
  my $cbenum = $mp->{cbenum};
  my $lbenum = $mp->lbenum(); ##-- use method here for backwards-compatibility
  my $qenum  = $hmm->{qenum};
  my $oenum  = $hmm->{oenum};
  my $wenum  = $bgpd->{enum}{enums}[0]; ##-- bigram enum
  ##
  ##-- HMM enums: qenum
  push(@{$qenum->{id2sym}}, grep {!exists($lbenum->{sym2id}{$_})} @{$cbenum->{id2sym}});
  $qenum->{sym2id}{$qenum->{id2sym}[$_]}=$_ foreach (0..$#{$qenum->{id2sym}});
  ##
  ##-- HMM enums: oenum
  push(@{$oenum->{id2sym}}, @{$tenum->{id2sym}});
  $oenum->{sym2id}{$oenum->{id2sym}[$_]}=$_ foreach (0..$#{$oenum->{id2sym}});

  $N = $qenum->size;
  $M = $oenum->size;

  ##-- map cluster indices
  $mp->vmsg($vl_info, "toHMM(): index translation vectors\n");

  my $unknown = $hmm->{unknown};
  my $uid     = $oenum->{sym2id}{$unknown};

  ##-- $o2o : HMM "O" indices of all known observations
  my ($o2o);
  if    ($uid==0)      { $o2o = sequence(long,$M-1)+1; }
  elsif ($uid==($M-1)) { $o2o = sequence(long,$M-1); }
  else                 { $o2o = sequence(long,$uid)->append(sequence(long,$M-($uid+1))+($uid+1)); }

  ##-- $q2c->at($qid) = $clusterid
  my $q2c = pdl(long, [ @{$cbenum->{sym2id}}{ @{$qenum->{id2sym}} } ]);

  ##-- $c2q->at($cid) = $qid
  my $c2q = $mp->{cenum}->xlatePdlTo($qenum, badval=>-1);

  ##-- $o2t->at($oid) = $targetid
  my $o2t = pdl(long, [ @{$tenum->{sym2id}}{ @{$oenum->{id2sym}}[$o2o->list] } ]);

  ##-- $t2w : $targetid => $wid
  ##   $t2o : $oid      => $wid
  my $t2w = pdl(long, [ @{$wenum->{sym2id}}{ @{$tenum->{id2sym}} } ]);
  my $o2w = pdl(long, [ @{$wenum->{sym2id}}{ @{$oenum->{id2sym}}[$o2o->list] } ]);

  ##----------------------------
  ## create observation frequency matrix
  my $bmode = $args{bmode};
  $bmode = 'invert' if (!defined($bmode));
  $mp->vmsg($vl_info, "toHMM(): observation probabilities (mode='$bmode')\n");
  my $bf = zeroes($hmm->{type}, $N, $M);

  if ($bmode =~ /^sim/) {
    ## p(w|c)  = sim(c,w) / \sum_c' sim(c',w)  ##--> REALLY? ought to be: sim(c,w)/sum_w' sim(c,w') !
    my $bf_o  = $bf->dice_axis(1,$o2o);
    $bf_o    .= $phat->dice($q2c,$o2t);

    if ($bmode =~ /\+ebonus-([\d\.]+)/) {
      ##-- exponential bonus: looks good
      my $bonus = $1;
      $bf_o->inplace->pow($bonus);
      $bf_o->xchg(0,1) /= $bf_o->sumover; ##-- re-normalize
    }
    elsif ($bmode =~ /\+ebbonus-([\d\.]+)/) {
      ##-- beta-dependent exponential bonus: not so hot
      my $bonus  = $1;
      my $tbeta  = $mp->{tbeta};
      $bf_o->inplace->pow( $bonus * $tbeta->dice($o2t)->slice("*1,")**-1 );
      $bf_o->xchg(0,1) /= $bf_o->sumover; ##-- re-normalize
    }
    elsif ($bmode =~ /\+pebbonus-([\d\.]+)-([\d\.]+)/) {
      ##-- beta-dependent exp-linear function: not so hot
      my $pbonus = $1;
      my $ebonus  = $2;
      my $tbeta  = $mp->{tbeta};
      $bf_o->inplace->pow($ebonus * ($tbeta->dice($o2t)->slice("*1,")**-1 + $pbonus));
      $bf_o->xchg(0,1) /= $bf_o->sumover; ##-- re-normalize
    }

    #$bf_o   /= $bf_o->sumover->slice("*1,");
    $bf_o    *= $mp->{phatm}->dice($q2c,$o2t) if ($bmode =~ /\+mask/);
    #$bf_o   /= $bf_o->sumover->slice("*1,");
    $bf_o->inplace->setnantobad->inplace->setbadtoval(0);
  }
  elsif ($bmode =~ /^invert/) {
    ##-- create observation probability matrix: step 1: unigrams (old indices)
    $mp->vmsg($vl_info, "toHMM(): target unigram PDL\n");
    my $ugp = $bgpd->{pdl}->sumover->todense->index($t2w)->convert($args{type})->slice("*1,");

    ##-- create observation probability matrix: step 2: invert
    $mp->vmsg($vl_info, "toHMM(): observation probabilities\n");
    ##
    ## ^f(c,w) = ^p(c,w) * N             ##-- MLE
    ##         = ^p(c|w) * ^p(w) * N     ##-- mult rule
    ##         = ^p(c|w) *  p_ML(w) * N  ##-- assume MLE: ^p(w) = p_ML(w) = f(w)/N
    ##         = ^p(c|w) *  f(w)         ##-- substitution
    ## \hfill\Box
    ##   ... but bad results!
    ##   -> looks like these have to do with n-best heuristic and
    ##      link-method (max)
    ##
    #$bf->dice_axis(1,$o2o) .= $phat->dice($q2c,$o2t) * $ugp->dice_axis(1,$o2t);
    my $bf_o = $bf->dice_axis(1,$o2o);
    $bf_o   .= $phat->dice($q2c,$o2t);

    if ($bmode =~ /\+bonus-(\d+)/) {
      my $bonus = $1;
      my $cids = $mp->{clusterids};
      my $cidi = zeroes(long,2,$cids->nelem);
      $cidi->dice(0,$o2t)->slice("(0),") .= $cids;
      $cidi->dice(1,$o2t)->slice("(0),") .= sequence(long,$o2t->nelem);
      $bf_o->indexND($cidi)    += $bonus;
      $bf_o->xchg(0,1) /= $bf_o->sumover; ##-- re-normalize
    }
    elsif ($bmode =~ /\+ebonus-([\d\.]+)/) {
      ##-- exponential bonus: looks good at 7 <= $bonus <= 8
      my $bonus = $1;
      $bf_o->inplace->pow($bonus);
      $bf_o->xchg(0,1) /= $bf_o->sumover; ##-- re-normalize
    }
    elsif ($bmode =~ /\+ebbonus-([\d\.]+)/) {
      ##-- beta-dependent exponential bonus: not so hot
      my $bonus  = $1;
      my $tbeta  = $mp->{tbeta};
      $bf_o->inplace->pow( $bonus * $tbeta->dice($o2t)->slice("*1,")**-1 );
      $bf_o->xchg(0,1) /= $bf_o->sumover; ##-- re-normalize
    }
    elsif ($bmode =~ /\+pebbonus-([\d\.]+)-([\d\.]+)/) {
      ##-- beta-dependent exp-linear function: not so hot
      my $pbonus = $1;
      my $ebonus  = $2;
      my $tbeta  = $mp->{tbeta};
      $bf_o->inplace->pow($ebonus * ($tbeta->dice($o2t)->slice("*1,")**-1 + $pbonus));
      $bf_o->xchg(0,1) /= $bf_o->sumover; ##-- re-normalize
    }
    elsif ($bmode =~ /\+eipwbonus-([\-\d\.]+)/) {
      ##-- inverse wordprob exponential bonus: \fhat(w,c) = \phat(c|w) ** (p(w)**-$bonus)
      my $bonus = $1;
      my $ugn = $ugp->sum;
      $bf_o->inplace->pow($bonus * $ugp->dice_axis(1,$o2t)**(-1));
      $bf_o->xchg(0,1) /= $bf_o->sumover; ##-- re-normalize
    }

    ##-- back to ye olde grinde
    if ($bmode =~ /\+mask/) {
      $bf_o *= $mp->{phatm}->dice($q2c,$o2t);
      $bf_o->xchg(0,1) /= $bf_o->sumover; ##-- re-normalize
    }
    $bf_o *= $ugp->dice_axis(1,$o2t);
  } else {
    confess(ref($mp), "::toHMM(): unknown observation probability mode '$bmode'!");
  }

  ##----------------------------
  ## observations: number of types
  $mp->vmsg($vl_info, "toHMM(): cluster sizes\n");
  my $btypec = $mp->{cm}->clusterSizes()->index($q2c);

  ##----------------------------
  ## observations: unknown handling: by number of types
  if (defined($args{smoothu}) && $args{smoothu} =~ /^types-([\d\.]+)/) {
    $args{smoothutotal} = $1;
    $args{smoothu} = 'types';
  }

  ##----------------------------
  ## name classes
  $mp->vmsg($vl_info, "toHMM(): cluster names\n");
  my $cemask = $mp->{cm}->clusterElementMask();
  my $q2o    = zeroes(long,2,$N);
  ($bf->dice_axis(1,$o2o) * $cemask->dice($q2c,$o2t))->xchg(0,1)->maximum_n_ind($q2o);
  my ($cstr,$cid);
  foreach $cid (0..($N-1)) {
    #($cstr=$qenum->symbol($cid)) ~= tr/0-9/A-J/;
    #$cstr .= join('_', map { $oenum->symbol($_) } $q2o->slice(",($cid)")->list);
    ##--
    $cstr = join('_',
		 $qenum->{id2sym}[$cid],
		 map { $oenum->{id2sym}[$_] } $o2o->index($q2o->slice(",($cid)"))->list
		);
    $qenum->addIndexedSymbol($cstr, $cid);
  }

  ##----------------------------
  ##-- arc probabilities
  my $af  = zeroes($hmm->{type}, $N, $N);
  my $pif = zeroes($hmm->{type}, $N);
  my $omegaf = zeroes($hmm->{type}, $N);

  if ($arcmode =~ /^est/) {
    $mp->vmsg($vl_info, "toHMM(): transition probabilities (mode=$arcmode)\n");

    ##-- bos,eos detection
    my ($bos,$eos) = @$mp{qw(bos eos)};

    ##-- temporary
    #my $phatd = $phat->dice_axis(0,$q2c);
    #my $prod1 = zeroes($af->type, $af->dim(0));
    #my $prod2 = zeroes($af->type, $af->dims);

    ##-- 'estimate1': use hard assignments to estimate arc probabilities
    if ($arcmode eq 'estimate1') {

      ##-- arcmode==estimate1: get hard cluster-id assignment
      my ($cids);
      if (defined($mp->{clusterids})) {
	$cids = $mp->{clusterids};
      } elsif (defined($mp->{cm}{clusterids}) && $mp->{cm}{clusterids}->nelem == $mp->{tenum}->size) {
	$cids = $mp->{cm}{clusterids};
      } else {
	$cids = $phat->maximum_ind;
      }

      ##-- arcmode==estimate1: get (word->state) PDL::CCS::Nd translation matrix:
      ##     $w2q($NWords,$NStates) : $wid -> cid(tid($w))
      ##   + no BOS, EOS here yet
      my $cenum  = $mp->{cenum};
      my $w2t_xi = $wenum->xlatePdlTo($tenum, badval=>-1); ##-- $wid => $tid
      my $w2t_xw = which($w2t_xi>=0);                      ##-- good($wid=>$tid)
      my $xw2c   = $cids->index($w2t_xi->index($w2t_xw));  ##-- $wid => $cid
      my $w2q    = PDL::CCS::Nd->newFromWhich(
					      $w2t_xw->cat($c2q->index($xw2c))->xchg(0,1),
					      ones(byte, $w2t_xw->dim(0)),
					      missing => 0,
					      pdims   => pdl(long, $wenum->size, $qenum->size),
					     );

      ##-- arcmode==estimate1: get (state->state) transition frequencies: $af
      ##     ( w2q (W, Q ) x bgww (W1,W2) ) -> bgwq(W1,Q2)
      ##     ( bgwq(W1,Q2) x w2q^T(Q, W)  ) -> bgqq(Q1,Q2)
      my $bgwq = $w2q  x $bgpd->{pdl};
      my $bgqq = $bgwq x $w2q->xchg(0,1);
      $bgqq->decode($af);

      ##-- arcmode==estimate1: get (bos->state) transition frequencies: $pif
      ##     ( w2q(W,Q) x bg_bosw(1, W2) ) -> bg_bosq(1,Q)
      my $bg_bosw = $bgpd->{pdl}->dice_axis(0,$wenum->{sym2id}{$bos}); ##-- bg_bosw(1,W2)
      my $bg_bosq = $w2q x $bg_bosw;
      $pif .= $bg_bosq->decode->flat;

      ##-- arcmode==estimate1: get (state->eos) transition frequencies: $omegaf
      ##     ( bg_eosw(W1, 1) x w2q^T(Q,W) ) -> bg_eosq(Q,1)
      my $bg_eosw = $bgpd->{pdl}->dice_axis(1,$wenum->{sym2id}{$eos});
      my $bg_eosq = $bg_eosw x $w2q->xchg(0,1);
      $omegaf .= $bg_eosq->decode->flat;
    }
    ##--------------------------------------------------------------
    ## 'estimateb': use unigram probabilities drawn from 'bf' matrix
    elsif ($arcmode eq 'estimateb') {
      ##-- (later)
      $mp->vmsg($vl_info, "toHMM(): arcmode='estimateb': delayed.\n");
      $af     .= 1;
      $pif    .= 1;
      $omegaf .= 1;
    }
    else { #if ($arcmode eq 'estimate')
      ##--------------------------------------------------------------
      ## 'estimate': use $phat to estimate transition probabilities

      ##-- arcmode==estimate: get (word->state) PDL::CCS::Nd translation matrix:
      ##   $w2q($NWords,$NStates) : ($wid,$cid) -> p($cid|$wid)
      my $cenum   = $mp->{cenum};
      my $w2t_xi  = $wenum->xlatePdlTo($tenum, badval=>-1);      ##-- $wid       => $tid
      my $w2t_xw  = which($w2t_xi>=0);                           ##-- sequence() => $wid : good($wid=>$tid)
      my $xw_phat = $phat * $mp->{phatm};                        ##-- ($cid,$tid) => p($cid|$tid)
      $xw_phat   /= $xw_phat->sumover->slice("*1,");
      #$xw_phat = $xw_phat->dice($q2c,$w2t_xi->index($w2t_xw));   ##-- ($qid,good($tid)) => p($qid|$tid)
      my $xw_phat_which  = $xw_phat->whichND;                    ##-- ($cid,$tid)
      my $xw_phat_nzvals = $xw_phat->indexND($xw_phat_which);
      my $t2w_xi  = $tenum->xlatePdlTo($wenum, badval=>-1);
      $xw_phat_which->slice("(1),") .= $t2w_xi->index($xw_phat_which->slice("(1),"));
      $xw_phat_which->slice("(0),") .= $c2q->index($xw_phat_which->slice("(0),"));

      my $w2q  = PDL::CCS::Nd->newFromWhich(
					    $xw_phat_which->slice("-1:0,"),
					    $xw_phat_nzvals,
					    missing => 0,
					    pdims   => pdl(long, $wenum->size, $qenum->size),
					   );

      ##-- arcmode==estimate: get (state->state) transition frequencies: $af
      ##     ( w2q (W, Q ) x bgww (W1,W2) ) -> bgwq(W1,Q2)
      ##     ( bgwq(W1,Q2) x w2q^T(Q, W)  ) -> bgqq(Q1,Q2)
      my $bgwq = $w2q  x $bgpd->{pdl};
      my $bgqq = $bgwq x $w2q->xchg(0,1);
      $bgqq->decode($af);

      ##-- arcmode==estimate: get (bos->state) transition frequencies: $pif
      ##     ( w2q(W,Q) x bg_bosw(1, W2) ) -> bg_bosq(1,Q)
      my $bg_bosw = $bgpd->{pdl}->dice_axis(0,$wenum->{sym2id}{$bos}); ##-- bg_bosw(1,W2)
      my $bg_bosq = $w2q x $bg_bosw;
      $pif .= $bg_bosq->decode->flat;

      ##-- arcmode==estimate: get (state->eos) transition frequencies: $omegaf
      ##     ( bg_eosw(W1, 1) x w2q^T(Q,W) ) -> bg_eosq(Q,1)
      my $bg_eosw = $bgpd->{pdl}->dice_axis(1,$wenum->{sym2id}{$eos});
      my $bg_eosq = $bg_eosw x $w2q->xchg(0,1);
      $omegaf .= $bg_eosq->decode->flat;
    }
  }
  else {
    $mp->vmsg($vl_info, "toHMM(): transition probabilities (mode=uniform)\n");
    $af     .= 1;
    $pif    .= 1;
    $omegaf .= 1;
  }

  ##-- debug
  #@$mp{qw(af bf pif omegaf)} = ($af,$bf,$pif,$omegaf);
  #return ($af,$bf,$hmm,$c2q,$t2o,$ugp); ##-- DEBUG

  ##----------------------------
  ## compile
  $mp->vmsg($vl_info, "toHMM(): compile()\n");
  $hmm->compilePdls($af,$bf,$pif,$omegaf, btypec=>$btypec,%args);

  ##----------------------------
  ## restrict
  if (defined($args{restrictby})) {
    if ($args{restrictby} =~ /^freq-z([\d\.\+\-e]+)\+q([\d\.]+)/) {
      $mp->vmsg($vl_info, "toHMM(): restrictByValue (zero=>$1,Q=>$2)\n");
      $hmm->restrictByValue($hmm->bf(), zero=>$1, Q=>$2);
    }
    elsif ($args{restrictby} ne 'none') {
      confess(ref($mp), "::toHMM(): unknown restriction method '$args{restrictby}'!");
    }
  }

  ##----------------------------
  ## estimate: arcs: 'estimateb'
  if (defined($args{arcmode}) && $args{arcmode} eq 'estimateb') {
    ##-- delayed arc-probability estimation
    $mp->vmsg($vl_info, "toHMM(): estimate: arcs (finally): mode=$args{arcmode}\n");

    my $b1f         = ($hmm->bf() * $hmm->restrictionMask())->xchg(0,1)->sumover;
    my $a1f         = $b1f / ($N+1);
    #$af            .= $a1f;
    $af->xchg(0,1) .= $a1f;
    $pif           .= $a1f;
    $omegaf        .= 1;

    ##-- recompile
    $hmm->compileArcs($af, $omegaf, %args);
    $hmm->compilePi($pif, %args);
    $hmm->smootha(%args);
  }


  ##-- return
  return $hmm;
}

########################################################################
## Description Length : BUGGY
########################################################################

##------------------------------------------------------
## $log2n = log2($n);
our $LOG2 = log(2);
sub log2 { return log($_[0])/$LOG2; }

##------------------------------------------------------
## $nbits = listLambda($list_length)
##   + $nbits is the number of bits required to declare
##     a list of length $list_length in an optimal encoding
##   + see Goldsmith (2001) & the references cited there for details
sub listLambda {
  return
    #log2($_[0]); ##-- this is not quite enough...
    log2($_[0]+1); ##-- how about this?
}

##------------------------------------------------------
## $length = $mp->modelDescLen()
## $length = $mp->modelDescLen($clusterSizesPdl)
##   + get description length of model

#*modelDescLen = \&modelDescriptionLength;
#*modelDescLen = *modelDescriptionLength = \&modelDescriptionLength_v1;
*modelDescLen = *modelDescriptionLength = \&modelDescriptionLength_v2;
#*modelDescLen = *modelDescriptionLength = \&modelDescriptionLength_v3;

sub modelDescriptionLength_v3 {
  my ($mp,$csizes) = @_;
  $csizes = $mp->{cm}->clusterSizes()      ##-- get cluster sizes
    if (!defined($csizes));

  my $ntyps = $csizes->sumover + 1;        ##-- get number of targets (+ UNKNOWN)

  my $len = listLambda($csizes->nelem);    ##-- list of all classes

  $len += listLambda($csizes)->sumover;    ##-- foreach class: list of class elements

  ##--
  $len += sumover($csizes*log2($csizes));  ##-- v2: foreach class: foreach word: word-id
                                            ##-- where p(w) ~= 1/|class(w)|
                                            ##   -> all class-internal word-types are equiprobable for model
                                            ##   -> prefers lots of small clusters

  ##-- unknown class
  #$len += 0; # 1*log2(1) == 0

  return $len->at(0);
}

sub modelDescriptionLength_v2 {
  ##   + "small" models are those with fewer classes
  my ($mp,$csizes) = @_;

  $csizes = $mp->{cm}->clusterSizes()      ##-- get cluster sizes
    if (!defined($csizes));

  my $ntyps = $csizes->sum + 1;            ##-- get number of target-types (+ UNKNOWN)
  my $nclasses = $csizes->dim(0) + 1;      ##-- get number of class-types  (+ UNKNOWN)

  my $len = listLambda($ntyps);            ##-- list of all targets (+ UNKNOWN)

  $len += $ntyps * log2($nclasses);        ##-- foreach target w: c=class(w),
                                           ##   -> where p(c) ~= 1 / nclusters
                                           ##   -> no a priori class probability for model encoding

  return $len;
}

sub modelDescriptionLength_v1 {
  my ($mp,$csizes) = @_;
  $csizes = $mp->{cm}->clusterSizes()      ##-- get cluster sizes
    if (!defined($csizes));

  my $ntyps = $csizes->sumover + 1;        ##-- get number of targets (+ UNKNOWN)

  my $len = listLambda($csizes->nelem);    ##-- list of all classes

  $len += listLambda($csizes)->sumover;    ##-- foreach class: list of class elements

  ##------
  ##--
  #$len += sumover($csizes*log2($ntyps));   ##-- v1: foreach class: foreach word: word-id
                                            ##-- where p(w) ~= 1/|Targets|
                                            ##   -> all word-types are equiprobable for model-encoding
                                            ##   -> favors a single large cluster (but not too strongly...)
  ##--
  $len += $csizes->sumover*log2($ntyps);   ##-- as (v1), above
  ##--
  #$len += sumover($csizes*log2($csizes));  ##-- v2: foreach class: foreach word: word-id
                                            ##-- where p(w) ~= 1/|class(w)|
                                            ##   -> all class-internal word-types are equiprobable for model
                                            ##   -> prefers lots of small clusters

  ##-- unknown class
  #$len += 0; # 1*log2(1) == 0

  return $len->at(0);
}


##------------------------------------------------------
## $length = $mp->ugDescLen($mudl_unigrams,$cids)
## $length = $mp->ugDescLen($mudl_unigrams)
##   + get description length of corpus (as unigrams)

#*corpusDescLen = \&corpusDescriptionLength;
*corpusDescLen = *corpusDescriptionLength = \&corpusDescriptionLength_v2;

#our $DEFAULT_CDL_MODE = 'u,u';
#our $DEFAULT_CDL_MODE = 'u,ml';
our $DEFAULT_CDL_MODE = 'ml,u';
#our $DEFAULT_CDL_MODE = 'ml,ml';
sub corpusDescriptionLength_v2 {
  my ($mp,$ugs,$cids,$mode) = @_;
  $mode=$DEFAULT_CDL_MODE if (!defined($mode));

  ##-- get cluster-id map & target enum
  $cids = $mp->{clusterids} if (!defined($cids));
  my $tenum = $mp->{tenum};

  ##-- DON'T reshape cluster-id map (NOT adding "unknown" word)
  my $nclusters = $cids->max+1;

  ##-- get cluster sizes (NOT adding "unknown" cluster)
  my $csizes = zeroes(long,$nclusters);
  PDL::Cluster::clustersizes($cids,$csizes);

  ##-- get cluster-frequency pdl
  my $cf   = zeroes(double,$nclusters);

  ##-- get cluster unigram frequencies
  my ($w,$wid,$cid,$fw, $nunknown);
  my $widf = zeroes(double,$tenum->size);
  foreach $wid (0..($tenum->size-1)) {
    $w   = $tenum->{id2sym}[$wid];
    $fw  = $ugs->{$w};
    $cid = $cids->at($wid);
    $cf->index($cid) += $fw;
    $widf->index($wid) += $fw;
  }

  ##-- compute corpus description length
  my $len = 0;
  my $ftotal = $cf->sumover;
  if ($mode eq 'u,u') {
    ## p(c)   [UNIFORM:1/nclusters]
    ## p(w|c) [UNIFORM:1/cluster-size]
    ## --> favors single huge cluster
    my $ntyps = pdl(double,$widf->nelem);
    $len += sumover($widf * (
			     -log2($nclusters**-1)               ## p(c)   [UNIFORM:1/nclusters]
			     -log2($ntyps**-1)                   ## p(w|c) [ML:word-tokens/cluster-tokens]
			    )
		   );
  }
  elsif ($mode eq 'u,ml') {
    ## p(c)   [UNIFORM:1/nclusters]
    ## p(w|c) [ML:word-tokens/cluster-tokens]
    ## --> favors single huge cluster
    $len += sumover($widf * (
			     -log2($nclusters**-1)               ## p(c)   [UNIFORM:1/nclusters]
			     -log2($widf/$cf->index($cids))      ## p(w|c) [ML:word-tokens/cluster-tokens]
			    )
		   );
  }
  elsif ($mode eq 'ml,u') {
    ## p(c)   [ML]
    ## p(w|c) [UNIFORM:1/cluster-size]
    ## --> favors many small clusters
    $len += sumover($cf * (
			   -log2($cf/$ftotal)            ## p(c)   [ML]
			   -log2($csizes**-1)            ## p(w|c) [UNIFORM:1/cluster-size]
			  )
		   );
  }
  elsif ($mode eq 'ml,ml') {
    ## p(c)   [ML]
    ## p(w|c) [ML:word-tokens/cluster-tokens]
    ## --> doesn't favor any model at all: it's all ML...
    $len += sumover($widf * (
			     -log2(($cf/$ftotal)->index($cids))  ## p(c)   [ML]
			     -log2($widf/$cf->index($cids))      ## p(w|c) [ML:word-tokens/cluster-tokens]
			    )
		   );
  }

  return $len->at(0);
}

sub corpusDescriptionLength_v1 {
  my ($mp,$ugs,$cids0) = @_;

  ##-- get cluster-id map & target enum
  $cids0 = $mp->{clusterids} if (!defined($cids0));
  my $tenum = $mp->{tenum};

  ##-- reshape cluster-id map (adding "unknown" word)
  my $cids = pdl($cids0);
  my $nclusters = $cids->max+1;
  my $ucid = $nclusters;
  my $uwid = $cids->nelem;
  $cids->reshape($uwid+1);
  $cids->set($uwid,$ucid);

  ##-- get cluster sizes (adding "unknown" cluster)
  my $csizes = zeroes(long,$ucid+1);
  PDL::Cluster::clustersizes($cids,$csizes);

  ##-- get cluster-frequency pdl
  my $cf   = zeroes(double,$ucid+1); ##-- handle unknown cluster

  ##-- get cluster unigram frequencies
  my ($w,$wid,$cid,$fw, $nunknown);
  while (($w,$fw)=each(%$ugs)) {
    if (defined($wid=$tenum->{sym2id}{$w})) {
      $cid = $cids->at($wid)
    } else {
      $cid = $ucid;
      $nunknown++;             ##-- update "unknown word" cluster size
    }
    $cf->index($cid) += $fw;
  }
  $csizes->set($ucid,$nunknown);

  ##-- compute corpus description length
  my $len = 0;
  my $ftotal = $cf->sumover;
  if (1) {
    ## p(w|c) [UNIFORM:1/cluster-size]
    $len += sumover($csizes * (
			       -log2($cf/$ftotal)
			       -log2($csizes**-1)
			      )
		   );
  }
  elsif (0) {
    ## p(w|c) [ML:word-tokens/cluster-tokens]
    while (($w,$fw)=each(%$ugs)) {
      $wid=$tenum->{sym2id}{$w};
      $cid = defined($wid) ? $cids->at($wid) : $ucid;

      $len += $fw * (
		     -log2($cf->index($cid)/$ftotal)            ## p(c) [ML]
		     ##--
		     #-log2($csizes->index($cid)**-1)           ## p(w|c) [UNIFORM:1/cluster-size]
		     -log2(pdl(double,$fw)/$cf->index($cid))    ## p(w|c) [ML:word-tokens/cluster-tokens]
		  );
    }
  }

  return $len->at(0);
}


########################################################################
## Viewing
########################################################################

##======================================================================
## Viewing: Tree
##======================================================================

##------------------------------------------------------
## $tree = $mp->toTree(%args)
##   + %args : passed to $mp->{cm}->toTree();
##   + also available: $args{cm} : cluster-method to use for conversion
sub toTree {
  my ($mp,%args)=@_;
  my $cm = defined($args{cm}) ? $args{cm} : $mp->{cm};
  return $cm->toTree(encoding=>$mp->{encoding},%args);
}

##------------------------------------------------------
## $dg = $mp->toDendogram(%args)
##   + %args : passed to $mp->toTree(@_)->toDendogram(@_)
sub toDendogram {
  my $mp = shift;
  return $mp->toTree(@_)->toDendogram(@_);
}

##------------------------------------------------------
## undef = $mp->view(%args)
##   + %args (?)
sub view {
  my $mp = shift;
  return $mp->toDendogram(@_)->view;
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
