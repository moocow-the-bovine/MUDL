##-*- Mode: CPerl -*-

## File: MUDL::Corpus::MetaProfile.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus meta-profiles (stage-(k>1))
##======================================================================

package MUDL::Corpus::MetaProfile;
use MUDL::Corpus::Profile;
use MUDL::Cluster::Method;
use PDL;
use PDL::Cluster;
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
		   DEFAULT => 'MUDL::Corpus::MetaProfile::Deep',
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
##  tenum => $enum,        ## previous+current targets (token) enum (T_{<=$i})
##  benum => $enum,        ## previous+current bounds (token) enum (B_{<=$i}) (targets + bos,eos)
##  #ctenum => $enum,       ## (target-)class enum
##  cbenum => $enum,       ## (bound-)class enum: includes bos,eos
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

## $hmm = $mp->toHMM($bigrams,%args)
##   + %args are passed to MUDL::HMM->New(),
##     execpt for:
##      arcmode => $mode, ##-- either 'uniform' (default) or 'estimate'
##   + returns a new HMM with uniform arc transition probabilities,
##     and observation probabilities initialized by Bayesian inversion
##     of $mp->{phat}
##   + $unigrams is a MUDL::Dist (or similar) representing word-unigrams,
##     used for inversion
sub toHMM {
  my ($mp,$bgd,%args) = @_;
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
  my $cenum = $mp->{cbenum};
  my $qenum = $hmm->{qenum};
  my $oenum = $hmm->{oenum};

  $qenum->addSymbol($_)
    foreach (grep { $_ ne $mp->{bos} && $_ ne $mp->{eos} } @{$cenum->{id2sym}});

  $oenum->addSymbol($_)
    foreach (grep { $_ ne $mp->{bos} && $_ ne $mp->{eos} } @{$tenum->{id2sym}});

  $N = $qenum->size;
  $M = $oenum->size;

  ##-- map cluster indices
  $mp->vmsg($vl_info, "toHMM(): index translation vectors\n");

  my $unknown = $hmm->{unknown};
  my $uid     = $oenum->{sym2id}{$unknown};

  ##-- $o2o : HMM O indices of all known observations
  my $o2o = sequence(long,$M);
  $o2o    = $o2o->where($o2o != $uid);

  ##-- $q2c->at($qid) = $clusterid
  my $q2c = pdl(long, [ @{$cenum->{sym2id}}{ @{$qenum->{id2sym}} } ]);

  ##-- $o2t->at($oid) = $targetid
  my $o2t = pdl(long, [ @{$tenum->{sym2id}}{ @{$oenum->{id2sym}}[$o2o->list] } ]);

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
    my $ugd = $bgd->project1(0);
    my $ugp = zeroes($args{type}, 1, $tenum->size);
    my ($w,$f,$wid);
    while (($w,$f)=each(%$ugd)) {
      next if (!defined($wid=$tenum->index($w))); ##-- ignore non-targets (?)
      $ugp->set(0,$wid, $f);
    }

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
    $bf_o .= $phat->dice($q2c,$o2t);

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
    $cstr = join('_', $qenum->symbol($cid), map { $oenum->symbol($_) } $o2o->index($q2o->slice(",($cid)"))->list);
    $qenum->addIndexedSymbol($cstr, $cid);
  }

  ##----------------------------
  ##-- arc probabilities
  my $af  = zeroes($hmm->{type}, $N, $N);
  my $pif = zeroes($hmm->{type}, $N);
  my $omegaf = zeroes($hmm->{type}, $N);

  if ($arcmode =~ /^est/) {
    $mp->vmsg($vl_info, "toHMM(): transition probabilities (mode=$arcmode)\n");
    my ($w12, $f, $w1,$w2, $w1id,$w2id);

    ##-- bos,eos detection
    my ($bos,$eos) = @$mp{qw(bos eos)};

    ##-- temporary
    my $phatd = $phat->dice_axis(0,$q2c);
    my $prod1 = zeroes($af->type, $af->dim(0));
    my $prod2 = zeroes($af->type, $af->dims);

    ##-- 'estimate1': use hard assignments to estimate arc probabilities
    if ($arcmode eq 'estimate1') {

      ##-- get hard cluster-id assignment
      my ($cids);
      if (defined($mp->{clusterids})) {
	$cids = $mp->{clusterids};
      } elsif (defined($mp->{cm}{clusterids}) && $mp->{cm}{clusterids}->nelem == $mp->{tenum}->size) {
	$cids = $mp->{cm}{clusterids};
      } else {
	$cids = $phat->maximum_ind;
      }

      ##-- get translation vector: $c2q->at($cid) == $qid
      my $c2q = pdl(long, [ @{$qenum->{sym2id}}{ @{$cenum->{id2sym}} } ]);

      while (($w12,$f)=each(%{$bgd->{nz}})) {
	($w1,$w2) = $bgd->split($w12);
	
	$w1id=$tenum->index($w1);
	$w2id=$tenum->index($w2);
	
	##-- dispatch
	if (defined($w1id) && defined($w2id)) {
	  ##-- normal case: w1 and w2 are 'real' targets: update af
	  $af->dice($c2q->at($cids->at($w1id)), $c2q->at($cids->at($w2id))) += $f;
	}
	elsif ($w1 eq $bos && defined($w2id)) {
	  ##-- w1==bos: update pi
	  $pif->dice($c2q->at($cids->at($w2id))) += $f;
	}
	elsif ($w2 eq $eos && defined($w1id)) {
	  ##-- w2==eos: update omega
	  $omegaf->dice($c2q->at($cids->at($w1id))) += $f;
	} else {
	  next;			##-- ignore "real" unknowns (?)
	}
      }
    }
    ##-- 'estimateb': use unigram probabilities drawn from 'bf' matrix
    elsif ($arcmode eq 'estimateb') {
      ##-- (later)
      $mp->vmsg($vl_info, "toHMM(): arcmode='estimateb': delayed.\n");
      $af     .= 1;
      $pif    .= 1;
      $omegaf .= 1;
    }
    else { #if ($arcmode eq 'estimate')
      ##-- loop: 'estimate'
      while (($w12,$f)=each(%{$bgd->{nz}})) {
	($w1,$w2) = $bgd->split($w12);

	$w1id=$tenum->index($w1);
	$w2id=$tenum->index($w2);

	##-- dispatch
	if (defined($w1id) && defined($w2id)) {
	  ##-- normal case: w1 and w2 are 'real' targets: update af
	  PDL::mult( $phatd->slice(",($w1id)"),  $f,  $prod1, 0 );
	  PDL::mult( $prod1, $phatd->slice(",$w2id")->xchg(0,1), $prod2, 0 );
	  $af += $prod2;
	} elsif ($w1 eq $bos && defined($w2id)) {
	  ##-- w1==bos: update pi
	  PDL::mult( $phatd->slice(",($w2id)"), $f, $prod1, 0 );
	  $pif  += $prod1;
	} elsif ($w2 eq $eos && defined($w1id)) {
	  ##-- w2==eos: update omega
	  PDL::mult( $phatd->slice(",($w1id)"), $f, $prod1, 0 );
	  $omegaf += $prod1;
	} else {
	  next;			##-- ignore "real" unknowns (?)
	}
      }
    }
  }
  else {
    $mp->vmsg($vl_info, "toHMM(): transition probabilities (mode=uniform)\n");
    $af .= 1;
    $pif .= 1;
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
    $mp->vmsg($vl_info, "toHMM(): estimate: arcs (delayed): mode=$args{arcmode})\n");

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
## Description Length
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
