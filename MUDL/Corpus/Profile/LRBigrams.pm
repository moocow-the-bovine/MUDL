##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRBigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R bigrams
##======================================================================

package MUDL::Corpus::Profile::LRBigrams;
use MUDL::Corpus::Profile::LR;
use MUDL::Corpus::Profile::PdlProfile;
use MUDL::Corpus::Profile::PdlProfile::Bigrams;
use MUDL::Object;
use PDL;
use PDL::CCS;
#use MUDL::PdlDist::Sparse2d;
use MUDL::PdlDist::SparseNd;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile::LR MUDL::Corpus::Profile::PdlProfile); #)

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
##       smoothgt=>$which,           ## whether/where to apply Good-Turing smoothing: false,'bigrams','pdl'
##       smoothli=>$where,           ## whether/where to smooth by (deleted) interpolation: 'global', 'local'
##       li_which=>$which,           ## + independent value to add in with \lambda1: 'b':bound (default), 't':target
##       li_hapax =>$how,            ## + how to handle hapax events for li-smoothing (default: ignore)
##
##   + data acquired [NEW: PDL-ized]
##       pleft =>$left_bigrams,      ## MUDL::PdlDist::SparseNd: ($target_id, $left_bound_id) => $freq  (~ $pdl3d->xvals==0), bound-is-left
##       pright=>$right_bigrams,     ## MUDL::PdlDist::SparseNd: ($target_id,$right_bound_id) => $freq  (~ $pdl3d->xvals==1), bound-is-right
##       ptugs =>$target_unigrams,   ## MUDL::PdlDist: w2-unigram totals for targets
##       pbugs =>$target_unigrams,   ## MUDL::PdlDist: w2-unigram totals for bounds
##       ftotal=>$total,             ## total number of w2 tokens processed (perl scalar)
##
##   + linear interpolation data:
##       diLambdasLR=>$lambdas,    ## labmdas for left-to-right prediction, see MUDL::PDL::Smooth::diLambdas2()
##       diLambdasRL=>$lambdas,    ## labmdas for right-to-left prediction, see MUDL::PDL::Smooth::diLambdas2()
##
##   + backwards-compatibility data:
##       left =>$left_bigrams,       ## ($target_id,$lneighbor_id) => $count
##       right=>$right_bigrams,      ## ($target_id,$rneighbor_id) => $count
##       tugs =>$target_unigrams,    ## w1-unigram totals for targets (ids)
##       bugs =>$target_unigrams,    ## w1-unigram totals for bounds (ids)
##
##   + data acquired [OLD: string-based]
##       left =>$left_bigrams,       ## ($target_id,$lneighbor_id) => $count
##       right=>$right_bigrams,      ## ($target_id,$rneighbor_id) => $count
##       tugs =>$target_unigrams,    ## w1-unigram totals for targets (ids)
##       bugs =>$target_unigrams,    ## w1-unigram totals for bounds (ids)
##       ftotal=>$ftotal,            ## total number of tokens processed
sub new {
  my ($that,%args) = @_; 
  my $self = $that->SUPER::new(
			       nfields=>1,
			       donorm=>1,
			       norm_min=>0,
			       smoothgt=>0,
			       smoothli=>0,
			       #li_which=>'b',
			       #li_hapax=>'ignore',
			       %args,
			      );
  $self->{tugs} = MUDL::EDist->new(enum=>$self->{targets}) if (!$self->{tugs});
  $self->{bugs} = MUDL::EDist->new(enum=>$self->{bounds})  if (!$self->{bugs});
  $self->{ftotal} = 0 if (!defined($self->{ftotal}));
  return $self;
}

## $prof = $prof-reset();
sub reset {
  my $prf = shift;

  ##-- clear: pdls
  delete(@$prf{qw(ptugs pbugs pleft pright diLambdasLR diLambdasRL)});

  ##-- clear: unigram EDists (backwards-compatibility)
  #delete(@$prf{qw(tugs bugs left right)});
  $prf->{tugs}->clear() if (defined($prf->{tugs}));
  $prf->{bugs}->clear() if (defined($prf->{bugs}));
  $prf->{ftotal} = 0;

  ##-- clear: profile EDists (backwards-compatibility)
  $prf->{left}->clear  if (defined($prf->{left}));
  $prf->{right}->clear if (defined($prf->{right}));

  return $prf;
}

## $lr2 = $lr->shadow(%args)
##  + return new profile of same form
##    - empty {left},{right} and unigram distributions
##    - everything else copied
sub shadow {
  my $lr = shift;

  ##-- save temps: pdls
  my (%pdtmp);
  my @ptmpkeys = qw(pleft pright ptugs pbugs diLambdasLR diLambdasRL);
  foreach (@ptmpkeys) {
    next if (!defined($lr->{$_}));
    $pdtmp{$_} = $lr->{$_};
    #$lr->{$_} = ref($pdtmp{$_})->new();
    delete($lr->{$_});
  }

  ##-- save temps: backwards-compat EDists
  my (%nztmp);
  foreach (qw(left right tugs bugs)) {
    next if (!defined($lr->{$_}));
    $nztmp{$_} = $lr->{$_}{nz};
    $lr->{$_}{nz} = ref($nztmp{$_})->new();
  }


  ##-- copy
  my $lr2 = $lr->copy(@_);
  $lr2->{ftotal} = 0;

  ##-- restore temps: EDists
  foreach (qw(left right tugs bugs)) {
    next if (!exists($nztmp{$_}));
    $lr->{$_}{nz} = $nztmp{$_};
  }
  ##-- restore temps: pdls
  foreach (@ptmpkeys) {
    $lr->{$_}  = $pdtmp{$_};
    $lr2->{$_} = $lr->{$_}->new if (defined($lr->{$_}));
  }

  return $lr2;
}


##======================================================================
## MUDL::Profile API

## undef = $profile->addSentence(\@sentence)
##  + push to pdl buffer
#(inherited from Corpus::Profile::PdlProfile)


##======================================================================
## Profiling:: Deprecated: addBigrams($bg)

## $lr = $lr->addBigrams($bg,%args);
##  + %args or $lr flags: passed to addPdlBigrams()
##  + profiles to EDists
sub addBigrams {
  my ($lr,$bg,%args) = @_;

  ##-- warn
  warn(ref($lr),"::addBigrams() is deprecated");

  ##-- fast dispatch to pdl
  return $lr->addPdlBigrams($bg,%args)
    if ($bg->isa('MUDL::Corpus::Profile::PdlProfile::Bigrams'));

  ##-- standard bigrams: pdl-ize 'em
  my ($tgs,$bds,$lbg,$rbg) = @$lr{qw(targets bounds left right)};
  my ($tugs,$bugs)         = @$lr{qw(tugs bugs)};
  my ($w12,$f12,$w1,$w2, $tid,$bid);
  while (($w12,$f12)=each(%{$bg->{nz}})) {
    ##-- split
    my ($w1,$w2) = $bg->split($w12);

    ##-- left-bound
    if (defined($bid=$bds->{sym2id}{$w1}) && defined($tid=$tgs->{sym2id}{$w2})) {
      $lbg->{nz}{$tid.$lbg->{sep}.$bid} += $f12;
    }

    ##-- right-bound
    if (defined($tid=$tgs->{sym2id}{$w1}) && defined($bid=$bds->{sym2id}{$w2})) {
      $rbg->{nz}{$tid.$rbg->{sep}.$bid} += $f12;
    }

    ##-- unigrams (on w1)
    $lr->{tugs}{nz}{$tid} += $f12 if (defined($tid=$tgs->{sym2id}{$w1}));
    $lr->{bugs}{nz}{$bid} += $f12 if (defined($bid=$bds->{sym2id}{$w1}));

    ##-- total freq
    $lr->{ftotal} += $f12;
  }

  ##-- set enums (just in case)
  $lr->{tugs}{enum} = $tgs;
  $lr->{bugs}{enum} = $bds;
  @{$lr->{left}{enum}{enums}}  = ($tgs,$bds);
  @{$lr->{right}{enum}{enums}} = ($tgs,$bds);

  ##-- pdl-ize
  $lr->{pleft}  = $lbg->toSparsePdlDist();
  $lr->{pright} = $rbg->toSparsePdlDist();
  $lr->{ptugs}  = $lr->{tugs}->toPdlDist();
  $lr->{pbugs}  = $lr->{bugs}->toPdlDist();

  ##-- ... and convert to integer types
  $lr->{pleft}      = $lr->{pleft}->convert(long);
  $lr->{pright}     = $lr->{pright}->convert(long);
  $lr->{ptugs}{pdl} = $lr->{ptugs}{pdl}->convert(long);
  $lr->{pbugs}{pdl} = $lr->{pbugs}{pdl}->convert(long);

  ##-- clear EDists
  $lbg->clear();
  $rbg->clear();
  $lr->{tugs}->clear();
  $lr->{bugs}->clear();

  return $lr;
}

##======================================================================
## MUDL::Corpus::Profile::PdlProfile API

## undef = $profile->finishPdlProfile(%args)
##  + perform pdl-sensitive finishing actions
##  + called by default finish() method
##    - when this method is called, the buffer (if any) has been filled and pdl-ized
##    - after this completes, the buffer (if any) is deleted
##  + %args or $lr keys: passed to $lr->addPdlBigrams()
##      smoothgt => $which,  ##-- call smoothGTLogLin on bigrams and sets $lr->{norm_zero_f} if $which eq 'bigrams'
sub finishPdlProfile {
  my ($lr,%args) = @_;

  ##-- get raw bigrams
  my $bgpd = MUDL::Corpus::Profile::PdlProfile::Bigrams->new(bos=>$lr->{bos},
							     eos=>$lr->{eos},
							     buffer=>$lr->{buffer});
  $bgpd->finish();

  ##-- ... and dispatch
  return $lr->addPdlBigrams($bgpd);
}



##======================================================================
## Profiling: new: addPdlBigrams($bg_pdldist_sparse2d)

## $lr = $lr->addPdlBigrams($bg,%args);
##   + profiles to PDLs
sub addPdlBigrams {
  my ($lr,$bgpd,%args) = @_;

  ##-- smoothing: GT
  $lr->{smoothgt} = $args{smoothgt} if (defined($args{smoothgt}));
  if ($lr->{smoothgt} && $lr->{smoothgt} eq 'bigrams') {
    print STDERR "<<DEBUG>>: ", ref($lr)."::addPdlBigrams() GT-smoothing bigrams!\n";
    $bgpd->smoothGTLogLin();
    $lr->{norm_zero_f} += $bgpd->zeroCount->sclr;
  }

  ##-- smoothing: LI: global
  $lr->{smoothli} = $args{smoothli} if (defined($args{smoothli}));
  if ($lr->{smoothli} && ($lr->{smoothli} eq 'global' || $lr->{smoothli} eq 'bigrams')) {
    require MUDL::PDL::Smooth;
    my $f12 = $bgpd->{pdl};
    my $f21 = $f12->xchg(0,1);
    my $f2  = $f12->sumover;
    my $f1  = $f21->sumover;
    my $N   = $f1->sumover;
    $lr->{diLambdasLR} = $f12->diLambdas2(f1=>$f1,f2=>$f2,N=>$N,hapax=>$lr->{li_hapax}); ##-- left  -> right
    $lr->{diLambdasRL} = $f21->diLambdas2(f1=>$f2,f2=>$f1,N=>$N,hapax=>$lr->{li_hapax}); ##-- right -> left
  }

  ##-- get enums
  my $bge  = $bgpd->{enum}{enums}[0];
  my $bds  = $lr->{bounds};
  my $tgs  = $lr->{targets};
  my $Nbge = $bge->size;
  my $Nbds = $bds->size;
  my $Ntgs = $tgs->size;

  ##-- sanity checks: bos/eos
  $bds->addSymbol($lr->{bos}) if (defined($lr->{bos}));
  $bds->addSymbol($lr->{eos}) if (defined($lr->{eos}));


  ##-- get translation PDLs: bounds
  my $bds2bge = pdl(long, @{$bge->{sym2id}}{ @{$bds->{id2sym}} }); ##-- $bds2bge: $bds_id => $bge_id
  my $bds_msk = zeroes(byte,$Nbge);                                ##-- $bds_msk: $bge_id => $is_bound
  my $bge2bds = zeroes(long,$Nbge)->setvaltobad(0);                ##-- $bge2bds: $bge_id => $bds_id_or_BAD
  $bds_msk->index($bds2bge) .= 1;
  $bge2bds->index($bds2bge) .= sequence(long,$Nbds);

  ##-- get translation PDLs: targets
  my $tgs2bge = pdl(long, @{$bge->{sym2id}}{ @{$tgs->{id2sym}} }); ##-- $tgs2bge: $tgs_id => $bge_id
  my $tgs_msk = zeroes(byte,$Nbge);                                ##-- $tgs_msk: $bge_id => $is_target
  my $bge2tgs = zeroes(long,$Nbge)->setvaltobad(0);                ##-- $bge2tgs: $bge_id => $tgs_id_or_BAD
  $tgs_msk->index($tgs2bge) .= 1;
  $bge2tgs->index($tgs2bge) .= sequence(long,$Ntgs);

  ##-- get bigram data
  my ($bgw1,$bgw2) = $bgpd->{pdl}->whichND;
  my $vals         = $bgpd->{pdl}->whichVals;
  my $missing      = $bgpd->{pdl}->missing;

  ##-- get {left} CCS distribution: bounds-on-left, targets-on-right
  my $l_isgood = $bds_msk->index($bgw1) & $tgs_msk->index($bgw2);
  my $lbi_bgi  = $bgw1->where($l_isgood);
  my $lti_bgi  = $bgw2->where($l_isgood);
  my $l_nz     = $vals->where($l_isgood);
  my $lbi_bds  = $bge2bds->index($lbi_bgi);
  my $lti_tgs  = $bge2tgs->index($lti_bgi);
  my $lpd      = $bgpd->{pdl}->shadow(
  				      pdims=>pdl(long,$Ntgs,$Nbds),
  				      vdims=>sequence(long,2),
  				      which=>$lti_tgs->cat($lbi_bds)->xchg(0,1),
  				      vals =>$l_nz->append($missing),
  				     )->sortwhich;             ##-- ensure index-sort on primary dimension
  $lpd->_whichND->badflag(0) if ($lpd->_whichND->isgood->all); ##-- bad flag gets set by setvaltobad() above
  $lpd->_vals->badflag(0)    if ($lpd->_vals->isgood->all);    ##-- ... ditto, probably

  ##-- get {right} CCS distribution: bounds-on-right, targets-on-left
  my $r_isgood = $bds_msk->index($bgw2) & $tgs_msk->index($bgw1);
  my $rbi_bgi  = $bgw2->where($r_isgood);
  my $rti_bgi  = $bgw1->where($r_isgood);
  my $r_nz     = $vals->where($r_isgood);
  my $rbi_bds  = $bge2bds->index($rbi_bgi);
  my $rti_tgs  = $bge2tgs->index($rti_bgi);
  my $rpd      = $bgpd->{pdl}->shadow(
  				      pdims=>pdl(long,$Ntgs,$Nbds),
  				      vdims=>sequence(long,2),
  				      which=>$rti_tgs->cat($rbi_bds)->xchg(0,1),
  				      vals =>$r_nz->append($missing),
  				     )->sortwhich;             ##-- ensure index-sort on primary dimension
  $rpd->_whichND->badflag(0) if ($rpd->_whichND->isgood->all); ##-- bad flag gets set by setvaltobad() above
  $rpd->_vals->badflag(0)    if ($rpd->_vals->isgood->all);    ##-- ... ditto, probably

  ##-- pack up {right} and {left} distributions into {pright}, {pleft}
  my $tbenum   = MUDL::Enum::Nary->new(nfields=>2, enums=>[$tgs,$bds]);
  $lr->{pleft} = MUDL::PdlDist::SparseNd->new(enum=>$tbenum, pdl=>$lpd);
  $lr->{pright} = MUDL::PdlDist::SparseNd->new(enum=>$tbenum, pdl=>$rpd);

  ##-- get target & bound unigram pdls
  my $ccs_sum   = $bgpd->{pdl}->sumover->todense;
  $lr->{ptugs}  = MUDL::PdlDist->new(enum=>$tgs,pdl=>$ccs_sum->index($tgs2bge));
  $lr->{pbugs}  = MUDL::PdlDist->new(enum=>$bds,pdl=>$ccs_sum->index($bds2bge));
  $lr->{ftotal}  = $ccs_sum->sum;

  ##-- invalidate any EDists we might have hanging around
  foreach (qw(left right tugs bugs)) {
    $lr->{$_}->clear() if (defined($lr->{$_}));
  }

  return $lr;
}


##======================================================================
## Conversion: Unigrams: PDL

## $target_unigram_pdl = $lr->targetUgPdl();
sub targetUgPdl {
  #$_[0]{tugs}{enum} = $_[0]{targets}; ##-- sanity check
  #return $_[0]{tugs}->toPDL();
  ##--
  return $_[0]{ptugs}{pdl};
}

## $bound_unigram_pdl = $lr->boundUgPdl();
sub boundUgPdl {
  #$_[0]{bugs}{enum} = $_[0]{bounds}; ##-- sanity check
  #return $_[0]{bugs}->toPDL();
  ##--
  return $_[0]{pbugs}{pdl};
}

##======================================================================
## MetaProfile interface
##
## Common arguments:
##  + $xlateMatrix :
##    - a PDL::CCS::Nd matrix probabilistically mapping old items to new items
##    - dims : ($nOldItems,$nNewItems)
##    - vals : [$oldId,$newId] --> p($newId|$oldId)
##    - can be generated from $xlateMap
##  + $xlateMap :
##    - a PDL univocally mapping old to new items, representing a "hard" mapping
##    - dims : ($nOldItems)
##    - vals : [$oldId] --> $newId
##    - can be generated from $xlateMatrix, via maximum_ind()


## $xlateMatrix = $lr->xlateMap2Matrix($xlateMap)
##  + see "Common Arguments", above
sub xlateMap2Matrix {
  my ($lr,$xmap) = @_;
  return PDL::CCS::Nd->newFromWhich(
				    $xmap->sequence->cat($xmap)->xchg(0,1),
				    $xmap->ones->append(0)->double,
				    pdims  =>pdl(long,$xmap->dim(0),$xmap->max+1),
				    sorted =>1,
				    steal  =>1,
				   );
}

## $xlateMap = $lr->xlateMatrix2Map($xlateMatrix)
##  + see "Common Arguments", above
sub xlateMatrix2Map {
  my ($lr,$xmatrix) = @_;
  return $xmatrix->xchg(0,1)->maximum_ind->todense;
}

## $lr = $lr->updateBounds($xlateBoundsMatrix, $newBoundsEnum)
##  + $xlateBoundsMatrix :
##    - a PDL::CCS::Nd matrix probabilistically mapping old to new bounds
##    - dims : ($nOldBounds,$nNewBounds)
##    - vals : [$oldBoundId,$newBoundId] --> p($newBound|$oldBound)
##    - default : from $xlateBoundsMap, if specified
##  + $newBoundsEnum
##    - MUDL::Enum for new bounds (undef for a new, empty MUDL::Enum)
##  + child classes can define hooks:
##      $lr->updateBoundsPreHook ($xmatrix,$xenum)
##      $lr->updateBoundsPostHook($xmatrix,$xenum)
sub updateBounds {
  my ($lr,$xmatrix,$xenum) = @_;

  ##-- sanity check
  croak(ref($lr)."::updateBounds(): no \$xlateBoundsMatrix specified!") if (!defined($xmatrix));

  ##-- translation hook: pre
  $lr->updateBoundsPreHook($xmatrix,$xenum) if ($lr->can('updateBoundsPreHook'));

  ##-- translate: {pleft},{pright}
  ##   : $xmatrix(nOldBds,nNewBds) x $wvpdl(nTgs,nOldBds) --> $wbpdl(nTgs,nNewBds)
  my ($wvdist,$wvpdl,$wbpdl);
  foreach my $dirkey (qw(pleft pright)) {
    $wvdist = $lr->{$dirkey};
    $wvpdl  = $pdist->{pdl};       ##-- [w,b_old] -->   f(w,b_old)
    $wbpdl  = $xmatrix x $wvpdl;   ##-- [w,b_new] --> E(f(w,b_new))
    $wvdist->{pdl} = $wbpdl;
  }

  ##-- translate: {pbugs}
  ##   : $xmatrix(nOldBds,nNewBds) x $fb_old(1,nOldBds) --> $fb_new(1,nNewBds)
  my $fbdist = $lr->{pbugs};
  my $fb_old = $fvdist->{pdl};
  my $fb_new = ($xmatrix x $fb_old->toccs->dummy(0,1))->todense->flat;
  $fvdist->{pdl} = $fb_new;

  ##-- translate: {enum}
  if (!defined($xenum)) { $xenum = MUDL::Enun->new(); } ##-- new, empty enum
  $lr->{pleft}{enum}{enums}[1]  = $xenum;
  $lr->{pright}{enum}{enums}[1] = $xenum;
  $lr->{pbugs}{enum}            = $xenum;
  $lr->{bounds}                 = $xenum;

  ##-- translation hook: post
  $lr->updateBoundsPostHook($xmat,$xenum) if ($lr->can('updateBoundsPostHook'));

  return $lr;
}

## undef = $lr->updateTargets($xlateTargetsMatrix, $newTargetsEnum)
##  + $xlateTargetsMatrix :
##    - a PDL::CCS::Nd matrix probabilistically mapping old to new targets
##    - dims : ($nOldTargets,$nNewTargets)
##    - vals : [$oldTargetId,$newTargetId] --> p($newTarget|$oldTarget)
##  + $newTargetsEnum
##    - MUDL::Enum for new targets (undef for a new, empty MUDL::Enum)
##  + child classes can define hooks:
##      $lr->updateTargetsPreHook ($xmatrix,$xenum)
##      $lr->updateTargetsPostHook($xmatrix,$xenum)
sub updateTargets {
  my ($lr,$xmatrix,$xenum) = @_;

  ##-- sanity check
  croak(ref($lr)."::updateTargets(): neither \$xlateTargetsMatrix specified!") if (!defined($xmatrix));

  ##-- translation hook: pre
  $lr->updateTargetsPreHook($xmatrix,$xenum) if ($lr->can('updateTargetsPreHook'));

  ##-- translate: {pleft},{pright}
  ##   : $wvpdl(nOldTgs,nBds) x $xmatrix^T(nNewTgs,nOldTgs) --> $wbpdl(nNewTgs,nBds)
  my ($wvdist,$wvpdl,$wbpdl);
  foreach my $dirkey (qw(pleft pright)) {
    $wvdist = $lr->{$dirkey};
    $wvpdl  = $pdist->{pdl};                  ##-- [w_old,b] -->   f(w_old,b)
    $tvpdl  = $wvpdl x $xmatrix->xchg(0,1);   ##-- [w_new,b] --> E(f(w_new,b))
    $tvdist->{pdl} = $tvpdl;
  }

  ##-- translate: {ptugs}
  ##   : xmatrix(nOldTgs,nNewTgs) x ft_old(1,nOldTgs) --> ft_new(1,nNewTgs)
  my $fwdist = $lr->{ptugs};
  my $ft_old = $fwdist->{pdl};
  my $ft_new = ($xmatrix x $ft_old->toccs->dummy(0,1))->todense->flat;
  $fwdist->{pdl} = $ft_new;

  ##-- translate: {enum}
  if (!defined($xenum)) { $xenum = MUDL::Enun->new(); } ##-- new, empty enum
  $lr->{pleft}{enum}{enums}[0]  = $xenum;
  $lr->{pright}{enum}{enums}[0] = $xenum;
  $lr->{ptugs}{enum}            = $xenum;
  $lr->{targets}                = $xenum;

  ##-- translation hook: post
  $lr->updateTargetsPostHook($xmat,$xenum) if ($lr->can('updateTargetsPostHook'));

  return $lr;
}


##======================================================================
## Conversion: to PDL

##----> TODO:::: fix MUDL::Corpus::Profile::LR !!!
## + pdl-ize it, afap

##-- inherited from MUDL:::Corpus::Profile::LR

## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl)

## $pdl3d = $lr->getPdl3d($pdl_3d,%args)
##  + gets suitable 3d-pdl, and zeroes it
##  + %args: clobber %$lr
sub getPdl3d {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args; ##-- args: clobber

  ##-- enums
  my $nfields  = $lr->{nfields} = 1;      ##-- nfields is always 1 for LRBigrams
  my ($eb,$et) = @$lr{qw(bounds targets)};
  my $net      = $et->size;
  my $neb      = $eb->size;

  ##-- pdl
  $pdl = zeroes(double,1) if (!defined($pdl));
  $pdl->reshape(2, ($neb**$nfields), $net)
    if ($pdl->ndims < 3 || $pdl->dim(0) < 2 || $pdl->dim(1) < ($neb**$nfields) || $pdl->dim(2) < $net);
  $pdl .= 0;

  return $pdl;
}

## $pdl_3d = $lr->toPDL3d()
## $pdl_3d = $lr->toPDL3d($pdl_3d,%args)
##   + converts to pdl
##   + returned $pdl_3d is of dimensions: (2,$d,$n), where:
##     - $n == number-of-targets
##     - $d == (number-of-bounds ^ $nfields)   ##-- left-bounds & right-bounds
##   + may call the following:
##     - undef = $lr->finishPdl($pdl_3d)
##     - undef = $lr->normalizePdl($pdl_3d)
##   + %args: clobber %$lr
sub toPDL3d {
  my ($lr,$pdl,%args) = @_;
  @$lr{keys %args} = values %args; ##-- args: clobber

  $pdl = $lr->getPdl3d($pdl);

  ##-- frequency data: left-context
  $lr->{pleft}{pdl}->decode( $pdl->slice('(0),')->xchg(0,1) );

  ##-- frequency data: right-context
  $lr->{pright}{pdl}->decode( $pdl->slice('(1),')->xchg(0,1) );

  ##-- smoothing: default
  $lr->smoothPdl($pdl) if ($lr->can('smoothPdl'));

  ##-- data munging
  $lr->finishPdl($pdl) if ($lr->can('finishPdl'));

  ##-- normalization
  $lr->normalizePdl($pdl) if ($lr->{donorm});

  ##-- post-normalization log
  $lr->logPdl($pdl) if ($lr->{dolog});

  return $pdl;
}

## undef = $lr->smoothPdl($pdl);
##  + applies LI-smoothing if requested
sub smoothPdl {
  my ($lr,$pdl) = @_;

  ##-- smoothing: LI
  if ($lr->{smoothli}) {
    my $which = $lr->{li_which};
    my ($ft,$fb,$N) = ($lr->{ptugs}{pdl},$lr->{pbugs}{pdl},$lr->{ftotal});
    $which    = 'b' if (!$which);

    ##-- smoothing: LI: local
    if ($lr->{smoothli} eq 'local') {
      require MUDL::PDL::Smooth;
      my ($pl,$pr)    = ($lr->{pleft}{pdl},$lr->{pright}{pdl});
      if ($which eq 't') {
	##-- bound->target
	$lr->{diLambdasLR} = $pl->diLambdas2(f1=>$ft,f2=>$fb,N=>$N); ##-- target -> leftBound
	$lr->{diLambdasRL} = $pr->diLambdas2(f1=>$ft,f2=>$fb,N=>$N); ##-- target -> rightBound
      } else {
	##-- target->bound
	$lr->{diLambdasLR} = $pl->xchg(0,1)->diLambdas2(f1=>$fb,f2=>$ft,N=>$N); ##-- leftBound  -> target
	$lr->{diLambdasRL} = $pr->xchg(0,1)->diLambdas2(f1=>$fb,f2=>$ft,N=>$N); ##-- rightBound -> target
      }
    }

    ##-- smoothing: LI: apply
    my $lamLR = $lr->{diLambdasLR};
    my $lamRL = $lr->{diLambdasRL};

    if ($which eq 't') {
      ##-- bound->target
      $pdl->slice("(0)") *= $lamLR->index(1);
      $pdl->slice("(0)") += $lamLR->index(0)*$ft->index($pdl->slice("(0)")->yvals);
      $pdl->slice("(1)") *= $lamRL->index(1);
      $pdl->slice("(1)") += $lamRL->index(0)*$ft->index($pdl->slice("(1)")->yvals);
    } else {
      ##-- target->bound
      $pdl->slice("(0)") *= $lamRL->index(1);
      $pdl->slice("(0)") += $lamRL->index(0)*$fb->index($pdl->slice("(0)")->xvals);
      $pdl->slice("(1)") *= $lamLR->index(1);
      $pdl->slice("(1)") += $lamLR->index(0)*$fb->index($pdl->slice("(1)")->xvals);
    }
  }

  return $pdl;
}

## undef = $lr->finishPdl($pdl);

## undef = $lr->normalizePdl($pdl);

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right-bigram profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  smoothgt=WHICH   [default=0] : one of 'bigrams','pdl',0\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
}

##======================================================================
## I/O: Native
## - (output only!)

## $bool = $obj->saveNativeFh($fh,%args)
sub saveNativeFh {
  my ($obj,$fh) = @_;
  $fh->print("##-- BIGRAMS: LEFT\n");
  $obj->{pleft}->toEDist->toDist->saveNativeFh($fh,@_);
  $fh->print("\n\n##-- BIGRAMS: RIGHT\n");
  $obj->{pright}->toEDist->toDist->saveNativeFh($fh,@_);
  $fh->print("\n\n##-- UNIGRAMS: TARGETS\n");
  $obj->{ptugs}->toEDist->toDist->saveNativeFh($fh,@_);
  $fh->print("\n\n##-- UNIGRAMS: BOUNDS\n");
  $obj->{pbugs}->toEDist->toDist->saveNativeFh($fh,@_);
  $fh->print("\n\n##-- FTOTAL\n");
  $fh->print($obj->{ftotal}, "\n");
  return $obj;
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
