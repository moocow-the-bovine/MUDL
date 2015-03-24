##-*- Mode: Perl -*-

## File: MUDL::Corpus::MetaProfile::Attach3.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner
##    : attaching meta-profiles (stage-(k>1))
##    : for trigram clustering
##======================================================================

package MUDL::Corpus::MetaProfile::Attach3;
use MUDL;
use MUDL::Corpus::MetaProfile qw(:vlevels);
use MUDL::Corpus::Profile::LR3;
use MUDL::Trigrams;
use MUDL::Bigrams;
use PDL;
use PDL::Cluster;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::MetaProfile);

##======================================================================
## Corpus::MetaProfile::Attach: Constructor
## {
##  ##-- global data
##  cm => $cluster_method, ## a MUDL::Cluster::Method subclass object used for clustering
##  clusterids => $pdl,    ## pdl($n) : best cluster-ids
##  stage => $i,           ## stage number
##  verbose => $level,     ## verbosity level
##  keeptmps => $bool,     ## keep temporary data?
##
##  ##-- enums
##  tenum3 => $enum3,      ## previous+current 3-gram target enum (T^3_{<=k})
##  tenum1 => $enum1,      ## previous+current 1-gram target enum (T^1_{<=k})
##
##  benum1 => $benum1,     ## current unigram bound-enum = project_2(T^3_{<=k})
##  benum0 => $benum0,     ## unigram bounds we always want as literals
##
##  tenum3_k => $enum,     ## current 3-gram targets
##  tenum1_k => $enum,     ## current 1-gram targets
##
##  #tenum3_ltk => $enum,  ## previous 3-gram targets
##  #tenum1_ltk => $enum,  ## previous 1-gram targets
##
##  #tgenum => $tgenum,    ## a flat enum over all possible target trigrams
##  #ugenum => $ugenum,    ## a flat enum over all possible target unigrams
##
##  ##-- base data
##  f2     => $bigrams,    ## MUDL::Bigrams object  (strings)
##  f2lr   => $bgs_l2r,    ## nested bigrams: { $w1 => { $w2 => $f_w12, ... }, ... }
##  f2rl   => $bgs_r2l,    ## nested bigrams: { $w2 => { $w1 => $f_w12, ... }, ... }
##  f3     => $trigrams,   ## MUDL::Trigrams object (strings)
##  fiw    => $fiw_dist,   ## MUDL::Dist::Nary: unigram frequency by position,unigram-string
##
##  ##-- template data
##  prof3  => $lr3_prof,   ## LR3 (trigram) profile template
##  prof1  => $lr_prof,    ## Unigram profile template
##
##  ##-- temporary current iteration data
##  ctrprof => $ctr_prof,  ## LR3 profile for centroids
##  curprof => $cur_prof,  ## LR3 profile for new target-trigrams
##  = {
##     targets => $tgs3,   ## 3-gram flat target enum
##     uprof => $uprof,    ## underlying word-type profile
##     = {
##        targets=>$tgs1,  ## 1-gram word-type target enum
##        bounds=>$bds1,   ## 1-gram word-type bound enum
##       }
##    }
##
##
##  ##-- info
##  #stage_info => { ##-- stage unigram-frequency info for new targets
##  #         ntgs_k=>$n_new_targets,
##  #         avg=>$freq,
##  #         prms=>$stddev,
##  #         median=>$freq,
##  #         min=>$freq,
##  #         max=>$freq,
##  #         adev=>$abs_stddev,
##  #         rms=>$est_stddev,
##  #        },
## }
##
## $mp = MUDL::Corpus::MetaProfile::Attach3->new(%args)
sub new {
  my ($that,%args) = @_;

  my $mp = $that->SUPER::new(
			     stage => 0,
			     verbose => $vl_default,
			     keeptmps => 1,            ##-- DEBUG
			     %args,
			    );

  ##-- delete useless stuff added by MetaProfile->new()  [hack]
  delete(@$mp{qw(prof phat)});

  return $mp;
}

##======================================================================
## Utilities
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->rmtmps(%args)
##  + %args:
##     tmps => \@tmp_keys,   ##-- temporary keys to (possibly) remove
##     force => $force,      ##-- boolean
##  + removes temporaries if !$mp->{keeptmps} or $force
sub rmtmps {
  my ($mp,%args) = @_;

  $args{tmps} = [
		 qw(prof1_ltk prof3_ltk prof1_k prof3_k cm_k),
		 qw(tenum3_ltk tenum1_ltk tenum3_k tenum1_k),
		 qw(f2_lek_l2r f2_lek_r2l),
		]
    if (!$args{tmps});

  delete(@$mp{@{$args{tmps}}}) if (!$mp->{keeptmps} || $args{force});

  return $mp;
}



##--------------------------------------------------------------
## undef = $mp->sanityCheck(%args)
##  + %args:
##     label => $label, ##-- default = 'sanityCheck()'
##     keys  => \@keys, ##-- additional $mp keys which must be defined
##  + confess()s if $mp is not sane
##  + loads relevant sub-modules
sub sanityCheck {
  my ($mp,%args) = @_;
  $args{label}   = 'sanityCheck()' if (!defined($args{label}));
  my @required   = qw(prof1 benum1 tenum3 cm f3 f2 f2lr f2rl);
  push(@required, @{$args{keys}}) if ($args{keys});

  ##-- sanity checks
  foreach (@required) {
    confess(ref($mp), "::$args{label}: missing required key '$_'")
      if (!defined($mp->{$_}));
  }
  $mp->{prof3} = MUDL::Corpus::Profile::LR3->new()
    if (!defined($mp->{prof3}));

  ##-- load modules
  MUDL::CmdUtils::loadModule($mp->{prof1})
      or confess(ref($mp), "::$args{label}: could not load profile1 module '", ref($mp->{prof1}), "': $!");
  MUDL::CmdUtils::loadModule($mp->{cm})
      or confess(ref($mp), "::$args{label}: could not load cluster module '", ref($mp->{cm}), "': $!");

  ##-- instantiate benum0
  my $benum0 = $mp->{benum0};
  if (!defined($benum0)) {
    $benum0 = $mp->{benum0} = MUDL::Enum->new();
    $benum0->addSymbol($_) foreach (@{$mp->{f3}}{qw(bos eos)});
  }
}



##======================================================================
## Initialization
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->initialize(%args)
##  + %args:
##      f3   => $trigrams,   ##-- trigrams, REQUIRED
##      f2   => $bigrams,    ##-- bigrams, optional
##      f2lr => $bgs_l2r,    ##-- nested bigrams, left->right: derived
##      f2rl => $bgs_r2l,    ##-- nested bigrams, right->left: derived
##  + initializes $mp->{f3}, $mp->{f2}, $mp->{f2n}
sub initialize {
  my ($mp,%args) = @_;
  @$mp{keys(%args)} = values(%args);

  $mp->vmsg($vl_info, "initialize()\n");

  ##-- bigrams
  if (!defined($mp->{f2})) {
    $mp->{f2} = MUDL::Bigrams->new(%{$mp->{f3}},
				   nz=>$mp->{f3}->projectN(1,2)->{nz});
  }

  ##-- nested bigrams
  my $f2   = $mp->{f2};
  my $f2nz = $f2->{nz};
  my $f2lr = $mp->{f2lr} = {};
  my $f2rl = $mp->{f2rl} = {};
  my ($w12,$f12,$w1,$w2);
  while (($w12,$f12)=each(%$f2nz)) {
    ($w1,$w2) = $f2->split($w12);
    $f2lr->{$w1}{$w2} = $f12;
    $f2rl->{$w2}{$w1} = $f12;
  }

  return $mp;
}

##======================================================================
## Bootstrapping (stage = 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->bootstrap(%args)
##  + relevant %args: (clobbers %$mp)
##     tenum3 => $initial_target_enum,
##     benum1 => $initial_bound_enum,
##     benum0 => $literal_bound_enum,             ##-- default: {bos,eos}
##     prof   => $trigram_profile_template,
##     uprof  => $unigram_profile_template,
##     cm     => $cluster_method_profile_template
##  + profiles and performs initial clustering
sub bootstrap {
  my ($mp,%args) = @_;
  @$mp{keys %args} = values %args;

  $mp->vmsg($vl_info, "bootstrap()\n");

  ##-- sanity checks
  $mp->sanityCheck(label=>'bootstrap()');

  ##-- set base data
  $mp->{stage}    = 1;

  ##-- profile1
  $mp->vmsg($vl_info, "bootstrap(): prof1 ~ ^f_{k,z}(w,b)\n");
  my $prof1_k = $mp->{prof1_k} = $mp->{prof1}->shadow;

  $prof1_k->{bounds}->addEnum($mp->{benum0});
  $prof1_k->{bounds}->addEnum($mp->{benum1});
  $prof1_k->{targets}->addSymbol($_) foreach ($mp->{tenum3}->allComponents);
  $mp->{tenum1} = $prof1_k->{targets};

  $prof1_k->addBigrams($mp->{f2});

  ##-- profile3
  $mp->vmsg($vl_info, "bootstrap(): prof3 ~ LR3(prof1)\n");
  my $prof3_k = $mp->{prof3_k} = $mp->{prof3}->shadow;
  #$prof3_k->{targets}{enum}->addEnum($mp->{tenum3}{enum});
  $prof3_k->{targets} = $mp->{tenum3};
  $prof3_k->{uprof} = $prof1_k;

  ##-- PDL
  $mp->vmsg($vl_info, "bootstrap(): prof3->toPDL\n");
  my $pdl3 = $prof3_k->toPDL;

  ##-- cluster
  $mp->vmsg($vl_info, "bootstrap(): cluster()\n");
  my $cm_k = $mp->{cm_k} = $mp->{cm}->shadow;
  $cm_k->{svd}->clear if (defined($cm_k->{svd}));
  $cm_k->data($pdl3);
  $cm_k->cluster();

  $mp->vmsg($vl_info, "bootstrap(): cut()\n");
  $cm_k->cut();
  $mp->{clusterids} = $cm_k->{clusterids};

  ##-- cluster enums
  $mp->vmsg($vl_info, "bootstrap(): cluster enums\n");
  $mp->{cenum}  = $cm_k->clusterEnum;
  $mp->{cbenum} = $mp->{cenum}->copy;
  $mp->{cbenum}->addEnum($mp->{benum0});

  my $cjenum   = $mp->{cjenum}   = MUDL::Enum::Nary::Flat->new(nfields=>2);
  my $cjenum3  = $mp->{cjenum3}  = MUDL::Enum::Nary::Flat->new(nfields=>3);
  my ($ci, @cji);
  foreach $ci (0..($mp->{clusterids}->nelem-1)) {
    @cji = map { $cjenum->addSymbols($ci,$_) } (0,1,2);
    $cjenum3->addIndexedSymbols(@cji,$ci);
  }
  my $cjidenum = $mp->{cjidenum} = MUDL::Enum::Nary::Flat->new(nfields=>2);
  $cjidenum->addIndexedSymbol($_,$_) foreach (0..($cjenum->size-1));


  ##-- word+position frequencies
  $mp->vmsg($vl_info, "bootstrap(): fwj ~ ^f( w_j = w : T^1_{<=k}, j : {0,1,2} )\n");
  $mp->bootstrapFwj();

  ##-- cleanup
  $mp->vmsg($vl_info, "bootstrap(): cleanup\n");
  $mp->rmtmps();

  return $mp;
}


##--------------------------------------------------------------
## $mp = $mp->bootstrapFwj()
##  + bootstraps $mp->{fwj} ~ ^f( w_j = w : T^1_{<=1}, j : {0,1,2} )
##    using $mp->{tenum1}->index($w)
sub bootstrapFwj {
  my $mp = shift;

  my $fwj = $mp->{fwj} = MUDL::Dist::Nary->new(nfields=>2);
  my $fwjnz = $fwj->{nz};
  my $fwjfs = $fwj->{sep};

  my $tenum3 = $mp->{tenum3};
  my $tenum1 = $mp->{tenum1};
  my $f3nz   = $mp->{f3}{nz};

  my ($w123i,$f123, $w123s,@w123i, $j);
  foreach $w123i (0..($tenum3->size-1)) {
    $w123s = $tenum3->{enum}{id2sym}[$w123i];
    $f123  = $f3nz->{$w123s};
    @w123i = @{$tenum1->{sym2id}}{$tenum3->split($w123s)};

    ##-- populate position-dependent frequencies
    foreach $j (0,1,2) {
      $fwjnz->{$w123i[$j].$fwjfs.$j} += $f123;
    }
  }

  return $mp;
}


##======================================================================
## Update (stage > 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->update(%args)
##  + relevant %args: (clobbers %$mp)
##     tenum3 => $current_and_previous_trigram_target_enum,
##  + profiles and performs initial clustering
sub update {
  my ($mp,%args) = @_;

  ##-- arg-parsing
  my $tenum3_ltk = $mp->{tenum3_ltk} = $mp->{tenum3}; ##-- save previous targets
  my $tenum1_ltk = $mp->{tenum1_ltk} = $mp->{tenum1}; ##-- save previous targets
  @$mp{keys(%args)} = values(%args);

  ##-- update: stage-number
  ++$mp->{stage};
  $mp->vmsg($vl_info, "update()\n");

  ##-- update: sanity checks
  $mp->sanityCheck(label=>'update()', keys=>[qw(cjenum cjidenum cjenum3 cbenum fwj)]);

  ##-- update: enums
  $mp->vmsg($vl_info, "update(): enums\n");
  $mp->updateEnums();

  ##-- update: unigram profiles: prof1_*
  $mp->vmsg($vl_info, "update(): unigram profiles:\n");
  $mp->vmsg($vl_info, "        : prof1_ltk ~ f_{z,<k}(c_{\@2} : C_{k-1}, b : C_{k-1})\n");
  $mp->vmsg($vl_info, "        : prof1_k   ~ f_{z, k}(w       : T^1_k  , b : C_{k-1})\n");
  $mp->updateUnigramProfiles();

  ##-- update: trigram profiles: prof3_*
  $mp->vmsg($vl_info, "update(): trigram profiles:\n");
  $mp->vmsg($vl_info, "        : prof3_ltk ~ \\phi( f_{z,<k}(c,b) )\n");
  $mp->vmsg($vl_info, "        : prof3_k   ~ \\phi( f_{z, k}(w,b) )\n");
  $mp->updateTrigramProfiles();

  ##-- update: data
  $mp->vmsg($vl_info, "update(): centroid data: prof3_ltk->toPDL\n");
  my $data_ltk = $mp->{prof3_ltk}->toPDL;

  $mp->vmsg($vl_info, "update(): new target data: prof3_k->toPDL\n");
  my $data_k = $mp->{prof3_k}->toPDL;

  ##-- update: attach
  $mp->vmsg($vl_info, "update(): attach\n");
  my $cm_k = $mp->{cm_k} = $mp->{cm}->shadow;
  my ($cids_k,$cdist_k) = $cm_k->attach(data=>$data_k, cdata=>$data_ltk);

  ##-- attach: integrate
  my $cids_lek = $mp->{clusterids};
  $cids_lek->reshape($mp->{tenum3}->size);
  my ($w123i_k, $w123i_lek);
  foreach $w123i_k (0..($mp->{tenum3_k}->size-1)) {
    $w123i_lek = $mp->{tenum3}{enum}{sym2id}{$mp->{tenum3_k}{enum}{id2sym}[$w123i_k]};
    $cids_lek->set($w123i_lek, $cids_k->at($w123i_k));
  }
  $mp->{clusterids} = $cids_lek;

  ##-- update: fwj: position-dependent word-frequencies
  $mp->vmsg($vl_info, "update(): fwj ~ ^f( w_j = w : T^1_{<=k}, j : {0,1,2} )\n");
  $mp->updateFwj();

  ##-- cleanup
  $mp->vmsg($vl_info, "update(): cleanup\n");
  $mp->rmtmps();

  return $mp;
}

##--------------------------------------------------------------
## $mp = $mp->updateTrigramProfiles()
##  + creates and populates:
##      $mp->{prof3_ltk}  ~ \phi( f_{z,<k}(c@j, b) )
##      $mp->{prof3_k}    ~ \phi( f_{z, k}(w,   b) )
##  + requires:
##      $mp->{cjenum3}    ~ <c@0,c@1,c@2>
##      $mp->{uprof_ltk}  ~ f_{z,<k}(c@j, b)
##      $mp->{uprof_k}    ~ f_{z, k}(w,   b)
sub updateTrigramProfiles {
  my $mp = shift;

  ##-- prof3_ltk
  my $prof3_ltk = $mp->{prof3_ltk} = $mp->{prof3}->shadow;
  $prof3_ltk->{uprof}   = $mp->{prof1_ltk};
  $prof3_ltk->{targets} = $mp->{cjenum3};

  ##-- prof3_k
  my $prof3_k = $mp->{prof3_k} = $mp->{prof3}->shadow;
  $prof3_k->{uprof}   = $mp->{prof1_k};
  $prof3_k->{targets} = $mp->{tenum3_k};

  return $mp;
}

##--------------------------------------------------------------
## $mp = $mp->updateUnigramProfiles()
##  + creates and populates:
##      $mp->{prof1_ltk} ~ f_{<k,z}(c_{\@2} : C_{k-1}, b : C_{k-1})
##      $mp->{prof1_k}   ~ f_{ k,z}(w       : T^1_k  , b : C_{k-1})
##    from
##      $mp->{f2}        ~ f_{0, r}( w       : A,        v : A       )
##      $mp->{fwj}       ~ f_{<k  }( w_j = w : T^1_{<k}, j : {0,1,2} )
##  + where:
##    - $mp->{prof1_ltk} is indexed by:
##        c_{\@2} ~ $mp->{cenum}
##        b       ~ $mp->{cbenum}
##    - $mp->{prof1_k} is indexed by:
##        w       ~ $mp->{tenum1_k}
##        b       ~ $mp->{cbenum}
sub updateUnigramProfiles {
  my $mp = shift;

  $mp->vmsg($vl_info, "updateUgProfs()\n");

  ##-- enum vars
  my $tenum1_k   = $mp->{tenum1_k};
  my $tenum1_lek = $mp->{tenum1};
  my $tenum1_ltk = $mp->{tenum1_ltk};
  my $tenum3_ltk = $mp->{tenum3_ltk};
  my $cenum      = $mp->{cenum};
  my $cbenum     = $mp->{cbenum};
  my $cjenum     = $mp->{cjenum};


  ##-- create word+bound pseudo-bigram distribution (nested)
  ##   f2_lek ~ f_k(w,b) = \sum_{v_1^3 : T_{<k}^3} p(v_1^3 | v_2) f_0(w,   v_2)
  ##          u f_k(b,w) = \sum_{v_1^3 : T_{<k}^3} p(v_1^3 | v_2) f_0(v_2, w)
  ##   + using w:string, b:string(cbenum)
  $mp->vmsg($vl_info, "updateUgProfs():\n");
  $mp->vmsg($vl_info, "  f2_lek ~ f_k(w,b) = \\sum_{v_1^3 : T_{<k}^3} p(v_1^3 | v_2) f_0(w,   v_2)\n");
  $mp->vmsg($vl_info, "         u f_k(b,w) = \\sum_{v_1^3 : T_{<k}^3} p(v_1^3 | v_2) f_0(v_2, w  )\n");
  $mp->vmsg($vl_info, "         u f_0(w  : T^1_{<=k}, b0 : B_0      )\n");
  $mp->vmsg($vl_info, "         u f_0(b0 : B_0,       w  : T^1_{<=k})\n");

  my $f2_lek_l2r  = $mp->{f2_lek_l2r}  = {};
  my $f2_lek_r2l = $mp->{f2_lek_r2l} = {};
  my $cids  = $mp->{clusterids};
  my $f3    = $mp->{f3};
  my $f3nz  = $f3->{nz};
  my $Fwj   = $mp->{fwj};
  my $Fwjnz = $Fwj->{nz};
  my $f2lr  = $mp->{f2lr};
  my $f2rl  = $mp->{f2rl};

  my ($bs,$bi,$fwb0, $v123i,$v123s, $fv123, $v2s, $fv2,$pbv2, $ws,$fwv);
  ##-- literal bounds
  foreach $bs (@{$mp->{benum0}{id2sym}}) {
    $bi = $cbenum->{sym2id}{$bs};

    ##-- right-bigrams for $ws to $b0: ~ f($ws,$b0s)
    while (($ws,$fwb0)=each(%{$f2rl->{$bs}})) {
      next if (!defined($tenum1_lek->{sym2id}{$ws}));
      $f2_lek_l2r->{$ws}{$bs} = $fwb0;
    }
    ##-- left-bigrams for $ws to $b0 ~ f($b0s,$ws)
    while (($ws,$fwb0)=each(%{$f2lr->{$bs}})) {
      next if (!defined($tenum1_lek->{sym2id}{$ws}));
      $f2_lek_r2l->{$ws}{$bs} = $fwb0;
    }
  }

  ##-- trigram-bounds: use v2
  foreach $v123i (0..($tenum3_ltk->size-1)) {
    $v123s = $tenum3_ltk->{enum}{id2sym}[$v123i];
    $fv123 = $f3nz->{$v123s};

    $bi    = $cids->at($v123i);
    $bs    = $cbenum->{id2sym}[$bi];

    $v2s   = ($tenum3_ltk->split($v123s))[1];

    ##-- cluster probability: p(c | W_2 = v_2)
    $fv2  = $Fwjnz->{$tenum1_ltk->{sym2id}{$v2s}."\t1"};
    $pbv2 = $fv123 / $fv2;

    ##-- right-bigrams for $w to $v2 ~ f($ws,$v2s)
    while (($ws,$fwv)=each(%{$f2rl->{$v2s}})) {
      next if (!defined($tenum1_lek->{sym2id}{$ws}));
      $f2_lek_l2r->{$ws}{$bs} += $pbv2 * $fwv;
    }
    ##-- left-bigrams for $w to $v2 ~ f($v2s,$ws)
    while (($ws,$fwv)=each(%{$f2lr->{$v2s}})) {
      next if (!defined($tenum1_lek->{sym2id}{$ws}));
      $f2_lek_r2l->{$ws}{$bs} += $pbv2 * $fwv;
    }
  }

  ##-- create centroid unigram profile:
  ##   $prof1_ltk ~ f_{z,k}(c@j,b) = \sum_{w_1^3 : c} p(w_1^3 | w_j) f_{z,k}(w_j,b)
  $mp->vmsg($vl_info, "updateUgProfs()\n");
  $mp->vmsg($vl_info, "  prof1_ltk ~ f_{z,<k}(c\@j : C_{k-1} x {0,1,2}, b : C_{k-1})\n");

  my $prof1_ltk = $mp->{prof1_ltk} = $mp->{prof1}->shadow;
  $prof1_ltk->setEnums($mp->{cjidenum}, $cbenum);

  my $prof1_ltk_left_nz  = $prof1_ltk->{left}{nz};
  my $prof1_ltk_right_nz = $prof1_ltk->{right}{nz};

  my ($w123i,$w123s,$fw123, $ci,$cji, @ws,@wi, $j,$fwj,$pcwj, $fwjbs);
  foreach $w123i (0..($tenum3_ltk->size-1)) {
    $w123s = $tenum3_ltk->{enum}{id2sym}[$w123i];
    $fw123 = $f3nz->{$w123s};
    $ci    = $cids->at($w123i);

    @ws    = $tenum3_ltk->split($w123s);
    @wi    = @{$tenum1_ltk->{sym2id}}{@ws};

    foreach $j (0,1,2) {
      $cji  = $cjenum->{enum}{sym2id}{$ci."\t".$j};
      $fwj  = $Fwjnz->{$wi[$j]."\t".$j};
      $pcwj = $fw123 / $fwj;

      ##-- right-bigrams for $wj to $bs ~ f($wjs,$bs)
      while (($bs,$fwjbs)=each(%{$f2_lek_l2r->{$ws[$j]}})) {
	$bi = $cbenum->{sym2id}{$bs};
	$prof1_ltk_right_nz->{$cji."\t".$bi} += $pcwj * $fwjbs;
      }
      ##-- left-bigrams for $wj to $bs ~ f($bs,$wjs)
      while (($bs,$fwjbs)=each(%{$f2_lek_r2l->{$ws[$j]}})) {
	$bi = $cbenum->{sym2id}{$bs};
	$prof1_ltk_right_nz->{$cji."\t".$bi} += $pcwj * $fwjbs;
      }
    }
  }

  ##-- create current iteration unigram profile:
  ##   $prof1_k ~ f_{z,k}(w,b)
  $mp->vmsg($vl_info, "updateUgProfs(): prof1_k ~ f_{z,k}(w : T^1_k, b : C_{k-1})\n");

  my $prof1_k   = $mp->{prof1_k}   = $mp->{prof1}->shadow;
  $prof1_k->setEnums($mp->{tenum1_k}, $mp->{cbenum});

  my $prof1_k_left_nz  = $prof1_k->{left}{nz};
  my $prof1_k_right_nz = $prof1_k->{right}{nz};

  my ($wbdist, $wi, $fwb);
  foreach $wi (0..($tenum1_k->size-1)) {
    $ws = $tenum1_k->{id2sym}[$wi];

    ##-- right-bigrams for $w to $bs ~ f($w,$bs)
    while (($bs,$fwb)=each(%{$f2_lek_l2r->{$ws}})) {
      $bi = $cbenum->{sym2id}{$bs};
      $prof1_k_right_nz->{$wi."\t".$bi} = $fwb;
    }

    ##-- left-bigrams for $w to $bs ~ f($bs,$w)
    while (($bs,$fwb)=each(%{$f2_lek_r2l->{$ws}})) {
      $bi = $cbenum->{sym2id}{$bs};
      $prof1_k_left_nz->{$wi."\t".$bi} = $fwb;
    }
  }

  ##-- cleanup
  $mp->rmtmps(tmps=>[qw(f2_lek_l2r f2_lek_r2l)]);

  return $mp;
}

##--------------------------------------------------------------
## $mp = $mp->updateEnums()
##  + creates $mp->{tenum3_k}, $mp->{tenum1_k}
sub updateEnums {
  my $mp = shift;

  my $tenum3_lek = $mp->{tenum3};
  my $tenum3_ltk = $mp->{tenum3_ltk};
  my $tenum3_k   = $mp->{tenum3_k} = MUDL::Enum::Nary::Flat->new(nfields=>3,
								 sep=>$tenum3_ltk->{sep});

  $tenum3_k->addSymbol($_) foreach (
				    grep {
				      !defined($tenum3_ltk->{enum}{sym2id}{$_})
				    } @{$tenum3_lek->{enum}{id2sym}}
				   );

  my $tenum1_k   = $mp->{tenum1_k} = MUDL::Enum->new();
  foreach ($tenum3_k->allComponents) {
    $tenum1_k->addSymbol($_);
  }
  my $tenum1_lek = $mp->{tenum1} = $mp->{tenum1_ltk}->copy;
  $tenum1_lek->addEnum($tenum1_k);

  return $mp;
}


##--------------------------------------------------------------
## $mp = $mp->updateFwj()
##  + updates
##      $mp->{fwj} ~ ^f( w_j = w : T^1_{<=k}, j : {0,1,2} )
##    using
##      T^1_{<=k}  ~ $mp->{tenum1}->index($w)
##    by adding in data from:
##      T^3_{k}    ~ $mp->{tenum3_k}
##
sub updateFwj {
  my $mp = shift;

  my $fwj   = $mp->{fwj};
  my $fwjnz = $fwj->{nz};
  my $fwjfs = $fwj->{sep};

  my $tenum3_k   = $mp->{tenum3_k};
  my $tenum1_lek = $mp->{tenum1};
  my $f3nz       = $mp->{f3}{nz};

  my ($w123i,$f123, $w123s,@w123i, $j);
  foreach $w123i (0..($tenum3_k->size-1)) {
    $w123s = $tenum3_k->{enum}{id2sym}[$w123i];
    $f123  = $f3nz->{$w123s};
    @w123i = @{$tenum1_lek->{sym2id}}{$tenum3_k->split($w123s)};

    ##-- populate position-dependent frequencies
    foreach $j (0,1,2) {
      $fwjnz->{$w123i[$j].$fwjfs.$j} += $f123;
    }
  }

  return $mp;
}


##======================================================================
## Update (Stage > 1) [OLD]
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->update($profile3, $trigrams)
##  + sets $mp->{prof} to $profile3 (a Profile::LR3 over bound-words & target-trigrams)
##  + $profile3->{uprof} should be a unigram type profile over
##    - bounds:  project(2, $mp->{tenum3})
##    - targets: $mp->{prof}{uprof}{targets} \cup T^1_k
##  + $profile3 may be destructively altered!
sub update1 {
  my ($mp,$prof,$trigrams) = @_;

  ##-- update: stage-number
  ++$mp->{stage};

  ##-- update: load modules
  MUDL::CmdUtils::loadModule(ref($prof))
      or confess(ref($mp), "::update(): could not load profile module '", ref($prof), "': $!");
  MUDL::CmdUtils::loadModule(ref($mp->{cm}))
      or confess(ref($mp), "::update(): could not load cluster module '", ref($mp->{cm}), "': $!");

  ##-- update: trigrams
  $trigrams = $mp->{trigrams} if (!defined($trigrams));

  ##-- update: profile
  $mp->vmsg($vl_info, "update(): raw profile\n");
  $mp->vmsg($vl_info, "update(): nbounds1     = ", $prof->{uprof}{bounds}->size, "\n");
  $mp->vmsg($vl_info, "update(): ntargets1    = ", $prof->{uprof}{targets}->size, "\n");
  $mp->vmsg($vl_info, "update(): ntargets3    = ", $prof->{targets}->size, "\n");
  my $prof_ltk = $mp->{prof};
  $mp->{prof} = $prof;

  ##-- update: enums
  $mp->vmsg($vl_info, "update(): enums ~ T_k\n");
  my $tenum3_ltk = $mp->{tenum3_ltk} = $mp->{tenum3}->copy;
  $mp->{tenum3}->addEnum($prof->{targets});

  ##-- tenum3_k: new targets
  my $tenum3_k = $mp->{tenum3_k} = MUDL::Enum->new();
  my $tk2t = $mp->{tk2t} = {};
  my $t2tk = $mp->{t2tk} = {};
  my ($tid,$tid_k);
  foreach $tid (grep {!exists($tenum3_ltk->{enum}{sym2id}{$_})} (@{$prof->{targets}{id2sym}})) {
    $tid_k = $tenum3_k->addSymbol($tid);
    $t2tk->{$tid}   = $tid_k;
    $tk2t->{$tid_k} = $tid;
  }

  ##-- tk2tp: index pdl, used for clusterid extension
  my $tk2tp = pdl(long, [@$tk2t{0..($tenum3_k->size-1)}]);

  ##-- tenum1_k: unigram target types
  my $tenum1_k = $mp->{tenum1_k} = MUDL::Enum->new();
  $tenum1_k->addSymbol($_) foreach ($tenum3_k->allComponents());

  ##-- report
  $mp->vmsg($vl_info, "update(): ntargets_k   = ", $tenum3_k->size, "\n");


  ##-- update: tweaked profiles
  $mp->vmsg($vl_info, "update(): ctrprof ~ f_{<k}(d, c_{\@2,<k}, c_{<k})\n");
  $mp->vmsg($vl_info, "        : curprof ~ f_{ k}(d, c_{\@2,<k}, w_k   )\n");

  my ($ctrprof,$curprof) = $mp->populateProfiles($prof);                                   ##-- TODO!
  ##-- CONTINUE HERE:
  ##   + update from raw trigrams + targets ? i.e. no reprofiling?
  ##     -> this would give us a better handle on p(c|w_i)

  ##-- update: cluster: C_{k-1}: toPDL
  $mp->vmsg($vl_info, "update(): C_{k-1} : toPDL\n");
  my $cm    = $mp->{cm};
  my $cdata = $ctrprof->toPDL;
  #my $cmask = !$ctrprof->isbad;
  #@$mp{qw(cdata cmask)} = ($cdata,$cmask);  ##-- DEBUG

  ##-- update: cluster: T_k: toPDL
  $mp->vmsg($vl_info, "update(): T_k : toPDL()\n");
  my $data_k   = $curprof->toPDL;
  #@$mp{qw(data_k mask_k weight_k tids_k)} = ($data_k, $mask_k, $weight_k, $tids_k); ##-- DEBUG

  ##-- update: cluster
  $mp->vmsg($vl_info, "update(): attach()\n");
  my $cm_k = $mp->{cm_k} = $cm->shadow(class=>'MUDL::Cluster::Method',enum=>$mp->{tenum_k});

  my ($cids_k,$cdist_k) = $cm_k->attach(
					data=>$data_k,
					mask=>(!$data_k->isbad),
					weight=>ones(double, $data_k->dim(0)),
					rowids=>sequence(long,$data_k->dim(1)),
					cdata=>$cdata,
					cmask=>(!$cdata->isbad),
				       );

  ##-- append clusterids
  $mp->vmsg($vl_info, "update(): append clusterids\n");
  $mp->{clusterids}->reshape($mp->{tenum3}->size);
  $mp->{clusterids}->dice($tk2tp) .= $cids_k;

  ##-- update: cm
  $mp->vmsg($vl_info, "update(): cm\n");
  $mp->updateCm();


  ##-- cleanup
  delete(@$mp{qw(tenum3_k tenum3_ltk t2tk tk2t ctrprof curprof)});

  return $mp;
}


##--------------------------------------------------------------
## $mp = $mp->populateAuxDists()
## $mp = $mp->populateAuxDists($trigrams)
##  + creates & populates:
##     $mp->{f3}   ~ Dist::Nary ~ ^f( id_{tenum3}(w_1^3 ? T^3_{<=k})                   )
##     $mp->{fiw}  ~ Dist::Nary ~ ^f( i : {0,1,2}, id_{tenum1}((w ? T^1_{<k}) = c.i) )
##  + from $trigrams, $mp->{tenum3}, $mp->{tenum1}
sub populateAuxDists {
  my ($mp,$trigrams) = @_;
  $trigrams = $mp->{trigrams} if (!defined($trigrams));

  $mp->{f3}   = MUDL::Dist->new();
  $mp->{fiw}  = MUDL::Dist::Nary->new(nfields=>2);
  my $f3nz    = $mp->{f3};
  my $fiwnz   = $mp->{fiw}{nz};

  my $tenum1 = $mp->{tenum1};
  my $tenum3 = $mp->{tenum3};

  my ($w123i, $w123,$f123, @wi,$j);
  $mp->vmsg($vl_info, "populateAuxDists(): ^f( w_1^3 ? T^3_{<=k} )");
  $mp->vmsg($vl_info, "                  : ^f( i     ? {0,1,2}, w_i = w ¤ T^1_{<k})");
  foreach $w123i (0..($tenum3->size-1)) {
    $w123 = $tenum3->{enum}{id2sym}[$w123i];
    $f123 = $trigrams->{nz}{$w123};
    @wi   = @{$tenum1->{sym2id}}{$tenum3->split($w123)};

    ##-- populate position-dependent frequencies
    foreach $j (0,1,2) {
      $fiwnz->{$j."\t".$wi[$j]} += $f123;
    }

    $f3nz->{$w123i} += $f123;
  }

  return $mp;
}

##--------------------------------------------------------------
## ($ctrprof3,$curprof3) = $mp->populateProfiles()
## ($ctrprof3,$curprof3) = $mp->populateProfiles($prof3)
##  + creates and returns:
##     $mp->{ctrprof3} ~ Profile::LR3 ~ f_{k,z}( c : C_{k-1} , b : C_{k-1} )
##     $mp->{curprof3} ~ Profile::LR3 ~ f_{k,z}( w : T^3_k   , b : C_{k-1} )
##  + basically just calls $mp->populateUgProfiles()
##  + requires:
##     ???
sub populateProfiles {
  my ($mp,$prof3) = @_;
  $prof3 = $mp->{prof} if (!$prof3);

  my ($ctr_uprof,$cur_uprof) = populateUgProfiles($prof3->{uprof});
  my $ctrprof = ref($prof3)->new(%$prof3, uprof=>$ctr_uprof);
  my $curprof = ref($prof3)->new(%$prof3, uprof=>$cur_uprof);

  ##-- replace enums
  $ctrprof->{targets} = $mp->{tenum3_ltk};
  $curprof->{targets} = $mp->{tenum3_k};

  return ($ctrprof, $curprof);
}

##--------------------------------------------------------------
## ($ctr_uprof,$cur_uprof) = $mp->populateUgProfiles()
## ($ctr_uprof,$cur_uprof) = $mp->populateUgProfiles($uprof)
##  + creates and returns:
##      $uprof_ctr ~ f_{k,z}( c.i -b¤ C_{k-1}.i , b ¤ C_{k-1}   ) , 1 <= i <= 3-A
##      $uprof_cur ~ f_{k,z}( w   -b¤ T^1_k     , b ¤ C_{k-1}   )-A
##    from
##      $uprof     ~ f_{k,z}( w   -b¤ T^1_{<=k} , v ¤ C_{k-1}.2 )-A
##    and
##      $mp->{clusterids} ~ T^3_{<=k} -> C_{k-1} ~ arg max_{c -b¤ C_{k-1}} p(c | w_1^3)-A
##  + requires:
##      ???

##-- CONTINUE roundabouts HERE: think about LITERAL BOUNDS

sub populateUgProfiles {
  my ($mp,$uprof) = @_;
  $uprof = $mp->{prof}{uprof} if (!defined($uprof));

  ##-- profiles: step 0: copy profile (hack: shadow distributions)
  my $uprof_ctr = $uprof->shadow();
  my $uprof_cur = $uprof->shadow();

  ##-- profiles: step 1: possibly create centroid+position enum
  if (!defined($mp->{cjenum})) {
    my $cjenum = $mp->{cjenum} = MUDL::Enum::Nary::Flat->new(nfields=>2);
    my ($j);
    foreach $j (0..($mp->{clusterids}->nelem-1)) {
      $cjenum->addSymbols($j,$_) foreach (0,1,2);
    }
  }

  ##-- profiles: step 1: tweak profile distributions
  $mp->updateProfileDists($uprof->{left},  $uprof_ctr->{left},  $uprof_cur->{left});
  $mp->updateProfileDists($uprof->{right}, $uprof_ctr->{right}, $uprof_ctr->{right});

  ##-- profiles: step 2a: replace enums: curprof
  foreach (@{$uprof_cur->{enum}{enums}}) {
    $_ = $mp->{cbenum}   if ($_ eq $uprof_cur->{bounds});
    $_ = $mp->{tenum1_k} if ($_ eq $uprof_cur->{targets});
  }
  $uprof_cur->{bounds}  = $mp->{cbenum};
  $uprof_cur->{targets} = $mp->{tenum1_k};

  ##-- profiles: step 2b: replace enums: ctrprof
  foreach (@{$uprof_ctr->{enum}{enums}}) {
    $_ = $mp->{cbenum} if ($_ eq $uprof_ctr->{bounds});
    $_ = $mp->{cenum}  if ($_ eq $uprof_ctr->{targets});
  }
  $uprof_ctr->{bounds}  = $mp->{cbenum};
  $uprof_ctr->{targets} = $mp->{cenum};

  return ($uprof_ctr,$uprof_cur);
}

##--------------------------------------------------------------
## undef = $mp->updateProfileDists($rawDistNaryZ, $ctrDistNaryZ, $curDistNaryZ)
##  + populates $ctrDistNaryZ and $curDistNaryZ from $rawDistNaryZ, where:
##    - $rawDistNaryZ : directed profile dist ~ f_z( w -b¤ T^1_{<=k}, v ¤ T^1_{<k}    )-A
##    - $ctrDistNaryZ : directed profile dist ~ f_z( c -b¤ C_{<k},    b ¤ B_k=C_{k-1} )-A
##    - $curDistNaryZ : directed profile dist ~ f_z( w -b¤ T^1_k,     b ¤ B_k=C_{k-1} )-A
##  + output distributions should be subsequently changed to use enums:
##    - $ctrDistNaryZ : targets=$mp->{cenum}    ; bounds=$mp->{cbenum}
##    - $curDistNaryZ : targets=$mp->{tenum1_k} ; bounds=$mp->{cbenum}
##  + requires:
##    ~ ? $mp->{tenum1}
##    ~ ? $mp->{tenum1_k}
##    ~ ? $mp->{tenum3_ltk}
##    ~ ? $mp->{clusterids}
##    ~ ? $mp->{f3}
##    ~ ? $mp->{fiw}
sub updateProfileDists {
  my ($mp,$wvdist,$cbdist,$wbdist) = @_;

  my $wenum      = $wvdist->{enum}{enums}[0];
  my $venum      = $wvdist->{enum}{enums}[1];
  my $tenum3_ltk = $mp->{tenum3_ltk};
  my $tenum1_k   = $mp->{tenum1_k};
  my $tenum1     = $mp->{tenum1};

  ##-- step 1: create index $vs => { ($ws,f_{z,0}($ws,$vs)) : f_{z,0}($ws,$vs) > 0 }
  ##   + using STRINGS
  my $v2wnz = {};
  my ($wivi,$fwv, $wi,$vi, $ws,$vs);
  while (($wivi,$fwv)=each(%{$wvdist->{nz}})) {
    ($wi,$vi) = $wvdist->split($wivi);
    $ws = $wenum->{id2sym}[$wi];
    $vs = $venum->{id2sym}[$vi];
    $v2wnz->{$vs}{$ws} = $fwv;
  }

  ##-- step 2: create initial f_{z,k}(w,b) = \sum_{v_1^3 -b¤ T_{<k}^3} p(v_1^3 | v_2) f_{z,0}(w,v_2)-A
  ##   + using w:string, c:id(cenum)
  ##   + TODO: work in literal bounds here!
  my $clusterids = $mp->{clusterids};
  my $f3 = $mp->{f3};
  my $fiwnz = $mp->{fiw}{nz};
  my $fzkwbnz = {};
  my ($v123i,$f123, $v2s,$bi,$fv2,$pbv2);
  while (($v123i,$f123)=each(%$f3)) {
    next if (!defined($v2s = ($tenum3_ltk->lsymbols($v123i))[1]));
    $bi   = $clusterids->at($v123i);
    $fv2  = $fiwnz->{"2\t".$mp->{tenum1}{sym2id}{$vs}};
    $pbv2 = $f123 / $fv2;

    while (($ws,$fwv)=each(%{$v2wnz->{$v2s}})) {
      $fzkwbnz->{"$ws\t$bi"} += $pbv2 * $fwv;
    }
  }


  ##-- step 3: create centroid uprof dist:
  ##           f_{z,k}(c@j,b) = \sum_{w_1^3 -b¤ c} p(w_1^3 | w_j) f_{z,k}(w_j,b)-A
  ##  + using c@j:id(cjenum), b:id(cenum)
  my ($w123i,$w123,@ws,@ti, $ci, $j,$cjiflat, $p123cj, $fwb);
  my $fzkcjbnz = $cbdist->{nz};
  my $tenum3 = $mp->{tenum3};
  my $cjenum = $mp->{cjenum};
  %$fzkcjbnz = qw();
  while (($w123i,$f123)=each(%$f3)) {
    $ci   = $clusterids->at($w123i);
    $w123 = $tenum3->{enum}{id2sym}[$w123i];
    @ws   = $tenum3->split($w123);
    @ti   = @{$tenum1->{id2sym}}{@ws};

    foreach $j (0,1,2) {
      $cjiflat = $cjenum->{sym2id}{"$ci\t$j"};
      $p123cj  = $f123 / $fiwnz->{$ti[$j]};
      foreach $bi (0..($clusterids->nelem-1)) {
	next if (!defined($fwb = $fzkwbnz->{"$ws[$j]\t$bi"}));
	$fzkcjbnz->{"$cjiflat\t$bi"} += $p123cj * $fwb;
      }
    }
  }

  ##-- step 4: create word uprof
  ##           f_{z,k}(w -b¤ T_k, b) = f_{z,k}(w ¤ T_{<=k},b)-A
  ##  + using {tenum1_k}, {cenum}
  my ($wsbi, $wki);
  my $fzktbnz = $wbdist->{nz};
  %$fzktbnz = qw();
  while (($wsbi,$fwb)=each(%$fzkwbnz)) {
    ($ws,$bi) = split(/\t/,$wsbi,2);
    next if (!defined($wki = $tenum1_k->{sym2id}{$ws}));
    $fzktbnz->{"$wki\t$bi"} = $fwb;
  }

  ##-- output: enums
  my $cenum  = $mp->{cenum};
  my $cbenum = $mp->{cbenum};

  ##-- output: uprof_ctr
  @$cbdist{qw(nfields sep enum)} = (@$wvdist{qw(nfields sep)},
				     MUDL::Enum::Nary->new(nfields=>$wvdist->{enum}{nfields},
							   enums=>[$cenum, $cbenum]));

  ##-- output: {uprof_cur}
  @$wbdist{qw(nfields sep enum)} = (@$wvdist{qw(nfields sep)},
				    MUDL::Enum::Nary->new(nfields=>$wvdist->{enum}{nfields},
							  enums=>[$tenum1_k, $cbenum]));

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
  my $phat_k = zeroes(double, $mp->{cbenum}->size, $mp->{tenum_k}->size);
  my $cm_k   = $mp->{cm_k};
  $cm_k->membershipProbPdl(@_, beta=>$beta, r2cprobs=>$phat_k);

  ##-- allocate new ^p_{<=k}()
  $mp->vmsg($vl_info, "updatePhat(): adjust ~ ^p_{<=k}( c_k | t_{<=k} )\n");
  my $phat = $mp->{phat} = zeroes(double, $mp->{cbenum}->size, $mp->{tenum}->size);

  ##----------------------------------------------
  ## phat: create: ^p_{<=k} = p_k u p_{<k}

  ## phat: create: adopt old targets
  $mp->vmsg($vl_info, "updatePhat(): add<k ~ ^p_{<=k} u= ^p_{<k}\n");
  my $tenum     = $mp->{tenum};
  my $tenum_ltk = $mp->{tenum_ltk};
  my $tenum_k   = $mp->{tenum_k};
  $phat->slice(",0:".($phat_ltk->dim(1)-1)) .= $phat_ltk;

  ##-- phat: create: add new targets
  $mp->vmsg($vl_info, "updatePhat(): add=k ~ ^p_{<=k} u= t_k\n");
  my ($tid_k,$tid_lek);
  foreach $tid_k (0..($tenum_k->size-1)) {
    $tid_lek = $tenum->index($tenum_k->symbol($tid_k));
    #next if (!defined($tid_lek));  ##-- ignore old clusters (shouldn't happen here at all)
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
sub updateCm {
  my $mp = shift;

  ##-- enums
  my $tenum   = $mp->{tenum};
  my $tenum_k = $mp->{tenum_k};

  ##-- create new cm
  $mp->vmsg($vl_debug, "updateCm(): cm\n");
  my $cm_ltk = $mp->{cm_ltk} = $mp->{cm};
  my $cm_k   = $mp->{cm_k};
  my $cm     = $mp->{cm} = $cm_ltk->shadow(class=>'MUDL::Cluster::Method',enum=>$mp->{tenum});

  ##-- populate cm: {clusterids}
  my $cids_ltk = $cm_ltk->{clusterids};
  my $cids_k   = $cm_k->{clusterids};
  my $cids     = zeroes(long, $tenum->size);
  my ($tid,$tid_k,$cid);

  ##-- add cluster-ids
  $cids->slice("0:".($cids_ltk->dim(0)-1)) .= $cids_ltk;
  $cids->slice($cids_ltk->dim(0).":-1")    .= $cids_k;

=begin comment

  foreach $tid (0..($tenum->size-1)) {
    if (defined($tid_k=$tenum_k->index($tenum->symbol($tid)))) {
      ##-- new target
      $cids->set($tid, $cids_k->at($tid_k));
    } else {
      ##-- old target
      $cids->set($tid, $cids_ltk->at($tid));
    }
  }

=end comment

=cut

  $mp->{clusterids} = $cm->{clusterids} = $cids;

  ##-- populate cm: dimensions
  $cm->{nfeatures} = $mp->{cbenum}->size;
  $cm->{ndata}     = $cids->nelem;

  ##-- populate cm: cluster distance matrix
  my $cdm = $cm->{cdmatrix} = zeroes(double, $cm->{nclusters}, $cm->{ndata});
  $cdm->slice(",0:".($cm_ltk->{cdmatrix}->dim(1)-1))   .= $cm_ltk->{cdmatrix};
  $cdm->slice(",".($cm_ltk->{cdmatrix}->dim(1)).":-1") .= $cm_k->{cdmatrix};

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

##------------------------------------------------------
## $tree = $mp->toTree(%args)
##   + %args : passed to $mp->{cm}->toTree();
##   + also available: $args{cm} : cluster-method to use for conversion
sub toTree {
  my ($mp,%args)= @_;
  if (!defined($args{cm})) {
    my $cm = $args{cm} = bless $mp->{cm}->shadow, 'MUDL::Cluster::Method';
    $cm->{enum}       = $mp->{tenum3};
    $cm->{clusterids} = $mp->{clusterids};
  }
  return $mp->SUPER::toTree(%args);
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
