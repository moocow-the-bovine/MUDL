##-*- Mode: Perl -*-

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
  delete(@$mp{qw(
		 prof pprof ctrprof curprof
		 tenum_k tenum_ltk
		 cm cm_k
		 t2tk tk2t ugs_k
		 cdata cmask
		)});
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

  my $o2o = sequence(long,$M);
  $o2o    = $o2o->where($o2o != $uid);

  my $q2c = pdl(long, [ @{$cenum->{sym2id}}{ @{$qenum->{id2sym}} } ]);
  my $o2t = pdl(long, [ @{$tenum->{sym2id}}{ @{$oenum->{id2sym}}[$o2o->list] } ]);

  ##-- create observation probability matrix
  my $bmode = $args{bmode};
  $bmode = 'invert' if (!defined($bmode));
  $mp->vmsg($vl_info, "toHMM(): observation probabilities (mode='$bmode')\n");
  my $bf = zeroes($hmm->{type}, $N, $M);

  if ($bmode =~ /^sim/) {
    ## p(w|c)  = sim(c,w) / \sum_c' sim(c',w)
    my $bf_o  = $bf->dice_axis(1,$o2o);
    $bf_o    .= $phat->dice($q2c,$o2t);

    if ($bmode =~ /\+ebonus-([\d\.]+)/) {
      ##-- exponential bonus
      my $bonus = $1;
      $bf_o->inplace->pow($bonus);
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
    }
    elsif ($bmode =~ /\+ebonus-([\d\.]+)/) {
      ##-- exponential bonus
      my $bonus = $1;
      $bf_o->inplace->pow($bonus);
    }

    ##-- back to ye olde grinde
    $bf_o *= $ugp->dice_axis(1,$o2t);
    $bf_o *= $mp->{phatm}->dice($q2c,$o2t) if ($bmode =~ /\+mask/);

    ##-- possibly hack (old)
    #if ($args{hackb}) {
    #  $args{bhacked} = $bf / $bf->sum;
    #}
  } else {
    confess(ref($mp), "::toHMM(): unknown observation probability mode '$bmode'!");
  }

  ##-- name classes
  $mp->vmsg($vl_info, "toHMM(): cluster names\n");
  my $q2o = zeroes(long,2,$N);
  $bf->xchg(0,1)->maximum_n_ind($q2o);
  my ($cstr,$cid);
  foreach $cid (0..($N-1)) {
    #($cstr=$qenum->symbol($cid)) ~= tr/0-9/A-J/;
    #$cstr .= join('_', map { $oenum->symbol($_) } $q2o->slice(",($cid)")->list);
    ##--
    $cstr = join('_', $qenum->symbol($cid), map { $oenum->symbol($_) } $q2o->slice(",($cid)")->list);
    $qenum->addIndexedSymbol($cstr, $cid);
  }

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
      ##-- smooth, Laplace-like (NOT HERE: now in MUDL::HMM, where it belongs)
      #my $minf  = $af->where($af!=0)->min;
      #my $minf2 = $pif->where($pif!=0)->min;
      #$minf     = $minf2 if ($minf2 < $minf);

      #$minf2 = $omegaf->where($omegaf!=0)->min;
      #$minf = $minf2 if ($minf2 < $minf);

      #$minf = 0.5 if ($minf > 0.5);

      #$af  += $minf/2;
      #$pif += $minf/2;
      #$omegaf += $minf/2;
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

  ##-- compile
  $mp->vmsg($vl_info, "toHMM(): compile()\n");
  $hmm->compilePdls($af,$bf,$pif,$omegaf,%args);

  ##-- possibly hack
  #if ($args{hackb}) {
  #  require PDL::HMM;
  #  no strict 'subs';
  #  my $hmmb = $hmm->{b}->dice_axis(1,$o2o);
  #  $hmmb .= $args{bhacked}->dice_axis(1,$o2o);
  #  $hmmb->inplace->log;
  #  $hmmb->inplace->setnantobad->inplace->setbadtoval(PDL::HMM::logzero);
  #}

  ##-- return
  return $hmm;
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
