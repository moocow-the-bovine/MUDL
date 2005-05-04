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
##  cbtenum => $enum_nary, ## ($bound_class,$target_word) enum (<=$i)
##  tree => $cluster_tree, ## MUDL::Cluster::Tree
##  vtree => $viewing_tree, ## viewable tree
##  ##
##  ##-- previous data
##  pprof => $prev_prof,   ## ^f_{<=k}($dir, $bound_cluster, $target_word)
##  ##
##  ##-- current iteration data
##  prof => $profile,      ## current profile: ^f_{$dir}($dir, $bound_word, $target_word)
##  stage => $i,           ## stage number
##  d2p => {method=>'nbest_base',n=>4,b=>2},  ## distance-to-probability arguments
##  tenum_k => $enum,      ## current (token) targets only
##  tcenum_k => $enum,     ## current (token) targets + previous (target) classes
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
  $mp->{cbtenum} = MUDL::Enum::Nary->new(nfields=>2, enums=>[@$mp{qw(cbenum tenum)}]);

  ##-- bound-class-hack: bos,eos
  foreach (@$mp{qw(bos eos)} = @$prof{qw(bos eos)}) {
    $mp->{cbenum}->addSymbol($_);
  }

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
  $mp->{tree}->membershipProbPdl( %{$mp->{d2p}}, @_, pdl=>$phat );

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
  $mp->vmsg($vl_info, "update(): nbounds  = ", $prof->{bounds}->size, "\n");
  $mp->vmsg($vl_info, "update(): ntargets = ", $prof->{targets}->size, "\n");
  $mp->vmsg($vl_info, "update(): profile ~ f_{<=k}(d, v_b, w)\n");
  $mp->{prof} = $prof;

  ##-- update: enums
  $mp->vmsg($vl_info, "update(): enums ~ B_<=k, T_<=k, T_k\n");
  $mp->{tenum}->addEnum($prof->{targets});
  $mp->{benum}->addEnum($prof->{bounds});

  $mp->{tenum_k} = $mp->{tenum}->copy;
  $tenum_k->removeSymbol($_) foreach (grep {!defined($tenum_ltk->index($_))} (@{$tenum->{id2sym}}));

  ##-- update: pprof
  $mp->vmsg($vl_info, "update(): pprof ~ f_{<=k}(d, c_b, t_k u c_<k)\n");
  my $pprof = $mp->populatePprof($prof); ##-- NEEDS A-FIXIN': CONTINUE HERE

  return $mp;
}

##--------------------------------------------------------------
## $enum = $mp->tenum_k($prof)
##   + sets $mp->{tenum_k}=$enum : enum over new token-targets in $prof and old clusters
##   + populates $mp->{cenum_k} : maps plain cluster-ids {tenum_k} cluster-ids
sub tenum_k {
  my ($mp,$prof) = @_;
  ##... CONTINUE HERE
}


##--------------------------------------------------------------
## $pprof = $mp->populatePprof()
## $pprof = $mp->populatePprof($prof)
##  + populate
##      $mp->{pprof} ~ f_{<=k}(d, c_b, w)
##    from
##      $prof        ~ f_{<=k}(d, v_b, w)
##    and
##      $mp->{phat}  ~ ^p_{<=k}(c_b | v_b)
##  + requires: Bounds_{stage=$i} \subseteq Bounds_{stage=$i+1}, \forall i, 1 <= $i <= $mp->{stage}

##-- NEEDS A-FIXIN'

sub populatePprof {
  my ($mp,$prof) = @_;
  $prof = $mp->{prof} if (!defined($prof));

  ##-- CONTINUE HERE

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
  $pprof->{left}  = $mp->addProfileDist($prof->{left}); ##-- NEEDS A-WRITIN' (aka FIXIN')
  $pprof->{right} = $mp->addProfileDist($prof->{right});##-- NEEDS A-WRITIN' (aka FIXIN')

  ##-- pprof: step 2: replace enums
  foreach (@{$pprof->{enum}{enums}}) {
    $_ = $mp->{cbenum} if ($_ eq $pprof->{bounds});
    $_ = $mp->{tenum}  if ($_ eq $pprof->{targets});
  }
  $pprof->{bounds}  = $mp->{cbenum};
  $pprof->{targets} = $mp->{tenum};

  return $pprof;
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
  my $pprof = $mp->populatePprof($prof); ##-- NEEDS A-FIXIN'

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
  $mp->populatePhat();  ##-- NEEDS A-FIXIN'

  ##-- update: pprof
  #$mp->vmsg($vl_info, "recluster(): pprof ~ f_{<=k}(d, c_b, w)\n");
  #$mp->populatePprof($prof);

  return $mp;
}



##--------------------------------------------------------------
## $tweakedDist = $mp->addProfileDist($profileDistNary)
##  + generated $tweakedDist, a distribution over target-(words||classes) & bound-classes
##    from $profileDistNary, over target- and bound-words.
sub addProfileDist {
  my ($mp,$wvdist) = @_;

  my $wenum   = $wvdist->{enum}{enums}[0];
  my $venum   = $wvdist->{enum}{enums}[1];
  my $tenum   = $mp->{tenum};
  my $cbenum  = $mp->{cbenum};

  my $tenum_ltk = $mp->{tenum_ltk};
  my $benum_ltk = $mp->{benum_ltk};
  my $tenum_k   = $tenum->copy;
  $tenum_k->removeSymbol($_) foreach (grep {!defined($tenum_ltk->index($_))} (@{$tenum->{id2sym}}));
  $tenum_k->compact();

  #my $benum_k   = $cbenum->copy;
  #$benum_k->removeSymbol($_) foreach (grep {!defined($benum_ltk->index($_))} (@{$benum->{id2sym}}));

  ##-- CONTINUE HERE!
  my $Ncb     = $cbenum->size;
  my $Nt      = $tenum->size;
  my $Ntk     = $tenum_k->size;
  my $Nct     = $mp->{tree}{nclusters};

  my $fwb_pdl = zeroes(double, $Ntk, $Ncb);
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
