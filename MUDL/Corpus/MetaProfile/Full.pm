##-*- Mode: Perl -*-

## File: MUDL::Corpus::MetaProfile::Full.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: full corpus meta-profiles (stage-(k>1))
##======================================================================

package MUDL::Corpus::MetaProfile::Full;
use MUDL::Corpus::MetaProfile qw(:vlevels);
use PDL;
use PDL::Cluster;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::MetaProfile);


##======================================================================
## Corpus::MetaProfile::Full: Constructor
##   + special %args:
##     method => $reclusterMethod,  ##-- subclass flag: see %methods
##   + Object structure:
## {
##  ##
##  ##-- global data
##  phat => $pdlDist,      ## dims: ($NBoundClusters, $NPrevTargets) : $phat->at($cid,$tid) = ^p_{<=$i}($cid|$tid)
##  tenum => $enum,        ## previous+current targets (token) enum (T_{<=$i})
##  benum => $enum,        ## previous+current bounds (token) enum (B_{<=$i}) (targets + bos,eos)
##  cbenum => $enum,       ## (bound-)class enum: includes bos,eos
##  cm => $cluster_method, ## a MUDL::Cluster::Method object
##  bos=>$bos,eos=>$eos,   ## as for Profile
##
##  ##-- previous data
##  pprof => $prev_prof,   ## ^f_{<=k}($dir, $bound_cluster, $target_word)
##
##  ##-- current iteration data
##  prof => $profile,      ## current profile: ^f_{$dir}($dir, $bound_word, $target_word)
##  stage => $i,           ## stage number
##
##  ##-- messages
##  verbose => $level
##
##  ##-- viewing
##  encoding => $encoding, ##-- for viewing
## }
##
## $mp = MUDL::Corpus::MetaProfile::Full->new(%args)
sub new {
  my ($that,%args) = @_;
  return $that->SUPER::new(%args);
}

########################################################################
## API
########################################################################

##======================================================================
## API: Bootstrapping (stage = 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->bootstrap($profile1, $cm1, %args)
##  + initializes $mp->{phat}
##  + %args contains additional arguments for $cm1->membershipProbPdl()
##  + $profile1 and $cm1 may both be destructively altered!
sub bootstrap {
  my ($mp,$prof,$cm,%args) = @_;
  $mp->vmsg($vl_info, "bootstrap().\n");

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

  ##-- target-hack: bos,eos
  foreach (@$mp{qw(bos eos)} = @$prof{qw(bos eos)}) {
    $mp->{cbenum}->addSymbol($_);
  }

  ##-- populate {phat}: p(class|word)
  $mp->vmsg($vl_info, "bootstrap(): phat ~ ^p(c|w)\n");
  $mp->populatePhat(%args);

  return $mp;
}

##--------------------------------------------------------------
## $pprof = $mp->populatePprof()
## $pprof = $mp->populatePprof($prof)
##  + populate $mp->{pprof} ~  f_{<=k}(d, c_b, w)
##    from     $prof        ~  f_{<=k}(d, v_b, w)
##    and      $mp->{phat}  ~ ^p_{<=k}(c_b | v_b)
sub populatePprof {
  my ($mp,$prof) = @_;
  $prof = $mp->{prof} if (!defined($prof));

  ##-- copy profile
  my $pprof = $mp->{pprof} = $prof->shadow();

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
##    from $mp->{cm}
##  + %args:
##    - passed to $mp->{cm}->membershipProbPdl()
sub populatePhat {
  my $mp = shift;

  my $phat = zeroes(double, $mp->{cbenum}->size, $mp->{tenum}->size);
  $mp->{cm}->membershipProbPdl(@_, r2cprobs=>$phat);

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
## Utilities: Re-clustering (Stage >= 1)
##======================================================================

## $mp = $mp->recluster()
## $mp = $mp->recluster($prof)
##  + updates $mp->{pprof} based on $prof (default=$mp->{prof}) and $mp->{phat}
##  + updates $mp->{cm} (reclusters data) based on $mp->{pprof}
##  + updates $mp->{phat} based on reclustering
sub recluster {
  my ($mp,$prof) = @_;
  $prof = $mp->{prof} if (!defined($prof));
  my $cm  = $mp->{cm};

  ##-- recluster: pprof
  $mp->vmsg($vl_info, "recluster(): pprof ~ f_{<=k}(d, c_b, w)\n");
  my $pprof = $mp->populatePprof($prof);

  ##-- recluster: data
  $mp->vmsg($vl_info, "recluster(): toPDL()\n");
  $cm->data($pprof->toPDL);
  $cm->{enum} = $mp->{tenum};

  $mp->vmsg($vl_info, "recluster(): cluster()\n");
  $cm->cluster();

  $mp->vmsg($vl_info, "recluster(): cut()\n");
  $cm->cut();

  ##-- update: phat
  $mp->vmsg($vl_info, "recluster(): phat ~ ^p(c|w)\n");
  $mp->populatePhat();

  return $mp;
}


##======================================================================
## API: Update (Stage > 1)
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

  ##-- update: load modules
  MUDL::CmdUtils::loadModule(ref($prof))
      or confess(ref($mp), "::bootstrap(): could not load profile module '", ref($prof), "': $!");
  MUDL::CmdUtils::loadModule(ref($mp->{cm}))
      or confess(ref($mp), "::bootstrap(): could not load cluster module '", ref($mp->{cm}), "': $!");

  ##-- update: enums
  $mp->vmsg($vl_info, "update(): enums ~ T_k, B_k\n");
  $mp->{tenum}->addEnum($prof->{targets});
  $mp->{benum}->addEnum($prof->{bounds});

  ##-- update: recluster
  $mp->recluster($prof);

  return $mp;
}


########################################################################
## Export & Conversion
########################################################################

## (inherited)


########################################################################
## Viewing
########################################################################

## (inherited)

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
