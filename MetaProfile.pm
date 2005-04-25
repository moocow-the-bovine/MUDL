#-*- Mode: Perl -*-

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
our @ISA = qw(MUDL::Object);

##======================================================================
## Corpus::MetaProfile: Constructor
## {
##  ##
##  ##-- global data
##  phat => $pdlDist,      ## dims: ($NClusters, $NPrevTargets) : $phat->at($cid,$tid) = ^p_{<=$i}($cid|$tid)
##  cenum => $enum,        ## class enum
##  tenum => $enum,        ## previous-targets (token) enum (T_{<=$i})
##  ctenum => $enum_nary,  ## ($class,$target)
##  tree => $cluster_tree, ## MUDL::Cluster::Tree
##  ##
##  ##-- previous data
##  Mprev => $pdl2d,       ## dims: 2*$n_clusters_bounds, $n_clusters_targets
##  Mhat  => $pdl2d,       ## dims: 2*$n_clusters_bounds, $n_current_word_targets + $n_clusters_targets_old
##  ##
##  ##-- current iteration data
##  #fhat => $edist_nary,  ## $fhat($dir,$classid,$tokid) = ^f_${dir}($tokid,$bound_classid)
##  prof => $profile,      ## current profile
##  stage => $i,           ## stage number
##  d2p => {method=>'nbest_base',n=>4,b=>2},
## }
## $mp = MUDL::Corpus::MetaProfile->new(%args)
sub new {
  my ($that,%args) = @_;

  #$args{cenum} = MUDL::Enum->new() if (!defined($args{cenum}));
  #$args{tenum} = MUDL::Enum->new() if (!defined($args{tenum}));
  #$args{ctenum} = MUDL::Enum::Nary->new(enums=>[@args{qw(cenum tenum)}])
  #  if (!defined($args{ctenum}));

  return $that->SUPER::new(stage => 0,
			   #phat  => zeroes(double, 1, 1),
			   phat  => undef,
			   ##--
			   #fhat  => MUDL::EDist::Nary->new(nfields=>3),
			   #cenum => MUDL::Enum->new(),
			   #tenum => MUDL::Enum->new(),
			   #ctenum=> MUDL::Enum::Nary->new(nfields=>2,enums=>[@args{qw(cenum tenum)}]),
			   prof  => undef,
			   ##-- distance-to-probability args
			   d2p => {method=>'nbest_base',n=>4,b=>2},
			   ##--
			   %args);
}

##======================================================================
## Bootstrapping (stage = 1)
##======================================================================

##--------------------------------------------------------------
## $mp = $mp->bootstrap($profile1, $tree1, %args)
##  + initializes $mp->{phat} and $mp->{Mprev}
##  + %args contains arguments for $tree1->membershipProbs()
##  + $profile1 and $tree1 may both be destructively altered!
sub bootstrap {
  my ($mp,$prof,$tree,%args) = @_;
  $mp->{prof} = $prof;
  $mp->{tree} = $tree;
  $mp->{stage} = 1;

  ##-- initialize enums
  $mp->{tenum} = $prof->{targets};
  $mp->{cenum} = $tree->clusterEnum();
  $mp->{ctenum} = MUDL::Enum::Nary->new(nfields=>2, enums=>[@$mp{qw(cenum tenum)}]);

  ##-- hack: bos,eos
  foreach (@$mp{qw(bos eos)} = @$prof{qw(bos eos)}) {
    $mp->{cenum}->addSymbol($_);
    $mp->{tenum}->addSymbol($_);
  }

  ##-- populate {phat}: p(class|target)
  $mp->{phat} = zeroes(double, $mp->{cenum}->size, $mp->{tenum}->size);
  $tree->membershipProbPdl( %{$mp->{d2p}}, pdl=>$mp->{phat}, %args );
  foreach (@$mp{qw(bos eos)}) {
    $mp->{phat}->set($mp->{ctenum}->indices($_,$_), 1.0);
  }

  ##-------- populate Mprev: ($ndirs, $bcluster, $tcluster)

  ##-- Mprev: step 1: tweak profile distributions
  $prof->{left}  = $mp->bootstrapProfileDist($prof->{left});
  $prof->{right} = $mp->bootstrapProfileDist($prof->{right});

  ##-- Mprev: step 2: replace literal bounds with cluster-enum
  $_ = $mp->{cenum} foreach (grep { $_ eq $prof->{bounds} || $_ eq $prof->{targets} } @{$prof->{enum}{enums}});
  $prof->{bounds} = $prof->{targets} = $mp->{cenum};

  ##-- Mprev: step 3: generate pdl (using tweaked profile)
  $mp->{Mprev} = $prof->toPDL;

  return $mp;
}

##--------------------------------------------------------------
## $tweakedDist = $mp->bootstrapProfileDist($profileDistNary)
##  + generated $tweakedDist, a distribution over target & bounds classes
##    from $profileDistNary, over target and bound words.
##  + still pretty slow, but the best we've got yet [ca. 10s on test 200x200 / nbest=4]
sub bootstrapProfileDist {
  my ($mp,$wvdist) = @_;

  my $k        = $mp->{cenum}->size;
  my $fwb_pdl  = zeroes(double, $k, $k);
  my $phat     = $mp->{phat};

  my ($key,$f, $w,$v, $Pcw,$Pbv);
  while (($key,$f)=each(%{$wvdist->{nz}})) {
    ($w,$v) = $wvdist->split($key);

    $Pcw      = $phat->slice(",($w)");
    $Pbv      = $phat->slice(",$v")->xchg(0,1);
    $fcb_pdl += $Pcw * $Pbv * $f;
  }

  my $fcb_ed = MUDL::EDist::Nary->new(nfields=>$wvdist->{nfields},
				      sep=>$wvdist->{sep},
				      enum=>MUDL::Enum::Nary->new(nfields=>$wvdist->{enum}{nfields},
								  enums=>[$mp->{cenum},$mp->{cenum}]),
				     );

  return MUDL::PdlDist->new(pdl=>$fcb_pdl,enum=>$fcb_ed->{enum})->toEDist($fcb_ed);
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

  ##----------------------------
  ## Dummy
  carp(ref($mp),"::attach(): WARNING: not yet fully implemented!");
  #croak(ref($mp),"::attach(): not yet implemented!");

  ##----------------------------
  ## Assign new profile (loses old)
  $mp->{prof} = $prof;
  ++$mp->{stage};

  ##----------------------------
  ## Tweak profile: f() -> ^f()
  $prof->{left}  = $mp->attachProfileDist($prof->{left});
  $prof->{right} = $mp->attachProfileDist($prof->{right});

  ##----------------------------
  ## Tweak profile: replace literal bounds with cluster-enum
  $_ = $mp->{cenum} foreach (grep { $_ eq $prof->{bounds} } @{$prof->{enum}{enums}});
  $prof->{bounds} = $mp->{cenum};

  ##----------------------------
  ## Tweak profile: generate new data PDL-2d: ^M(d*Nb+b,w) ~ $mp->{Mhat}
  ##
  ##                                 0..(n_k-1)                (n_k..(n_k+n_c-1))
  ##  + Mhat : (2*$nclusters_bounds, $ntargets_current_words + $ntargets_clusters)
  my $Mhat  = $mp->{Mhat} = $prof->toPDL;
  my $Mprev = $mp->{Mprev};
  my $Nt    = $mp->{prof}{targets}->size;     ## == $Mhat->dim(1)
  my $Nc    = $mp->{cenum}->size;             ## == $Mprev->dim(1)
  $Mhat->reshape($Mhat->dim(0), $Nt + $Nc);
  $Mhat->slice(",$Nt:".($Nt+$Nc-1)) .= $Mprev;

  ##----------------------------
  ## Attach Data: get new distances ~~ add this to Tree() ?!
  my $Dists   = $mp->{Dists} = zeroes(double, $Nc, $Nt);
  my $rowids  = sequence(long, $Nt);
  my $mask    = ones(long, $Mhat->dims);
  my $weights = ones(double, $Nc);
  foreach $cid (0..($Nc-1)) {
    rowdistances($Mhat,
		 $mask,
		 $weights,
		 $rowids,
		 $cid+$Nt,
		 $Dists->slice("($cid)"),
		 $mp->{tree}{dist},
		 $mp->{tree}{method});
  }
  return $mp;
  #$Dists->inplace->setnantobad->inplace->setbadtoval(-1); ##-- eos,bos get 'inf'

  ##----------------------------
  ## Attach Data: convert distances to probabilities
  my $phat = $mp->{phat};
  $phat->reshape($phat->dim(0), $phat->dim(1) + $cmin);
  $mp->{tree}->membershipProbPdl(leafdist=>$Dists,
				 pdl=>$phat->slice(",".($phat->dim(1)-$cmin).":".($phat->dim(1)-1)),
				 %{$mp->{d2p}},
				);
  #my $phat_new = $mp->{tree}->membershipProbPdl(leafdist=>$Dists, %{$mp->{d2p}});

  return $mp;
}


##--------------------------------------------------------------
## $tweakedDist = $mp->attachProfileDist($profileDistNary)
##  + generated $tweakedDist, a distribution over target-WORDS & bound-CLASSES
##    from $profileDistNary, over target-words and bound-words.
*attachProfileDist = \&attachProfileDist1;
sub attachProfileDist1 {
  my ($mp,$wvdist) = @_;

  my $k        = $mp->{cenum}->size;
  my $n        = $wvdist->{enum}{enums}[0]->size;
  my $fwb_pdl  = zeroes(double, $n, $k);
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
