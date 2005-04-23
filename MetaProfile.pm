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
##  Mprev => $pdl2d,       ## dims: 2*$nclusters_bounds, $nclusters_targets
##  ##
##  ##-- current iteration data
##  fhat => $edist_nary,   ## $fhat($dir,$classid,$tokid) = ^f_${dir}($tokid,$bound_classid)
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
## Bootstrapping
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
*bootstrapProfileDist = \&bootstrapProfileDist2;
sub bootstrapProfileDist2 {
  my ($mp,$wvdist) = @_;

  my $k        = $mp->{cenum}->size;
  my $fcb_pdl  = zeroes(double, $k, $k);
  my $phat     = $mp->{phat};

  my ($key,$f, $Pcw,$Pbv);
  while (($key,$f)=each(%{$wvdist->{nz}})) {
    ($w,$v) = $wvdist->split($key);

    $Pcw      = $phat->slice(",($w)");
    $Pbv      = $phat->slice(",$v")->xchg(0,1);
    $fcb_pdl += $Pcw * $Pbv * $f;
  }

  my $fcb_ed = MUDL::EDist::Nary->new(nfields=>$wvdist->{nfields},
				      sep=>$wvdist->{sep},
				      enum=>MUDL::EDist::Nary->new(nfields=>$wvdist->{enum}{nfields},
								   enums=>[$mp->{cenum},$mp->{cenum}]),
				     );

  return MUDL::PdlDist->new(pdl=>$fcb_pdl,enum=>$fcb_ed->{enum})->toEDist($fcb_ed);
}
##-- pretty darn slow [ca. 18s on test 200x200 / nbest=4]
sub bootstrapProfileDist0 {
  my ($mp,$wvdist) = @_;
  my $cbdist = MUDL::EDist::Nary->new(nfields=>$wvdist->{nfields},
				      sep=>$wvdist->{sep},
				      enum=>MUDL::EDist::Nary->new(nfields=>$wvdist->{enum}{nfields},
								   enums=>[$mp->{cenum},$mp->{cenum}]),
				     );
  my $phat = $mp->{phat};
  my ($key,$f, $w,$v, $c,$b, $Pcw,$Pvw);
  while (($key,$f)=each(%{$wvdist->{nz}})) {
    ($w,$v) = $wvdist->split($key);
    $Pcw = $phat->slice(",($w)");
    $Pcv = $phat->slice(",($v)");

    ##-- actual frequency-smearing: dog slow!
    foreach $c ($Pcw->which->list) {
      foreach $b ($Pcv->which->list) {
	$cbdist->{nz}{$c.$cbdist->{sep}.$b} += $Pcv->at($b) * $Pcw->at($c) * $f;
      }
    }
  }
  return $cbdist;
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
