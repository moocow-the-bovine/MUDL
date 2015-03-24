##-*- Mode: CPerl -*-

## File: MUDL::Corpus::MetaProfile::LR3.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner
##    - trigram profile with respect to unigram-clusters
##======================================================================

package MUDL::Corpus::MetaProfile::LR3;
use MUDL;
use MUDL::Corpus::MetaProfile qw(:vlevels);
use MUDL::Corpus::Profile::LR3;
use MUDL::Trigrams;
use MUDL::Bigrams;
use PDL;
use PDL::Cluster;
use PDL::CCS;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile::LR3);

##======================================================================
## Corpus::MetaProfile::LR3: Constructor
## {
##  ##-- inherited from MUDL::Corpus::Profile::LR3
##  uprof    => $lrprof,       # MUDL::Corpus::Profile::LR subclass
##                             #     -> underlying l/r profile over words : REQUIRED
##  targets  => $tg3_enum,     # target TRIGRAMS (flat)
##
##  ##-- word-type unigram data
##  tenum1   => $ugtargets,     # enum of known (i.e. clustered) word-types
##  tbenum   => $ugtargets_bds, # enum of known (i.e. clustered) word-types + literal targets
##  cids     => $clusterids,    # pdl($n)    : cluster-id by word-id
##  phat     => $phat,          # pdl($k,$n) : $phat->at($cid,$wid) = ^p($cid|$tid)
##
##  ##-- cluster + bound data
##  cenum    => $clusterenum,       # enum of cluster-names   #[should be disjoint to word-type names!]
##  cbenum   => $clusterbound_enum, # enum of clusters+bounds #[disjoint to word-types, except for literal bounds]
##
##  ##-- behavioral flags
##  w2cmethod => $method,           # one of 'hard', 'phat'
##                                  # default: 'phat' if $mp3->{phat} is present, otherwise 'hard'
##  bsmethod => $bsmethod,          # one of 'dense', 'hard' : default 'dense'
## }
##
## $mp3 = MUDL::Corpus::MetaProfile::LR3->new(%args)
sub new {
  my ($that,%args) = @_;

  my $mp3 = $that->SUPER::new(
			      verbose => $vl_info,
			      tenum1=>MUDL::Enum->new(),
			      cids=>undef,
			      phat=>undef,
			      cenum=>undef,
			      cbenum=>undef,
			      w2cmethod=>'phat',
			      bsmethod=>'dense',
			      %args,
			     );

  ##-- delete useless stuff added by MetaProfile->new()  [hack]
  delete($mp3->{prof});

  return $mp3;
}

##======================================================================
## Utilities
##======================================================================


##--------------------------------------------------------------
## undef = $mp3->sanityCheck(%args)
##  + %args:
##     label => $label, ##-- default = 'sanityCheck()'
##     keys  => \@keys, ##-- additional $mp3 keys which must be defined
##  + confess()s if $mp3 is not sane
##  + loads relevant sub-modules
sub sanityCheck {
  my ($mp3,%args) = @_;
  $args{label}   = 'sanityCheck()' if (!defined($args{label}));
  my @required   = qw(uprof tenum1 cenum cbenum);
  push(@required, @{$args{keys}}) if ($args{keys});

  ##-- sanity checks
  foreach (@required) {
    confess(ref($mp3), "::$args{label}: missing required key '$_'")
      if (!defined($mp3->{$_}));
  }

  ##-- load modules
  MUDL::CmdUtils::loadModule($mp3->{uprof})
      or confess(ref($mp3), "::$args{label}: could not load profile1 module '", ref($mp3->{uprof}), "': $!");
}


## $mp->vmsg($level, @msg)
sub vmsg {
  my ($mp3,$level,@msg) = @_;
  $mp3->{verbose} = $vl_default if (!defined($mp3->{verbose}));
  return if (!defined($mp3->{verbose}) || $mp3->{verbose} < $level);
  print STDERR ref($mp3), ": ", @msg;
}

##======================================================================
## Utilities: previous-target-word + literal-bound enum
##======================================================================

## $tbenum = $mp3->tbenum()
##  + gets/computes $mp3->{tbenum}
sub tbenum {
  my $mp3 = shift;
  return $mp3->{tbenum} if (defined($mp3->{tbenum}));

  ##-- compute {tbenum} if not defined
  my $tbenum = $mp3->{tbenum} = MUDL::Enum->new();
  $tbenum->addEnum($mp3->{tenum1});

  ##-- add literal bounds
  $tbenum->addSymbol($_) foreach (
				  grep {
				    !defined($mp3->{cenum}{sym2id}{$_})
				  } @{$mp3->{cbenum}{id2sym}}
				 );

  return $tbenum;
}


##======================================================================
## Profiling
##======================================================================

## undef = $profile->addSentence(\@sentence)
##  + dispatch to underlying LR-profile
##  + inherited from MUDL::Corpus::Profile::LR3

## undef = $profile->addBigrams($bigrams)
##  + dispatch to underlying LR-profile
##  + inherited from MUDL::Corpus::Profile::LR3

## undef = $profile->finish(@_)
##  + dispatch to underlying LR-profile
##  + calls bootstrap()
##  + args are passed to {uprof}->finish(), bootstrap()
sub finish {
  my $lr3 = shift;
  MUDL::CmdUtils::loadModule($lr3->{uprof});
  $lr3->{uprof}->finish(@_);
  $lr3->bootstrap(@_);
}

##======================================================================
## Context-Tagger Utilities
##======================================================================

##--------------------------------------------------------------
## $mp3 = $mp3->reset()
##  + clears $mp3->{uprof}, $mp3->{uprof}{targets}
##  + sets $mp3->{uprof}{bounds} to use word-type enum $mp3->tbenum()
sub reset {
  my $mp3 = shift;
  $mp3->{uprof}->reset();
  $mp3->{uprof}{targets}->clear();
  $mp3->{uprof}->setEnums($mp3->{uprof}{targets}, $mp3->tbenum());
  return $mp3;
}

##--------------------------------------------------------------
## $mp3 = $mp3->setTargets($targets3_enum)
##  + computes $targets1, an enum of $targets3 components
##  + adds $targets1 to $uprof->{targets}
##  + to be called after reset()
##  + INHERITED from MUDL::Corpus::Profile::LR3



##======================================================================
## Meta-Profiling
##======================================================================

##--------------------------------------------------------------
## $mp3 = $mp3->bootstrap(%args)
##  + bootstraps $mp3 from $uprof, which should be a profile
##    over $mp3->{tenum1} and literal word-type bounds in
##    $mp3->{tenum1} u ($mp3->{cbenum} - $mp3->{cenum})
##  + $args{verbose} only temporarily replaces $mp3->{verbose}
sub bootstrap {
  my ($mp3,%args) = @_;
  my %mp3_saved = map { ($_=>$mp3->{$_}) } qw(verbose);
  @$mp3{keys %args} = values %args;

  $mp3->vmsg($vl_info, "bootstrap()\n");
  $mp3->sanityCheck();

  $mp3->vmsg($vl_info, "  w2cmethod = $mp3->{w2cmethod}\n");
  $mp3->vmsg($vl_info, "  bsmethod  = ", (defined($mp3->{bsmethod}) ? $mp3->{bsmethod} : '-undef-'), "\n");
  $mp3->vmsg($vl_info, "  |T^1|     = ", $mp3->{uprof}{targets}->size, "\n");
  $mp3->vmsg($vl_info, "  |T^3|     = ", $mp3->{targets}->size, "\n");
  $mp3->vmsg($vl_info, "  |C|       = ", $mp3->{cenum}->size, "\n");
  $mp3->vmsg($vl_info, "  |C u B|   = ", $mp3->{cbenum}->size, "\n");
  $mp3->vmsg($vl_info, "  |U C^-1|  = ", $mp3->{tenum1}->size, "\n");
  $mp3->vmsg($vl_info, "  |B_v|     = ", $mp3->{uprof}{bounds}->size, "\n");

  ##-- bootstrap dist
  my $uprof = $mp3->{uprof};
  $mp3->vmsg($vl_info, "bootstrap(): z-profile: left\n");
  $mp3->bootstrapProfileDist($uprof->{left});

  $mp3->vmsg($vl_info, "bootstrap(): z-profile: right\n");
  $mp3->bootstrapProfileDist($uprof->{right});

  $uprof->setEnums($uprof->{targets}, $mp3->{cbenum});

  ##-- restore saved values
  @$mp3{keys %mp3_saved} = values %mp3_saved;

  return $mp3;
}

##--------------------------------------------------------------
## $phat_dense = $mp3->phat_dense()
##   + computes & assigns $mp3->{phat}
sub phat_dense {
  my $mp3 = shift;

  return $mp3->{phat}
    if ($mp3->{w2cmethod} eq 'phat' && defined($mp3->{phat}));

  ##-- compute phat from clusterids
  my $cids = $mp3->{cids};
  my $cidi = cat($cids, $cids->xvals)->xchg(0,1);
  my $phat = zeroes(byte, $mp3->{cbenum}->size, $mp3->{tenum1}->size);
  $phat->indexND($cidi) .= 1;

  return $mp3->{phat} = $phat;
}


##--------------------------------------------------------------
## $mp3 = $mp3->bootstrapProfileDist($rawDistNaryZ)
##  + dispatch to underlying method
##  + modifies $rawDistNaryZ->{nz}
##  + input:
##    - $rawDistNaryZ->{nz} : profile nary z-dist over target- and bound-words
##  + output:
##    - $rawDistNaryZ->{nz} : profile nary z-dist over target-words and bound-classes
##  + output profile should be subsequently changed to use enums:
##    - targets : (unchanged)
##    - bounds  : $mp3->{cbenum}
sub bootstrapProfileDist {
  my $mp3 = shift;

  my $bsmethod = $mp3->{bsmethod};
  if (!defined($bsmethod)) {
    if ($mp3->{w2cmethod} eq 'phat' && defined($mp3->{phat})) {
      $bsmethod = $mp3->{bsmethod} = 'dense';
    } else {
      $bsmethod = $mp3->{bsmethod} = 'hard';
    }
  }

  my $bssub = $mp3->can("bootstrapProfileDist_${bsmethod}");
  if (!defined($bssub)) {
    confess(ref($mp3), "::bootstrapProfileDist(): unknown bootstrap method '$bsmethod'");
  }

  #$mp3->vmsg($vl_debug, "bootstrapProfileDist(): method=$bsmethod\n");
  return $bssub->($mp3,@_);
}


##--------------------------------------------------------------
## $mp3 = $mp3->bootstrapProfileDist_hard($rawDistNaryZ)
##  + guts for bootstrapProfileDist()
##  + uses $mp3->{cids} to map from words to clusters
##  + should be faster than bootstrapProfileDist_dense() for sparse data, and...
sub bootstrapProfileDist_hard {
  my ($mp3,$wvdist) = @_;

  my $wenum   = $wvdist->{enum}{enums}[0];
  my $venum   = $wvdist->{enum}{enums}[1];
  my $tenum1  = $mp3->{tenum1};
  my $cbenum  = $mp3->{cbenum};
  my $cenum   = $mp3->{cenum};

  my $Nc      = $cenum->size;
  my $Ncb     = $cbenum->size;
  my $Nw      = $wenum->size;

  my $fwbnz = MUDL::Dist->new();
  my $cids  = $mp3->{cids};

  my ($wvkey,$fwv, $wi,$vi, $vs,$bi, $colii_max,$colii);
  while (($wvkey,$fwv)=each(%{$wvdist->{nz}})) {
    ($wi,$vi) = $wvdist->split($wvkey);

    ##-- target-index ($wi) *stays*

    ##-- get bound-index ($vi)
    $vs = $venum->symbol($vi);
    if (defined($vi = $tenum1->index($vs))) {
      ##-- hard mapping
      $fwbnz->{$wi."\t".$cids->at($vi)} += $fwv;
    }
    elsif (defined($bi = $cbenum->index($vs))) {
      ##-- whoa: bound-word is a literal singleton class-bound -- i.e. {BOS},{EOS}
      $fwbnz->{"$wi\t$bi"} += $fwv;
    }
  }

  ##-- output
  $wvdist->{nz} = $fwbnz;

  return $mp3;
}


##--------------------------------------------------------------
## $mp3 = $mp3->bootstrapProfileDist_dense($rawDistNaryZ)
##  + modifies $rawDistNaryZ->{nz}
##  + input:
##    - $rawDistNaryZ->{nz} : profile nary z-dist over target- and bound-words
##  + output:
##    - $rawDistNaryZ->{nz} : profile nary z-dist over target-words and bound-classes
##  + output profile should be subsequently changed to use enums:
##    - targets : (unchanged)
##    - bounds  : $mp3->{cbenum}
sub bootstrapProfileDist_dense {
  my ($mp3,$wvdist) = @_;

  my $wenum   = $wvdist->{enum}{enums}[0];
  my $venum   = $wvdist->{enum}{enums}[1];
  my $tenum1  = $mp3->{tenum1};
  my $cbenum  = $mp3->{cbenum};
  my $cenum   = $mp3->{cenum};

  my $Nc      = $cenum->size;
  my $Ncb     = $cbenum->size;
  my $Nw      = $wenum->size;

  my $fwb_pdl = zeroes(double, $Nw, $Ncb);
  my $phat    = $mp3->phat_dense();

  my ($wvkey,$fwv,  $wi,$vi,  $vs,$bi, $Pbv, $opdl);
  while (($wvkey,$fwv)=each(%{$wvdist->{nz}})) {
    ($wi,$vi) = $wvdist->split($wvkey);

    ##-- target-index ($wi) *stays*

    ##-- get bound-index ($vi)
    $vs = $venum->symbol($vi);
    if (defined($vi = $tenum1->index($vs))) {
      $Pbv                        = $phat->slice(",($vi)");
      $fwb_pdl->slice("($wi),:") += $Pbv * $fwv;
    }
    elsif (defined($bi = $cbenum->index($vs))) {
      ##-- whoa: bound-word is a literal singleton class-bound -- i.e. {BOS},{EOS}
      $fwb_pdl->slice("$wi,$bi") += $fwv;
    }
  }

  ##-- output
  my $wbenum    = MUDL::Enum::Nary->new(nfields=>2, enums=>[$wenum,$cbenum]);
  $wvdist->{nz} = MUDL::PdlDist->new(pdl=>$fwb_pdl, enum=>$wbenum)->toEDist()->{nz};

  return $mp3;
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
