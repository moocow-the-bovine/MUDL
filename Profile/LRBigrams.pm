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
##
##   + data acquired [NEW: PDL-ized]
##       pleft =>$left_bigrams,      ## MUDL::PdlDist::SparseNd: ($target_id, $left_bound_id) => $freq  (~ $pdl3d->xvals==0)
##       pright=>$right_bigrams,     ## MUDL::PdlDist::SparseNd: ($target_id,$right_bound_id) => $freq  (~ $pdl3d->xvals==1)
##       ptugs =>$target_unigrams,   ## MUDL::PdlDist: w2-unigram totals for targets
##       pbugs =>$target_unigrams,   ## MUDL::PdlDist: w2-unigram totals for bounds
##       ftotal=>$total,             ## total number of w2 tokens processed (perl scalar)
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
  delete(@$prf{qw(ptugs pbugs pleft pright)});

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
  foreach (qw(pleft pright ptugs pbugs)) {
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
  foreach (qw(pleft pright ptugs pbugs)) {
    $lr->{$_}  = $pdtmp{$_};
    $lr2->{$_} = $lr->{$_}->new;
  }

  return $lr2;
}


##======================================================================
## MUDL::Profile API

## undef = $profile->addSentence(\@sentence)
##  + push to pdl buffer
#(inherited from Corpus::Profile::PdlProfile)

sub addSentence_OLD {
  my ($pr,$s) = @_;

  ##-- sanity checks: bos/eos
  if (defined($pr->{bos})) {
    $pr->{bounds}->addSymbol($pr->{bos});
  }
  if (defined($pr->{eos})) {
    $pr->{bounds}->addSymbol($pr->{eos});
  }


  ##------ temporary sentence index profiles

  ##-- @st: sentence text
  my @st = ((defined($pr->{bos}) ? $pr->{bos} : qw()),
	    (map { $_->text } @$s),
	    (defined($pr->{eos}) ? $pr->{eos} : qw()));

  ##-- @tids, @bids: sentence target (bound) ids
  my @tids = map { $pr->{targets}{sym2id}{$_} } @st;
  my @bids = map { $pr->{bounds}{sym2id}{$_}  } @st;

  my $lbg = $pr->{left};
  my $rbg = $pr->{right};

  my ($i,$tid,$bid);
  for ($i=0; $i <= $#st; $i++) {
    next if (!defined($tid=$tids[$i]));

    ##-- left
    if ($i > 0 && defined($bid=$bids[$i-1])) {
      ++$lbg->{nz}{$tid.$lbg->{sep}.$bid};
    }

    ##-- right
    if ($i < $#st && defined($bid=$bids[$i+1])) {
      ++$rbg->{nz}{$tid.$rbg->{sep}.$bid};
    }
  }

  ##-- unigrams
  my $tugs = $pr->{tugs};
  my $bugs = $pr->{bugs};
  ++$tugs->{nz}{$_} foreach (grep {defined($_)} @tids);
  ++$bugs->{nz}{$_} foreach (grep {defined($_)} @bids);

  ##-- total
  $pr->{ftotal} += scalar(@$s);

  return $pr;
}


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

sub addBigrams_OLD {
  my ($lr,$bg,%args) = @_;
  require MUDL::Bigrams;

  ##-- sanity checks: bos/eos
  if (defined($lr->{bos})) {
    $lr->{bounds}->addSymbol($lr->{bos});
  }
  if (defined($lr->{eos})) {
    $lr->{bounds}->addSymbol($lr->{eos});
  }

  ##-- smoothing
  $lr->{smoothgt} = $args{smoothgt} if (defined($args{smoothgt}));
  if ($lr->{smoothgt} && $lr->{smoothgt} eq 'bigrams') {
    $bg->smoothGTLogLin;
    $lr->{norm_zero_f} += $bg->zeroCount;
  }

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

  ##-- maybe smooth
  $lr->{smoothgt} = $args{smoothgt} if (defined($args{smoothgt}));
  if ($lr->{smoothgt} && $lr->{smoothgt} eq 'bigrams') {
    print STDERR "<<DEBUG>>: ", ref($lr)."::addPdlBigrams() GT-smoothing bigrams!\n";
    $bgpd->smoothGTLogLin();
    $lr->{norm_zero_f} += $bgpd->zeroCount->sclr;
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
  ##
  ##-- BUG: PDL::CCS::Nd::shadow() doesn't sort 'which' argument!
  #  my $lpd      = $bgpd->{pdl}->shadow(
  #				      pdims=>pdl(long,$Ntgs,$Nbds),
  #				      vdims=>sequence(long,2),
  #				      which=>$lti_tgs->cat($lbi_bds)->xchg(0,1),
  #				      vals =>$l_nz->append($missing),
  #				     );
  ##--
  my $lpd      = $bgpd->{pdl}->shadow(
  				      pdims=>pdl(long,$Ntgs,$Nbds),
  				      vdims=>sequence(long,2),
  				      which=>$lti_tgs->cat($lbi_bds)->xchg(0,1),
  				      vals =>$l_nz->append($missing),
  				     )->sortwhich;
  $lpd->_whichND->badflag(0) if ($lpd->_whichND->isgood->all);
  $lpd->_vals->badflag(0)    if ($lpd->_vals->isgood->all);

  ##-- get {right} CCS distribution: bounds-on-right, targets-on-left
  my $r_isgood = $bds_msk->index($bgw2) & $tgs_msk->index($bgw1);
  my $rbi_bgi  = $bgw2->where($r_isgood);
  my $rti_bgi  = $bgw1->where($r_isgood);
  my $r_nz     = $vals->where($r_isgood);
  my $rbi_bds  = $bge2bds->index($rbi_bgi);
  my $rti_tgs  = $bge2tgs->index($rti_bgi);
  ##
  ##-- BUG: PDL::CCS::Nd::shadow() doesn't sort 'which' argument!
  #  my $rpd      = $bgpd->{pdl}->shadow(
  #				      pdims=>pdl(long,$Ntgs,$Nbds),
  #				      vdims=>sequence(long,2),
  #				      which=>$rti_tgs->cat($rbi_bds)->xchg(0,1),
  #				      vals =>$r_nz->append($missing),
  #				     );
  ##--
  my $rpd      = $bgpd->{pdl}->shadow(
  				      pdims=>pdl(long,$Ntgs,$Nbds),
  				      vdims=>sequence(long,2),
  				      which=>$rti_tgs->cat($rbi_bds)->xchg(0,1),
  				      vals =>$r_nz->append($missing),
  				     )->sortwhich;
  $rpd->_whichND->badflag(0) if ($rpd->_whichND->isgood->all);
  $rpd->_vals->badflag(0)    if ($rpd->_vals->isgood->all);

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
## Conversion: to PDL

##----> TODO:::: fix MUDL::Corpus::Profile::LR !!!
## + pdl-ize it, afap

##-- inherited from MUDL:::Corpus::Profile::LR

## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl)

## $pdl_3d = $lr->toPDL3d()
## $pdl_3d = $lr->toPDL3d($pdl_3d,%args)
##   + converts to pdl
##   + returned $pdl_3d is of dimensions: (2,$d,$n), where:
##     - $n == number-of-targets
##     - $d == (number-of-bounds ^ $nfields)   ##-- left-bounds & right-bounds
##   + may call the following:
##     - undef = $lr->finishPdl($pdl_3d)
##     - undef = $lr->normalizePdl($pdl_3d)
##   + %args:
##     clobber %$lr
sub toPDL3d {
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

  ##-- frequency data: variables
  #my ($spdl,$xi,$yi);

  ##-- frequency data: left-context
  #$spdl = $lr->{pleft};
  #($xi,$yi) = ccswhichND(@$spdl{qw(ptr rowids nzvals)});
  #$pdl->slice('(0),')->index2d($yi,$xi) .= $spdl->{nzvals};
  ##--
  $lr->{pleft}{pdl}->decode( $pdl->slice('(0),')->xchg(0,1) );

  ##-- frequency data: right-context
  #$spdl = $lr->{pright};
  #($xi,$yi) = ccswhichND(@$spdl{qw(ptr rowids nzvals)});
  #$pdl->slice('(1),')->index2d($yi,$xi) .= $spdl->{nzvals};
  ##--
  $lr->{pright}{pdl}->decode( $pdl->slice('(1),')->xchg(0,1) );

  ##-- smoothing
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
