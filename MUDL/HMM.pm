##-*- Mode: Perl -*-

## File: MUDL::HMM.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Hidden Markov Model (state-emitting)
##======================================================================

package MUDL::HMM;
use Carp;
use MUDL::Corpus::Model;
use MUDL::Enum;
use PDL;
use PDL::HMM;

our @ISA = qw(MUDL::Corpus::Model);

##======================================================================
## Globals

## limit values
#our $EPS = 1.19209290E-06F
our $EPS = 1E-06;
#our $ZERO = -1E+38;
our $ZERO = logzero;
our $NEG  = -3E+38;
our $ONE  = 0;
#our $NONE = 1;

##======================================================================
## Constructor
##
## $hmm = MUDL::HMM->new(%args);
##
## + Notation / additional args
##
## + Object Structure:
##   {
##    ##
##    ##-- alphabet
##    bos => $bos_str,     ## bos-string (__$) : used only for native I/O
##    eos => $eos_str      ## eos-string (__$) : used only for native I/O
#     unknown => $unk_str, ## unknown-string ('@UNKNOWN')
##    oenum => $enum,      ## observation-alphabet enum
##    qenum => $enum,      ## state-label enum
##    ##
##    ##-- properties
##    N => number of states
##    M => alphabet size
##    atotal => total a count  ## (for native I/O) (includes omega)
##    btotal => total b count  ## (for native I/O)
##    pitotal => total pi count ## (for native I/O)
##    type=>PDL::Type,         ## computation datatype (default=double)
##    ##
##    ##-- smoothing
##    smoothb=>$which,        ## how to smooth B for unknowns on compile:
##                             #  known values: qw(hapax all uniform_c uniform none)
##                             #  default='uniform'
##    smoothbmin=>$cutoff,    ## for smoothb=>'hapax', minimum value (default=1.1*min($bf))
##    smoothbval=>$prob,      ## for smoothb=>'uniform', probability value p(u|c) [default=0.5*min(f(w,c))/max(f(c))]
##    ##
##    ##-- parameters
##    a1 => $unigram_prob_pdl ## pdl (N)  : $a1->at($i)   ~ log(p($i)) [arc] # for smoothing, lex save, etc.
##    b1 => $unigram_prob_pdl ## pdl (N)  : $b1->at($i)   ~ log(p($i)) [lex] # for smoothing, lex save, etc.
##    a => $arc_prob_pdl,     ## pdl (N,N): $a->at($i,$j) ~ log(p($i --> $j))
##    b => $obs_prob_pdl,     ## pdl (N,M): $b->at($i,$k) ~ log(p($k  @  $i))
##    pi => $init_prob_pdl,   ## pdl (N)  : $pi->at($i)   ~ log(p($i | init)) # uses 'bos' marker in text model
##    omega => $final_probs,  ## pdl (N)  : $omega->at($i)~ log(p(eos | $i))  # uses 'eos' marker in text model
##    ##
##    ##-- Native I/O
##    afd => $dist_nary,      ## source for A: may use bos,eos
##    bfd => $dist_nary,      ## source for b
##   }
##
sub new {
  my ($that,%args) = @_;
  my $hmm = $that->SUPER::new(bos=>'__$',
			      eos=>'__$',
			      unknown=>'@UNKNOWN',
			      atotal=>100000,
			      btotal=>100000,
			      verbose=>0,
			      smoothb=>'uniform',
			      smoothbval=>undef,
			      smoothbmin=>undef,
			      %args);
  return $hmm->resize(%args);
}

##--------------------------------------------------------------
## $hmm = $hmm->resize(%args)
##   + %args: N=>$N, M=>$M, oenum=>$oenum, qenum=>$qenum
##   + eliminates any previously stored information
sub resize {
  my ($hmm,%args) = @_;

  my $oenum = defined($args{oenum}) ? $args{oenum} : $hmm->{oenum};
  my $qenum = defined($args{qenum}) ? $args{qenum} : $hmm->{qenum};

  $oenum = $hmm->{oenum} = MUDL::Enum->new() if (!defined($oenum));
  $qenum = $hmm->{qenum} = MUDL::Enum->new() if (!defined($qenum));

  #$oenum->addSymbol($_) foreach (@$hmm{qw(bos eos unknown)});
  #$qenum->addSymbol($_) foreach (@$hmm{qw(bos eos)});
  ##--
  $oenum->addSymbol($_) foreach (@$hmm{qw(unknown)});

  my $type  = defined($args{type})  ? $args{type} : double;
  my $N     = defined($args{N})     ? $args{N}    : ($qenum->size ? $qenum->size : 1);
  my $M     = defined($args{M})     ? $args{M}    : ($oenum->size ? $oenum->size : 1);
  my $a     = defined($args{a})     ? $args{a}    : zeroes($type, $N, $N);
  my $b     = defined($args{b})     ? $args{b}    : zeroes($type, $N, $M);
  my $pi    = defined($args{pi})    ? $args{pi}   : zeroes($type, $N);
  my $omega = defined($args{omega}) ? $args{omega}: zeroes($type, $N);
  my $a1    = defined($args{a1})    ? $args{a1}   : zeroes($type, $N);
  my $b1    = defined($args{b1})    ? $args{b1}   : zeroes($type, $N);


  delete($hmm->{ea})
    if (defined($hmm->{ea}) && $hmm->{ea}->dim(0) != $N);

  delete($hmm->{eb})
    if (defined($hmm->{eb}) && ($hmm->{eb}->dim(0) != $N || $hmm->{eb}->dim(1) != $M));

  delete($hmm->{epi})
    if (defined($hmm->{epi}) && $hmm->{epi}->dim(0) != $N);

  delete($hmm->{eomega})
    if (defined($hmm->{eomega}) && $hmm->{eomega}->dim(0) != $N);

  @$hmm{qw(type N M a a1 b b1 pi omega)} = ($type,$N,$M,$a,$a1,$b,$b1,$pi,$omega);
  return $hmm;
}


##--------------------------------------------------------------
## $hmm = $hmm->grow(%args)
##   + %args:
##      M=>$M,  ##-- default: $hmm->{oenum}->size
##   + does not alter any previously stored information
##   + initializes new observation probabilities to match those of
##     $hmm->{unknown}
##   + normalizes unknown and new probabilities in {b} to ensure
##     consistency (sum-to-one over states)
sub grow {
  my ($hmm,%args) = @_;

  my $oenum = $hmm->{oenum};

  my $Mold   = $hmm->{M};
  my $Mnew   = defined($args{M}) ? $args{M} : $oenum->size;

  return $hmm if ($Mnew-$Mold <= 0);

  my $unk = $oenum->index($hmm->{unknown});
  my $b   = $hmm->{b} = $hmm->{b}->reshape($hmm->{b}->dim(0), $Mnew);
  my $bu  = $b->slice(":,$unk");

  ##-- normalize and assign:
  ##     p'(unk|q) = p'(w_new|q) = p(unk|q) / (1+M'-M) , \forall q \in Q
  $bu -= log(1+$Mnew-$Mold);
  $b->slice(":,$Mold:-1") .= $bu;

  ##-- update expectation matrix, if applicable
  if (defined($hmm->{eb})) {
    my $eb   = $hmm->{eb} = $hmm->{eb}->reshape($hmm->{b}->dim(0), $Mnew);
    my $ebu  = $eb->slice(":,$unk");

    ##-- normalize and assign: E(f'(unk,q)) = E(f'(w_new,q)) = E(f(unk,q)) / (1+M'-M) , \forall q \in Q
    $ebu -= log(1+$Mnew-$Mold);
    $eb->slice(":,$Mold:-1") .= $ebu;
  }

  $hmm->{M} = $Mnew;
  return $hmm;
}

##======================================================================
## clear

## $hmm = $hmm->clear()
sub clear {
  my $hmm = shift;
  $hmm->{qenum}->clear;
  $hmm->{oenum}->clear;
  return $hmm->resize();
}

##======================================================================
## accessors
##======================================================================

##--------------------------------------------------------------
## $af = $hmm->af(%args)
##   + get n-gram joint frequency pdl ($N,$N)
##   + returns $hmm->{af} if defined
##   + %args:
##      total=>$total (default=$hmm->{atotal})
##      round=>$bool, (round to integer?)
##      type=>$type,  (output datatype)
sub af {
  my ($hmm,%args) = @_;
  return $hmm->{af} if (defined($hmm->{af}));

  my $total = pdl($hmm->{type}, log($args{total} ? $args{total} : $hmm->{atotal}));
  my $af    = zeroes($hmm->{type}, $hmm->{a}->dims);
  ($hmm->{a} + $hmm->{a1} + $total)->exp($af);
  $af->inplace->setnantobad->inplace->setbadtoval(0);
  $af->where(approx($af,0,$EPS)) .= 0;
  $af->inplace->rint if ($args{round});                       ##-- round
  return $af->convert($args{type}) if (defined($args{type})); ##-- convert
  return $af;
}

##--------------------------------------------------------------
## $bf = $hmm->bf(%args)
##   + get observation join frequency pdl ($N,$M)
##   + returns $hmm->{bf} if defined
##   + %args:
##      total=>$total, (default=$hmm->{btotal})
##      round=>$bool,  (round to integer?)
##      type=>$type,   (output datatype)
sub bf {
  my ($hmm,%args) = @_;
  return $hmm->{bf} if (defined($hmm->{bf}));

  my $total = pdl($hmm->{type}, log($args{total} ? $args{total} : $hmm->{btotal}));
  my $bf    = zeroes($hmm->{type}, $hmm->{b}->dims);
  ($hmm->{b} + $hmm->{b1} + $total)->exp($bf);
  $bf->inplace->setnantobad->inplace->setbadtoval(0);
  $bf->where(approx($bf,0,$EPS)) .= 0;
  $bf->inplace->rint if ($args{round});                       ##-- round
  return $bf->convert($args{type}) if (defined($args{type})); ##-- convert
  return $bf;
}


##--------------------------------------------------------------
## $pif = $hmm->pif(%args)
##   + get initial frequency pdl ($N)
##   + returns $hmm->{pif} if defined
##   + %args:
##      total=>$total (default=$hmm->{pitotal})
##      round=>$bool, (round to integer?)
##      type=>$type,  (output datatype)
sub pif {
  my ($hmm,%args) = @_;
  return $hmm->{pif} if (defined($hmm->{pif}));

  my $total = pdl($hmm->{type}, log($args{total} ? $args{total} : $hmm->{pitotal}));
  my $pif   = zeroes($hmm->{type}, $hmm->{pi}->dims);
  ($hmm->{pi} + $total)->exp($pif);
  $pif->inplace->setnantobad->inplace->setbadtoval(0);
  $pif->where(approx($pif,0,$EPS)) .= 0;
  $pif->inplace->rint if ($args{round});                      ##-- round
  return $pif->convert($args{type}) if (defined($args{type})); ##-- convert
  return $pif;
}


##--------------------------------------------------------------
## $omegaf = $hmm->omegaf(%args)
##   + get final frequency pdl ($N)
##   + returns $hmm->{omegaf} if defined
##   + %args:
##      total=>$total (default=$hmm->{pitotal})
##      round=>$bool, (round to integer?)
##      type=>$type,  (output datatype)
sub omegaf {
  my ($hmm,%args) = @_;
  return $hmm->{omegaf} if (defined($hmm->{omegaf}));

  my $total  = pdl($hmm->{type}, log($args{total} ? $args{total} : $hmm->{atotal}));
  my $omegaf = zeroes($hmm->{type}, $hmm->{omega}->dims);
  ($hmm->{omega} + $hmm->{a1} + $total)->exp($omegaf);
  $omegaf->inplace->setnantobad->inplace->setbadtoval(0);
  $omegaf->where(approx($omegaf,0,$EPS)) .= 0;
  $omegaf->inplace->rint if ($args{round});                       ##-- round
  return $omegaf->convert($args{type}) if (defined($args{type})); ##-- convert
  return $omegaf;
}



##======================================================================
## Compilation (from frequency distributions)
##======================================================================

##--------------------------------------------------------------
## $hmm = $hmm->compilePdls($af,$bf,$pif,$omegaf,%args)
##  + $af : pdl($N,$N)  : $af    ->at($i,$j) = f($i --> $j)
##  + $bf : pdl($N,$M)  : $bf    ->at($i,$k) = f($k  @  $i)
##  + $pif    : pdl($N) : $pif   ->at($i)    = f(BOS--> $i)
##  + $omegaf : pdl($N) : $omegaf->at($i)    = f($i --> $EOS)
##  + %args
##    + smoothb=>$which, smoothbmin=>$cutoff,
sub compilePdls {
  my ($hmm,$af,$bf,$pif,$omegaf,%args) = @_;

  $hmm->compileArcs($af,$omegaf,%args)
  && $hmm->compileB($bf,%args)
  && $hmm->compilePi($pif,%args)
  && return $hmm;

  return undef;
}


##--------------------------------------------------------------
## $hmm = $hmm->compileArcs($af,$omegaf,%args)
##  + $af : pdl($N,$N)  : $af    ->at($i,$j) = f($i --> $j)
##  + $omegaf : pdl($N) : $omegaf->at($i)    = f($i --> $EOS)
##  + %args: (none)
sub compileArcs {
  my ($hmm,$af,$omegaf,%args) = @_;

  $hmm->resize(N=>$af->dim(0)) if ($af->dim(0) != $hmm->{N});

  my ($a,$a1,$omega) = @$hmm{qw(a a1 omega)};

  $hmm->{atotal} = $af->sum + $omegaf->sum;
  my $afsumover = ($omegaf + $af->xchg(0,1)->sumover)->inplace->log;

  ##-- a
  $a1    .= $afsumover   - log($hmm->{atotal});
  $a     .= log($af)     - $afsumover;
  $omega .= log($omegaf) - $afsumover;

  ##-- log-transform
  #$a1->inplace->log;
  #$a->inplace->log;

  ##-- pseudo-zero
  $a->inplace->setnantobad->inplace->setbadtoval($ZERO);
  $omega->inplace->setnantobad->inplace->setbadtoval($ZERO);

  return $hmm;
}


##--------------------------------------------------------------
## $hmm = $hmm->compileB($bf,%args)
##  + $bf: pdl($N,$M) : $bf->at($i,$k) = f($k  @  $i)
##  + %args
##     smoothb=>$type,       ##-- how to smooth unknown values (default=$hmm->{smoothb})
##     smoothbmin=>$cutoff,  ##-- cutoff value for smoothing
##     smoothbval=>$prob,    ##-- for smoothb='uniform'
sub compileB {
  my ($hmm,$bf,%args) = @_;

  $hmm->resize( N=>$bf->dim(0), M=>$bf->dim(1) )
    if ($bf->dim(0) != $hmm->{N} || $bf->dim(1) != $hmm->{M});

  my $smooth = exists($args{smoothb}) ? $args{smoothb} : $hmm->{smoothb};
  $args{smoothbmin} = $hmm->{smoothbmin} if (!defined($args{smoothbmin}));

  my ($b,$b1) = @$hmm{qw(b b1)};
  $hmm->{btotal} = $bf->sum;

  ##-- special handling for 'unknown' token
  my ($ui,$fmin,$Fw,$hapax_i,$fwtnz_i,$fwtnz_n,$fwtnz_v,$fwtnz_which);
  if (defined($smooth) && defined($hmm->{unknown})) {
    $ui = $hmm->{oenum}->index($hmm->{unknown});

    if ($smooth eq 'uniform') {
      my $smoothbval = defined($args{smoothbval}) ? $args{smoothbval} : $hmm->{smoothbval};

      ##-- double-computation hack
      my $sbf1 = $bf->xchg(0,1)->sumover; ## f(q)

      if (!defined($smoothbval)) {
	##-- p(u|c) ~= 0.5 * min f(w,c) / max f(c)
	my $bfmin = $bf->where($bf!=0)->minimum;
	$smoothbval = 0.5 * $bfmin / $sbf1->maximum;
      }

      ##-- assign: f(u,c) = p(u|c) * f(c)
      $bf->slice(",($ui)") .= $sbf1 * $smoothbval;
    }
    elsif ($smooth eq 'uniform_c') {
      ##-- unknown-handling: use uniform *counts*
      $fmin = 0.5*min($bf->where($bf>0));
      $bf->slice(",($ui)") .= $fmin;
    }
    elsif ($smooth eq 'hapax') {
      ##-- unknown-handling: use hapax legomena: f(state,unknown) = sum_{w:f(w)=min(f(w))} f(state,w)
      $Fw      = $bf->sumover;
      $fmin    = defined($args{smoothbmin}) ? $args{smoothbmin} : (1.1*min($Fw->where($Fw>0)));
      #print STDERR "smooth: hapax: fmin=$fmin\n";
      $hapax_i = which( ($Fw > 0) & ($Fw <= $fmin) );
      $bf->slice(",($ui)") .= $bf->dice_axis(1,$hapax_i)->xchg(0,1)->sumover;
      #print STDERR "smooth: bf(:,ui)=", $bf->slice(",($ui)"), "\n";
    }
    elsif ($smooth eq 'all') {
      ##-- unknown-handling: use all data: f(state,unknown) = |{w : f(w,state)!=0}|
      $fwtnz_i = $bf->whichND;
      ($fwtnz_n,$fwtnz_v) = $fwtnz_i->slice("(0)")->qsort->rle;
      $fwtnz_which = $fwtnz_n->which;
      $bf->slice(",($ui)")->index($fwtnz_v->index($fwtnz_which)) .= $fwtnz_n->index($fwtnz_which);
    }
    elsif ($smooth && $smooth ne 'none') {
      carp(ref($hmm)."::compileB(): ignoring unknown B-smoothing flag '$smooth'\n");
    }
  }

  my $bfsumover = $bf->xchg(0,1)->sumover->inplace->log;

  $b1 .= $bfsumover - log($hmm->{btotal});
  $b  .= log($bf) - $bfsumover;

  ##-- uniform b smoothing
  #if (defined($smooth) && defined($hmm->{unknown}) && $smooth eq 'uniform') {
  #  my $bnz = $b->where($b>logzero);
  #  my $smoothbval = defined($args{smoothbval}) ? $args{smoothbval} : $hmm->{smoothbval};
  #  $smoothbval = $bnz->minimum + log(0.5) if (!defined($smoothbval));
  #  ##-- remove N*p(u|c)/N_nonzero from known values p(w|c) [w!=u]
  #  $bnz->inplace->logdiff($smoothbval + log($b->dim(0)) - log($bnz->nelem));
  #  ##-- assign
  #  $b->slice(",($ui)") .= $smoothbval;
  #}

  ##-- pseudo-zero
  $b->inplace->setnantobad->inplace->setbadtoval($ZERO);

  return $hmm;
}

##--------------------------------------------------------------
## $hmm = $hmm->compilePi($pif,%args)
##  + %args: (none)
##  + $pif    : pdl($N) : $pif   ->at($i)    = f(BOS--> $i)
sub compilePi {
  my ($hmm,$pif,%args) = @_;

  ##-- old
  #my $bosid = $hmm->{qenum}->index($hmm->{bos});
  #$hmm->{pi} .= logzero;
  #$hmm->{pi}->set($bosid, $ONE);

  $hmm->{pitotal} = $pif->sum;
  $hmm->{pi}     .= log($pif) - log($hmm->{pitotal});

  return $hmm;
}


##--------------------------------------------------------------
## $hmm = $hmm->compileEDists($aed,$bed,%args)
##  + $aed: MUDL::EDist::Nary:
##      $aed->{nz}{"$i\t$j"} == f($i --> $j)
##  + $bed: MUDL::EDist::Nary
##      $bed->{nz}{"$i\t$k"} == f($k  @  $i)
##  + $pied: MUDL::EDist
##      $pied->{nz}{"$i"} == f(BOS --> $i)
##  + $omegaed: MUDL::EDist
##      $omegaed->{nz}{"$i"} == f($i --> EOS)
##  + edists should use $hmm->{qenum} and $hmm->{oenum} enums as appropriate
sub compileEDists {
  my ($hmm,$aed,$bed,$pied,$omegaed,%args) = @_;
  return $hmm->compilePdls($aed->toPDL,
			   $bed->toPDL,
			   $pied->toPDL,
			   $omegaed->toPDL,
			   %args);
}


##--------------------------------------------------------------
## $hmm = $hmm->compileDists($ad,$bd,%args)
##  + $ad: Dist::Nary:
##      $af->{nz}{"$ilab\t$jlab"} == f( $hmm->{qenum}->indices($ilab,$jlab) ) == f($ilab --> $jlab)
##  + $bd: Dist::Nary
##      $bf->{nz}{"$ilab\t$klab"} == f( $hmm->{oenum}->indices($ilab,$klab) ) == f($klab  @  $ilab)
##  + $pid: MUDL::Dist
##      $pid->{nz}{"$ilab"}       == f( BOS , $hmm->{qenum}->indices($ilab) ) == f( BOS  --> $ilab)
##  + $omegad: MUDL::Dist
##      $omegad->{nz}{"$ilab"}    == f( $hmm->{qenum}->indices($ilab) , EOS ) == f($ilab --> EOS)
sub compileDists {
  my ($hmm,$ad,$bd,$pid,$omegad,%args) = @_;
  my $aed = $ad->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$hmm->{qenum},$hmm->{qenum}]));
  my $bed = $bd->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$hmm->{qenum},$hmm->{oenum}]));
  my $pied = $pid->toEDist($hmm->{qenum});
  my $omegaed = $omegad->toEDist($hmm->{qenum});
  return $hmm->compileEDists($aed,$bed,$pied,$omegaed,%args);
}


##--------------------------------------------------------------
## ($ad,$pid,$omegad) = $hmm->separateArcDist($ad,%args)
##  + $ad: Dist::Nary:
##      $af->{nz}{"$ilab\t$jlab"} == f( $hmm->{qenum}->indices($ilab,$jlab) ) == f($ilab --> $jlab)
##  + $pid: MUDL::Dist
##      $pied->{nz}{"$ilab"}      == f( BOS , $hmm->{qenum}->indices($ilab) ) == f( BOS  --> $ilab)
##  + $omegad: MUDL::Dist
##      $omegaed->{nz}{"$ilab"}   == f( $hmm->{qenum}->indices($ilab) , EOS ) == f($ilab --> EOS)
##
##  + separates BOS,EOS entries out of $ad into new dists $pid and $omegad.
##  + destructively alters $ad in the process, unless $args{keep} is set
sub separateArcDist {
  my ($hmm,$ad,%args) = @_;

  my $pid    = MUDL::Dist->new();
  my $omegad = MUDL::Dist->new();
  my ($bos,$eos) = @$hmm{qw(bos eos)};

  my @badkeys = qw();
  my ($k,$f,$q1,$q2);
  while (($k,$f)=each(%{$ad->{nz}})) {
    ($q1,$q2) = $ad->split($k);
    if ($q1 eq $bos && $q2 ne $eos) {
      ##-- BOS-->$q2
      $pid->{$q2} += $f;
      push(@badkeys,$k);
    }
    elsif ($q1 ne $bos && $q2 eq $eos) {
      ##-- $q1-->$EOS
      $omegad->{$q1} += $f;
      push(@badkeys,$k);
    }
    elsif ($q1 eq $bos || $q1 eq $eos || $q2 eq $bos || $q2 eq $eos) {
      push(@badkeys,$k);
    }
  }

  delete(@{$ad->{nz}}{@badkeys}) if (!$args{keep});

  return ($ad,$pid,$omegad);
}


##======================================================================
## Utilities: Buffering / Compatibility
##======================================================================

## $pdlBuffer = $hmm->_buffer(%args)
##  + creates and returns a new MUDL::Corpus::Buffer::Pdl using
##    $hmm->{oenum} and bashing unknowns to $hmm->{unknown}.
##  + %args are passed to MUDL::Corpus::Buffer::Pdl->new()
sub _buffer {
  my $hmm = shift;
  require MUDL::Corpus::Buffer::Pdl;
  return MUDL::Corpus::Buffer::Pdl->new(txtenum=>$hmm->{oenum},
					dobash=>1,
					bashto=>$hmm->{unknown},
					dobos=>0,
					doeos=>0,
					@_);
}

## $sequencePdl = $hmm->sentence2sequence($mudl_sentence)
##   + converts $mudl_sentence to a valid pdl sequence
##   + adds pseudo-elements bos, eos to sequence
sub sentence2sequence {
  my ($hmm,$s) = @_;
  my ($id);
  return pdl(long, [
		    #$hmm->{oenum}->index($hmm->{bos}),
		    (map {
		      (defined($id=$hmm->{oenum}->index(ref($_) ? $_->text : $_))
		       ? $id
		       : $hmm->{oenum}->index($hmm->{unknown}))
		    } @$s),
		    #$hmm->{oenum}->index($hmm->{eos}),
		   ]);
}

## $hmm = $hmm->growFromReader($cr,%args)
##  + adds unknown tokens from reader to $hmm->{oenum} and (possibly) calls grow()
##  + %args are passed to $hmm->grow(), except for:
##    dogrow=>$bool,  ##-- grow() is called iff $bool is true (default=1)
sub growFromReader {
  my ($hmm,$cr,%args) = @_;
  my $oenum = $hmm->{oenum};
  my ($s,$w);
  while (defined($s=$cr->getSentence)) {
    foreach $w (@$s) {
      $oenum->addSymbol(ref($w) ? $w->text : $w);
    }
  }
  $hmm->grow(%args) if (!defined($args{dogrow}) || $args{dogrow});
  return $hmm;
}

##======================================================================
## Text-probability
##======================================================================

##--------------------------------------------------------------
## Text-probability: Sequences

## $alpha = $hmm->alpha($seq)
##   + no bos/eos are added
sub alpha {
  my ($hmm,$o) = @_;
  return hmmalpha(@$hmm{qw(a b pi)}, $o);
}
## $beta = $hmm->beta($seq)
##   + no bos/eos are added
sub beta {
  my ($hmm,$o) = @_;
  return hmmbeta(@$hmm{qw(a b omega)}, $o);
}

## $logprob = $hmm->sequenceProbability($seq)
##   + no bos/eos are added
*seqprob = \&sequenceProbability;
sub sequenceProbability {
  my ($hmm,$o) = @_;
  my $alpha = hmmalpha(@$hmm{qw(a b pi)}, $o);
  return logsumover($alpha->slice(':,-1') + $hmm->{omega});
}


##--------------------------------------------------------------
## Text-probability: MUDL::Corpus::Model overrides

## $log_sentprob = $hmm->sentenceProbability($mudl_sentence)
sub sentenceProbability {
  return $_[0]->sequenceProbability($_[0]->sentence2sequence($_[1]));
}

###### $log_sum_sentprobs = $hmm->readerProbability($corpusReader)
## \%info = $hmm->readerProbability($corpusReader)
##  + sentence probabilities are added with logadd()
##  + returns total log(sum(p($s))) for each $s in $corpusReader
sub readerProbability {
  my ($hmm,$cr) = @_;

  my ($a,$b,$pi,$omega) = @$hmm{qw(a b pi omega)};
  my $alpha = zeroes($hmm->{type},1,1);

  my $logsum  = logzero;
  my $logprod = pdl(double,0); ##-- log(1)
  my ($o,$id,$logp);
  my $nsents = 0;

  while (defined($s=$cr->getSentence)) {
    ++$nsents;
    $o = $hmm->sentence2sequence($s);
    $alpha->reshape($hmm->{N}, $o->dim(0));
    hmmalpha($a,$b,$pi, $o, $alpha);
    $logp = logsumover($alpha->slice(":,-1") + $omega);
    $logprod += $logp;
    $logsum->inplace->logadd($logp);
  }

  return {logprod=>$logprod, logsum=>$logsum, nsents=>$nsents};
}

## $log_sum_sentprobs = $hmm->fileProbability($file,%args)
##   + %args are passed to MUDL::CorpusIO::fileReader()
##
##-- inherited from MUDL::Corpus::Model


###### $log_sum_sentprobs = $model->bufferProbability($buffer,@args)
## \%info = $model->bufferProbability($buffer,@args)
##  + returns log(sum(p($s))) for each sentence $s in $buffer
sub bufferProbability {
  my ($hmm,$buf) = splice(@_,0,2);
  if (UNIVERSAL::isa($buf,'MUDL::Corpus::Buffer::Pdl')) {
    ##-- special handling for pdl-ized buffers
    return $hmm->pdlBufferProbability($buf,@_);
  }
  ##-- use inherited method otherwise
  return $hmm->SUPER::bufferProbability($buf,@_);
}


##--------------------------------------------------------------
## Text-probability: from MUDL::Corpus::Buffer::Pdl

##### $log_sum_sentprobs = $hmm->pdlBufferProbability($pdl_buffer)
## \%info = $hmm->pdlBufferProbability($pdl_buffer)
##  + returns log(sum(p($s))) for each sentence $s in $pdl_buffer
##  + no limit-checking is performed, i.e. buffer is assumed
##    already to have bashed unknown tokens to $hmm->{unknown},
##    and to have accounted for {bos} and {eos} elements.
sub pdlBufferProbability {
  my ($hmm,$buf) = @_;
  require MUDL::Corpus::Buffer::Pdl;

  my ($a,$b,$pi,$omega,$N) = @$hmm{qw(a b pi omega N)};
  my $logsum = logzero;
  my $logprod = pdl(double,0);

  my ($o,$logp);
  my $alpha = zeroes($hmm->{type},1,1);
  foreach $o (@{$buf->{sents}}) {
    $alpha->reshape($N, $o->dim(0));
    hmmalpha($a,$b,$pi, $o, $alpha);

    $logp = logsumover($alpha->slice(",-1") + $omega);
    $logsum->inplace->logadd($logp);
    $logprod += $logp;
  }

  return { logprod=>$logprod, logsum=>$logsum, nsents=>scalar(@{$buf->{sents}}) };
}


##======================================================================
## Re-estimation
##======================================================================

## ($ea,$eb,$epi,$eomega) = $hmm->expect0()
##   + initializes @$hmm{qw(ea eb epi eomega)} for re-estimation
sub expect0 {
  my $hmm = shift;
  if (!(defined($hmm->{ea}) && defined($hmm->{eb}) && defined($hmm->{epi}) && defined($hmm->{eomega}))) {
    @$hmm{qw(ea eb epi eomega)} = hmmexpect0(@$hmm{qw(a b pi omega)});
  } else {
    $hmm->{ea}  .= logzero;
    $hmm->{eb}  .= logzero;
    $hmm->{epi} .= logzero;
    $hmm->{eomega} .= logzero;
  }
  return @$hmm{qw(ea eb epi eomega)};
}

## ($ea,$eb,$epi,$eomega) = $hmm->expect($seq)
##   + (partial) re-estimation for input $seq
sub expect {
  my ($hmm,$o) = @_;
  my ($a,$b,$pi,$omega, $ea,$eb,$epi,$eomega) = @$hmm{qw(a b pi omega ea eb epi eomega)};
  my $alpha = hmmalpha($a,$b,$pi,    $o);
  my $beta  = hmmbeta ($a,$b,$omega, $o);
  hmmexpect($a,$b,$pi,$omega, $o,$alpha,$beta, $ea,$eb,$epi,$eomega);
  return ($ea,$eb,$epi,$eomega);
}

## ($ahat,$bhat,$pihat,$omegahat) = $hmm->maximize(%args)
##   + maximization step for Baum-Welch re-estimation
##   + uses expectation values in @$hmm{qw(ea eb epi eomega)}
##   + %args:
##     - maxa  => $bool,    ## maximize $a ? default=$hmm->{maxa}  || 1
##     - maxb  => $bool,    ## maximize $b ? default=$hmm->{maxb}  || 1
##     - maxpi => $bool,    ## maximize $pi? default=$hmm->{maxpi} || 1
##     - maxomega => $bool, ## maximize $omega? default=$hmm->{maxomega} || 1
sub maximize {
  my ($hmm,%args) = @_;
  foreach (qw(maxa maxb maxpi maxomega)) {
    next if (defined($args{$_}));
    $args{$_} = defined($hmm->{$_}) ? $hmm->{$_} : 1;
  }
  my ($ea,$eb,$epi,$eomega) = @$hmm{qw(ea eb epi eomega)};
  my ($ahat,$bhat,$pihat,$omegahat) = hmmmaximize(@$hmm{qw(ea eb epi eomega)});

  $hmm->{a}     = $ahat     if ($args{maxa});
  $hmm->{b}     = $bhat     if ($args{maxb});
  $hmm->{pi}    = $pihat    if ($args{maxpi});
  $hmm->{omega} = $omegahat if ($args{maxomega});

  return ($ahat,$bhat,$pihat,$omegahat);
}


##--------------------------------------------------------------
## Reestimation: CorpusIO / MUDL::Corpus::Profile stuff

## ($log_prod_ps, $ea,$eb,$epi,$eomega) = $hmm->expectReader($corpusReader,%args)
##  + run a single E- part of an EM-iteration on the sentences from $corpusReader
##  + calls $hmm->expect0() unless all $hmm->{eX} members are defined
##  + %args:
sub expectReader {
  my ($hmm,$cr,%args) = @_;

  my ($a,$b,$pi,$omega,$N) = @$hmm{qw(a b pi omega N)};
  my ($ea,$eb,$epi,$eomega) = @$hmm{qw(ea eb epi eomega)};

  ($ea,$eb,$epi,$eomgega) = $hmm->expect0()
    unless (defined($ea) && defined($eb) && defined($epi) && defined($eomega));

  my ($s,$o);
  my $alpha = zeroes($hmm->{type}, $N,128);
  my $beta  = zeroes($hmm->{type}, $N,128);
  my $log_prod_ps  = 0;
  while (defined($s=$cr->getSentence)) {
    $o = $hmm->sentence2sequence($s);

    $alpha->reshape($N,$o->dim(0));
    $beta ->reshape($N,$o->dim(0));

    hmmalpha($a,$b,$pi,    $o, $alpha);
    hmmbeta ($a,$b,$omega, $o, $beta);

    hmmexpect($a,$b,$pi,$omega, $o,$alpha,$beta, $ea,$eb,$epi,$eomega);

    ##-- save text probability (multiply)
    $log_prod_ps += logsumover($alpha->slice(',-1') + $omega);
  }

  return ($log_prod_ps, $ea,$eb,$epi,$eomega);
}

## ($log_prod_ps, $ea,$eb,$epi,$eomega) = $hmm->expectFiles(\@files,%args)
##  + run a single E- part of an EM-iteration on the sentences from \@files
##  + just calls $hmm->expectReader() for each file
##  + %args: (none)
sub expectFiles {
  my ($hmm,$files,%args) = @_;

  my $log_prod_ps = 0;
  my $log_prod_ps_file = 0;

  my ($cr,@expect);
  foreach $file (@$files) {
    $cr = MUDL::CorpusReader->fromFile($file);

    ($log_prod_ps_file,@expect) = $hmm->expectReader($cr,%args);
    $log_prod_ps += $log_prod_ps_file;
  }

  return ($log_prod_ps, $ea,$eb,$epi,$eomega);
}

## $hmm = $hmm->emReader($corpusReader,%args)
##  + run a single EM iteration on the sentences from $corpusReader
##  + %args: passed to $hmm->expectReader() and to $hmm->maximize()
##     - maxa  => $bool,    ## maximize $a ? default=$hmm->{maxa}  || 1
##     - maxb  => $bool,    ## maximize $b ? default=$hmm->{maxb}  || 1
##     - maxpi => $bool,    ## maximize $pi? default=$hmm->{maxpi} || 1
##     - maxomega => $bool, ## maximize $omega? default=$hmm->{maxomega} || 1
sub emReader {
  my ($hmm,$cr,%args) = @_;
  $hmm->expectReader($cr,%args);
  $hmm->maximize(%args);
  return $hmm;
}


##--------------------------------------------------------------
## Reestimation: Corpus::Buffer

## ($log_prod_ps, $ea,$eb,$epi,$eomega) = $hmm->expectBuffer($corpusBuffer,%args)
##  + run a single E- part of an EM-iteration on the sentences from $corpusBuffer
##  + %args: (none)
sub expectBuffer {
  my ($hmm,$buf,%args) = @_;
  if (UNIVERSAL::isa($buf,'MUDL::Corpus::Buffer::Pdl')) {
    ##-- special handling for pdl-ized buffers
    return $hmm->expectPdlBuffer($buf,@_);
  }
  ##-- use inherited method otherwise
  return $hmm->expectReader($buf->reader());
}

## ($log_prod_ps, $ea,$eb,$epi,$eomega) = $hmm->expectPdlBuffer($corpusBufferPdl,%args)
##  + run a single E- part of an EM-iteration on the sentences from $corpusBufferPdl,
##    which should use hmm's enums and bash to $hmm->{unknown}
##  + %args: (none)
sub expectPdlBuffer {
  my ($hmm,$buf,%args) = @_;

  my ($a,$b,$pi,$omega,$N) = @$hmm{qw(a b pi omega N)};
  my ($ea,$eb,$epi,$eomega) = $hmm->expect0();

  my ($o);
  my $alpha = zeroes($hmm->{type}, $N, 128);
  my $beta  = zeroes($hmm->{type}, $N, 128);

  my $log_prod_ps = 0;

  foreach $o (@{$buf->{sents}}) {
    $alpha->reshape($N,$o->dim(0));
    $beta ->reshape($N,$o->dim(0));

    hmmalpha($a,$b,$pi,    $o, $alpha);
    hmmbeta ($a,$b,$omega, $o, $beta);

    hmmexpect($a,$b,$pi,$omega, $o,$alpha,$beta, $ea,$eb,$epi,$eomega);

    ##-- save text probability (multiply)
    $log_prod_ps += logsumover($alpha->slice(',-1') + $omega);
  }

  return ($log_prod_ps, $ea,$eb,$epi,$eomega);
}

## $hmm = $hmm->emBuffer($corpusBuffer,%args)
##  + run a single EM iteration on the sentences in $corpusBuffer
##  + %args: passed to $hmm->expectBuffer() and to $hmm->maximize()
##     - maxa  => $bool,    ## maximize $a ? default=$hmm->{maxa}  || 1
##     - maxb  => $bool,    ## maximize $b ? default=$hmm->{maxb}  || 1
##     - maxpi => $bool,    ## maximize $pi? default=$hmm->{maxpi} || 1
##     - maxomega => $bool, ## maximize $omega? default=$hmm->{maxomega} || 1
sub emBuffer {
  my ($hmm,$buf,%args) = @_;
  my ($logp,@expect) = $hmm->expectBuffer($buf,%args);
  $hmm->maximize(%args);
  return $hmm;
}


##======================================================================
## Viterbi
##======================================================================

## ($delta,$psi) = $hmm->viterbi($seq)
sub viterbi {
  my ($hmm,$o) = @_;
  my $delta = zeroes(double, $hmm->{N}, $o->dim(0));
  my $psi   = zeroes(long,   $delta->dims);
  hmmviterbi(@$hmm{qw(a b pi)}, $o, $delta, $psi);
  return ($delta,$psi);
}

## $bestpath = $hmm->bestpath($psi)
## $bestpath = $hmm->bestpath($psi, $qfinal)
sub bestpath {
  my ($hmm,$psi,$qfinal) = @_;
  #$qfinal = $hmm->{qenum}->index($hmm->{eos}) if (!defined($qfinal));
  $qfinal = maximum_ind($delta->slice(":,(-1)") + $hmm->{omega})->at(0) if (!defined($qfinal));
  return hmmpath($psi, $qfinal);
}

## $bestpath = $hmm->viterbiPath($seq)
sub viterbiPath {
  my ($hmm,$o) = @_;
  my $delta = zeroes(double, $hmm->{N}, $o->dim(0));
  my $psi   = zeroes(long,   $delta->dims);
  hmmviterbi(@$hmm{qw(a b pi)}, $o, $delta, $psi);
  my $qfinal = maximum_ind($delta->slice(":,-1") + $hmm->{omega})->at(0);
  return hmmpath($psi, $qfinal);
}


##--------------------------------------------------------------
## Viterbi: CorpusIO style / MUDL::Corpus::Filter overrides

## $filter = $hmm->viterbiFilter(%args)
#   + returns a MUDL::Corpus::Filter::Viterbi for this HMM, shared
sub viterbiFilter {
  my $hmm = shift;
  require MUDL::Corpus::Filter::Viterbi;
  return MUDL::Corpus::Filter::Viterbi->new(%$hmm,@_);
}

## \@sentence = $hmm->viterbiTagSentence(\@sentence)
sub viterbiTagSentence {
  my ($hmm,$s) = @_;
  my $path = $hmm->viterbiPath($hmm->sentence2sequence($s));
  foreach $i (0..$#$s) {
    #$s->[$i]->tag($hmm->{qenum}->symbol($path->at($i+1)));
    $s->[$i]->tag($hmm->{qenum}->symbol($path->at($i)));
  }
  return $s;
}


##======================================================================
## I/O: Native: Lex
##======================================================================
__PACKAGE__->registerIOMode('lex',{saveFh=>'saveLexFh',loadFh=>'loadLexFh'});
__PACKAGE__->registerFileSuffix('.lex','lex');

## $bool = $obj->saveLexFh($fh,%args)
##   + %args:
##      + total=>$total_lex_count
##      + round=>$bool,
sub saveLexFh {
  my ($hmm,$fh,%args) = @_;
  my ($oenum,$qenum) = @$hmm{qw(oenum qenum)};
  my $bf = $hmm->bf(%args);
  my $bt = $bf->sumover;

  my ($ws,$qi);
  local $, = '';
  foreach $ki (0..($bf->dim(1)-1)) {
    $w = $oenum->symbol($ki);
    next if ($w eq $hmm->{bos} || $w eq $hmm->{eos}
	     ||
	     ($hmm->{smooth} && defined($hmm->{unknown}) && $w eq $hmm->{unknown}));
    $fh->print(join("\t",
		    $w, $bt->at($ki),
		    (map {
		      ($qenum->symbol($_), $bf->at($_,$ki))
		    } $bf->slice(",($ki)")->which->list)),
	       "\n");
  }

  return $hmm;
}

## $obj = $class_or_obj->loadLexFh($fh,%args)
##   + really just populates $obj->{bfd} with the dist
sub loadLexFh {
  my ($hmm,$fh,%args) = @_;
  $hmm = $hmm->new(%args) if (!ref($hmm));

  require MUDL::Lex;
  my $lex = MUDL::Lex->loadNativeFh($fh);
  $hmm->{bfd} = $lex->toDist->projectN(1,0);

  return $hmm;
}


##======================================================================
## I/O: Native: 123 (n-grams)
##======================================================================
__PACKAGE__->registerIOMode('123',{saveFh=>'save123Fh',loadFh=>'load123Fh'});
__PACKAGE__->registerFileSuffix('.123','123');

## $bool = $obj->save123Fh($fh,%args)
##   + %args
##      total=>$total_bigram_count,
##      round=>$bool,
##      verbose=>$bool,
sub save123Fh {
  my ($hmm,$fh,%args) = @_;

  my ($oenum,$qenum) = @$hmm{qw(oenum qenum)};
  my $af  = $hmm->af(%args);
  my $pif = $hmm->pif(%args);
  my $omegaf = $hmm->omegaf(%args);
  my $af1 = $af->xchg(0,1)->sumover + $omegaf;

  my $verbose = defined($args{verbose}) ? $args{verbose} : $hmm->{verbose};

  my ($qi,$qj, $qis);
  local $, = '';

  ##-- print initial arcs: f(BOS-->$qi) [pi]
  if ($hmm->{pitotal} > 0) {
    $fh->print($hmm->{bos}, "\t",
	       $hmm->{pitotal} + $omegaf->sum, ##-- bos hack
	       "\n");
    foreach $qi ($pif->which->list) {
      $qis = $qenum->symbol($qi);
      $fh->print(($verbose ? $hmm->{bos} : ''), "\t",
		 $qis, "\t",
		 $pif->at($qi), "\n");
    }
  }

  ##-- print non-initial arcs [a,omega]
  foreach $qi (0..($af->dim(0)-1)) {
    $qis = $qenum->symbol($qi);
    next if (!$af1->at($qi));  ##-- ignore zero-frequency unigrams

    ##-- print unigram frequency
    $fh->print($qis, "\t", $af1->at($qi), "\n");


    ##-- print final arcs: f($qi --> EOS) [omega]
    $fh->print(($verbose ? $qis : ''), "\t",
	       $hmm->{eos}, "\t",
	       $omegaf->at($qi), "\n")
      if ($omegaf->at($qi) > 0);

    ##-- print internal arcs f($qi --> $qj) [a]
    foreach $qj ($af->slice("($qi)")->which->list) {
      $fh->print(($verbose ? $qis : ''), "\t",
		 $qenum->symbol($qj), "\t",
		 $af->at($qi,$qj), "\n");
    }
  }

  return $hmm;
}

## $obj = $class_or_obj->load123Fh($fh,%args)
##   + really just populates $obj->{afd} with the dist
sub load123Fh {
  my ($hmm,$fh,%args) = @_;
  $hmm = $hmm->new(%args) if (!ref($hmm));

  require MUDL::Ngrams;
  $hmm->{afd} = MUDL::Ngrams->loadNativeFh($fh,nfields=>2,%args);

  return $hmm;
}

##======================================================================
## I/O: Native: model (lex & n-grams)
##======================================================================
__PACKAGE__->registerIOMode('model', {saveFile=>'saveModelFile',loadFile=>'loadModelFile'});
__PACKAGE__->registerIOMode('native',{saveFile=>'saveModelFile',loadFile=>'loadModelFile'});
__PACKAGE__->registerFileSuffix('.model','model');


## $obj = $obj->saveModelFile($filename,%args)
##   + saves:
##     * lexicon "${filename}.lex"
##     * n-grams "${filename}.123"
sub saveModelFile {
  my ($hmm,$filename,%args) = @_;

  $hmm->saveFile("$filename.lex", %args, mode=>'lex')
    or confess(ref($hmm),"::saveModelFile(): save failed for lexicon file '$filename.lex': $!");
  $hmm->saveFile("$filename.123", %args, mode=>'123')
    or confess(ref($hmm),"::saveModelFile(): save failed for n-gram file '$filename.123': $!");

  return $hmm;
}


## $obj = $class_or_obj->loadModelFile($filename,%args)
##   + loads:
##     * lexicon "${filename}.lex"
##     * n-grams "${filename}.123"
##   + compiles
##   + deletes temporary distributions $hmm->{afd,bfd} unless $args{debug} is true
sub loadModelFile {
  my ($hmm,$filename,%args) = @_;

  $hmm = $hmm->new(%args) if (!ref($hmm));

  $hmm = $hmm->loadFile("$filename.lex", %args, mode=>'lex')
    or confess(ref($hmm),"::loadModelFile(): load failed for lexicon file '$filename.lex': $!");
  $hmm = $hmm->loadFile("$filename.123", %args, mode=>'123')
    or confess(ref($hmm),"::loadModelFile(): load failed for n-gram file '$filename.123': $!");

  my ($afd,$pifd,$omegafd) = $hmm->separateArcDist($hmm->{afd})
    or confess(ref($hmm),"::loadModelFile(): separateArcDist() failed for model '$filename': $!");

  $hmm = $hmm->compileDists($afd, $hmm->{bfd}, $pifd, $omegafd, %args)
    or confess(ref($hmm),"::loadModelFile(): compile() failed for model '$filename': $!");

  delete(@$hmm{qw(afd bfd)}) if (!$args{debug});

  return $hmm;
}


##======================================================================
## I/O: Tapas Kanungo style
##======================================================================

__PACKAGE__->registerIOMode('kanungo', {saveFh=>'saveKanungoFh',loadFh=>'loadKanungoFh'});
__PACKAGE__->registerFileSuffix('.kanungo','kanungo');

## $bool = $obj->saveKanungoFh($fh,%args)
sub saveKanungoFh {
  my ($hmm,$fh) = @_;
  $fh->print("M= ", $hmm->{M}+1, "\n", ##-- +eos pseudo-observation
	     "N= ", $hmm->{N}+1, "\n", ##-- +eos-sink state
	    );

  $fh->print("A:\n");
  my $ap = $hmm->{a}->exp;
  foreach $i (0..($hmm->{N}-1)) {
    $fh->print(join(' ', $ap->slice("($i)")->list, $hmm->{omega}->at($i)), "\n");
  }
  $fh->print(join(' ', (map { 0 } (1..$hmm->{N})), 1), "\n"); ##-- +eos pseudo-state

  $fh->print("B:\n");
  my $bp = $hmm->{b}->exp;
  foreach $i (0..($hmm->{N}-1)) {
    $fh->print(join(' ', $bp->slice("($i)")->list, 0), "\n");
  }
  $fh->print(join(' ', (map { 0 } (1..($hmm->{M}))), 1), "\n"); ##-- +eos pseudo-observation

  $fh->print("pi:\n",
	     join(' ', $hmm->{pi}->exp->list, 0), "\n");

  return $hmm;
}

## $bool = $obj->saveKanungoFh($fh,%args)
*loadKanungoFh = MUDL::Object::dummy('loadKanungoFh');


1;

__END__

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
