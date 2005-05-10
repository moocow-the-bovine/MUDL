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
use Inline Pdlpp;

our @ISA = qw(MUDL::Corpus::Model);

##======================================================================
## Globals

## limit values
#our $EPS = 1.19209290E-06F
our $EPS = 1E-06;
our $ZERO = -1E+38;
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
##    bos => $bos_str,     ## bos-string (__$)
##    eos => $eos_str      ## eos-string (__$)
#     unknown => $unk_str, ## unknown-string (__UNKNOWN__)
##    oenum => $enum,      ## observation-alphabet enum
##    qenum => $enum,      ## state-label enum
##    ##
##    ##-- properties
##    N => number of states
##    K => alphabet size
##    atotal => total A count ## (for native I/O)
##    btotal => total B count ## (for native I/O)
##    type=>PDL::Type,        ## computation datatype (default=double)
##    smooth=>$which,         ## how to smooth for unknowns ('hapax' or 'all' : default='all')
##    ##
##    ##-- parameters
##    A1 => $unigram_prob_pdl ## pdl (N)  : $A1->at($i)   ~ log(p($i)) [arc] # for smoothing, lex save, etc.
##    B1 => $unigram_prob_pdl ## pdl (N)  : $B1->at($i)   ~ log(p($i)) [lex] # for smoothing, lex save, etc.
##    A => $arc_prob_pdl,     ## pdl (N,N): $A->at($i,$j) ~ log(p($i --> $j))
##    B => $obs_prob_pdl,     ## pdl (N,K): $B->at($i,$k) ~ log(p($k  @  $i))
##    ##
##    ##-- Native I/O
##    Bfd => $dist_nary,      ## source for B
##    Afd => $dist_nary,      ## source for A
##   }
##
sub new {
  my ($that,%args) = @_;
  my $hmm = $that->SUPER::new(bos=>'__$',
			      eos=>'__$',
			      unknown=>'__UNKNOWN__',
			      atotal=>100000,
			      btotal=>100000,
			      verbose=>0,
			      smooth=>'all',
			      %args);
  return $hmm->resize(%args);
}

##--------------------------------------------------------------
## $hmm = $hmm->resize(%args)
##   + %args: N=>$N, K=>$K, oenum=>$oenum, qenum=>$qenum
##   + eliminates any previously stored information
sub resize {
  my ($hmm,%args) = @_;

  my $oenum = defined($args{oenum}) ? $args{oenum} : $hmm->{oenum};
  my $qenum = defined($args{qenum}) ? $args{qenum} : $hmm->{qenum};

  $oenum = $hmm->{oenum} = MUDL::Enum->new() if (!defined($oenum));
  $qenum = $hmm->{qenum} = MUDL::Enum->new() if (!defined($qenum));

  $oenum->addSymbol($_) foreach (@$hmm{qw(bos eos unknown)});
  $qenum->addSymbol($_) foreach (@$hmm{qw(bos eos)});

  my $type  = defined($args{type})  ? $args{type} : double;
  my $N     = defined($args{N})     ? $args{N}    : $qenum->size;
  my $K     = defined($args{K})     ? $args{K}    : $oenum->size;
  my $A     = defined($args{A})     ? $args{A}    : zeroes($type, $N, $N);
  my $B     = defined($args{B})     ? $args{B}    : zeroes($type, $N, $K);
  my $A1    = defined($args{A1})    ? $args{A1}   : zeroes($type, $N);
  my $B1    = defined($args{B1})    ? $args{B1}   : zeroes($type, $N);

  @$hmm{qw(type N K A1 A B B1)} = ($type,$N,$K,$A1,$A,$B,$B1);
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
## Accessors
##======================================================================

##--------------------------------------------------------------
## $af = $hmm->Af(%args)
##   + get n-gram joint frequency pdl ($N,$N)
##   + %args:
##      total=>$total (default=$hmm->{atotal})
##      round=>$bool, (round to integer?)
##      type=>$type,  (output datatype)
sub Af {
  my ($hmm,%args) = @_;
  my $total = pdl($hmm->{type}, log($args{total} ? $args{total} : $hmm->{atotal}));
  my $af    = zeroes($hmm->{type}, $hmm->{A}->dims);
  ($hmm->{A} + $hmm->{A1} + $total)->exp($af);
  $af->inplace->setnantobad->inplace->setbadtoval(0);
  $af->where(approx($af,0,$EPS)) .= 0;
  $af->inplace->rint if ($args{round});                       ##-- round
  return $af->convert($args{type}) if (defined($args{type})); ##-- convert
  return $af;
}

##--------------------------------------------------------------
## $bf = $hmm->Bf(%args)
##   + get observation join frequency pdl ($N,$K)
##   + %args:
##      total=>$total, (default=$hmm->{btotal})
##      round=>$bool,  (round to integer?)
##      type=>$type,   (output datatype)
sub Bf {
  my ($hmm,%args) = @_;
  my $total = pdl($hmm->{type}, log($args{total} ? $args{total} : $hmm->{btotal}));
  my $bf    = zeroes($hmm->{type}, $hmm->{B}->dims);
  ($hmm->{B} + $hmm->{B1} + $total)->exp($bf);
  $bf->inplace->setnantobad->inplace->setbadtoval(0);
  $bf->where(approx($bf,0,$EPS)) .= 0;
  $bf->inplace->rint if ($args{round});                       ##-- round
  return $bf->convert($args{type}) if (defined($args{type})); ##-- convert
  return $bf;
}



##======================================================================
## Compilation (from frequency distributions)
##======================================================================

##--------------------------------------------------------------
## $hmm = $hmm->compilePdls($Af,$Bf,%args)
##  + $Af: pdl($N,$N) : $Af->at($i,$j) = f($i --> $j)
##  + $Bf: pdl($N,$K) : $Bf->at($i,$k) = f($k  @  $i)
##  + %args
##    + smooth=>$bool,  ##-- whether to smooth unknown values (default=true)
sub compilePdls {
  my ($hmm,$Af,$Bf,%args) = @_;

  $hmm->resize( N=>$Af->dim(0), K=>$Bf->dim(1) )
    if ($Af->dim(0) != $hmm->{N} || $Bf->dim(1) != $hmm->{K});

  my $smooth = exists($args{smooth}) ? $args{smooth} : $hmm->{smooth};
  my ($A,$B,$A1,$B1) = @$hmm{qw(A B A1 B1)};

  $hmm->{atotal} = $Af->sum;
  $hmm->{btotal} = $Bf->sum;

  ##-- special handling for unknown
  if (defined($smooth) && defined($hmm->{unknown})) {
    my $ui = $hmm->{oenum}->index($hmm->{unknown});
    if ($smooth eq 'hapax') {
      ##-- unknown-handling: use hapax legomena: f(state,unknown) = sum_{w:f(w)=min(f(w))} f(state,w)
      my $Fw      = $Bf->sumover;
      my $hapax_i = which($Fw->approx(min($Fw->where($Fw>0))));
      $Bf->slice(",($ui)") .= $Bf->dice_axis(1,$hapax_i)->xchg(0,1)->sumover;
    }
    elsif ($smooth eq 'all') {
      ##-- unknown-handling: use all data: f(state,unknown) = |{w : f(w,state)!=0}|
      my $fwtnz_i = $Bf->whichND;
      my ($fwtnz_n,$fwtnz_v) = $fwtnz_i->slice("(0)")->qsort->rle;
      my $fwtnz_which = $fwtnz_n->which;
      $Bf->slice(",($ui)")->index($fwtnz_v->index($fwtnz_which)) .= $fwtnz_n->index($fwtnz_which);
    }
    elsif ($smooth && $smooth ne 'none') {
      carp(ref($hmm)."::compilePdls(): ignoring unknown smooth flag '$smooth'\n");
    }
  }

  my $Afsumover = $Af->xchg(0,1)->sumover->inplace->log;
  my $Bfsumover = $Bf->xchg(0,1)->sumover->inplace->log;

  $A1 .= $Afsumover - log($hmm->{atotal});
  $B1 .= $Bfsumover - log($hmm->{btotal});
  $A  .= log($Af) - $Afsumover;
  $B  .= log($Bf) - $Bfsumover;

  ##-- special handling for bos,eos
  my ($sym,$oid,$qid);
  foreach $sym (@$hmm{qw(bos eos)}) {
    $oid = $hmm->{oenum}->index($sym);
    $qid = $hmm->{qenum}->index($sym);
    $B->slice(",$oid") .= $ZERO;
    $B->slice("$qid,") .= $ZERO;
    $B->set($qid,$oid, $ONE);
  }

  ##-- log-transform
  #$A1->inplace->log;
  #$B1->inplace->log;
  #$A->inplace->log;
  #$B->inplace->log;

  ##-- pseudo-zero
  $A->inplace->setnantobad->inplace->setbadtoval($ZERO);
  $B->inplace->setnantobad->inplace->setbadtoval($ZERO);

  return $hmm;
}

##--------------------------------------------------------------
## $hmm = $hmm->compileEDists($Aed,$Bed,%args)
##  + $Aed: MUDL::EDist::Nary:
##      $Aed->{nz}{"$i\t$j"} == f($i --> $j)
##  + $Bed: MUDL::EDist::Nary
##      $Bed->{nz}{"$i\t$k"} == f($k  @  $i)
##  + $Aed and $Bed should use $hmm->{qenum} and $hmm->{oenum} enums, respectively
sub compileEDists {
  my ($hmm,$Aed,$Bed,%args) = @_;
  return $hmm->compilePdls($Aed->toPDL, $Bed->toPDL,%args);
}


##--------------------------------------------------------------
## $hmm = $hmm->compileDists($Ad,$Bd,%args)
##  + $Ad: Dist::Nary:
##      $Af->{nz}{"$ilab\t$jlab"} == f( $hmm->{qenum}->indices($ilab,$jlab) ) == f($ilab --> $jlab)
##  + $Bd: Dist::Nary
##      $Bf->{nz}{"$ilab\t$klab"} == f( $hmm->{oenum}->indices($ilab,$klab) ) == f($klab  @  $ilab)
sub compileDists {
  my ($hmm,$Ad,$Bd,%args) = @_;
  my $Aed = $Ad->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$hmm->{qenum},$hmm->{qenum}]));
  my $Bed = $Bd->toEDist(MUDL::Enum::Nary->new(nfields=>2,enums=>[$hmm->{qenum},$hmm->{oenum}]));
  return $hmm->compileEDists($Aed,$Bed,%args);
}



##======================================================================
## Text-probability (MUDL::Corpus::Model overrides)
##======================================================================

## $logp = $hmm->sentenceProbability(\@sentence,@args)
##   + returns log-probability of \@sentence given model
sub sentenceProbability {
  my ($hmm,$sent) = @_;

  my ($wi);
  my $ui   = $hmm->{oenum}->index($hmm->{unknown});
  my $sids = pdl(long, [$hmm->{oenum}->index($hmm->{bos}),
			(map { defined($wi=$hmm->{oenum}->index($_->text)) ? $wi : $ui } @$sent),
			$hmm->{oenum}->index($hmm->{eos})]);
  my $alpha = $hmm->alpha($sids);

  return log(exp($alpha->slice($alpha->dim(1)-1)))->sum;
}


## $alpha = $hmm->alpha($sids_pdl)
##   + computes forward-probabilities $alpha [dims=($N,$T)]
##     for observations $sids_pdl, a vector observations of length $T,
##     where: 0 <= $q < $hmm->{N}, 0 <= $t < $T
sub alpha {
  my ($hmm,$sids) = @_;
  my ($A,$B) = @$hmm{qw(A B)};
  my $alpha = zeroes($hmm->{type}, $hmm->{N}, $sids->dim(0));

  $alpha->slice(",(0)") .= $B->slice(",(".$sids->at(0).")"); ##-- should be bos
  foreach $t (0..($alpha->dim(1)-2)) {
    foreach $j (0..($alpha->dim(0)-1)) {
      foreach $i (0..($alpha->dim(0)-1)) {
	$alpha->slice("$j,".($t+1)) += exp($alpha->at($i,$t) + $A->at($i,$j) + $B->at($i,$sids->at($t)));
      }
    }
    $alpha->slice(",".($t+1))->inplace->log;
  }

  return $alpha;
}


##======================================================================
## Re-estimation
##======================================================================

##-- TODO

##======================================================================
## Viterbi
##======================================================================

##-- TODO

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
  my $bf = $hmm->Bf(%args);
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
##   + really just populates $obj->{Bfd} with the dist
sub loadLexFh {
  my ($hmm,$fh,%args) = @_;
  $hmm = $hmm->new(%args) if (!ref($hmm));

  require MUDL::Lex;
  my $lex = MUDL::Lex->loadNativeFh($fh);
  $hmm->{Bfd} = $lex->toDist->projectN(1,0);

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
  my $af  = $hmm->Af(%args);
  my $af1 = $af->sumover;
  $af1->set(0, $af->slice("0")->sum * 2); ##-- BOS hack
  my $verbose = defined($args{verbose}) ? $args{verbose} : $hmm->{verbose};

  my ($qi,$qj, $qis);
  local $, = '';
  foreach $qi (0..($af->dim(0)-1)) {
    $qis = $qenum->symbol($qi);
    next if (!$af1->at($qi));  ##-- ignore zero-frequency unigrams

    $fh->print($qis, "\t", $af1->at($qi), "\n");
    foreach $qj ($af->slice("($qi)")->which->list) {
      $fh->print(($verbose ? $qis : ''), "\t",
		 $qenum->symbol($qj), "\t",
		 $af->at($qi,$qj), "\n");
    }
  }

  return $hmm;
}

## $obj = $class_or_obj->load123Fh($fh,%args)
##   + really just populates $obj->{Afd} with the dist
sub load123Fh {
  my ($hmm,$fh,%args) = @_;
  $hmm = $hmm->new(%args) if (!ref($hmm));

  require MUDL::Ngrams;
  $hmm->{Afd} = MUDL::Ngrams->loadNativeFh($fh,%args)->projectN(0,1);

  return $hmm;
}

##======================================================================
## I/O: Native: model (lex & n-grams)
##======================================================================
__PACKAGE__->registerIOMode('model',{saveFile=>'saveModelFile',loadFile=>'loadModelFile'});
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
##   + deletes temporary distributions $hmm->{Afd,Bfd} unless $args{debug} is true
sub loadModelFile {
  my ($hmm,$filename,%args) = @_;

  $hmm->loadFile("$filename.lex", %args, mode=>'lex')
    or confess(ref($hmm),"::loadModelFile(): load failed for lexicon file '$filename.lex': $!");
  $hmm->loadFile("$filename.123", %args, mode=>'123')
    or confess(ref($hmm),"::loadModelFile(): load failed for n-gram file '$filename.123': $!");

  $hmm->compileDists(@$hmm{qw(Afd Bfd)},%args)
    or confess(ref($hmm),"::loadModelFile(): compile() failed for model '$filename': $!");

  delete(@$hmm{qw(Afd Bfd)}) if (!$args{debug});

  return $hmm;
}


1;

__DATA__

__Pdlpp__

pp_def('alpha2',
       Pars => 'A(n,n); B(n,k); O(t); [o]alpha(n,t)',
       Code =>(''
	       ##-- initialize alpha(:,0)
	       .q(loop(n) %{ $alpha(t=>0) = $B(k=>$O(t=>0)); %})
	      ),
      );

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
