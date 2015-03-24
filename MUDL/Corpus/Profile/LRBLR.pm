#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRBLR.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile:
##    L-R log-binomial likelihood ratio
##======================================================================

package MUDL::Corpus::Profile::LRBLR;
use MUDL::Corpus::Profile::LRBigrams;
use MUDL::Object;
use MUDL::EDist;
use PDL;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile::LRBigrams);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
##       left=>$left_bigrams,       ## ($target,$lneighbor)
##       right=>$right_bigrams,     ## ($target,$rneighbor)
sub new {
  my ($that,%args) = @_; 
  return $that->SUPER::new(nfields=>1,donorm=>0,%args);
  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##  + inherited

##======================================================================
## Conversion: to PDL

##-- inherited from MUDL:::Corpus::Profile::LR

## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl)


## $pdl3d = $lr->finishPdl($pdl3d);
sub finishPdl0 {
  my ($lr,$pdl) = @_;

  my ($N,$Nbt,$Nt,$Nb, $nbt,$nt,$nb);
  foreach my $dir (0,1) {
    $Nbt  = $pdl->slice("($dir),,");
    $N    = $Nbt->sum;
    #$Ptb /= $Ptb->sum;

    $Nt   = $Nbt->sumover;
    $Nb   = $Nbt->xchg(0,1)->sumover;

    foreach my $t (0..($Nt->nelem-1)) {
      $nt = $Nt->at($t);
      foreach my $b (0..($Nb->nelem-1)) {
	$nbt = $Nbt->at($b,$t);
	$nb  = $Nb->at($b);

	$Nbt->set($b, $t,
		  logL($nbt, $nt, $nb/$N)   + logL($nb-$nbt, $N-$nt, $nb/$N)
		  -
		  logL($nbt, $nt, $nbt/$nt) - logL($nb-$nbt, $N-$nt, ($nb-$nbt)/($N-$nt))
		 );
      }
    }
  }
  $pdl->inplace->setnantobad->inplace->setbadtoval(0);

  return $pdl;
}

*finishPdl = \&finishPdl1;
sub finishPdl1 {
  my ($lr,$pdl) = @_;

  ##-- DEBUG
  #print STDERR
  #  (('~' x 80), "\n",
  #   ref($lr), "::finishPdl1(): called\n",
  #   ('~' x 80), "\n",
  #  );

  my ($N,$Nbt,$Nt,$Nb, $nbt,$nt,$nb);
  foreach my $dir (0,1) {
    $Nbt  = $pdl->slice("($dir),,");
    $N    = $Nbt->sum;
    #$Ptb /= $Ptb->sum;

    $Nt   = $Nbt->sumover;
    $Ntx  = $Nt->slice("*1");
    $Nb   = $Nbt->xchg(0,1)->sumover;

    $Pb              = $Nb  / $N;
    $Nb_minus_Nbt    = $Nb-$Nbt;
    $N_minus_Nt      = $N-$Ntx;

    $Pb_given_t      = $Nbt/$Ntx;
    $Pb_given_not_t  = $Nb_minus_Nbt / $N_minus_Nt;

    $Nbt .= -2*(logLp($Nbt, $Ntx, $Pb)         + logLp($Nb_minus_Nbt, $N_minus_Nt, $Pb)
		-
		logLp($Nbt, $Ntx, $Pb_given_t) - logLp($Nb_minus_Nbt, $N_minus_Nt, $Pb_given_not_t));

    ##-- hack: bash to negative when p(b | t) << p(b | !t)
    #$Nbt *= (($Pb_given_t > $Pb_given_not_t)-0.5)*2;

    ##-- hack: bash to zero when p(b | t) << p(b | !t)
    #$Nbt *= ($Pb_given_t > $Pb_given_not_t);
  }
  $pdl->inplace->setnantobad->inplace->setbadtoval(0);

  return $pdl;
}

## undef = $lr->normalizePdl($pdl);
##-- inherited


##======================================================================
## logLikelihood($k,$n,$p)

## $logLikelihood = logLikelihood($k,$n,$p)
##   + returns (binomial) log-likelhood log(b($k; $n, $p))
*logL = \&logLikelihood;
sub logLikelihood {
  my ($k,$n,$p) = @_;
  #return log(pow($p,$k) * pow(1-$p, $n-$k));
  return $k*log($p) + ($n-$k)*log(1-$p);
}
##-- old-version: underflow-endangered
*logL0 = \&logLikelihood0;
sub logLikelihood0 {
  return log(likelihood(@_));
}


## $likelihood = likelihood($k,$n,$p)
##   + returns (binomial) likelhood b($k; $n, $p)
##   + underflows for small values of $p
*L = \&likelihood;
sub likelihood {
  my ($k,$n,$p) = @_;
  return pow($p,$k) * pow(1-$p, $n-$k);
}


## $logLikelihoodPdl = logLikelihoodPdl($k,$n,$p)
##   + returns (binomial) log-likelhood log(b($k; $n, $p)) -- see 'likelihoodPdl()' for dimensions
*logLp = \&logLikelihoodPdl;
sub logLikelihoodPdl {
  my ($k,$n,$p) = @_;
  #return $k*log($p) + ($n-$k)*log(1-$p);

  my $k_p1 = $k*$p->log;
  $k_p1->where($k->approx(0)) .= 0;        ##-- assume: 0*log(0)==0

  my $notk    = ($n-$k);
  my $notk_p0 = $notk * (1.0-$p)->log;
  $notk_p0->where($notk->approx(0)) .= 0;  ##-- assume: 0*log(0)==0

  return $k_p1 + $notk_p0;
}

##--> underflow-endangered!
*logLp0 = \&logLikelihoodPdl0;
sub logLikelihoodPdl0 {
  return log(likelihoodPdl(@_));
}

## $likelihoodPdl = likelihoodPdl($kVals, $N, $Pvals)
##   + returns (binomial) likelhood b($k; $n, $p)
##   + $kVals(1,$Nk)
##   + $N(1)
##   + $Pvals($Np)
##   + returned pdl: ($Np, $Nk)
##   + underflow-endangered!
*Lp = \&likelihoodPdl;
sub likelihoodPdl {
  my ($k,$n,$p) = @_;
  return pow($p,$k) * pow(1-$p, $n-$k);
}

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right-MI profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
}


1;
