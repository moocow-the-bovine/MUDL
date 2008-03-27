#-*- Mode: CPerl -*-

## File: MUDL::Cluster::Brown.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: MI hierarchical clustering
##======================================================================

package MUDL::Cluster::Brown;
use PDL;
use MUDL::Cluster::Method;
use Carp;

our @ISA = qw(MUDL::Cluster::Method);

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


##======================================================================
## MI-clusterer: Constructor

## $args = MUDL::Cluster::Brown->new(%args);
##   + %args:
##       bigrams => $mudl_bigrams, ##-- bigram distribution (strings)
##   + optional data:
##       enum     => $enum,    # leaf-id enumerator
##       cenum    => $enum,    # cluster-id enumerator
##   + output data:
##       clusterids=>undef,
##       nclusters=>1,
##   + iteration data:
##       k     => 0,           # iteration number
##       #pk    => $edist_nary, # MUDL::EDist::Nary (class bigrams)
##       #pku   => $edist,      # MUDL::EDist       (class unigrams)
##       pk_l2r => \%hash,     # $pk_l2r->{$l}{$r} = p_k($l,$r)
##       pk_rl2 => \%hash,     # $pk_r2l->{$r}{$l} = p_k($r,$l)
##       plk   => undef,       # left-probabilities:   plk($l) = sum_m (p_k($l,$r))
##       prk   => undef,       # right-probabilities:  prk($r) = sum_m (p_k($l,$r))
##       qk    => undef,       # class code-lengths: qk($l,$r) = pk($l,$r) * log(pk($l,$r)/($plk($l)*$prk($r)))
##       ctree => $pdl_tree,   # as for PDL::Cluster (?)
##       nodids => \%hash,     # cluster-id => $node_id_in_tree
##       clusterids => $pdl,   # cluster-id assingment map
##       enum2    => $enum_nary, # (targets,targets)
##       Ik    => $avg_mi,     # for informational purposes
##       Lk    => $dist_nary,  # L_k(i,j)
##       Ck    => \@active_indices, ## sorted
sub new {
  my ($that,%args) = @_;

  my ($mic);
  $mic = $that->SUPER::new(
			   ##-- object-global data
			   bigrams=>undef,
			   enum=>MUDL::Enum->new(),
			   cenum=>undef,

			   ##-- iteration-dependent data
			   k => 0,
			   pk_l2r => undef,
			   pk_r2l => undef,
			   plk   => undef,
			   prk   => undef,
			   qk    => undef,

			   ##-- output data
			   nclusters=>1,
			   verbose=>$vl_default,
			   ctree=>pdl(long,[]),
			   %args
			  );

  return $mic;
}

##======================================================================
## $mic = $mic->shadow(%args)
##   + return a new tree of same type:
##     ~ no data
##     ~ copied enum, if present
##   + %args are passed to ref($tree)->new();
sub shadow {
  my $m1 = shift;
  return ref($m1)->new(
		       ##-- copied by method
		       (map {
			 (defined($m1->{$_}) ? ($_=>$m1->{$_}->copy) : qw())
		       } qw(enum)),
		       ##-- copied by perl
		       (map {
			 (defined($m1->{$_}) ? ($_=>$m1->{$_}) : qw())
		       } qw(nclusters)),
		       ##-- user args
		       @_
		      );
}

##======================================================================
## Messages

## $mic->vmsg($level, @msg)
sub vmsg {
  my ($mic,$level,@msg) = @_;
  $mic->{verbose} = $vl_default if (!defined($mic->{verbose}));
  return if (!defined($mic->{verbose}) || $mic->{verbose} < $level);
  print STDERR (ref($mic), "[k=$mic->{k}]: ", @msg);
}


##--------------------------------------------------------------
## $mic = $mic->initialize($bigrams)
##  + initialize new clustering for $bigrams
sub initialize {
  my ($mic,$bgd) = @_;

  ##-- stage
  $mic->{k} = 0;
  $mic->vmsg($vl_info, "Initializing...\n");

  ##-- clear clusterids, tree, nodids
  %{$mic->{nodids}} = qw();
  $mic->{clusterids} = pdl(long,[]);
  $mic->{linkdist}   = pdl(double,[]);
  $mic->{ctree}      = pdl(long,[]);

  ##-- enum, p_k
  $mic->vmsg($vl_info, "initialize(): p_k ~ p(w_1,w_2)\n");

  my $enum = $mic->{enum};
  $enum->clear;
  my $enum2 = $mic->{enum2} = MUDL::Enum::Nary->new(nfields=>2,enums=>[$enum,$enum]);

  ##-- first, prune bos,eos
  $bgd->{nz}->prune(which=>sub { $_[0] !~ /\b\Q$bgd->{bos}\E/ && $_[0] !~ /\b\Q$bgd->{eos}\E/ });

  my $pk = $bgd->toEDist($enum2);
  $pk->normalize;

  $mic->vmsg($vl_info, "initialize(): p_k indices (l2r,r2l)\n");
  my $pk_l2r = $mic->{pk_l2r} = {};
  my $pk_r2l = $mic->{pk_r2l} = {};
  my ($w12,$p_w12, $w1,$w2);
  while (($w12,$p_w12)=each(%{$pk->{nz}})) {
    ($w1,$w2) = $pk->split($w12);
    $pk_l2r->{$w1}{$w2} = $p_w12;
    $pk_r2l->{$w2}{$w1} = $p_w12;
  }

  ##-- plk, prk
  $mic->vmsg($vl_info, "initialize(): pl_k , pr_k ~ p(w1) , p(w2)\n");

  my $plk = $mic->{plk} = MUDL::EDist->new(enum=>$enum);
  my $prk = $mic->{prk} = MUDL::EDist->new(enum=>$enum);
  while (($w12,$p_w12)=each(%{$pk->{nz}})) {
    ($w1,$w2)=$pk->split($w12);

    $plk->{nz}{$w1} += $p_w12;
    $prk->{nz}{$w2} += $p_w12;
  }

  ##-- qk
  $mic->vmsg($vl_info, "initialize(): q_k ~ avg(len(code(c1,c2)))\n");

  my $qk = $mic->{qk} = MUDL::EDist::Nary->new(enum=>$enum2);
  my $Ik = 0;
  while (($w12,$p_w12)=each(%{$pk->{nz}})) {
    ($w1,$w2)=$pk->split($w12);

    $Ik += $qk->{nz}{$w12} = $p_w12 * log( $p_w12 / ($plk->{nz}{$w1} * $prk->{nz}{$w2}) );
  }
  $mic->{Ik} = $Ik; ##-- save average MI

  ##-- sk
  $mic->vmsg($vl_info, "initialize(): s_k ~ sum(avg(len(code(c))))\n");
  $sk = $mic->{sk} = MUDL::EDist->new(enum=>$enum);
  my ($q_w12);
  while (($w12,$q_w12)=each(%{$qk->{nz}})) {
    ($w1,$w2) = $qk->split($w12);

    $sk->{nz}{$w1} += $q_w12;
    $sk->{nz}{$w2} += $q_w12 if ($w1 != $w2);
  }

  ##-- report average MI
  $mic->vmsg($vl_info, "I_k(c1;c2) = $Ik\n");

  ##-- bootstrap
  $mic->vmsg($vl_info, "bootstrap()\n");
  $mic->bootstrap();


  return $mic;
}

##--------------------------------------------------------------
## $mic = $mic->bootstrap()
##  + populates $mic->{Lk} over cluster-pairs ($i,$j)
sub bootstrap {
  my $mic = shift;

  ##-- distributions
  my ($pk_l2r,$pk_r2l,$plk,$prk,$qk,$sk,$Ik) = @$mic{qw(pk_l2r pk_r2l plk prk qk sk Ik)};
  my $Lk = $mic->{Lk} = MUDL::EDist::Nary->new(enum=>$qk->{enum});

  ##-- numerous other vars
  my ($plk_ipj, $prk_ipj, $Ik_ij, $qk_ij, $qk_ji);
  my ($l,$r, $pk_lr, $pk_ipj_r, $pk_l_ipj, %pk_ipj_r, %pk_l_ipj);
  my ($pk_ipj_ipj);

  my ($jj,$ii, $j,$i);
  my $Ck = $mic->{Ck} = [0..($mic->{enum}->size-1)];

  foreach $jj (1..$#$Ck) {
    $j = $Ck->[$jj];
    foreach $ii (0..($jj-1)) {
      $i = $Ck->[$ii];
      #print STDERR "---------> ii=$ii, jj=$jj : i=$i ; j=$j\n";
      $plk_ipj = $plk->{nz}{$i} + $plk->{nz}{$j};
      $prk_ipj = $prk->{nz}{$i} + $prk->{nz}{$j};

      $qk_ij   = $qk->{nz}{"$i\t$j"};
      $qk_ji   = $qk->{nz}{"$j\t$i"};

      $qk_ij   = 0 if (!defined($qk_ij));
      $qk_ji   = 0 if (!defined($qk_ji));

      $Ik_ij = $Ik - $sk->{nz}{$i} - $sk->{nz}{$j} + $qk_ij + $qk_ji;

      ##-- get right- and left-contexts p_k(i+j,r) and p_k(l,i+j)
      %pk_ipj_r = %{$pk_l2r->{$i}};
      %pk_l_ipj = %{$pk_r2l->{$i}};

      while (($r,$pk_lr)=each(%{$pk_l2r->{$j}})) { $pk_ipj_r{$r} += $pk_lr; }
      while (($l,$pk_lr)=each(%{$pk_r2l->{$j}})) { $pk_l_ipj{$l} += $pk_lr; }

      $pk_ipj_r{$i} += $pk_ipj_r{$j} if ($pk_ipj_r{$j});
      $pk_l_ipj{$i} += $pk_l_ipj{$j} if ($pk_l_ipj{$j});

      delete($pk_ipj_r{$j});
      delete($pk_l_ipj{$j});

      ##-- compute and add in q_k(i+j,r), q_k(l,i+j)
      while (($r,$pk_lr)=each(%pk_ipj_r)) {
	next if ($r==$i || $r==$j);
	$Ik_ij += $pk_lr * log ( $pk_lr / ($plk_ipj * $prk->{nz}{$r}) )
	  if ($pk_lr);
      }
      while (($l,$pk_lr)=each(%pk_l_ipj)) {
	next if ($l==$i || $l==$j);
	$Ik_ij += $pk_lr * log ( $pk_lr / ($plk->{nz}{$l} * $prk_ipj) )
	  if ($pk_lr);
      }

      ##-- add in qk(i+j,i+j)
      $pk_ipj_ipj = $pk_l_ipj{$i};
      #$pk_ipj_ipj += $pk_l_ipj{$j} if (defined($pk_l_ipj{$j}));
      $Ik_ij += $pk_ipj_ipj * log ( $pk_ipj_ipj / ($plk_ipj * $prk_ipj) )
	if ($pk_ipj_ipj);

      ##-- assign $Lk_ij
      $Lk->{nz}{$i.$Lk->{sep}.$j} = $Ik - $Ik_ij;
    }
  }

  return $mic;
}


##======================================================================
## $mic = $mic->cluster()
## $mic = $mic->cluster($bigrams)
##  + actually do clustering
sub cluster {
  my ($mic,$bg) = @_;
  $bg = $mic->{bigrams} if (!defined($bg));

  ##-- initialize: bigrams, unigrams
  $mic->initialize($bg);

  ##-- iterate
  my $V = $mic->{enum}->size;
  my ($i,$j,$L);
  for (; $mic->{nclusters} < $V-$mic->{k}; ++$mic->{k}) {
    $mic->vmsg($vl_info, "--------------------k=$mic->{k}-----------------------------\n");

    ##-- find best merge candidates
    ($i,$j,$L) = $mic->findBestMerge();

    $mic->vmsg($vl_info,
	       " merge: i=$i (", $mic->{enum}->symbol($i), ")\n",
	       ref($mic), "[k=$mic->{k}]: merge: j=$j (", $mic->{enum}->symbol($j), ")\n",
	       ref($mic), "[k=$mic->{k}]: L_ij    = $L\n",
	       ref($mic), "[k=$mic->{k}]: I_{k+1} = ", ($mi->{Ik}-$L), "\n",
	      );

    ##-- merge 'em
    $mic->mergePair($i,$j);
  }

  return $mic;
}


##--------------------------------------------------------------
## ($i,$j,$Lbest) = $mic->findBestMerge()
##  + populates $mic->{Lk} over cluster-pairs ($i,$j)
sub findBestMerge {
  my $mic = shift;

  my $Lk = $mic->{Lk};
  my $Ck = $mic->{Ck};
  my ($ii,$jj, $i,$j,$L_ij);
  my ($ibest,$jbest,$Lbest);
  foreach $jj (1..$#$Ck) {
    $j = $Ck->[$jj];
    foreach $ii (0..($jj-1)) {
      $i = $Ck->[$ii];
      $L_ij = $Lk->{nz}{$i.$Lk->{sep}.$j};

      ($ibest,$jbest,$Lbest) = ($i,$j,$L_ij) if (!defined($L_best) || $L_ij < $Lbest);
    }
  }

  return ($ibest,$jbest,$Lbest);
}


##--------------------------------------------------------------
## $mic = $mic->mergePair($i,$j,$L_ij)
##  + merges clusters $i and $j
##  + requires: $i < $j
sub mergePair {
  my ($mic,$i,$j) = @_;
  #no warnings qw(uninitialized);

  my $L_ij = $mic->{Lk}{nz}{$i.$mic->{Lk}{sep}.$j};

  ##--------------------------------------------
  ## update: pl_k, pr_k
  $mic->vmsg($vl_info, "mergePair(): compute: pl_{k+1}(i+j), pr_{k+1}(i+j)\n");
  my ($plk,$prk) = @$mic{qw(plk prk)};
  my $plk_ipj = $plk->{nz}{$i} + $plk->{nz}{$j};
  my $prk_ipj = $prk->{nz}{$i} + $prk->{nz}{$j};


  ##--------------------------------------------
  ## update: p_k

  $mic->vmsg($vl_info, "mergePair(): compute: p_{k+1}(i+j)\n");
  my ($pk_l2r,$pk_r2l) = @$mic{qw(pk_l2r pk_r2l)};

  ##-- get right- and left-contexts p_k(i+j,r) and p_k(l,i+j)
  my (%pk_ipj_r, %pk_l_ipj, $r,$l, $pk_lr);

  %pk_ipj_r = %{$pk_l2r->{$i}};
  %pk_l_ipj = %{$pk_r2l->{$i}};

  while (($r,$pk_lr)=each(%{$pk_l2r->{$j}})) { $pk_ipj_r{$r} += $pk_lr; }
  while (($l,$pk_lr)=each(%{$pk_r2l->{$j}})) { $pk_l_ipj{$l} += $pk_lr; }

  $pk_ipj_r{$i} += $pk_ipj_r{$j} if ($pk_ipj_r{$j});
  $pk_l_ipj{$i} += $pk_l_ipj{$j} if ($pk_l_ipj{$j});

  delete($pk_ipj_r{$j});
  delete($pk_l_ipj{$j});


  ##--------------------------------------------
  ## update: q_k
  $mic->vmsg($vl_info, "mergePair(): compute: q_{k+1}(i+j,r) & q_{k+1}(l,i+j)\n");
  my %qk_ipj_r = qw();
  my %qk_l_ipj = qw();
  while (($r,$pk_lr)=each(%pk_ipj_r)) {
    next if ($r==$i || $r==$j);
    $qk_ipj_r{$r} = $pk_lr * log ( $pk_lr / ($plk_ipj * $prk->{nz}{$r}) )
      #if ($pk_lr && $plk_ipj && $prk->{nz}{$r})
      if ($pk_lr)
      ;
  }
  while (($l,$pk_lr)=each(%pk_l_ipj)) {
    next if ($l==$i || $l==$j);
    $qk_l_ipj{$l} = $pk_lr * log ( $pk_lr / ($plk->{nz}{$l} * $prk_ipj) )
      #if ($pk_lr && $plk->{nz}{$l} && $prk_ipj)q
      if ($pk_lr)
      ;
  }
  $qk_l_ipj{$i} = $qk_ipj_r{$i} =
    $pk_l_ipj{$i} * log ( $pk_l_ipj{$i} / ($plk_ipj * $prk_ipj) )
      #if ($pk_l_ipj{$i} && $plk_ipj && $prk_ipj)
      if ($pk_l_ipj{$i})
      ;


  ##--------------------------------------------
  ## update: s_k
  $mic->vmsg($vl_info, "mergePair(): compute: s_{k+1}(l,m)\n");
  my %sk_new = qw();
  my ($sk_l, $q_tmp);
  my ($qk,$sk) = @$mic{qw(qk sk)};
  while (($l,$sk_l)=each(%{$sk->{nz}})) {
    next if ($l==$i || $l==$j);
    $sk_l -= $q_tmp if ($q_tmp=$qk->{nz}{"$l\t$i"});
    $sk_l -= $q_tmp if ($q_tmp=$qk->{nz}{"$i\t$l"});
    $sk_l -= $q_tmp if ($q_tmp=$qk->{nz}{"$l\t$j"});
    $sk_l -= $q_tmp if ($q_tmp=$qk->{nz}{"$j\t$l"});
    $sk_l += $q_tmp if ($q_tmp=$qk_l_ipj{$l});
    $sk_l += $q_tmp if ($q_tmp=$qk_ipj_r{$l});
    $sk_new{$l} = $sk_l;
  }

  ##--------------------------------------------
  ## update: L_k
  $mic->vmsg($vl_info,"mergePair(): update: L_{k+1}(l,m) (inplace)\n");
  my $Lk = $mic->{Lk};
  my ($Lk_lm,$ln, $ll,$rr);
  my ($plk_lpm, $prk_lpm);
  my (%pk_lpm_rr, %pk_ll_lpm, $pk_llrr);
  my ($pk_lpm_i, $pk_i_lpm, $pk_lpm_j, $pk_j_lpm);
  foreach $lm (keys(%{$Lk->{nz}})) {
    ($l,$m) = $Lk->split($lm);

    if    ($l==$j || $m==$j) { delete($Lk->{nz}{$lm}); next; }
    elsif ($l==$i || $m==$i) { next; }

    $Lk_lm = $Lk->{nz}{$lm};

    $plk_lpm = $plk->{nz}{$l} + $plk->{nz}{$m};
    $prk_lpm = $prk->{nz}{$l} + $prk->{nz}{$m};

    ##-- get right- and left-contexts p_k(l+m,rr) and p_k(ll,l+m)
    %pk_lpm_rr = %{$pk_l2r->{$l}};
    %pk_ll_lpm = %{$pk_r2l->{$l}};

    while (($rr,$pk_llrr)=each(%{$pk_l2r->{$m}})) { $pk_lpm_rr{$rr} += $pk_llrr; }
    while (($ll,$pk_llrr)=each(%{$pk_r2l->{$m}})) { $pk_ll_lpm{$ll} += $pk_llrr; }

    $pk_lpm_rr{$l} += $pk_lpm_rr{$m} if ($pk_lpm_rr{$m});
    $pk_ll_lpm{$l} += $pk_ll_lpm{$m} if ($pk_ll_lpm{$m});

    delete($pk_lpm_rr{$m});
    delete($pk_ll_lpm{$m});

    ##-- subtract q_k(l+m,i), q_k(i,l+m)
    $pk_lpm_i = $pk_lpm_rr->{$i};
    $pk_i_lpm = $pk_ll_lpm->{$i};

    $Lk_lm -= $pk_lpm_i * log( $pk_lpm_i / ($plk_lpm * $prk->{nz}{$i}) )
      #if ($pk_lpm_i && $plk_lpm && $prk->{nz}{$i});
      if ($pk_lpm_i);

    $Lk_lm -= $pk_i_lpm * log( $pk_i_lpm / ($plk->{nz}{$i} * $prk_lpm) )
      #if ($pk_i_lpm && $plk->{nz}{$i} && $prk_lpm);
      if ($pk_i_lpm);

    ##-- subtract q_k(l+m,j), q_k(j,l+m)
    $pk_lpm_j = $pk_lpm_rr->{$j};
    $pk_j_lpm = $pk_ll_lpm->{$j};

    $Lk_lm -= $pk_lpm_j * log( $pk_lpm_j / ($plk_lpm * $prk->{nz}{$j}) )
      #if ($pk_lpm_j && $plk_lpm && $prk->{nz}{$j});
      if ($pk_lpm_j);

    $Lk_lm -= $pk_j_lpm * log( $pk_j_lpm / ($plk->{nz}{$j} * $prk_lpm) )
      #if ($pk_j_lpm && $plk->{nz}{$j} && $prk_lpm);
      if ($pk_j_lpm);


    ##-- add in q_{k+1}(l+m,i+j), q_{k+1}(i+j,l+m)
    $pk_lpm_i  = $pk_l_ipj{$l};
    $pk_lpm_i += $pk_l_ipj{$m} if ($pk_l_ipj{$m});
    $Lk_lm += $pk_lpm_i * log( $pk_lpm_i / ($plk_lpm * $prk_ipj) )
      #if ($pk_lpm_i && $plk_lpm && $prk_ipj);
      if ($pk_lpm_i);

    $pk_i_lpm  = $pk_ipj_r{$l};
    $pk_i_lpm += $pk_ipj_r{$m} if ($pk_ipj_r{$m});
    $Lk_lm += $pk_i_lpm * log( $pk_i_lpm / ($plk_ipj * $prk_lpm) )
      #if ($pk_i_lpm && $plk_ipj && $prk_lpm);
      if ($pk_i_lpm);

    ##-- update Lk
    $Lk->{nz}{$lm} = $Lk_lm;
  }

  ##--------------------------------------------
  ## update: cleanup and clobber

  $mic->vmsg($vl_info, "mergePair(): cleanup & clobber\n");

  ##-- cleanup + clobber: plk, prk
  $plk->{nz}{$i} = $plk_ipj;
  $prk->{nz}{$i} = $prk_ipj;
  delete($plk->{nz}{$j});
  delete($prk->{nz}{$j});

  ##-- cleanup: remove p_k($j,r) and p_k(l,$j)
  while (($r,$pk_lr)=each(%pk_ipj_r)) {
    $pk_r2l->{$r}{$i} = $pk_lr;
    delete($pk_r2l->{$r}{$j});
  }
  while (($l,$pk_lr)=each(%pk_l_ipj)) {
    $pk_l2r->{$l}{$i} = $pk_lr;
    delete($pk_l2r->{$l}{$j});
  }

  ##-- clobber: pk(i+j,r), pk(l,i+j)
  %{$pk_l2r->{$i}} = %pk_ipj_r;
  %{$pk_r2l->{$i}} = %pk_l_ipj;
  delete($pk_lr2->{$j});
  delete($pk_r2l->{$j});

  ##-- cleanup & clobber: q_k
  @{$qk->{nz}}{map {($i.$qk->{sep}.$_)} keys(%qk_ipj_r)} = values(%qk_ipj_r);
  @{$qk->{nz}}{map {($_.$qk->{sep}.$i)} keys(%qk_l_ipj)} = values(%qk_l_ipj);
  delete(@{$qk->{nz}}{map {($j.$qk->{sep}.$_)} keys(%qk_ipj_r)});
  delete(@{$qk->{nz}}{map {($_.$qk->{sep}.$j)} keys(%qk_l_ipj)});

  ##-- clobber: s_k
  %{$sk->{nz}} = %sk_new;

  ##-- cleanup: C_k
  @{$mic->{Ck}} = grep { $_ != $j } (@{$mic->{Ck}});

  ##-- clobber: I_k
  $mic->{Ik} -= $L_ij;
  my $Ik = $mic->{Ik};

  ##--------------------------------------------
  ## update: L_k (i+j)

  $mic->vmsg($vl_info, "mergePair(): update: L_{k+1}(i+j,i+m)\n");

  ##-- update: s_k for youngest cluster i+j (i)
  my ($qk_l_ipj, $qk_ipj_r);
  my $sk_i = 0;
  while (($l,$qk_l_ipj)=each(%qk_l_ipj)) {                   $sk_i += $qk_l_ipj;  }
  while (($r,$qk_ipj_r)=each(%qk_ipj_r)) { next if ($r==$i); $sk_i += $qk_ipj_r;  }
  $sk->{nz}{$i} = $sk_i;


  ##-- update: L_k for youngest cluster i+j (i)
  my $Ck = $mic->{Ck};
  my ($Ik_im, $plk_ipm,$prk_ipm, $qk_im, $qk_mi, %pk_ipm_r,%pk_l_ipm, $pk_ipm_ipm);
  foreach $m (@$Ck) {
    next if ($m==$i || $m==$j);

    $plk_ipm = $plk->{nz}{$i} + $plk->{nz}{$m};
    $prk_ipm = $prk->{nz}{$i} + $prk->{nz}{$m};

    $qk_im = $qk->{nz}{"$i\t$m"};
    $qk_mi = $qk->{nz}{"$m\t$i"};

    $qk_im = 0 if (!defined($qk_im));
    $qk_mi = 0 if (!defined($qk_mi));

    $Ik_im = $Ik - $sk->{nz}{$i} - $sk->{nz}{$m} + $qk_im + $qk_mi;

    ##-- get right- and left-contexts p_{k+1}(i+m,r) and p_{k+1}(l,i+m)
    %pk_ipm_r = %{$pk_l2r->{$i}};
    %pk_l_ipm = %{$pk_r2l->{$i}};

    while (($r,$pk_lr)=each(%{$pk_l2r->{$m}})) { $pk_ipm_r{$r} += $pk_lr; }
    while (($l,$pk_lr)=each(%{$pk_r2l->{$m}})) { $pk_l_ipm{$l} += $pk_lr; }

    $pk_ipm_r{$i} += $pk_ipm_r{$m} if ($pk_ipm_r{$m});
    $pk_l_ipm{$i} += $pk_l_ipm{$m} if ($pk_l_ipm{$m});

    delete($pk_ipm_r{$m});
    delete($pk_l_ipm{$m});


    ##-- compute and add in: q_{k+1}(i+m,r), q_{k+1}(l,i+m)
    while (($r,$pk_lr)=each(%pk_ipm_r)) {
      next if ($r==$i || $r==$m);
      $Ik_im += $pk_lr * log ( $pk_lr / ($plk_ipm * $prk->{nz}{$r}) )
	#if ($pk_lr && $plk_ipm && $prk->{nz}{$r});
	if ($pk_lr);
    }
    while (($l,$pk_lr)=each(%pk_l_ipm)) {
      next if ($l==$i || $l==$m);
      $Ik_im += $pk_lr * log ( $pk_lr / ($plk->{nz}{$l} * $prk_ipm) )
	#if ($pk_lr && $prk_ipm && $plk->{nz}{$l});
	if ($pk_lr);
    }

    ##-- compute and add in: q_{k+1}(i+m,i+m)
    $pk_ipm_ipm  = $pk_l_ipm{$i};
    $pk_ipm_ipm += $pk_l_ipm{$m} if (defined($pk_l_ipm{$m}));
    $Ik_im += $pk_ipm_ipm * log ( $pk_ipm_ipm / ($plk_ipm * $prk_ipm) )
      if ($pk_ipm_ipm); # && $plk_ipm && $prk_ipm

    ##-- assign $Lk_ij
    $Lk->{nz}{$m < $i ? ($m.$Lk->{sep}.$i) : ($i.$Lk->{sep}.$m)}
      = $Ik - $Ik_im;
  }

  ##--------------------------------------------
  ## maintain tree
  my ($nodid_i,$nodid_j) = @{$mic->{nodids}}{$i,$j};
  $nodid_i = $i if (!defined($nodid_i));
  $nodid_j = $j if (!defined($nodid_j));
  if (!$mic->{ctree}->isempty) {
    $mic->{ctree}->reshape(2,$mic->{ctree}->dim(1)+1);
    $mic->{ctree}->slice(",-1") .= pdl(long, [$nodid_i, $nodid_j]);
    $mic->{linkdist} = pdl(double, [$L_ij]);
  } else {
    $mic->{ctree} = pdl(long, [$nodid_i, $nodid_j]);
    $mic->{linkdist}->reshape($mic->{linkdist}->dim(0)+1);
    $mic->{linkdist}->set(-1, $L_ij);
  }
  $mic->{nodids}{$i} = -$mic->{k}-1;


  return $mic;
}

########################################################################
## Cut
########################################################################
sub cut {
  my ($mic,$nclusters) = @_;
  #$mic->{nclusters} = $nclusters if (defined($nclusters)); ##-- no!

  ##-- add dummy element(s) to end of tree
  $mic->{ctree}->reshape(2,$mic->{enum}->size);

  if (!defined($mic->{clusterids}) || $mic->{clusterids}->dim(0) != $mic->{ctree}->dim(1)) {
    $mic->{clusterids} = zeroes(long, $mic->{ctree}->dim(1));
  }

  require PDL::Cluster;
  PDL::Cluster::cuttree($mic->{ctree}, $nclusters, $mic->{clusterids});

  return $mic->{clusterids};
}

########################################################################
## Conversion
########################################################################

## (inherited: probably broken)


########################################################################
## Viewing
########################################################################

##======================================================================
## $tree = $tc->toTree(%args)
##  + returns a MUDL::Tree representing the clusters
##  + %args are passed to MUDL::Tree->fromClusters()
##  + INCOMPLETE (need clusterids) (?)
sub toTree {
  my $mic = shift;
  require MUDL::Tree;
  return MUDL::Tree->fromClusterPDL($mic->{ctree},
				    enum=>$mic->{enum},
				    dists=>$mic->{linkdist},
				    groups=>$mic->{clusterids},
				    #dmult=>100,
				    @_);
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
