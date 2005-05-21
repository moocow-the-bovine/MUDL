#-*- Mode: Perl -*-

## File: MUDL::Cluster::Method.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: clustering methods
##======================================================================

package MUDL::Cluster::Method;
use PDL;
use PDL::Cluster;
use MUDL::Object;
use Carp;

our @ISA = qw(MUDL::Object);
our @EXPORT_OK = qw();

##======================================================================
## Hierartchical clustering: Constructor

## $cm = MUDL::Cluster::Tree->new(%args);
##   + %args:
##       data     => $data,    # pdl($d,$n)
##       nclusters=> $k,       # number of desired clusters
##   + optional data:
##       enum     => $enum,    # leaf-id enumerator
##       cenum    => $enum,    # cluster-id enumerator
##   + additional data:
##     - post-cluster():
##         #tree     => $ctree,   # pdl(2,$n) gives structure of clustering tree (see below) [(2,n-1) used]
##         #linkdist => $vector,  # pdl($n) array giving distances between sister nodes [(n-1) used]
##     - post-cut($k):
##         nclusters  => $k,                # number of clusters
##         clusterids => $rowid2clusterid,  # pdl($n) maps data rows to cluster-id (range 0..($k-1))
##     - post-leafdistances():
##         leafdist   => $leaf2cid2dist, # pdl($k,$n) maps (clusterid,leaf) to distance(leaf,clusterid)
##   + where:
##       $n : number of data instances (rows)
##       $d : number of features per datum (columns)
##   + methods, metrics, structures: see PDL::Cluster and cluster-3.0 documentation
sub new {
  my $cm = $_[0]->SUPER::new(
			     data=>null,
			     ##-- output data
			     clusterids=>undef,
			     nclusters=>2,
			     @_[1..$#_]
			    );

  return $cm;
}


##======================================================================
## $data = $cm->data()
## $data = $cm->data($data)
##   + get/set data -- reset related pdls on set
sub data {
  my $cm = shift;
  return $cm->{data} if (!@_);

  my $data = $cm->{data} = shift;

  ##-- paranoia
  delete(@$cm{qw(mask weight ctree linkdist leafdist clusterids)});

  return $data;
}


##======================================================================
## $cm = $cm->cluster(%args)
##  + actually runs clustering algorithm
*cluster = MUDL::Object::dummy('cluster');


##======================================================================
## $clusterids = $cm->cut()
## $clusterids = $cm->cut($nclusters)
##   + cut tree, returns vector clusterids($n)
*cut = MUDL::Object::dummy('cut');


##======================================================================
## $pdl = $cm->leafdistances()
## $pdl = $cm->leafdistances($pdl)
##   + populates returns a $k-by-$n pdl representing distances
##     between each (cluster,leaf) pair.
*leafdistances = MUDL::Object::dummy('leafdistances');


########################################################################
## Conversion
########################################################################

##======================================================================
## ($leafEnum,$clusterEnum) = $cm->toEnums(%args)
##  + returns enums representing the clustering solution
sub toEnums {
  require MUDL::Enum;
  my ($cm,%args) = @_;
  return ($cm->leafEnum(%args), $cm->clusterEnum(%args));
}

## $leafEnum = $cm->leafEnum()
##  + returns enums representing the leaves
sub leafEnum {
  require MUDL::Enum;
  my ($cm,%args) = @_;
  #my $n = $cm->{data}->dim(1);
  my $n = defined($cm->{data}) ? $cm->{data}->dim(1) : $cm->{clusterids}->dim(0);

  ##-- generate enums
  my $lenum = $cm->{enum};
  if (!defined($lenum)) {
    $lenum = MUDL::Enum->new(%args);
    $lenum->addIndexedSymbol('x'.$_, $_) foreach (0..($n-1));
  }

  return $lenum;
}

## $clusterEnum = $cm->clusterEnum()
##  + returns enums representing the clusters
sub clusterEnum {
  require MUDL::Enum;
  my ($cm,%args) = @_;
  my $k = $cm->{nclusters};

  ##-- generate enums
  my $cenum = $cm->{cenum};
  if (!defined($cenum)) {
    $cenum = MUDL::Enum->new(%args);
    $cenum->addIndexedSymbol('c'.$_, $_) foreach (0..($k-1));
  }

  return $cenum;
}

## $clusterEnum = $cm->clusterEnumFull()
##  + returns enum representing the clusters
sub clusterEnumFull {
  require MUDL::Enum;
  my ($cm,%args) = @_;
  my $k = $cm->{nclusters};
  my $n = $cm->{data}->dim(1);

  ##-- generate enum
  my $lenum = $cm->leafEnum(%args);
  my ($cenum,$cid);
  if (!defined($cenum=$cm->{cenum})) {
    $cenum = MUDL::Enum->new(%args);

    ##-- add cluster ids
    foreach (0..($k-1)) {
      $cenum->{id2sym}[$_] = 'c'.$_.':';
    }

    ##-- add all old leaf labels
    foreach (0..($n-1)) {
      $cenum->{id2sym}[$cm->{clusterids}->at($_)] .= '_'.$lenum->symbol($_);
    }

    ##-- build reverse index
    foreach (0..$#{$cenum->{id2sym}}) {
      $cenum->{sym2id}{$cenum->{id2sym}[$_]} = $_;
    }
  }

  return $cenum;
}


##======================================================================
## Conversion: distance-to-probability (new)
##======================================================================

## %d2pMethods = ($name => \&sub, ...)
##   + methods such that &sub($leafdistance_pdl,%args) = $prob_pdl
##   + $leafdistance_pdl : $k by $n
##   + $prob_pdl         : $k by $n
our %d2pMethods =
  (
   nbest_inverse=> \&d2p_nbest_inverse,
   nbest_gath   => \&d2p_nbest_gath,
   nbest_pinskerL1=> \&d2p_nbest_pinskerL1,
   nbest_pinskerD=> \&d2p_nbest_pinskerD,
   nbest_linear => \&d2p_nbest_linear,
   nbest_base   => \&d2p_nbest_base,
   nbest_hughes => \&d2p_nbest_hughes,
   linear       => \&d2p_linear,
   inverse      => \&d2p_inverse,
   pinskerL     => \&d2p_pinskerL1,
   pinskerD     => \&d2p_pinskerD,
   gath         => \&d2p_gath,
   #DEFAULT      => \&d2p_nbest_base,
   DEFAULT      => \&d2p_nbest_inverse,
  );

## $probPdl = $cm->membershipProbPdl(%args)
##   + %args:
##       pdl      => $probPdl,
##       leafdist => $leafdistance_pdl,  [default=$cm->{leafdist}]
##       method   => $dist2probMethod,
##   + $dist2probMethod is a key %d2pMethods
##   + $leafdistance_pdl : $k by $n
##   + $prob_pdl         : $k by $n
##     - $prob_pdl->at($ki,$ni) ~= p($class_ki | $target_ni)
*membershipProbs = \&membershipProbPdl;
sub membershipProbPdl {
  my ($cm,%args) = @_;
  require PDL;

  my $ld = $args{leafdist};
  $ld = $cm->{leafdist}      if (!defined($ld));
  $ld = $cm->leafdistances() if (!defined($ld));

  my $method = $args{method} ? $args{method} : 'DEFAULT';
  my $d2psub = $cm->can($method);

  croak(ref($cm), "::membershipProbs(): unknown conversion method '$method'!")
    if (!defined($d2psub) && !defined($d2psub=$d2pMethods{$method}));

  return $d2psub->($cm,$ld,%args);
}

##----------------------------------------------------------------------
## Conversion: distance-to-probability: full
##----------------------------------------------------------------------

## $pdl = $cm->d2p_getPdl($leafdists,$args_pdl)
##  + %args:
##     pdl => $probPdl,
##  + returns a pdl at least as large as $leafdists
sub d2p_getPdl {
  my ($cm,$ld,$pdl) = @_;
  return zeroes($ld->dims) if (!defined($pdl));

  $pdl->reshape($ld->dims)
    if ($pdl->ndims != 2 || $pdl->dim(0) < $ld->dim(0) || $pdl->dim(1) < $ld->dim(1));

  return $pdl;
}

## $pdl_slice = $cm->d2p_slicePdl($leafdists,$pdl)
sub d2p_slicePdl {
  my ($cm,$ld,$pdl) = @_;
  return $pdl->slice("0:".($ld->dim(0)-1).",0:".($ld->dim(1)-1));
}

## $probPdl = $cm->d2p_linear($leafdists,%args)
##  + %args:
##     pdl => $probPdl,
sub d2p_linear {
  my ($cm,$ld,%args) = @_;
  my $pdl   = $cm->d2p_getPdl($ld,$args{pdl});
  my $pdls  = $cm->d2p_slicePdl($ld,$pdl);
  $pdls    .= $ld->max - $ld + $ld->where($ld!=0)->min; # (+1?) ??? (+ $ld->where($ld!=0)->min ???);
  $pdls    /= $pdls->sumover->transpose;
  return $pdl;
}

## $probPdl = $cm->d2p_inverse($leafdists,%args)
##  + %args:
##     pdl => $probPdl,
sub d2p_inverse {
  my ($cm,$ld,%args) = @_;
  my $pdl    = $cm->d2p_getPdl($ld,$args{pdl});
  my $pdls   = $cm->d2p_slicePdl($ld,$pdl);
  my $ldmin  = $ld->where($ld!=0)->min;

  $pdls     .= $ldmin / ($ldmin + $ld);
  $pdls     /= $pdls->sumover->transpose;

  return $pdl;
}

## $probPdl = $cm->d2p_pinskerL1($leafdists,%args)
##  + %args:
##     pdl => $probPdl,
##     b   => $base,       ##-- default=2
##     beta => $beta_pdl,  ##-- pdl ($n): exp coefficients (sample sizes by target index)
##  + theoretically motivated for L1 distance between probability distributions
sub d2p_pinskerL1 {
  my ($cm,$ld,%args) = @_;
  my $pdl    = $cm->d2p_getPdl($ld,$args{pdl});
  my $pdls   = $cm->d2p_slicePdl($ld,$pdl);
  my $beta   = defined($args{beta}) ? $args{beta} : ones($pdl->type, $pdls->dim(1));
  my $b      = defined($args{b}) ? $args{b} : 2;


  ##-- compute P(c|w) = b^( -beta/2 * dist(c,w)^2 )
  ##   + for L1 distance, this is Pinsker's inequality
  $pdls .= $ld * $cm->{data}->dim(0); ##-- factor out contribution of *weights*...
  $pdls->inplace->pow(2);
  $pdls *= (-$beta/2)->dummy(1)->xchg(0,1);
  PDL::pow($b, $pdls, $pdls);

  $pdls /= $pdls->xchg(0,1)->sumover;

  return $pdl;
}

## $probPdl = $cm->d2p_pinskerD($leafdists,%args)
##  + %args:
##     pdl => $probPdl,
##     b   => $base,       ##-- UNUSED: always exp(1)
##     beta => $beta_pdl,  ##-- pdl ($n): exp coefficients (sample sizes by target index)
##  + theoretically motivated for D(empirical||source) distance between probability distributions
sub d2p_pinskerD {
  my ($cm,$ld,%args) = @_;
  my $pdl    = $cm->d2p_getPdl($ld,$args{pdl});
  my $pdls   = $cm->d2p_slicePdl($ld,$pdl);
  my $beta   = defined($args{beta}) ? $args{beta} : ones($pdl->type, $pdls->dim(1));
  my $b      = exp(1);

  ##-- compute P(c|w) = b^( -beta * dist(c,w) )
  ##   + for dist(w,c)=D(w||c), this is Pinsker's inequality
  #$pdls .= $ld * $cm->{data}->dim(0); ##-- factor out contribution of *weights*...(gone for D())
  $pdls .= $ld;
  $pdls *= (-$beta)->dummy(1)->xchg(0,1);
  PDL::pow($b, $pdls, $pdls);

  $pdls /= $pdls->xchg(0,1)->sumover;

  return $pdl;
}


## $probPdl = $cm->d2p_gath($leafdists,%args)
##  + %args:
##     pdl => $probPdl,
##     q   => $fuzziness_control_parameter,   ## > 1 (2 ==> nbest_inverse) [alt='b']
##     pow => $bool,                          ## square distance [default=true]?
##  + theoretically motivated for L1 distance between probability distributions
sub d2p_gath {
  my ($cm,$ld,%args) = @_;
  my $pdl  = $cm->d2p_getPdl($ld,$args{pdl});
  my $pdls = $cm->d2p_slicePdl($ld,$pdl);
  my $q    = defined($args{'q'}) ? $args{'q'} : (defined ($args{b}) ? $args{b} : 4);
  my $pow  = defined($args{pow}) ? $args{pow} : 2;

  $pdls .= abs(1/$ld->pow($pow))->pow(1/$q-1);
  $pdls /= $pdls->xchg(0,1)->sumover;

  return $pdl;
}


##----------------------------------------------------------------------
## Conversion: distance-to-probability: n-best
##----------------------------------------------------------------------

##------------------------------------------------------
## $probPdl = $cm->d2p_nbest_inverse($leafdists,%args)
##  + %args:
##    n => $nbest,
##    pdl => $probPdl,
sub d2p_nbest_inverse {
  my ($cm,$ld,%args) = @_;
  my $n = $args{n} || 1;

  my $pdl   = $cm->d2p_getPdl($ld,$args{pdl});
  my $ipdl  = zeroes(long, $n);
  my $ldmin = $ld->where($ld!=0)->min;

  #my $pdls   = $cm->d2p_slicePdl($ld,$pdl);
  my ($lds,$ipsum,$pdl_ni);

  foreach $ni (0..($ld->dim(1)-1)) {
    $lds = $ld->slice(",($ni)");
    $lds->minimum_n_ind($ipdl);

    ##-- ?? slow!
    #$ipsum = sum(pdl($ldmin) / ($lds->dice($ipdl) + $ldmin));
    #foreach $ki (0..($n-1)) {
    #  $pdl->set($ipdl->at($ki), $ni, $ldmin / ($lds->at($ipdl->at($ki)) + $ldmin))
    #}
    #$pdl->slice(",($ni)") /= $pdl->slice(",($ni)")->sum;

    ##-- ?? not quite so slow
    $pdl_ni = $pdl->slice(",($ni)");
    $pdl_ni->dice($ipdl) .= $ldmin / ($lds->dice($ipdl) + $ldmin);
    $pdl_ni /= $pdl_ni->sum;
  }

  return $pdl;
}

## $probPdl = $cm->d2p_nbest_gath($leafdists,%args)
##  + %args:
##    n => $nbest,
##    q   => $fuzziness_control_parameter,   ## > 1 (2 ==> nbest_inverse) [alt='b']
##    pow => $bool,                          ## square distance [default=true]?
##    pdl => $probPdl,
sub d2p_nbest_gath {
  my ($cm,$ld,%args) = @_;
  my $n = $args{n} || 1;

  my $pdl  = $cm->d2p_getPdl($ld,$args{pdl});
  my $ipdl = zeroes(long, $n);
  my $q    = defined($args{'q'}) ? $args{'q'} : (defined ($args{b}) ? $args{b} : 4);
  my $pow  = defined($args{pow}) ? $args{pow} : 2;

  my ($lds, $pdl_ni, $lds_ni);

  foreach $ni (0..($ld->dim(1)-1)) {
    $lds = $ld->slice(",($ni)");
    $lds->minimum_n_ind($ipdl);

    $lds_ni = $lds->dice($ipdl);
    $pdl_ni = $pdl->slice(",($ni)")->dice($ipdl);

    $pdl_ni  .= abs(1/$lds_ni->pow($pow))->pow(1/$q-1);
    $pdl_ni /= $pdl_ni->sum;
  }

  return $pdl;
}

## $probPdl = $cm->d2p_nbest_pinskerL1($leafdists,%args)
##  + %args:
##     pdl => $probPdl,
##     b   => $base,       ##-- default=2
##     beta => $beta_pdl,  ##-- pdl ($n): exp coefficients (sample sizes by target index)
##  + theoretically motivated for L1 distance between probability distributions
sub d2p_nbest_pinskerL1 {
  my ($cm,$ld,%args) = @_;

  my $n    = $args{n} || 1;
  my $pdl  = $cm->d2p_getPdl($ld,$args{pdl});
  $pdl .= 0;
  my $ipdl = zeroes(long, $n);

  my $beta = defined($args{beta}) ? $args{beta} : ones($pdl->type, $ld->dim(1));
  my $b    = defined($args{b})    ? $args{b}    : 2;

  my ($lds,$pdl_ni, $lds_ni);
  foreach $ni (0..($ld->dim(1)-1)) {
    ##-- get indices
    $lds = $ld->slice(",($ni)");
    $lds->minimum_n_ind($ipdl);

    $lds_ni = $lds->dice($ipdl);
    $pdl_ni = $pdl->slice(",($ni)")->dice($ipdl);

    $lds_ni->pow(2,$pdl_ni);
    $pdl_ni *= -$beta->slice("($ni)")/2;
    PDL::pow($b, $pdl_ni, $pdl_ni);

    $pdl_ni /= $pdl_ni->sum;
  }

  return $pdl;
}

## $probPdl = $cm->d2p_nbest_pinskerD($leafdists,%args)
##  + %args:
##     pdl => $probPdl,
##     b   => $base,       ##-- UNUSED: default=exp(1)
##     beta => $beta_pdl,  ##-- pdl ($n): exp coefficients (sample sizes by target index)
##  + theoretically motivated for KL-divergence between source- and empirical probability distributions
sub d2p_nbest_pinskerD {
  my ($cm,$ld,%args) = @_;

  my $n    = $args{n} || 1;
  my $pdl  = $cm->d2p_getPdl($ld,$args{pdl});
  $pdl .= 0;
  my $ipdl = zeroes(long, $n);

  my $beta = defined($args{beta}) ? $args{beta} : ones($pdl->type, $ld->dim(1));
  my $b    = exp(1);

  my ($lds,$pdl_ni, $lds_ni);
  foreach $ni (0..($ld->dim(1)-1)) {
    ##-- get indices
    $lds = $ld->slice(",($ni)");
    $lds->minimum_n_ind($ipdl);

    $lds_ni = $lds->dice($ipdl);
    $pdl_ni = $pdl->slice(",($ni)")->dice($ipdl);

    ##-- compute P(c|w) = b^( -beta * dist(c,w) )
    ##   + for dist(w,c)=D(w||c), this is Pinsker's inequality
    $pdl_ni .= $lds_ni;
    $pdl_ni *= -$beta->slice("($ni)");
    PDL::pow($b, $pdl_ni, $pdl_ni);

    $pdl_ni /= $pdl_ni->sum;
  }

  return $pdl;
}



##------------------------------------------------------
## $probPdl = $cm->d2p_nbest_linear($leafdists,%args)
##  + %args:
##    n => $nbest,
##    pdl => $probPdl,
sub d2p_nbest_linear {
  my ($cm,$ld,%args) = @_;
  my $n = $args{n} || 1;

  my $pdl   = $cm->d2p_getPdl($ld,$args{pdl});
  my $ipdl  = zeroes(long, $n);
  my $ldmin = $ld->where($ld!=0)->min;
  my $ldmax = $ld->max;

  #my $pdls   = $cm->d2p_slicePdl($ld,$pdl);
  my ($lds,$ipsum,$pdl_ni);

  foreach $ni (0..($ld->dim(1)-1)) {
    $lds = $ld->slice(",($ni)");
    $lds->minimum_n_ind($ipdl);

    ##-- ?? not quite so slow
    $pdl_ni = $pdl->slice(",($ni)");
    $pdl_ni->dice($ipdl) .= $ldmin + $ldmax - $lds->dice($ipdl);
    $pdl_ni /= $pdl_ni->sum;
  }

  return $pdl;
}

##------------------------------------------------------
## $probPdl = $cm->d2p_nbest_hughes($leafdists,%args)
##  + %args:
##    n => $nbest,
##    pdl => $probPdl,
sub d2p_nbest_hughes {
  my ($cm,$ld,%args) = @_;
  my $n = $args{n} || 1;

  my $pdl   = $cm->d2p_getPdl($ld,$args{pdl});
  my $ipsum = (sequence($n)+1)->pow(-1)->sum;
  my $ipdl  = sequence(long, $n);
  foreach $ni (0..($ld->dim(1)-1)) {
    $ld->slice(",($ni)")->minimum_n_ind($ipdl);
    foreach $ki (0..($n-1)) {
      $pdl->set($ipdl->at($ki), $ni, 1/($ki+1)/$ipsum );
    }
  }

  return $pdl;
}

##------------------------------------------------------
## $probPdl = $cm->d2p_nbest_base($leafdists,%args)
##  + %args:
##     n => $nbest, # =1
##     b => $base   # =2
##    pdl => $probPdl,
sub d2p_nbest_base {
  my ($cm,$ld,%args) = @_;
  my $n = $args{n} || 1;
  my $b = $args{b} || 2;

  my $pdl   = $cm->d2p_getPdl($ld,$args{pdl});
  my $ipsum = sum(pow(pdl($b), -(sequence($n)+1)));
  my $ipdl  = sequence(long, $n);
  foreach $ni (0..($ld->dim(1)-1)) {
    $ld->slice(",($ni)")->minimum_n_ind($ipdl);
    foreach $ki (0..($n-1)) {
      $pdl->set($ipdl->at($ki), $ni, $b**-($ki+1)/$ipsum );
    }
  }

  return $pdl;
}

##======================================================================
## Conversion: distance-to-probability (old)
##======================================================================

##======================================================================
## $pdist = $cm->toJointPdlDist(%args)
##  + returns a MUDL::PdlDist representing the clusters
##  + returned dist has dimensions ($d,$n): $pdist->at($cid,$wid) = p($cid,$wid)
##  + %args are passed to MUDL::PdlDist->new()
*toPdlDist = \&toJointPdlDist;
sub toJointPdlDist {
  my $cm = shift;
  require MUDL::PdlDist;

  my ($k,$n) = ($cm->{nclusters}, $cm->{data}->dim(1));
  my ($lenum,$cenum) = $cm->toEnums();

  ##-- get (cluster,leaf) distance matrix
  my ($ld);
  $ld    = $cm->leafdistances() if (!defined($ld=$cm->{leafdist}));

  ##-- gnerate PdlDist
  my $pd  = $ld->max - $ld; #(+1 ?) (+ $ld->where($ld!=0)->min ???);
  $pd    /= $pd->sum;

  return MUDL::PdlDist->new(pdl=>$pd,
			    enum=>MUDL::Enum::Nary->new(enums=>[$cenum,$lenum]));
}

##======================================================================
## $edist = $cm->toJointEDist(%args)
##  + returns a MUDL::EDist::Nary representing the clusters
##  + returned dist has entries of the form "${target}${sep}${cluster}"=>f($target,$cluster)
##  + %args are passed to MUDL::EDist::Nary->new()
##  + returned dist structure:
##      ($datum_id, $clusterid) = $edist->split($event)
sub toJointEDist {
  my $cm = shift;
  require MUDL::Dist::Nary;

  my ($k,$n) = ($cm->{nclusters}, $cm->{data}->dim(1));
  my ($lenum,$cenum) = $cm->toEnums();

  ##-- get (cluster,leaf) distance matrix
  my ($ld,$ldmax);
  $ld    = $cm->leafdistances() if (!defined($ld=$cm->{leafdist}));
  $ldmax = $ld->max;

  ##-- gnerate edist
  my $edist = MUDL::EDist::Nary->new(enum=>MUDL::Enum::Nary->new(enums=>[$lenum,$cenum],nfields=>2),
				     nfields=>2,
				     @_);
  foreach $eid (0..($n-1)) {
    foreach $cid (0..($k-1)) {
      $edist->{nz}{$eid.$edist->{sep}.$cid} = $ldmax - $ld->at($cid,$eid)
	if ($ldmax - $ld->at($cid,$eid) > 0);
    }
  }

  return $edist;
}


##======================================================================
## $edist = $cm->toConditionalEDist(%args)
## $edist = $cm->toEDist(%args)
##  + returns a MUDL::EDist::Nary representing the clusters
##  + returned dist has entries of the form "${target}${sep}${cluster}"=>p($cluster|$target)
##  + %args are passed to MUDL::EDist::Nary->new()
##  + returned dist structure:
##      ($datum_id, $clusterid) = $edist->split($event)
*toEDist = \&toConditionalEDist;
sub toConditionalEDist {
  my $cm = shift;
  my $ed = $cm->toJointEDist(@_);
  return $ed->conditionalize([1]);
}


##======================================================================
## $lex = $cm->toLex(%args)
##  + returns a MUDL::Lex representing the clusters
##  + %args are passed to MUDL::EDist::Nary->new()
sub toLex {
  my $cm = shift;
  return $cm->toJointEDist(@_)->toDist->toLex();
}

##======================================================================
## $lex = $cm->toSupLex(%args)
##  + returns a MUDL::SupLex representing the clusters
##  + %args are passed to MUDL::Corpus::Profile::SupLex->new()
sub toSupLex {
  require MUDL::Corpus::Profile::SupLex;
  my $cm = shift;
  return MUDL::Corpus::Profile::SupLex->new(nz=>$cm->toJointEDist(@_)->{nz},
					    @_);
}

##======================================================================
## $map = $cm->toMap(%args)
##  + returns a MUDL::Map representing the clustering solution
##  + %args are passed to MUDL::Map->new()
sub toMap {
  require MUDL::Map;
  my $cm = shift;

  #my ($k,$n) = ($cm->{nclusters}, $cm->{data}->dim(1));
  my ($k,$n) = ($cm->{nclusters}, $cm->{clusterids}->dim(0));
  my ($lenum,$cenum) = $cm->toEnums();
  my $cids = $cm->{clusterids};

  ##-- generate map
  my $map = MUDL::Map->new(@_);
  foreach $i (0..($n-1)) {
    $map->{$lenum->symbol($i)} = $cenum->symbol($cids->at($i));
  }

  return $map;
}

##======================================================================
## $map = $cm->toMetaMap(%args)
##  + returns a MUDL::Map representing the clustering solution,
##    retaining all leaf labels on each cluster label
##  + %args are passed to MUDL::Map->new()
sub toMetaMap {
  require MUDL::Map;
  my $cm = shift;

  #my ($k,$n) = ($cm->{nclusters}, $cm->{data}->dim(1));
  my ($k,$n) = ($cm->{nclusters}, $cm->{clusterids}->dim(0));
  my $lenum  = $cm->leafEnum;
  my $cenum  = $cm->clusterEnumFull;
  my $cids   = $cm->{clusterids};

  ##-- generate map
  my $map = MUDL::Map->new(@_);
  foreach $i (0..($n-1)) {
    $map->{$lenum->symbol($i)} = $cenum->symbol($cids->at($i));
  }

  return $map;
}



########################################################################
## Viewing
########################################################################

##======================================================================
## $tree = $cm->toTree(%args)
##  + returns a MUDL::Tree representing the clusters
##  + %args are passed to MUDL::Tree->fromClusters()
*toTree = MUDL::Object::dummy('toTree');

##======================================================================
## $tree = $cm->viewTree(%args)
##  + view a tree
##  + %args are passed to MUDL::Tree->toTree(), fromClusters()
sub viewTree {
  my $cm = shift;
  my $t = $cm->toTree(@_)->view(@_);
  return $t;
}


##======================================================================
## $dg = $cm->toDendogram(%args)
##  + get a dendogram of the clustering results
##  + %args are passed to MUDL::Tk::Dendogram->new()
sub toDendogram {
  my $cm = shift;
  require MUDL::Tk::Dendogram;
  return $cm->toTree(@_)->toDendogram(@_);
}

##======================================================================
## undef = $cm->view(%args)
##  + view a dendogram of the clustering results
##  + %args are passed to MUDL::Tk::Dendogram->new()
sub view {
  $_[0]->toDendogram(@_[1..$#_])->view;
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
