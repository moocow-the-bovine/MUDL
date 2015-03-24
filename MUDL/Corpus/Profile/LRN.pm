#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRN.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: N-ary L-R profiles
##  + probably BROKEN
##======================================================================

package MUDL::Corpus::Profile::LRN;
use MUDL::Corpus::Profile::LR;
use MUDL::Dist::Nary;
use MUDL::EDist;
use MUDL::Object;
use PDL;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile::LR);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       left => [$left_edist_1, ..., $left_edist_${ndists}],
##       right=> [$right_edist_1, ..., $right_edist_${ndists}],
##       donorm => $do_normalize,
##       ###nfields => $number_of_bound_fields_per_dist, # ($nbf)
##       ndists => $ndists,
##  + event structure:
##       $lr->{$dir \in qw(left right)} : ($target_id, @nbf_bounds) = $lr->{dir}->split($event);
sub new {
  my ($that,%args) = @_;

  my $bounds = $args{bounds} || MUDL::Enum->new();
  my $targets = $args{targets} || MUDL::Enum->new();
  my $nfields = 1;
  my $ndists = $args{ndists} || 1;
  my $enum = $args{enum} || MUDL::Enum::Nary->new(enums=>[$targets,(map { $bounds } (0..1))]);
  delete($args{qw(bounds targets nfields ndists enum)});

  my $self = $that->SUPER::new
    (eos=>'__$',
     bos=>'__$',
     targets=>$targets,
     bounds=>$bounds,
     nfields=>$nfields,
     ndists=>$ndists,
     enum=>$enum,
     left=>[map { MUDL::EDist::Nary->new(nfields=>($nfields+1), enum=>$enum) } (1..$ndists)],
     right=>[map { MUDL::EDist::Nary->new(nfields=>($nfields+1), enum=>$enum) } (1..$ndists)],
     donorm=>1, ##-- normalize on pdl-ization ?
     %args);

  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
##-- not implemented here!


##======================================================================
## Conversion: to independent PDL

## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl)
##   + converts to pdl
##   + returned pdl is of dimensions: ($d,$n), where:
##     - $n == number-of-targets
##     - $d == 2 * number-of-dists * number-of-bounds
*toPDLi = \&toPDL;
sub toPDL {
  my ($lr,$pdl) = @_;

  ##-- enum
  my $ndists   = $lr->{ndists};
  my ($eb,$et) = @$lr{qw(bounds targets)};
  my $net      = $et->size;
  my $neb      = $eb->size;

  ##-- pdl
  $pdl = zeroes(double,1) if (!defined($pdl));
  $pdl->reshape($neb, $ndists, 2, $net);  ##-- ($bound_idx, $dist_idx, $l_or_r, $target) = $value
  $pdl .= 0;

  ##-- data
  my ($k,$v,$tid,$bid);
  foreach $di (0..($ndists-1)) {
    ##-- data: left context
    while (($k,$v)=each(%{$lr->{left}[$di]{nz}})) {
      ($tid,$bid) = $lr->{left}[$di]->split($k);
      $pdl->slice("$bid,$di,0,$tid") += $v;
    }
    ##-- data: right context
    while (($k,$v)=each(%{$lr->{right}[$di]{nz}})) {
      ($tid,@bids) = $lr->{right}[$di]->split($k);
      $pdl->slice("$bid,$di,1,$tid") += $v;
    }
  }


  ##-- normalization
  $lr->normalizePdl($pdl) if ($lr->{donorm});

  ##-- shape
  $pdl->reshape(2*$neb*$ndists, $net);

  return $pdl;
}


## $pdl = $lr->normalizePdl($pdl)
##  + normalize a pdl
sub normalizePdl {
  my ($lr,$pdl) = @_;
  my ($v,$sum);

  $pdl -= $pdl->min;
  foreach $di (0..($pdl->dim(1)-1)) {
    ##-- normalize distribution sub-vectors independently for each target
    foreach $ti (0..($pdl->dim(3)-1)) {
      ##-- : left subvector
      $v    = $pdl->slice(",$di,0,$ti");
      $sum  = $v->sum;
      $v   /= $sum if ($sum != 0);

      ##-- : right subvector
      $v    = $pdl->slice(",$di,1,$ti");
      $sum  = $v->sum;
      $v   /= $sum if ($sum != 0);
    }
  }
  return $pdl;
}

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Abstract class for N-ary left-/right-profiles wrt. a given boundary set.\n)
     .qq(--- WARNING: OBSOLETE ---\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  enum=ENUM_NARY   [default=new(enums=>[TARGETS, BOUNDS])]\n)
     .qq(  ndists=N         [default=1]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
}

1; ##-- make perl happy

