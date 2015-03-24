##-*- Mode: CPerl -*-

## File: MUDL::Enum::PdlVector.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: enumeration of pdl-vectors
##======================================================================

package MUDL::Enum::PdlVector;
use MUDL::Object;
use PDL::Lite;
use PDL::VectorValued;
#use PDL::CCS::Nd qw();
use strict;


our @ISA     = qw(MUDL::Object);
our $NoLabel = -1;
our $P_LONG  = PDL::long();


##======================================================================
## Constructors etc.

## $pve = $class_or_object->new(%args);
##  + Object structure:
##     missing => $missing,      ##-- missing value (default: -1)
##     qvecs   => $sorted_vecs,  ##-- pdl(Ndims,Nvecs) : sorted a la vv_qsortvec()
##     qvi2i   => $qvi2i,        ##-- pdl(long,Nvecs)  : $qvid  => $vecid : logical ids aligned with $sorted_vecs
##     i2qvi   => $id2qvi,       ##-- pdl(long,Nvecs)  : $vecid => $qvid  : sort positions aligned with logical ids
##  + args to %new
##     vecs    => $unsorted_vecs, ##-- inserted if requested
sub new {
  my ($that,%args) = @_;
  my $vecs = $args{vecs};
  delete($args{vecs});
  my $obj = bless { missing=>$NoLabel, %args }, ref($that)||$that;
  $obj->insert($vecs) if (defined($vecs));
  return $obj;
}

##======================================================================
## Low-level access

## $null_long = nullIndex()
sub nullIndex { return PDL->null->convert($P_LONG); }

## undef = $pve->checkDims($vecs)
## undef = $pve->checkDims($vecs,$opName)
sub checkDims {
  #my ($pve,$vecs,$opName) = @_;
  ##-- sanity check
  if (defined($_[0]{qvecs}) && !$_[0]{qvecs}->isempty && $_[1]->dim(0) != $_[0]{qvecs}->dim(0)) {
    barf(ref($_[0])."::".($_[2]||'checkDims').": vector-length mismatch in vecs() argument:",
	 " is ", $_[1]->dim(0), ", should be ", $_[0]{qvecs}->dim(0));
  }
}

## ($qvi_best, $qvi_which_good, $qvi_which_bad) = $pve->vecs2qvi_which($vecs)
## ($qvi_best, $qvi_which_good, $qvi_which_bad) = $pve->vecs2qvi_which($vecs,$opName)
sub vecs2qvi_which {
  my ($pve,$vecs,$opName) = @_;
  return (nullIndex, nullIndex, nullIndex) if ($vecs->isempty);
  $pve->checkDims($vecs,($opName||'vecs2qvi_which'));
  return (nullIndex, nullIndex, PDL->sequence($P_LONG,$vecs->dim(1))) if (!defined($pve->{qvecs}));

  my $foundi        = $vecs->vsearchvec($pve->{qvecs});
  my $foundi_isgood = ($vecs==$pve->{qvecs}->dice_axis(1,$foundi))->andover;
  my ($foundi_which_good,$foundi_which_bad) = $foundi_isgood->which_both;

  return ($foundi,$foundi_which_good,$foundi_which_bad);
}

## $qvi_or_missing = $pve->vecs2qvi($vecs)
## $qvi_or_missing = $pve->vecs2qvi($vecs,$opName)
sub vecs2qvi {
  my ($qvi, $qvi_which_good, $qvi_which_bad) = $_[0]->vecs2qvi_which($_[1],$_[2]);
  return $qvi if ($qvi_which_bad->isempty);
  $qvi->index($qvi_which_bad) .= $_[0]{missing};
  return $qvi;
}

## $pve = $pve->update()
##   + re-sort $qvecs
sub update {
  my $pve = shift;
  my $qvecs = $pve->{qvecs};
  my $qvecs_qsi = $qvecs->vv_qsortveci;               ##-- $new_qvi => $old_qvi
  $pve->{qvecs} = $qvecs->dice_axis(1,$qvecs_qsi);
  $pve->{qvi2i} = $pve->{qvi2i}->index($qvecs_qsi);
  $pve->{i2qvi}->index($pve->{qvi2i}) .= $pve->{i2qvi}->sequence;
  return $pve;
}

##======================================================================
## Basic information

## $size = $pve->size()
sub size { return defined($_[0]{i2qvi}) ? $_[0]{i2qvi}->nelem : 0; }

## $ids = $pve->ids()
##  + gets all logical ids in vectors() sort order
sub ids { defined($_[0]{qvi2i}) ? $_[0]{qvi2i} : nullIndex; }

## $vectors = $pve->vectors()
##  + gets all stored vectors in lexicographic sort order
*vecs = \&vectors;
sub vectors { defined($_[0]{qvecs}) ? $_[0]{qvecs} : nullIndex; }

##======================================================================
## Manipulation & Access

## $ids_or_missing = $pve->vector2id($vecs)
##  + get ids for $vecs
*vec2id = \&vector2id;
sub vector2id {
  my ($qvi, $qvi_which_good, $qvi_which_bad) = $_[0]->vecs2qvi_which($_[1],'ids');
  return $_[0]{qvi2i}->index($qvi) if ($qvi_which_bad->isempty);
  $qvi->index($qvi_which_bad)  .= $_[0]{missing};
  $qvi->index($qvi_which_good) .= $_[0]{qvi2i}->index($qvi->index($qvi_which_good));
  return $qvi;
}

## $vecs = $pve->id2vector($ids)
## $vecs = $pve->id2vector($ids,$missingVector)
*id2vec = \&id2vector;
sub id2vector {
  my ($pve,$ids,$missingVector) = @_;
  return nullIndex if ($ids->isempty);
  return PDL->zeroes($P_LONG,$ids->nelem)+$pve->{missing} if (!defined($pve->{qvecs}));

  my $ids_good_mask = (($ids>=0) & ($ids<$pve->{i2qvi}->nelem));
  if ($ids_good_mask->all) {
    return $pve->{qvecs}->dice_axis(1,$pve->{i2qvi}->index($ids));
  }
  my $vecs = PDL->zeroes($pve->{qvecs}->type, $pve->{qvecs}->dim(0), $ids->nelem);
  $missingVector = $pve->{missing} if (!defined($missingVector));
  $vecs         .= $missingVector  if (defined($missingVector));
  my $ids_good_which = $ids_good_mask->which;
  $vecs->dice_axis(1,$ids_good_which) .=
    $pve->{qvecs}->dice_axis(1,$pve->{i2qvi}->index($ids->index($ids_good_which)));
  return $vecs;
}

## $ids = $pve->insert($vecs)
##  + returns $ids for $vecs, which may or may not already be present
sub insert {
  my ($pve,$vecs) = @_;
  return $pve->append($vecs) if (!defined($pve->{qvecs})); ##-- no previous $vecs: just append

  ##-- check for existing indices (logarithmic)
  my ($qvi,$qvi_which_good,$qvi_which_bad) = $pve->vecs2qvi_which($vecs,'insert');

  if ($qvi_which_bad->isempty) {
    ##-- no new vectors were specified: just lookup logical ids
    return $pve->{qvi2i}->index($qvi);
  }

  ##-- some new vectors were specified
  $qvi->index($qvi_which_good) .= $pve->{qvi2i}->index($qvi->index($qvi_which_good)); ##-- get old logical ids
  $qvi->index($qvi_which_bad)  .= $pve->append($vecs->dice_axis(1,$qvi_which_bad));   ##-- ... and new ones
  return $qvi;
}

## $ids = $pve->append($vecs)
##  + returns $ids for $vecs, which are assumed to be new
sub append {
  my ($pve,$vecs) = @_;

  ##-- ignore empty vecs
  return nullIndex if ($vecs->isempty);
  $pve->checkDims($vecs,'append');
  my $qvecs = $pve->{qvecs};
  my $nvecs = $vecs->dim(1);

  ##-- empty object: instantiate
  if (!defined($qvecs) || $qvecs->isempty) {
    my $ids = PDL->sequence($P_LONG,$nvecs);
    $pve->{qvecs} = $vecs->pdl;
    $pve->{qvi2i} = $ids->pdl;
    $pve->{i2qvi} = $ids->pdl;
    $pve->update();
    return $ids;
  }

  ##-- non-empty enum: real append
  my $ids = PDL->sequence($P_LONG,$nvecs)+$pve->size;
  $pve->{qvecs} = $pve->{qvecs}->xchg(0,1)->append($vecs->xchg(0,1))->xchg(0,1);
  $pve->{qvi2i} = $pve->{qvi2i}->append($ids);
  $pve->{i2qvi} = $pve->{i2qvi}->append($ids);
  $pve->update();

  return $ids;
}

1; ##-- make perl happy
