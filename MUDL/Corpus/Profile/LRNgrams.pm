##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::LRBigrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R n-grams
##  + probably BROKEN
##======================================================================

package MUDL::Corpus::Profile::LRNgrams;
use MUDL::Corpus::Profile::LRN;
use MUDL::Object;
use PDL;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile::LRN);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
##       left=>\@left_dists,       ## ($target,$lneighbor)
##       right=>\@right_dists,     ## ($target,$rneighbor)
##       width=>$ndists,
sub new {
  my ($that,%args) = @_; 
  my $width = $args{width} ? $args{width} : ($args{ndists} ? $args{ndists} : 2);
  delete(@args{qw(width ndists)});
  return $that->SUPER::new(ndists=>$width,donorm=>1,%args);
  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($pr,$s) = @_;
  my $len = $pr->{ndists};

  ##-- sanity checks: bos/eos
  if (defined($pr->{bos})) {
    $pr->{bounds}->addSymbol($pr->{bos});
  }
  if (defined($pr->{eos})) {
    $pr->{bounds}->addSymbol($pr->{eos});
  }


  ##------ temporary sentence index profiles

  ##-- @st: sentence text
  my @st = ((map { $pr->{bos} } (1..$len)),
	    (map { $_->text }   @$s),
	    (map { $pr->{eos} } (1..$len)));

  ##-- @tids, @bids: sentence target (bound) ids
  my @tids = map { $pr->{targets}{sym2id}{$_} } @st;
  my @bids = map { $pr->{bounds}{sym2id}{$_}  } @st;

  my $lbgs = $pr->{left};
  my $rbgs = $pr->{right};

  my ($i,$j);
  for ($i=0; $i <= $#st; $i++) {
    next if (!defined($tid=$tids[$i]));

    ##-- window loop
    for ($j=1; $j <= $len; $j++) {

      ##-- left context
      if ($i-$j >= 0 && defined($bid=$bids[$i-$j])) {
	++$lbgs->[$j-1]{nz}{$tid.$lbgs->[$j-1]{sep}.$bid};
      }

      ##-- right context
      if ($i+$j <= $#st && defined($bid=$bids[$i+$j])) {
	++$rbgs->[$j-1]{nz}{$tid.$rbgs->[$j-1]{sep}.$bid};
      }

    }
  }

  return $pr;
}




##======================================================================
## Conversion: to PDL

##-- inherited from MUDL:::Corpus::Profile::LRN


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right-N-gram profile wrt. fixed boundary set.\n)
     .qq(--- WARNING: OBSOLETE ---\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
     .qq(  width=N          [default=2]\n)
    );
}

##======================================================================
## I/O: Native
## - (output only!)

## $bool = $obj->saveNativeFh($fh,%args)
sub saveNativeFh {
  my ($obj,$fh) = @_;

  foreach $i (0..($obj->{ndists}-1)) {
    $fh->print("\n",
	       "%%", ('-' x 72), "\n",
	       "%% Left (-$i)\n",
	       "%%", ('-' x 72), "\n");
    $obj->{left}[$i]->toDist->saveNativeFh($fh,@_);
  }

  foreach $i (0..($obj->{ndists}-1)) {
    $fh->print("\n",
	       "%%", ('-' x 72), "\n",
	       "%% Right (+$i)\n",
	       "%%", ('-' x 72), "\n");
    $obj->{right}[$i]->toDist->saveNativeFh($fh,@_);
  }

  return $obj;
}


1; ##-- make perl happy
