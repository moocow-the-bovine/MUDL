#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::LRBound.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R boundaries
##======================================================================

package MUDL::Corpus::Profile::LRBound;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use MUDL::EDist;
use MUDL::Object;
#use MUDL::Set;
use PDL;
use Storable;
use Carp;
our @ISA = qw(MUDL::EDist::Nary MUDL::Corpus::Profile);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args:
##       eos => $eos_str,
##       bos => $bos_str,
##       bounds => $bounds_enum,
##       targets => $targets_enum,
sub new {
  my ($that,%args) = @_;

  my $bounds = $args{bounds} || MUDL::Enum->new();
  my $targets = $args{targets} || MUDL::Enum->new();
  my $nfields = $args{nfields} || 3;
  delete($args{qw(bounds targets nfields)});

  my $self = $that->SUPER::new(eos=>'__$',
			       bos=>'__$',
			       bounds=>$bounds,
			       targets=>$targets,
			       enum=>MUDL::Enum::Nary->new(enums=>[$bounds,$targets,$bounds],
							   nfields=>$nfields),
			       nfields=>$nfields,
			       %args);

  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
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

  ##-- @bi: bound indices
  my @bi = ((defined($pr->{bos}) ? 0 : qw()),
	    (grep { defined($bids[$_]) } (1..$#bids)),
	    (defined($pr->{eos}) ? $#bids : qw()));

  #my ($bii,$sti);
  my ($tid);
  foreach $bii (0..$#bi) {

    ##-- add bound's own context, if it is also a target
    if ($bii > 0 && $bii < $#bi && defined($tid=$tids[$bi[$bii]])) {
      ++$pr->{nz}{join($pr->{sep}, $bids[$bi[$bii-1]], $tid, $bids[$bi[$bii+1]])};
    }

    ##-- add interior context
    if ($bii > 0) {
      foreach $sti ($bi[$bii-1]+1..$bi[$bii]-1) {
	next if (!defined($tid=$tids[$sti]));
	++$pr->{nz}{join($pr->{sep}, $bids[$bi[$bii-1]], $tid, $bids[$bi[$bii]])};
      }
    }
  }

  return $pr;
}




##======================================================================
## Conversion: to independent PDL

## $pdl = $lr->toPDLi()
## $pdl = $lr->toPDLi($pdl)
##   + converts to independent pdl
##   + returned pdl is of dimensions: ($d,$n), where:
##     - $n is the number of targets
##     - $d is twice the number of bounds (left-bounds & right-bounds)
*toPDL = \&toPDLi;
sub toPDLi {
  my ($lr,$pdl) = @_;

  ##-- enum
  my ($eb,$et) = @$lr{qw(bounds targets)};
  my $neb      = $eb->size;

  ##-- pdl
  $pdl = zeroes(double,1) if (!defined($pdl));
  $pdl->reshape($neb*2, $et->size);
  $pdl .= 0;

  ##-- data
  my ($k,$v,@fields);
  while (($k,$v)=each(%{$lr->{nz}})) {
    @fields = $lr->split($k);
    $pdl->slice($fields[0]      .','. $fields[1]) += $v;
    $pdl->slice($fields[2]+$neb .','. $fields[1]) += $v;
  }

  ##-- normalization
  foreach $k (0..$pdl->dim(1)-1) {
    $v  = $pdl->slice('0:'.($neb-1).",$k");
    $v /= $v->sum;

    $v  = $pdl->slice("$neb:".($neb*2-1).",$k");
    $v /= $v->sum;
  }

  return $pdl;
}


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right-periphery profile wrt. fixed "function word" set.\n)
     .qq(Options:\n)
     .qq(  bounds=SET       [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  dobounds=BOOL    [default=1]\n)
     .qq(  dononbounds=BOOL [default=1]));
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
