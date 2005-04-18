#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::LRBound.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R boundaries
##======================================================================

package MUDL::Corpus::Profile::LRBound;
use MUDL::Corpus::Profile::LR;
use PDL;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile::LR);

##======================================================================
## $lr = $class_or_obj->new(%args)
##   + %args: (see MUDL::Corpus::Profile::LR)
sub new {
  my ($that,%args) = @_;
  return $that->SUPER::new(nfields=>1,donorm=>1,%args);
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
    if (defined($tid=$tids[$bi[$bii]])) {
      ##-- left context
      ++$pr->{left}{nz}{$tid.$pr->{left}{sep}.$bids[$bi[$bii-1]]}
	if ($bii > 0);

      ##-- right context
      ++$pr->{right}{nz}{$tid.$pr->{right}{sep}.$bids[$bi[$bii+1]]}
	if ($bii < $#bi);
    }

    ##-- add interior context
    if ($bii > 0) {
      foreach $sti ($bi[$bii-1]+1..$bi[$bii]-1) {
	next if (!defined($tid=$tids[$sti]));

	##-- left context
	++$pr->{left}{nz}{$tid.$pr->{left}{sep}.$bids[$bi[$bii-1]]};

	##-- right context
	++$pr->{right}{nz}{$tid.$pr->{right}{sep}.$bids[$bi[$bii]]};
      }
    }
  }

  return $pr;
}




##======================================================================
## Conversion: to independent PDL

##-- inherited from MUDL:::Corpus::Profile::LR

## $pdl = $lr->toPDL()
## $pdl = $lr->toPDL($pdl)

## $pdl = $lr->toPDL3d()
## $pdl = $lr->toPDL3d($pdl)

## undef = $lr->finishPdl($pdl);

## undef = $lr->normalizePdl($pdl);


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract left- and right-periphery profile wrt. fixed boundary set.\n)
     .qq(Options:\n)
     .qq(  bounds=ENUM      [default=empty]\n)
     .qq(  targets=ENUM     [default=empty]\n)
     .qq(  eos=EOS_STRING   [default='__\$']\n)
     .qq(  bos=BOS_STRING   [default='__\$']\n)
     .qq(  donorm=BOOL      [default=1]\n)
    );
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
