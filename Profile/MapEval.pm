#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::MapEval.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: map evaluation
##======================================================================

package MUDL::Corpus::Profile::MapEval;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use MUDL::Map;
use MUDL::Object;
#use MUDL::Set;
#use PDL;
#use Carp;
our @ISA = qw(MUDL::Dist::Nary MUDL::Corpus::Profile);

##======================================================================
## new
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new('map'=>MUDL::Map->new(),
			       nfields=>3,
			       %args);
  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($pr,$s) = @_;

  my ($text,$mtag);
  foreach my $tok (@$s) {
    next if (!defined($mtag=$pr->{'map'}{$tok->text})); ##-- ignore map-unknowns

    ++$pr->{nz}{join($pr->{sep}, $tok->text(), $mtag, $tok->tag())};
  }

  return $pr;
}

##======================================================================
## Evaluation: Unigram modelling

## ($precision,$recall) = $pr->evaluateUg()
##   + also very strange (?)
*finish = \&evaluateUg;
sub evaluateUg {
  my $pr = shift;
  my $mgdist = $pr->{mgdist} = $pr->projectN(1,2);
  my $m2gmap = $pr->{m2gmap} = $mgdist->toBestMap([0],[1]);
  my $g2mmap = $pr->{g2mmap} = $mgdist->toBestMap([1],[0]);

  my ($tp,$fp,$fn) = (0,0,0);
  my ($evt,$f,@fields,$mk,$gk);
  while (($evt,$f)=each(%{$mgdist->{nz}})) {
    ($mk,$gk) = $mgdist->split($evt);
    $total += $f;

    if ($mk eq $g2mmap->{$gk}) {
      ##-- +(g->m)
      if ($gk eq $m2gmap->{$mk}) {
	##-- +(g->m), +(m->g)
	++$tp
      } else {
	##-- +(g->m), -(m->g)
	++$fp;
      }
    }
    elsif ($gk eq $m2gmap->{$mk}) {
      ##-- -(g->m), +(m->g)
      ++$fn;
    }
  }

  ##-- set precision, recall
  $pr->{precision} = $tp / ($tp+$fp);
  $pr->{recall}    = $tp / ($tp+$fn);

  return @$pr{qw(precision recall)};
}


##======================================================================
## Evaluation: Expectation

## ($precision,$recall) = $pr->evaluateExpect()
##   + very strange
sub evaluateExpect {
  my $pr = shift;
  my $distm = $pr->{distm} = $pr->projectN(0,1);
  my $distg = $pr->{distg} = $pr->projectN(0,2);

  my $total = $distm->total;

  $pr->{precision} = $pr->getExpected($distm, $total, 1);
  $pr->{recall}    = $pr->getExpected($distg, $total, 2);

  return @$pr{qw(precision recall)};
}


## $measure = getExpected($givenWordTagDist, $totalEvents, $givenTagField);
##   + low-level function for expected precision and recall
##   + computes E( p ( $condTagField | $givenTagField, $word ) )
##     - where $condTagField = $givenTagField==1 ? 2 : 1;
sub getExpected {
  my ($pr,$gtdist,$total,$gtf) = @_;
  my $sum = 0;
  my ($event,$f_event,$gevent);
  while (($event,$f_event) = each(%{$pr->{nz}})) {
    $gevent = join($pr->{sep}, ($pr->split($event))[0,$gtf]);
    $sum     += $f_event**2 / ($total * $gtdist->{nz}{$gevent});
  }
  return $sum;
}

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Evaluate token-text (unigram) maps [broken?]\n)
     .qq(Options:\n)
     .qq(  map=MAP [default=empty])
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
