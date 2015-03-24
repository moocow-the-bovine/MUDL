#-*- Mode: Perl -*-

## File: MUDL::Ranks.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: ranking
##======================================================================

########################################################################
## class MUDL::Ranks
########################################################################

package MUDL::Ranks;
use MUDL::Dist;
use MUDL::Object;
use Carp;
use IO::File;

our $VERSION = 0.01;
our @ISA = qw(Exporter MUDL::Dist);

our @EXPORT = qw(),
our @EXPORT_OK = qw(sort_asc sort_dsc);
our %EXPORT_TAGS = (sort=>\@EXPORT_OK, all=>\@EXPORT_OK);

##======================================================================
## MUDL::Ranks : Methods
##======================================================================
## $ranks = MUDL::Ranks->new(dist=>$dist,sort=>'asc'|'dsc')
##  + computes ranks from dist
##  + structure: $ranks->{$evt} = $rank
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new();
  $self->fromDist($args{dist},%args) if ($args{dist});
  return $self;
}

## $ranks = MUDL::Ranks->fromDist($dist,%args)
##  + computes ranks from dist
##  + structure: $ranks->{$evt} = $rank
##  + %args: sort=>'asc'|'dsc'  # default='dsc'
sub fromDist {
  my $r = shift;
  $r = $r->new() if (!ref($r));
  my $d = shift;
  my %args = (sort=>'dsc',@_);
  $r->clear;
  my $rank = 0;
  my @evts = ($args{sort} eq 'dsc'
	      ? (sort { sort_dsc($d,$a,$b) } keys(%$d))
	      : (sort { sort_asc($d,$a,$b) } keys(%$d)));
  foreach (@evts) {
    $r->{$_} = ++$rank;
  }
  return $r;
}

##======================================================================
## minimum, maximum key

sub min {
  my $r = shift;
  my ($mink,$minv) = (undef,undef);
  my ($k,$v);
  while (($k,$v)=each(%$r)) {
    ($mink,$minv) = ($k,$v) if (!defined($minv) || $v < $minv);
  }
  return wantarray ? ($mink,$minv) : $mink;
}
sub max {
  my $r = shift;
  my ($maxk,$maxv) = (undef,undef);
  my ($k,$v);
  while (($k,$v)=each(%$r)) {
    ($maxk,$maxv) = ($k,$v) if (!defined($maxk) || $v > $maxv);
  }
  return wantarray ? ($maxk,$maxv) : $maxk;
}

sub minRank { return ($_[0]->min)[1]; }
sub maxRank { return ($_[0]->max)[1]; }


##======================================================================
## Rank sorting subs
sub sort_asc { return $_[0]->{$_[1]} <=> $_[0]->{$_[2]} || $_[1] cmp $_[2]; }
sub sort_dsc { return $_[0]->{$_[2]} <=> $_[0]->{$_[1]} || $_[1] cmp $_[2]; }


########################################################################
## class MUDL::XRanks
##   + shared ranks
########################################################################

package MUDL::XRanks;
MUDL::Ranks->import(':sort');
our @ISA = qw(MUDL::Ranks);

sub fromDist {
  my $r = shift;
  $r = $r->new() if (!ref($r));
  my $d = shift;
  my %args = (sort=>'dsc',@_);
  $r->clear;
  my $rank = undef;
  my $lastf = undef;
  my @evts = ($args{sort} eq 'dsc'
	      ? (sort { sort_dsc($d,$a,$b) } keys(%$d))
	      : (sort { sort_asc($d,$a,$b) } keys(%$d)));
  foreach (@evts) {
    if (!defined($lastf) || $d->{$_} != $lastf) {
      $lastf = $d->{$_};
      ++$rank;
    }
    $r->{$_} = $rank;
  }
  return $r;
}


########################################################################
## class MUDL::Quanta
##   + quantized ranks
########################################################################

package MUDL::Quanta;
MUDL::Ranks->import(':sort');
our @ISA = qw(MUDL::Ranks);

## $quanta = $class_or_obj->fromDist($d,%args)
##   + %args:
##       sort => 'asc' | 'dsc'
##       qsize => $n
sub fromDist {
  my ($qr,$d) = splice(@_,0,2);
  return $qr->fromRanks(MUDL::Ranks->new->fromDist($d),@_);
}

## $quanta = $clas_or_obj->fromRanks($ranks)
## $quanta = $clas_or_obj->fromRanks($ranks,$qsize)
*fromXRanks = \&fromRanks;
sub fromRanks {
  my ($qr,$r) = splice(@_,0,2);
  $qr = $qr->new() if (!ref($qr));
  my %args = (sort=>'dsc',qsize=>1,@_);
  %$qr = %$r;
  my $maxrank = $r->maxRank;

  if ($args{sort} eq 'dsc') {
    ##-- descending order
    foreach (values(%$qr)) {
      $_ = (1 + $maxrank - $_) / $maxrank;
    }
  } else {
    ##-- ascending order
    foreach (values(%$qr)) {
      $_ /= $maxrank;
    }
  }

  return $qr->quantize($args{qsize});
}

## $quanta = $quanta->quantize($n)
sub quantize {
  my ($qr,$n) = @_;
  return $qr if (!$n || $n==1);
  foreach (values(%$qr)) {
    $_ *= $n;
    $_ = $_-int($_) < 0.5 ? int($_) : int($_)+1; ##-- round
  }
  return $qr;
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
