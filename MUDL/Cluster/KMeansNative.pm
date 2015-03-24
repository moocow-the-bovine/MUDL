#-*- Mode: CPerl -*-

## File: MUDL::Cluster::Kmeans.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner
##======================================================================

package MUDL::Cluster::KMeans;
use PDL;
use MUDL::Object;
use MUDL::PDL qw(:all);
use Carp;

our @ISA = qw(MUDL::Object);
our @EXPORT_OK = qw();

##======================================================================
## K-Means clustering: Constructor

## $args = KMeans->new(%args);
##   + initialize 
##   + %args:
##       data     => $data,    # 2d matrix, $n-by-$d
##       n        => $n,       # number of data instances (instances = columns of $data)
##       d        => $d,       # number of ftrs per datum (features = rows of $data)
##       k        => $k,       # number of desired clusters
##       centers  => $pdl,     # initial centroids (default: random)  ($k by $d)
##       indices  => $pdl,     # cluster labels (default: zeroes)   ($n) ; range=[0..$k]
##       dists    => $pdl,     # distance matrix (temporary)          ($n by $k)
##       metric   => \&metric, # distance metric (default: euclid)
##       srand    => $seed,    # random seed
##       maxiters => $max,     # maximum number of iterations (default: none)
sub new {
  my $km = $_[0]->SUPER::new(
			     data=>null,
			     k => 2,
			     metric=>'euclid',
			     iter => 0,
			     maxiters=>undef,
			     perturb => 2**-10,
			     changed => 1,
			     verbose=>1,
			     @_[1..$#_]
			    );

  $km->{n}       = $km->{data}->dim(0) if (!$km->{n});
  $km->{d}       = $km->{data}->dim(1) if (!$km->{d});
  $km->{centers} = $km->randomCenters() if (!defined($km->{centers}));
  $km->{indices} = zeroes($km->{n}) if (!defined($km->{indices}));
  $km->{dists}   = zeroes($km->{n}, $km->{k}) if (!defined($km->{dists}));

  return $km;
}

##======================================================================
## K-Means clustering: random center allocation

## $centers = $km->randomCenters()
sub randomCenters {
  my $km = shift;
  $km->{centers} = zeroes($km->{k}, $km->{d});

  ##-- allocate
  foreach $ki (0..$km->{k}-1) {
    $km->{centers}->slice($ki) .= $km->{data}->slice(int(rand($km->{n})));
  }

  ##-- perturb
  my ($min,$max) = $km->{data}->minmaximum;
  $km->{centers} +=  $km->{perturb} * (random($km->{k},$km->{d})-0.5) * ($max-$min)->transpose;
  return $km->{centers};
}

##======================================================================
## K-Means clustering: distance computation

## $dists = $km->getdists()
##  +  quite slow, not much difference for large datasets
*getdists = \&getdists1;
sub getdists1 {
  my $km  = shift;
  my ($data,$dists,$centers,$metric) = @$km{qw(data dists centers metric)};
  $metric = UNIVERSAL::can('MUDL::PDL',$metric);
  foreach $ki (0..$km->{k}-1) {
    $dists->slice(",($ki)") .= cdistance($data, $centers->slice($ki), $metric);
  }
  return $dists;
}

## $dists = $km->getdists()
##  + fast (for small datasets), but gobbles memory
sub getdists2 {
  my $km  = shift;
  my $metric = UNIVERSAL::can('MUDL::PDL',$km->{metric});
  return $km->{dists} .= cdistance($km->{data}->slice(",,*$km->{k}"),
				   $km->{centers}->xchg(0,1)->slice("*$km->{n},,"),
				   $metric);
}


##======================================================================
## K-Means clustering: Data-reallocation

## $indices = $km->realloc()
sub realloc {
  my $km = shift;
  $km->{indices_old} = $km->{indices};
  $km->{indices}     = $km->{dists}->xchg(0,1)->minimum_ind;
  $km->{changed}     = any($km->{indices_old} != $km->{indices}) ? 1 : 0;
  return $km->{indices};
}

##======================================================================
## K-Means clustering: Re-centering

## $centers = $km->recenter
sub recenter {
  my $km = shift;
  foreach $ki (0..$km->{k}-1) {
    $km->{centers}->slice("($ki)") .= $km->{data}->dice(which($km->{indices} == $ki))->average;
  }
}



##======================================================================
## K-Means clustering: iteration

## $km = $km->iter()
##  + single iteration
sub iter {
  my $km = shift;

  $km->getdists();
  $km->realloc();
  $km->recenter();
  ++$km->{iter};

  return $km;
}

## $km = $km->iterN()
sub iterN {
  my ($km,$niters) = @_;

  if (defined($niters)) {
    $km->{maxiters} = $km->{iter} + $niters;
  }

  do {
    $km->vmsg(1, $km->{iter});
    $km->iter();
    $km->vmsg(1, "\[$km->{changed}] ");
  }
    while ($km->{changed} && (!defined($km->{maxiters}) || $km->{iter} < $km->{maxiters}));

  $km->vmsg(1, "\n");
}


##======================================================================
## K-Means clustering: monitor

## $km->vmsg($lvl,@message)
##  + print $message to STDERR if $lvl <= $km->{verbose}
sub vmsg {
  my ($km,$lvl) = splice(@_,0,2);
  if ($lvl <= $km->{verbose}) {
    select STDERR; $| = 1; select STDOUT;
    #print STDERR ( ref($km), ": ", @_ );
    print STDERR ( @_ );
  }
}


##======================================================================
## OLD


sub kmeans {
  my ($mat,$k,$args) = @_;
  my $metric  = $args->{metric} || \&euclid;
  my $maxiters = $args->{maxiters};

  ##-- initialize dimenion variables
  my $n = $mat->dim(0);
  my $d = $mat->dim(1);

  ##-- possibly reseed
  srand($args->{seed}) if (defined($args->{seed}));

  ##-- initialize centroids
  my $centers = $args->{centers};
  if (!defined($centers)) {
    my ($min,$max) = $mat->minmaximum;
    $centers = random($k, $d) * ($max-$min)->transpose + $min->transpose;
  }

  ##-- initialize cluster indices (zero)
  my ($indices,$indices_old);
  $indices = $args->{indices};
  if (defined($indices)) {
    $indices_old = zeroes($n);
  }
  else {
    $indices    = zeroes($n);
    $indices_old = ones($n);
  }

  ##-- initialize distance matrix ($n by $k)
  my $dists = zeroes($n,$k);

  ##-- ye olde iterationne
  my $changed = 1;
  my $iter = 0;
  while ($changed && (!defined($maxiters) || $iter < $maxiters)) {
    $changed = 0;
    ++$iter;

    ##-- recompute distance matrix
    ## v--- gobbles memory
    #$dists = cdistance($mat->slice(",,*$k"), $centers->xchg(0,1)->slice("*$n,,"), $metric);
    foreach $ki (0..$k-1) {
      $dists->slice(",($ki)") .= cdistance($mat, $centers->slice($ki));
    }

    ##-- reallocate
    $indices_old .= $indices;
    $indices     .= $dists->xchg(0,1)->minimum_ind;
    $changed = 1 if (any $indices_old != $indices);

    ##-- recompute centroids
    foreach $ki (0..$k-1) {
      $centers->slice("($ki)") .= $mat->dice(which($indices == $ki))->average;
    }
  }

  ##-- update args data
  @$args{qw(centers indices dists changed iter)} = ($centers,$indices,$dists,$changed,$iter);

  return $indices;
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
