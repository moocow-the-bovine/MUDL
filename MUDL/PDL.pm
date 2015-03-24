#-*- Mode: Perl -*-

## File: MUDL::PDL.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: PDL objects
##======================================================================

package MUDL::PDL;
use PDL;
use PDL::IO::Storable;
use PDL::Math;
use MUDL::Object;

our @ISA = qw(Exporter PDL MUDL::Object);

our %EXPORT_TAGS =
  (
   distance => [qw(distance cdistance distances l1 euclid minkowski)],
  );
$EXPORT_TAGS{all} = [map { @$_ } values(%EXPORT_TAGS)];
our @EXPORT_OK    = @{$EXPORT_TAGS{all}};
our @EXPORT       = @EXPORT_OK;

##======================================================================
## Constructor

## $mpdl = MUDL::PDL->new($pdl)
##   + weirdness: probably shouldn't inherit from PDL
sub new {
  my $that = shift;
  my $pdl  = shift;
  $pdl = PDL->new() if (!defined($pdl));
  return bless $pdl, ref($that)||$that;
}


##======================================================================
## Distances

## $distance = $pdl->cdistance($center)
## $distance = $pdl->cdistance($center, \&metric)
##  + computes distance $pdl <-> $center , via \&metric
##  + returns a pdl of dimensionality @dims[0,2..$#dims], where @dims=$pdl->dims
##    - i.e. sums over y values; returns a singleton if $pdl and $center are column-vectors
##  + \&metric defaults to \&euclid
sub cdistance { return ($_[2] ? $_[2] : \&euclid)->(($_[0]-$_[1])->xchg(0,1)); }

## $dpdl = $pdl->distances($center, \&metric)
##  + computes distance pdl $pdl <-> $center , via \&metric
##  + copied verbatim from PDL::Basic(3pm) manpage
##  + returned $dpdl has same dimensionality as $pdl
sub distances {
  my ($pdl,$center,$f) = @_;
  my $r = $pdl->allaxisvals-$center;
  return &$f($r);
}

## $distance_pdl = l1($difference_pdl)
##   + Manhattan distance (L1 norm) -- from PDL::Basic(3pm) manpage
sub l1     {     sumover(abs($_[0])); }

## $distance_pdl = euclid($difference_pdl)
##   + Euclidean distance -- from PDL::Basic(3pm)
sub euclid { pow(sumover(pow($_[0],2)),0.5); }

## \&distance_sub = minkowski($l)
##   + General minkowski metric
##   + Returns distance sub \&distance_sub suitable for distance():
##     $distance_pdl = minkowski($l)->($difference_pdl);
sub minkowski {
  my $l = shift;
  return sub {
    pow(sumover( abs(pow($_[0], $l)) ), 1/$l);
  };
}


##======================================================================
## I/O: Conversion

## $ary = pdl2ary($pdl)
## $ary = pdl2ary($pdl,$ary)
sub pdl2ary {
  my ($pdl,$data) = @_;
  my @queue = ($pdl,\$data);
  my ($ref);
  while (($pdl,$ref) = splice(@queue,0,2)) {
    if ($pdl->ndims == 0) {
      $$ref = $pdl->sclr;
      next;
    }
    $$ref = [];
    push(@queue, $_, \$$ref->[@$$ref]) foreach ($pdl->dog);
  }
  return $data;
}


##======================================================================
## I/O: XML

##-- (inherited)

##======================================================================
## I/O: Native

## $bool = $pdl->saveNativeFh($fh,%args)
sub saveNativeFh { $_[0]->saveAutoclassFh($_[1], idx=>0, @_[1..$#_]); }

## $bool = $pdl->loadNativeFh($fh,%args)
#sub loadNativeFh {}

##======================================================================
## I/O: Autoclass

## $bool = $pdl->saveAutoclassFh($fh,%args)
##  + %args:
##     sep => $separator_string,   # default: ' '
##     bad => $bad_string,         # default: '?'
##     idx => $bool,               # default: '1'
##  + $pdl should be a 2-dimensional pdl
sub saveAutoclassFh {
  my ($pdl,$fh,%args) = @_;
  %args = (
	   sep=>' ',
	   bad=>'?',
	   idx=>1,
	   %args,
	  );

  carp(ref($pdl), "::saveAutoclassFh(): PDL is not 2-dimensional")
    if ($pdl->ndims != 2);

  foreach $x (0..$pdl->dim(0)-1) {
    $fh->print(join($args{sep},
		    ($args{idx} ? $x : qw()),
		    (map {
		      $_ eq 'BAD' ? $args{bad} : $_
		    } $pdl->slice("($x)")->flat->list)),
	       "\n");
  }
  return $pdl;
}

## $bool = $pdl->saveAutoclassBinFh($fh,%args)
##  + $pdl should be a 2-dimensional pdl
sub saveAutoclassBin {
  my ($pdl,$fh,%args) = @_;

  carp(ref($pdl), "::saveAutoclassBinFh(): PDL is not 2-dimensional")
    if ($pdl->ndims != 2);

  ##-- header
  $fh->print('.db2-bin',
	     pack('i', $pdl->clump(1..$pdl->ndims-1)->dim(1)));

  ##-- data
  foreach $x (0..$pdl->dim(0)-1) {
    $fh->print(pack("f*", $pdl->slice("($x)")->flat->list));
  }
  return $pdl;
}


##======================================================================
## Converters (see MUDL::Dist::Enum)


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
