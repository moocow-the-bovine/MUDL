#-*- Mode: Perl -*-

## File: MUDL::Dist.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: distributions
##======================================================================

package MUDL::Dist;
use MUDL::XML;
use IO::File;
use PDL;
use PDL::Fit::Linfit;
use Carp;
our $VERSION = 0.01;
our @ISA = qw(MUDL::XML::Object);

sub new {
  my ($that,%args) = @_;
  return bless {}, ref($that)||$that;
}

##======================================================================
## Accessors

## \%hash = $d->f()
## $freq  = $d->f($event)
##  + alias: p()
*p = \&f;
sub f {
  if (@_ == 2) {
    return $_[0]->{$_[1]};
  }
  return $_[0]->{$_[1]} = $_[2];
}

## $d = $d->clear()
sub clear {
  my $d = shift;
  %$d = qw();
  return $d;
}

## @events = $d->events()
sub events { return keys(%{$_[0]}); }

## $d2 = $d->copy
sub copy {
  my $d = shift;
  my $d2 = ref($d)->new();
  %$d2 = %$d;
  return $d2;
}

##======================================================================
## Smoothing & Normalization

## $d = $d->normalize()
## $d = $d->normalize($total)
sub normalize {
  my ($d,$total) = @_;
  $total = $d->total if (!$total);
  $_ /= $total foreach (values(%$d));
  return $d;
}

## $total = $d->total()
sub total {
  my $sum = 0;
  $sum += $_ foreach (values(%{$_[0]}));
  return $sum;
}

## $ccounts = $d->countcounts()
*ccounts = \&countcounts;
sub countcounts {
  my $d = shift;
  my $d2 = MUDL::Dist->new();
  my ($e,$f);
  while (($e,$f)=each(%$d)) {
    ++$d2->{$f};
  }
  return $d2;
}

## $smeared = $ccounts->smear()
##  + returned smeared countcounts-type distribution for countcounts-type distributions
sub smear {
  my $Nr = shift;
  my @rs = sort { $a<=>$b } keys(%$Nr);
  my ($i,$r,$r_lo,$r_hi);
  my $Zr = __PACKAGE__ -> new();
  for ($i=0; $i <= $#rs; $i++) {
    $r        = $rs[$i];
    $r_lo     = $i==0     ?  0                    :  $rs[$i-1];
    $r_hi     = $i==$#rs  ?  ($r + ($r - $r_lo))  :  $rs[$i+1];
    $N_r      = $Nr->{$r};
    $Zr->{$r} = 2*$N_r / ($r_hi - $r_lo);
  }
  return $Zr;
}

## @coeffs = $ccounts->linfit()
##  + returned coefficients for linear fit of data for countcounts-type distributions
##  + returned values can be used to predict by:
##      $newval = $coeffs[0] + $coeffs[1]*$key
sub linfit {
  my $d = shift;
  my $xdata = pdl([keys   %$d])->inplace->float;
  my $ydata = pdl([values %$d])->inplace->float;
  my ($yfit,$coeffs) = linfit1d($ydata, cat(ones($ydata->nelem), $xdata));
  undef $yfit;
  return wantarray ? ($coeffs->at(0),$coeffs->at(1)) : $coeffs;
}

## @coeffs = $ccounts->loglinfit()
##  + returned coefficients for loglinear fit of data for countcounts-type distributions
##  + returned values can be used to predict by:
##      $newval = $coeffs[0] * $key**$coeffs[1]
sub loglinfit {
  my $d = shift;
  my $xdata = pdl([keys   %$d])->inplace->float->inplace->log;
  my $ydata = pdl([values %$d])->inplace->float->inplace->log;
  my ($yfit,$coeffs) = linfit1d($ydata, cat(ones($ydata->nelem), $xdata));
  undef $yfit;
  return wantarray ? (exp($coeffs->at(0)),$coeffs->at(1)) : $coeffs;
}


## $missingMass = $dist->smoothGTLogLin()
##  + Good-Turing smoothing, log-linear
##  + returns total pseudo-count alloted for zeroes
##  + zero-values in the hash are not smoothed here
sub smoothGTLogLin {
  my $d = shift;
  my $N = $d->total;
  $N = 1 if (!$N);
  my $Nr = $d->countcounts;
  my $Zr = $Nr->smear;
  my ($S_a,$S_e) = $Zr->loglinfit;
  foreach (values(%$d)) {
    ##-- don't adjust zeroes here
    $_ = ($_+1) * ($S_a * ($_+1)**$S_e) / ($S_a * $_**$S_e);
  }
  return $Nr->{1} ? ($Nr->{1}/$N) : ($S_a/$N);
}


##======================================================================
## Metrics, etc.

## $mean = $d->mean()
## $mean = $d->mean($total)
## $mean = $d->mean($total,$nvalues)
##   + returns arithmetical mean
sub mean {
  my $d = shift;
  my $total = shift || $d->total;
  my $nvalues = shift || scalar(values(%$d));
  return $total / $nvalues;
}

## $H = $d->entropy()
## $H = $d->entropy($total)
*H = \&entropy;
sub entropy {
  my $d = shift;
  my $total = shift||$d->total;
  my $H = 0;
  $H += $_/$total * log($total/$_)/log(2) foreach (values(%$d));
  return $H;
}




##======================================================================
## I/O: Native

## $bool = saveNative($file_or_fh,%args)
##  + %args : sort=>\&sortFunc
sub saveNative {
  my ($d,$file,%args) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  croak( __PACKAGE__ , "::saveNative(): open failed for '$file': $!") if (!$fh);

  foreach $e ($args{sort} ? sort { &{$args{sort}}($a,$b) } keys(%$d) : keys(%$d)) {
    $fh->print($e, "\t", $d->{$e}, "\n");
  }

  $fh->close() if (ref($file));
  return $d;
}

## $bool = loadNative($file_or_fh)
sub loadNative {
  my ($d,$file) = @_;
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  croak( __PACKAGE__ , "::loadNative(): open failed for '$file': $!") if (!$fh);

  my ($line);
  while (defined($line=<$fh>)) {
    chomp $line;
    next if ($line =~ /^\s*$/);
    if ($line !~ /^(.*\S)\s+([\+\-\d\.]+)$/) {
      warn( __PACKAGE__ , "::loadNative(): parse error in file '$file' at line ", $fh->input_line_number);
      next;
    }
    $d->{$1} = $2;
  }

  $fh->close() if (ref($file));
  return $d;
}


##======================================================================
## I/O: XML

## $node = $obj->saveXMLNode()
sub saveXMLNode {
  my ($d,%args) = @_;
  (my $nodename = ref($d)) =~ s/::/./g;
  my $node = XML::LibXML::Element->new($nodename);

  my ($e,$f,$enode);
  foreach $e ($args{sort} ? sort { &{$args{sort}}($a,$b) } keys(%$d) : keys(%$d)) {
    $node->appendChild($enode = XML::LibXML::Element->new('event'));
    $enode->setAttribute('f', $d->{$e});
    if (ref($e)) {
      $enode->appendChildNode($e->saveXMLNode(@_));
    } else {
      $enode->appendText($e);
    }
  }

  return $node;
}

## $obj = $obj->loadXMLNode($node)
sub loadXMLNode {
  my ($d,$node) = @_;
  (my $nodename = ref($d)) =~ s/::/./g;
  carp( __PACKAGE__ , "::loadXMLNode() expected '$nodename' element, got '", $node->nodeName, "'\n")
    if ($node->nodeName ne $nodename);

  my ($enode,$e,$f);
  foreach $enode ($node->getChildrenByTagName('event')) {
    if ($enode->hasChildNodes) {
      $e = MUDL::XML::Object->newFromXMLNode($enode->firstChild);
    } else {
      $e = $enode->textContent;
    }
    $f = $enode->getAttribute('f');
    $d->{$e} = $f;
  }

  return $d;
}

##======================================================================
## MUDL::Dist::Partial
##======================================================================
package MUDL::Dist::Partial;
use Carp;
use IO::File;
our @ISA = qw(MUDL::XML::Object);


## object structure:
##   + size  : number of possible events
##   + zmass : total mass alotted to zero-count events ("missing mass")
sub new {
  my $that = shift;
  return bless {nz=>MUDL::Dist->new(), size=>0, zmass=>0}, ref($that)||$that;
}

## copy
sub copy {
  my $d = shift;
  my $d2 = ref($d)->new();
  @$d2{qw(nz size zmass)} = ($d->{nz}->copy, @$d{qw(size zmass)});
  return $d2;
}

##======================================================================
## Access
##======================================================================

## \%hash = $d->f()
## $freq  = $d->f($event)
##  + alias: p(), nz()
*p = \&f;
*nz = \&f;
sub f {
  if (@_ == 2) {
    return $_[0]->{nz}{$_[1]};
  }
  return $_[0]->{nz}{$_[1]} = $_[2];
}

## $d = $d->clear()
sub clear {
  my $d = shift;
  $d->{nz}->clear;
  $d->{size} = 0;
  $d->{zmass} = 0;
  return $d;
}

## @events = $d->events()

## $size = $d->size()
## $size = $d->size($size)
##  + should only be called in first form after all events have been added
sub size {
  my $d = shift;
  if (@_) {
    return $d->{size} = shift;
  }
  return $d->{size} if ($d->{size});
  return $d->getSize;
}

## $size = $d->getSize()
##  + recomputes {size} membert
sub getSize {
  my $d = shift;
  return $d->{size} = scalar(keys(%{$d->{nz}}));
}

##======================================================================
## Smoothing & Utilities

## $total = $d->total()
##   + includes "missing mass"
sub total {
  my $d = shift;
  my $nztotal = $d->{nz}->total;
  return $nztotal + $nztotal*$d->{zmass};
}

## $zmass = $d->missingMass()
##  + get total missing mass
*zmass = \&missingMass;
sub missingMass { return $_[0]->{zmass}; }

## $d = $d->normalize
## $d = $d->normlaize($total);
sub normalize {
  my ($d,$total) = @_;
  $total = $d->total if (!$total);
  $d->{nz}->normalize($total);
  return $d;
}

## $nnz = $d->nNonZero
sub nNonZero {
  my $d = shift;
  return scalar(grep {$_ != 0} values(%{$d->{nz}}));
}

## $nnz = $d->nZero
sub nZero {
  my $d = shift;
  return $d->size - $d->nNonZero(@_);
}

## $zmass1 = $d->zeroCount()
## $zmass1 = $d->zeroCount($nZeroes)
##  + returns quantity of missing mass alotted to any single zero-count event
##  + uses $d->size()
sub zeroCount {
  my ($d,$nz) = @_;
  $nz = $d->nZero if (!defined($nz));
  return $d->{zmass} / $nz;
}

## $zmass = $dist->smoothGTLogLin()
##  + Good-Turing smoothing, log-linear
##  + sets key 'zmass' for missing mass
sub smoothGTLogLin {
  my $d = shift;
  return $d->{zmass} = $d->{nz}->smoothGTLogLin;
}

##======================================================================
## Metrics, etc.

## $mean = $d->mean()
## $mean = $d->mean($total)
## $mean = $d->mean($total,$nvalues)
sub mean {
  my $d = shift;
  my $total = shift || $d->total;
  my $size = shift || $d->size;
  $total += $d->zmass;
  return $d->{nz}->mean($total,$size);
}

## $H = $d->entropy()
## $H = $d->entropy($total)
*H = \&entropy;
sub entropy {
  my $d = shift;
  my $total = shift||$d->total;
  my $H = $d->{nz}->entropy($total);
  my $nz = $d->nZero;
  if ($d->{zmass} && $nz) {
    #$H += $nz * ($d->{zmass}/$nz * log($total/($d->{zmass}/$nz))/log(2));
    $H += $d->{zmass} * log($total/($d->{zmass}/$nz))/log(2) if ($d->{zmass} && $nz);
  }
  return $H;
}


##======================================================================
## I/O: Native

## $bool = saveNative($file_or_fh)
##   + zmass, N are not saved in native format
sub saveNative {
  my $d = shift;
  return $d if ($d->{nz}->saveNative(@_));
  return undef;
}

## $bool = loadNative($file_or_fh)
sub loadNative {
  my $d = shift;
  return $d if ($d->{nz}->loadNative(@_));
  return undef;
}


##======================================================================
## I/O: XML

## $node = $obj->saveXMLNode()
sub saveXMLNode {
  my $d = shift;
  (my $nodename = ref($d)) =~ s/::/./g;
  my $node = XML::LibXML::Element->new($nodename);
  $node->setAttribute('size', defined($d->{size}) ? $d->{size} : 0);
  $node->setAttribute('zmass', defined($d->{zmass}) ? $d->{zmass} : 0);
  $node->appendChild($d->{nz}->saveXMLNode(@_));
  return $node;
}

## $obj = $obj->loadXMLNode($node)
sub loadXMLNode {
  my ($d,$node) = @_;
  (my $nodename = ref($d)) =~ s/::/./g;
  carp( __PACKAGE__ , "::loadXMLNode() expected '$nodename' element, got '", $node->nodeName, "'\n")
    if ($node->nodeName ne $nodename);
  $d->{zmass} = $node->getAttribute('zmass');
  $d->{size} = $node->getAttribute('size');
  $d->{nz}->loadXMLNode($node->firstChild);
  return $d;
}

##======================================================================
## AUTOLOAD: pass to 'nz' member
sub DESTROY {}

our $AUTOLOAD;
sub AUTOLOAD {
  my $d = shift;
  return undef if (!defined($d) || !defined($d->{nz}));
  (my $name = $AUTOLOAD) =~ s/.*:://; ##-- strip qualification
  my ($sub);
  if (!($sub=$d->{nz}->can($name))) {
    croak( __PACKAGE__ , "::$name() not defined in AUTOLOAD.\n");
  }
  return &$sub($d->{nz},@_);
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
