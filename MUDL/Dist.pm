#-*- Mode: Perl -*-

## File: MUDL::Dist.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: distributions
##======================================================================

package MUDL::Dist;
use MUDL::Object;
use IO::File;
use PDL;
use PDL::Fit::Linfit;
use Carp;

our @ISA = qw(MUDL::Object);

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
  $H += $_/$total * log($total/$_)/log(2) foreach (grep { $_ != 0 } values(%$d));
  return $H;
}




##======================================================================
## I/O: Native
##======================================================================

sub DEFAULT_SORT {
  my $d = shift;
  return sub { $d->{$b} <=> $d->{$a}; };
}

## $obj = $obj->saveNativeFh($fh,%args)
##  + %args : sort=>\&sortFunc
sub saveNativeFh {
  my ($d,$fh,%args) = @_;
  return undef if (!$d || !$fh);
  my $sortsub = $args{sort} ? $args{sort} : DEFAULT_SORT($d);
  foreach my $e (sort { &$sortsub($a,$b) } keys(%$d)) {
    $fh->print($e, "\t", $d->{$e}, "\n");
  }
  return $d;
}

## $obj = $class_or_obj->loadNativeFh($fh,%args)
sub loadNativeFh {
  my ($d,$fh) = @_;
  return undef if (!$d || !$fh);
  $d = $d->new if (!ref($d));

  my ($line);
  while (defined($line=$fh->getline)) {
    chomp $line;
    next if ($line =~ /^\s*$/);
    if ($line !~ /^(.*\S)\s+([\+\-\d\.]+)$/) {
      warn( __PACKAGE__ , "::loadNativeFh(): parse error at input line ", $fh->input_line_number);
      next;
    }
    $d->{$1} = $2;
  }

  return $d;
}


##======================================================================
## I/O: XML
##======================================================================

## $node = $dist->entry2XMLNode($event,$freq)
sub entry2XMLNode {
  #my ($d,$e,$f) = @_;
  my $node = XML::LibXML::Element->new('event');
  $node->setAttribute('f',$_[2]);
  $node->appendText($_[1]);
  return $node;
}

## undef = $dist->XMLNode2Entry($node)
sub XMLNode2Entry {
  my ($d,$node) = @_;
  $d->{$node->textContent} = $node->getAttribute('f');
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
