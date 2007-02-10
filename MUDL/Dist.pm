#-*- Mode: CPerl -*-

## File: MUDL::Dist.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: distributions
##======================================================================

package MUDL::Dist;
use MUDL::Object;
use MUDL::Set;
use MUDL::Map;
use IO::File;
use PDL;
use PDL::Fit::Linfit;
use Carp;

use strict;
our @ISA = qw(MUDL::Map);

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
##  -- inherited from MUDL::Map

## @events = $d->events()
sub events { return keys(%{$_[0]}); }

## $domain = $d->domain()
sub domain { return MUDL::Set->new(keys(%{$_[0]})); }

## [$domains] = $d->domains()
*domains = \&getDomains;
sub getDomains { return [$_[0]->domain]; }

## $size = $d->size()
sub size { return scalar(values(%{$_[0]})); }

## \@sizes = $d->sizes()
sub sizes { return [ $_[0]->size ]; }

## 1 = $d->nFields()
sub nFields { return 1; }

##======================================================================
## Manipulators

## $dist = $dist->addDist($dist2)
##  + adds in data from $dist2
sub addDist {
  my ($d1,$d2) = @_;
  my ($k,$v);
  while (($k,$v)=each(%$d2)) {
    $d1->{$k} += $v;
  }
  return $d1;
}

##======================================================================
## Smoothing & Normalization

## $d = $d->normalize()
## $d = $d->normalize($total)
sub normalize {
  my ($d,$total) = @_;
  $total = $d->total if (!$total);
  return $d if ($total==0);
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
  my ($i,$r,$r_lo,$r_hi,$N_r);
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
  my $xdata = pdl([keys   %$d])->inplace->double;
  my $ydata = pdl([values %$d])->inplace->double;
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
  my $xdata = pdl([keys   %$d])->inplace->double->inplace->log;
  my $ydata = pdl([values %$d])->inplace->double->inplace->log;
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

## $p = $d->perplexity()
## $p = $d->perplexity($total)
sub perplexity { return 2**$_[0]->entropy($_[1]); }


##======================================================================
## Pruning
##======================================================================

## $d = $d->prune(%args)
##  + %args:
##    which  => \&code, # $keepit = code($event,$freq,\%args);   # default: pruneZeroSub()
##    keep   => $bool,  # whether keep positive results (true) or prune them (false)
##    bash   => $bool,  # bash pruned symbols to a single symbol
##    bashto => $event, # event to bash pruned symbols to
sub prune {
  my ($d,%args) = @_;
  %args = (which=>$d->pruneByZeroSub, bash=>0, bashto=>'@UNKNOWN', keep=>1, %args);
  my ($k,$v);
  my $bashval = 0;
  while (($k,$v) = each(%$d)) {
    if (($args{keep} && !$args{which}->($k,$v,\%args))
	||
	(!$args{keep} && $args{which}->($k,$v,\%args)))
      {
	delete($d->{$k});
	$bashval += $v;
      }
  }
  ##-- bash
  if ($args{bash} && defined($args{bashto})) {
    $d->{$args{bashto}} = $bashval;
  }
  return $d;
}

##======================================================================
## Pruning: Aliases

## $d = $d->pruneByZero()
sub pruneByZero  { my $d=shift; $d->prune(which=>$d->pruneByZeroSub,  @_); }

## $d = $d->pruneByValue(min=>$min, max=>$max)
sub pruneByValue { my $d=shift; $d->prune(which=>$d->pruneByValueSub(@_),  @_); }

## $d = $d->pruneByRank(min=>$min_rank, max=>$max_rank, ...)
sub pruneByRank  { my $d=shift; $d->prune(which=>$d->pruneByRankSub(@_),  @_); }

## $d = $d->pruneByEnum(enum=>$e, ...)
sub pruneByEnum { my $d=shift; $d->prune(which=>$d->pruneByEnumSub(@_), @_); }

## $d = $d->pruneBySet(set=>$s, ...)
sub pruneBySet { my $d=shift; $d->prune(which=>$d->pruneBySetSub(@_), @_); }

## $d = $d->pruneByRegex(re=>$re, ...)
sub pruneByRegex { my $d=shift; $d->prune(which=>$d->pruneByRegexSub(@_), @_); }

##======================================================================
## Pruning: Selection

## \&prunesub = $d->pruneByZeroSub()
##   + returns sub to prune events with zero values
sub pruneByZeroSub { return shift->pruneByValueSub(min=>1); }

## \&prunesub = $d->pruneByValueSub(%args)
##   + %args: min=>$min, max=>$max
##   + returns sub to prune values not in range $min..$max (inclusive)
sub pruneByValueSub {
  my ($d,%args) = @_;
  my ($min,$max) = @args{qw(min max)};
  return sub {
    (!defined($min) || $_[1] >= $min) && (!defined($max) || $_[1] <= $max);
  };
}

## \&prunesub = $d->pruneByRankSub(%args)
##   + returns sub to prune events by rank
##   + %args:
##      min   => $min_rank,   # (inclusive)
##      max   => $max_rank,   # (inclusive)
##      ranks => $ranks,      # otherwise auto-created
##      sort  => 'asc'|'dsc', # for rank creation
##      qsize => $qsize,      # for rank creation
##      shared=> $bool,       # for rank creation
sub pruneByRankSub {
  my ($d,%args) = @_;
  my $ranks = $args{ranks};
  if (!$ranks) {
    my $rclass = $args{shared} ? 'MUDL::XRanks' : 'MUDL::Ranks';
    $ranks = $rclass->fromDist($d,%args);
    $ranks = MUDL::Quanta->fromRanks($ranks,$args{qsize}) if ($args{qsize});
  }
  my ($min,$max) = @args{qw(min max)};
  return sub {
    (!defined($min) || $ranks->{$_[0]} >= $min) && (!defined($max) || $ranks->{$_[0]} <= $max);
  }
}

## \&prunesub = $d->pruneByRegexSub(re=>$re,...)
##   + returns sub to prune events not matching $re
sub pruneByRegexSub {
  my ($d,%args) = @_;
  #my $re = qr/$args{re}/o;
  my $re = qr/$args{re}/;
  return sub { $_[0] =~ $re; };
}

## \&prunesub = $d->pruneByEnumSub(enum=>$enum,...)
##   + returns sub to prune events not defined by $enum
sub pruneByEnumSub {
  my ($d,%args) = @_;
  my $enum = $args{enum};
  return sub { defined($enum->index($_[0])); };
}

## \&prunesub = pruneBySetSub(set=>$set,...)
##   + returns sub to prune events not defined in $set
sub pruneBySetSub {
  my ($d,%args) = @_;
  my $set = $args{set};
  return sub { defined($set->{$_[0]}); };
}

##======================================================================
## Conversion: enumeration
##======================================================================

## $enum = $d->toEnum()
## $enum = $d->toEnum($enum)
##   + enumerates events, does not alter dist
sub toEnum {
  my ($d,$e) = @_;
  $e = MUDL::Enum->new() if (!$e);
  $e->addSymbol($_) foreach (keys(%$d));
  return $e;
}

## $edist = $d->toEDist()
## $edist = $d->toEDist($enum)
##   + returns a MUDL::EDist using enumerated events
sub toEDist {
  my ($d,$enum) = @_;
  $enum = MUDL::Enum->new() if (!$enum);
  my $ed = MUDL::EDist->new(enum=>$enum);
  @{$ed->{nz}}{map { $enum->addSymbol($_) } keys(%$d)} = values(%$d);
  return $ed;
}


##======================================================================
## I/O: Native
##======================================================================

sub DEFAULT_SORT {
  my $d = shift;
  return sub { $d->{$b} <=> $d->{$a}; };
}
sub ALPHA_SORT {
  return sub { $a cmp $b };
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
sub hashEntry2XMLNode {
  #my ($d,$e,$f) = @_;
  my $node = XML::LibXML::Element->new('event');
  $node->setAttribute('f',$_[2]);
  $node->appendText($_[1]);
  return $node;
}

## undef = $dist->XMLNode2HashEntry($node)
sub XMLNode2HashEntry {
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
