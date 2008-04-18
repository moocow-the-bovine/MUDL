#-*- Mode: CPerl -*-

## File: MUDL::PDL::Plot.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL Plotting utilities
##======================================================================

package MUDL::PDL::Plot;
use PDL;
use PDL::Graphics::PGPLOT;
use MUDL::PDL::Smooth;
use MUDL::PDL::Stats;
use strict;

our @ISA = qw(Exporter);
our %EXPORT_TAGS =
  (
   all => ['errbin','qqplot','qqline'],
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK   = @{$EXPORT_TAGS{all}};
our @EXPORT      = @EXPORT_OK;


##======================================================================
## histograms

## errbin($y,   \%opts)
## errbin($x,$y,\%opts)
##  + like bin($x,$y,\%opts)
sub errbin {
  my ($x,$y,$opts);
  if (@_==1) {
    $y=shift;
  }
  elsif (@_==2) {
    if (UNIVERSAL::isa($_[1],'HASH')) {
      ($y,$opts) = @_;
    } else {
      ($x,$y) = @_;
    }
  }
  else {
    ($x,$y,$opts) = @_;
  }
  $x = $y->sequence if (!defined($x));
  $opts = {} if (!defined($opts));

  return errb($x,$y->zeroes, undef,$x->zeroes, undef,$y, $opts);
}

##======================================================================
## distribution comparison test: quantile-quantile plot

## undef = qqplot($data)
## undef = qqplot($data,\%commonOpts)
## undef = qqplot($data,\%pointOpts,\%lineOpts)
##  + compares vs. standard normal distribution
##  + see: http://en.wikipedia.org/wiki/Q-Q_plot,
##         http://www.nist.gov/stat.handbook
BEGIN { *PDL::qqplot = \&qqplot; }
sub qqplot {
  my ($data,$popts,$lopts) = @_;
  $data  = $data->flat->qsort;  ##-- require sorted data
  $popts  = {} if (!defined($popts));
  $popts->{xtitle} = 'Normal Theoretical Quantiles' if (!defined($popts->{xtitle}));
  $popts->{ytitle} = 'Sample Quantiles' if (!defined($popts->{ytitle}));
  my $noline = $popts->{noline};
  delete($popts->{noline});
  my $uosm  = $data->sequence->uosm();   ##-- uniform order statistic medians
  my $qvals = gaussqvals($uosm);         ##-- (standard) normal theoretical values
  points( $qvals, $data, $popts );
  if (!$noline) {
    my ($qfit,$qcoeffs) = $data->mooLinfit($qvals);
    $lopts = $popts if (!defined($lopts));
    hold;
    line( $qvals, $qcoeffs->slice("(0)")*$qvals+$qcoeffs->slice("(1)"), $lopts );
    release;
  }
}


1;
