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
   all => ['errbin','qqplot','qqplotx'],
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
## distribution comparison test: quantile-quantile plot (generic)


## undef = qqplotx($xdata,$ydata)
## undef = qqplotx($xdata,$ydata,\%commonOpts)
## undef = qqplotx($xdata,$ydata,\%pointOpts,\%lineOpts)
##  + see: http://www.nist.gov/stat.handbook
##  + additional %commonOpts, %pointOpts:
##     noline => $bool,  ##-- if true, no line is drawn
##  + additional \%lineOpts
##     hide => $bool,    ##-- draw no line
BEGIN { *PDL::qqplotx = \&qqplotx; }
sub qqplotx {
  my ($xdata,$ydata,$popts,$lopts) = @_;

  ##-- require sorted data
  $xdata = $xdata->flat->qsort;
  $ydata = $ydata->flat->qsort;

  ##-- points() plot
  $popts  = {} if (!defined($popts));
  my $noline = $popts->{noline} || (defined($lopts) && $lopts->{hide});
  delete($popts->{noline});
  delete($lopts->{hide}) if (defined($lopts));
  points( $xdata, $ydata, $popts );

  ##-- line() plot (fit $xdata->$ydata)
  if (!$noline) {
    my ($yfit,$coeffs) = $ydata->mooLinfit($xdata);
    $lopts = $popts if (!defined($lopts));
    hold;
    line( $xdata, $coeffs->slice("(0)")*$xdata+$coeffs->slice("(1)"), $lopts );
    release;
  }
}

## undef = qqplot($data)
## undef = qqplot($data,\%commonOpts)
## undef = qqplot($data,\%pointOpts,\%lineOpts)
##  + compares vs. standard normal distribution
##  + see: http://en.wikipedia.org/wiki/Q-Q_plot,
##         http://www.nist.gov/stat.handbook
BEGIN { *PDL::qqplot = \&qqplot; }
sub qqplot {
  my ($data,$popts,$lopts) = @_;

  ##-- options
  $popts  = {} if (!defined($popts));
  $popts->{xtitle} = 'Normal Theoretical Quantiles' if (!defined($popts->{xtitle}));
  $popts->{ytitle} = 'Sample Quantiles' if (!defined($popts->{ytitle}));

  my $uosm  = $data->sequence->double->uosm();   ##-- uniform order statistic medians
  my $qvals = gaussqvals($uosm);                 ##-- (standard) normal theoretical values
  return qqplotx($data,$qvals, $popts,$lopts);
}


1;
