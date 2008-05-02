#-*- Mode: CPerl -*-

## File: MUDL::PDL::Plot.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL Plotting utilities
##======================================================================

package MUDL::PDL::Plot;
use PDL;
use PDL::Graphics::PGPLOT;
use MUDL::PDL::Smooth ':all';
use MUDL::PDL::Stats;
use strict;

our @ISA = qw(Exporter);
our %EXPORT_TAGS =
  (
   'hist' => ['hist1','loghist','errbin','errbin_gfit'],
   'qq'   => ['qqplot','qqplotx'],
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK   = @{$EXPORT_TAGS{all}};
our @EXPORT      = @EXPORT_OK;


##======================================================================
## histograms

## ($x,$y) = hist1($data,$eps=1,$min=undef,$max=undef,$step=undef)   ##-- list context
## ($x,$y) = hist1($data,$eps=1,$min=undef,$max=undef,$step=undef)   ##-- array context
##   + really just a wrapper for PDL::Basic::hist() which adds $eps to all vals (x+y)
BEGIN { *PDL::hist1 = \&hist1; }
sub hist1 {
  my ($data, $eps, $min,$max,$step) = @_;
  $eps = 1 if (!defined($eps));
  my ($x,$hist) = hist($data,$min,$max,$step);
  $x    += $eps;
  $hist += $eps;
  return wantarray ? ($x,$hist) : $hist;
}

## ($x,$y) = loghist($data,$eps=1,$min=undef,$max=undef,$step=undef)   ##-- list context
## ($x,$y) = loghist($data,$eps=1,$min=undef,$max=undef,$step=undef)   ##-- array context
##   + really just a wrapper for PDL::Basic::hist() which uses exponentially sized bins
##   + buggy with PDL::Graphics::PGPLOT::autolog(1) set
BEGIN { *PDL::loghist = \&loghist; }
sub loghist {
  my ($data, $eps, $min,$max,$step) = @_;
  $eps = 1 if (!defined($eps));
  my ($x,$hist) = hist(($data+$eps)->log,
		       (defined($min)  ? log($min+$eps)  : undef),
		       (defined($max)  ? log($max+$eps)  : undef),
		       (defined($step) ? log($step+$eps) : undef),
		      );
  $hist += $eps;
  return wantarray ? ($x->exp-$eps,$hist) : $hist;
}

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
  $x = ($y->sequence+1) if (!defined($x));
  $opts = {} if (!defined($opts));
  my $eps = $opts->{eps};
  $eps    = 0 if (!defined($eps));
  delete($opts->{eps});

  return errb($x+$eps,$y->zeroes+$eps, undef,$x->zeroes+$eps, undef,$y+$eps, $opts);
}

##======================================================================
## distribution comparison test: quantile-quantile plot (generic)


## undef = qqplotx($xdata,$ydata)
## undef = qqplotx($xdata,$ydata,\%commonOpts)
## undef = qqplotx($xdata,$ydata,\%pointOpts,\%lineOpts)
##  + see: http://www.nist.gov/stat.handbook
##  + additional %commonOpts, %pointOpts:
##     noline   => $bool,  ##-- if true, no line is drawn
##     uniqline => $bool,  ##-- if true, line is fit to unique values only
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
  my $noline   = $popts->{noline} || (defined($lopts) && $lopts->{hide});
  #my $uniqline = $popts->{noline} || (defined($lopts) && $lopts->{uniq});
  delete(@$popts{'noline','uniqline'});
  delete(@$lopts{'hide','uniq'}) if (defined($lopts));
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

  my $uosm  = $data->flat->sequence->double->uosm();  ##-- uniform order statistic medians
  my $mu    = $data->flat->double->mean;
  my $sigma = $data->flat->double->stddev;
  my $qvals = gaussqvals($uosm,0,1);                  ##-- (standard) normal theoretical values
  return qqplotx($qvals, ($data-$mu/$sigma), $popts,$lopts);
}

##======================================================================
## histogram + gaussian fit

## undef = errbin_gfit($x,$y, \%errbin_opts, \%gfit_opts)
##  + %errbin_opts:
##     peak =>$which,   ##-- false or 'f':frequency, 'p':probability, literal: force value
##     mu   =>$mu,      ##-- if given, replaces fit $mu
##     sigma=>$sigma,   ##-- if given, replaces fit $sigma
##     raw  =>$raw,     ##-- if given, used to fit $mu,$sigma in place of smoothGaussian()
##     nx   =>$nx,      ##-- number of x points for gaussian fit (default=100)
##  + %gfit_opts: none
BEGIN { *PDL::errbin_gfit = \&errbin_gfit; }
sub errbin_gfit {
  my ($x,$y,$ebopts,$gfopts) = @_;
  $ebopts = {} if (!defined($ebopts));
  $gfopts = { color=>'red',%$ebopts } if (!defined($gfopts));
  my %opts  = (%$ebopts,%$gfopts);
  $opts{nx} = 100 if (!$opts{nx});
  my @localkeys = qw(peak mu sigma raw nx);
  delete(@$ebopts{@localkeys});
  delete(@$gfopts{@localkeys});

  ##-- pre-fit
  my @fit = $y->double->smoothGaussian($x->double); ##-- ($yfit,$ypk,$mu,$sigma)

  my ($mu);
  if (defined($opts{mu}))     { $mu = $opts{mu}; }
  elsif (defined($opts{raw})) { $mu = $opts{raw}->double->mean; }
  else                        { $mu = $fit[2]; }

  my ($sigma);
  if (defined($opts{sigma}))  { $sigma = $opts{sigma}; }
  elsif (defined($opts{raw})) { $sigma = $opts{raw}->double->stddev; }
  else                        { $sigma = $fit[3]; }

  my ($y_adj,$gpeak);
  if    (!$opts{peak} || $opts{peak} eq 'f') { $gpeak=$y->max; $y_adj=$y; }
  elsif ( $opts{peak} && $opts{peak} eq 'p') { $gpeak=gausspeak($sigma); $y_adj=$y->double/$y->max*$gpeak; }
  elsif ( $opts{peak} )                      { $gpeak=$opts{peak};       $y_adj=$y->double/$y->max*$gpeak; }

  my $xr = $ebopts->{xrange} ? $ebopts->{xrange} : [$x->minmax];
  my ($gx,$gy) = gausspoints($gpeak,$mu,$sigma, @$xr,$opts{nx});
  my $yr = [0,$y_adj->append($gy)->max];

  errbin($x,$y_adj,{xrange=>$xr,yrange=>$yr,%$ebopts});
  hold;
  line($gx,$gy,{xrange=>$xr,yrange=>$yr,%$gfopts});
  release;
}



1;
