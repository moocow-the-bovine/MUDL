#-*- Mode: CPerl -*-

## File: MUDL::PDL::Plot.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL Plotting utilities
##======================================================================

package MUDL::PDL::Plot;
use PDL;
#use PDL::Graphics::PGPLOT;
use MUDL::PDL::Smooth ':all';
use MUDL::PDL::Stats;
use strict;

our @ISA = qw(Exporter);
our %EXPORT_TAGS =
  (
   'hist' => ['hist1','loghist','errbin','errbin_gfit',
	      #'logbins', 'makebins','makebins_exp','findbins', ##-- now in MUDL::PDL::Smooth
	     ],
   'qq'   => ['qqfit', ##-- generic
	      'qqplot','qqplotx', ##-- pgplot
	      'gqqplot','gqqplotx', ##-- gnuplot
	     ],
   'gunplot' => ['glogscale', 'glogscale_parse', 'gwith', '%GWITH_OPTS'],
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK   = @{$EXPORT_TAGS{all}};
our @EXPORT      = @EXPORT_OK;

our (%GWITH_OPTS); ##-- for gnuplot

##======================================================================
## histograms & binning

## ($x,$y) = hist1($data,$eps=1,$min=undef,$max=undef,$step=undef)   ##-- list context
## $y      = hist1($data,$eps=1,$min=undef,$max=undef,$step=undef)   ##-- scalar context
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
## $y      = loghist($data,$eps=1,$min=undef,$max=undef,$step=undef)   ##-- scalar context
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


##======================================================================
## Plots

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
  my ($eps,$xeps,$yeps) = @$opts{qw(eps xeps yeps)};
  $eps = 0 if (!defined($eps));
  $xeps = $eps if (!defined($xeps));
  $yeps = $eps if (!defined($yeps));
  delete(@$opts{qw(eps xeps yeps)});

  return errb($x+$xeps,$y->zeroes+$yeps, undef,$x->zeroes+$xeps, undef,$y+$yeps, $opts);
}

##======================================================================
## distribution comparison test: quantile-quantile plot (generic)

## ($xline,$yline,$ycoeffs) = qqfit($xraw,$yraw,$opts)
##  + see: http://www.nist.gov/stat.handbook
##  + returned values are independently sorted
##  + %$opts:
##     nosort => $bool,  ##-- if true, data is assumed already flat and independently sorted
##     unique => $bool,  ##-- if true, line is fit to unique values only
BEGIN { *PDL::qqfit = \&qqfit; }
sub qqfit {
  my ($xdata,$ydata,$opts) = @_;

  ##-- require (independently) sorted data
  if ( !($opts && $opts->{nosort}) ) {
    $xdata = $xdata->flat->qsort;
    $ydata = $ydata->flat->qsort;
  }

  ##-- line() plot (fit $xdata->$ydata)
  my ($yfit,$coeffs) = $ydata->mooLinfit($xdata,%{$opts||{}});
  return ($xdata,$yfit,$coeffs);
}

## undef = qqplotx($xdata,$ydata)
## undef = qqplotx($xdata,$ydata,\%commonOpts)
## undef = qqplotx($xdata,$ydata,\%pointOpts,\%lineOpts)
##  + see: http://www.nist.gov/stat.handbook
##  + additional %commonOpts, %pointOpts:
##     noline => $bool,  ##-- if true, no line is drawn
##     unique => $bool,  ##-- if true, line is fit to unique values only
##  + additional \%lineOpts
##     hide => $bool,    ##-- draw no line
BEGIN { *PDL::qqplotx = \&qqplotx; }
sub qqplotx {
  my ($xdata,$ydata,$popts,$lopts) = @_;

  require PDL::Graphics::PGPLOT;
  PDL::Graphics::PGPLOT->import();
  no strict 'subs';

  ##-- require sorted data
  $xdata = $xdata->flat->qsort;
  $ydata = $ydata->flat->qsort;

  ##-- points() plot
  my %popts = %{$popts||{}};
  my %lopts = %{$lopts||\%popts};
  my $noline   = $popts{noline} || (defined($lopts) && $lopts{hide});
  delete(@popts{'noline','uniqline'});
  delete(@lopts{'hide','uniq'});
  points( $xdata, $ydata, $popts );

  ##-- line() plot (fit $xdata->$ydata)
  if (!$noline) {
    my ($xfit,$yfit,$coeffs) = $xdata->qqfit($ydata,{%lopts,nosort=>1});
    hold();
    line( $xdata, $coeffs->slice("(0)")*$xdata+$coeffs->slice("(1)"), \%lopts );
    release();
  }
}

## @gnuplot = gqqplotx($xdata,$ydata,\%globalOpts,\%pointOpts,\%lineOpts)
##  + generic q-q plot using PDL::Graphics::Gnuplot
##  + see: http://www.nist.gov/stat.handbook
##  + additional %globalOpts:
##     noline  => $bool,  ##-- if true, no line is drawn
##     unique  => $bool,  ##-- if true, line is fit to unique values only
##     noplot  => $bool,  ##-- if true, plot() is not actually called
##     logfit => $bool,   ##-- fit log values
##  + additional %globalOpts,%pointOpts:
##     pt => $point_type,
##     ps => $point_type,
##     pc => $point_color
##  + additional %globalOpts,%lineOpts:
##     lt => $line_type,
##     lw => $line_type,
##     lc => $line_color
BEGIN { *PDL::gqqplotx = \&gqqplotx; }
sub gqqplotx {
  my ($xdata,$ydata,$gopts,$popts,$lopts) = @_;

  ##-- require sorted data
  $xdata = $xdata->flat->qsort;
  $ydata = $ydata->flat->qsort;

  ##-- points() plot
  my %gopts = %{$gopts||{}};
  my %popts = %{$popts||{}};
  my %lopts = %{$lopts||{}};
  my $noline  = $gopts{noline} || $lopts{hide};
  my @nokeys = (qw(noline unique noplot),keys(%GWITH_OPTS));
  delete @gopts{@nokeys};
  delete @popts{@nokeys};
  delete @lopts{@nokeys};

  ##--points
  my %gp = (%{$gopts||{}},%{$popts||{}});
  $gp{lc} = $gp{pc} if ($gp{pc});
  my @plot = ({with=>gwith('p',%gp), %popts}, $xdata,$ydata);

  ##-- line() plot (fit $xdata->$ydata)
  if (!$noline) {
    my ($xfit,$yfit,$coeffs);
    if ($gopts && $gopts->{logfit}) {
      ($xfit,$yfit,$coeffs) = ($xdata,$ydata->loglinfit($xdata,%{$gopts||{}},%{$lopts||{}}));
      $coeffs = $coeffs->slice("-1:0");
    } else {
      ($xfit,$yfit,$coeffs) = $xdata->qqfit($ydata,{%lopts,nosort=>1});
    }
    my %gl = (%{$gopts||{}},%{$lopts||{}});
    push(@plot, {with=>gwith('l',%gl),%lopts}, $xfit,$yfit);
  }

  ##-- actual plot
  if ( !($gopts && $gopts->{noplot}) ) {
    require PDL::Graphics::Gnuplot;
    PDL::Graphics::Gnuplot::plot(%gopts, @plot);
  }
  return @plot;
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

  require PDL::Graphics::PGPLOT;
  PDL::Graphics::PGPLOT->import();
  no strict 'subs';

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

## undef = gqqplot($data,\%globalOpts,\%pointOpts,\%lineOpts)
##  + PDL::Graphics::Gnuplot version
##  + compares vs. standard normal distribution
##  + see: http://en.wikipedia.org/wiki/Q-Q_plot,
##         http://www.nist.gov/stat.handbook
BEGIN { *PDL::gqqplot = \&gqqplot; }
sub gqqplot {
  my ($data,$gopts,$popts,$lopts) = @_;

  ##-- options
  my %gopts = %{$gopts||{}};
  $gopts{xlabel} = 'Normal Theoretical Quantiles' if (!defined($gopts{xtitle}));
  $gopts{ylabel} = 'Sample Quantiles' if (!defined($gopts{ytitle}));
  $gopts{title}  = 'Q-Q Plot' if (!defined($gopts{title}));

  my $uosm  = $data->flat->sequence->double->uosm();  ##-- uniform order statistic medians
  my $mu    = $data->flat->double->mean;
  my $sigma = $data->flat->double->stddev;
  my $qvals = gaussqvals($uosm,0,1);                  ##-- (standard) normal theoretical values

  return gqqplotx($qvals, ($data-$mu/$sigma), \%gopts,$popts,$lopts);
}

##======================================================================
## gnuplot: logscale parsing

## $with = gwith($style,%opts)
##  + parses 'with' option hash for gnuplot
##  + e.g. plot({with=>gwith('p',linetype=>2,lw=>4,nohidden3d=>1,nosurface=>1)}, $x,$y)
%GWITH_OPTS = (map {($_=>undef)}
	       qw(linestyle ls), qw(linetype lt), qw(linewidth lw), qw(linecolor lc),
	       qw(pointtype pt), qw(pointsize ps),
	       qw(fill fs),
	       qw(nohidden3d nocontours nosurface),
	       qw(palette));
sub gwith {
  my ($style,%opts) = @_;
  return join(' ',
	      $style,
	      map {"$_ $opts{$_}"}
	      grep {exists($GWITH_OPTS{$_}) && defined($opts{$_})}
	      sort keys %opts);
}

## \%logscale = glogscale_parse( \%which, \@which, $which, $default, ...)
##  + parses a list of logscale specs $which
##  + $which elements can be an ARRAY, HASH-ref or SCALAR
##    - HASH  : (strictest) map of the form $SCALAR=>$base_or_0
##    - ARRAY : (less strict) elements of $SCALAR
##    - SCALAR: matches regex ($axes,$base)=/((x|y|cb)*(=\d+)?)*/ or ($default)=/^\d+$/
##  + in returns a strict HASH of the form ($axis=>$base, ...)
sub glogscale_parse {
  my $scale = {};
  my $base_default = 10;
  foreach my $which (@_) {
    if (!defined($which)) {
      next;
    } elsif (UNIVERSAL::isa($which,'HASH')) {
      foreach (keys %$which) {
	my $keyscale = glogscale_parse($_);
	@$scale{keys %$keyscale} = map {$which->{$_}} keys %$keyscale;
      }
    } elsif (UNIVERSAL::isa($which,'ARRAY')) {
      foreach (@$which) {
	my $keyscale = glogscale_parse($_);
	@$scale{keys %$keyscale} = values %$keyscale;
      }
    } elsif ($which =~ /^[\d\.]+$/) {
      $base_default = $which;
    } else {
      foreach my $spec (split(/[\s,]+/,$which)) {
	my ($axes,$base) = split(/=/,$spec);
	$base = $base_default if (!defined($base) || $base eq '');
	while ($axes =~ /((?:cb)|(?:x2)|(?:y2)|[xyz])/g) {
	  $scale->{$1} = $base;
	}
      }
    }
  }
  foreach (values %$scale) {
    $_ = 0 if (!$_);
  }
  return $scale;
}

## @commands = glogscale( $which, ... )
## $commands = glogscale( $which, ... )
##  + wrapper for gnuplot logscale commands
sub glogscale {
  my $scale = glogscale_parse(@_);
  my @cmds  = map {$scale->{$_} ? "set logscale $_ $scale->{$_};" : "unset logscale $_;"} keys %$scale;
  return wantarray ? @cmds : join(" ",@cmds);
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

  require PDL::Graphics::PGPLOT;
  PDL::Graphics::PGPLOT->import();
  no strict 'subs';

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
  hold();
  line($gx,$gy,{xrange=>$xr,yrange=>$yr,%$gfopts});
  release();
}



1;
