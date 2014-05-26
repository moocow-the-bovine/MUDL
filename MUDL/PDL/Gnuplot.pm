##-*- Mode: CPerl -*-

## File: MUDL::PDL::Gnuplot.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL Plotting utilities (gnuplot)
##======================================================================

package MUDL::PDL::Gnuplot;
use PDL;
use PDL::Graphics::Gnuplot;
use MUDL::PDL::Smooth ':all';
use MUDL::PDL::Stats;
use strict;

our @ISA = qw(Exporter);
our %EXPORT_TAGS =
  (
   'plot'  => ['gplot', 'gplot3d'],
   'qq'    => ['gqqplot','gqqplotx'],
   'utils' => ['gp_logscale', 'gp_logscale_parse', 'gp_with', '%GP_WITH_OPTS', 'gp_range'],
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK   = @{$EXPORT_TAGS{all}};
our @EXPORT      = @EXPORT_OK;

our (%GP_WITH_OPTS); ##-- for gp_with()

##======================================================================
## High-level wrapper

## @plotcmds = gplot(%global_opts, \%curve1_opts,@curve1_pdls, ...)
##  + wrapper for PDL::Graphics::Gnuplot::plot() with additional options:
##  + new %global_opts:
##     logscale => $logscale,	##-- see gp_logscale()
##     key      => $pos,        ##-- key position
##     dgrid3d  => $dgrid3d,	##-- see gp_dgrid3d()
##     grid     => $grid,       ##-- -> "set grid $grid;"
##     border   => $border,	##-- -> "set border $border;"
##     hidden3d => $bool,	##-- -> "(un)set hidden3d;"
##     view     => $view,	##-- -> "(un)set view $view;"
##     xformat  => $format,     ##-- -> "set format x \"$format\";"
##     yformat  => $format,     ##-- -> "set format y \"$format\";"
##     zformat  => $format,     ##-- -> "set format z \"$format\";"
##     [xyzcb]tics => $tics,     ##-- -> "set [xyzcb]tics $tics";
##     [xyzcb]range => $range,    ##-- -> "[xyzcb](min|max)" => gp_range($range),
##     autoscale => $autoscale, ##-- --> "set autoscale $autoscale;"
##     noplot   => $bool,       ##-- if true, plot() isn't actually called
##     hardcopy => $file,       ##-- recognizes "*.gp", "*.gnuplot"
sub gplot {
  ##-- parse arguments
  my ($opts,@curves) = gp_options(@_);
  my %go = %$opts;

  ##-- get 'extracmds' as array (copied)
  my $cmds = [UNIVERSAL::isa($go{extracmds},'ARRAY') ? @{$go{extracmds}} : ($go{extracmds}||'')];
  $go{extracmds} = $cmds;

  ##-- option: logscale
  my $logscale = gp_logscale_parse($go{logscale});
  unshift(@$cmds, gp_logscale($logscale)) if (%$logscale);
  delete($go{logscale});

  ##-- option: key (e.g. key=>'off', key=>'outside left center box')
  unshift(@$cmds, ($go{key} ? '' : 'un')."set key ".($go{key}||'').";") if (defined($go{key}));
  delete($go{key});

  ##-- option: grid (e.g. grid=>'ztics lt 3 lw 0.25')
  unshift(@$cmds, ($go{grid} ? '' : 'un')."set grid ".($go{grid}||'').";") if (defined($go{grid}));
  delete($go{grid});

  ##-- option: dgrid3d
  my $dgrid3d = gp_dgrid3d($go{dgrid3d});
  unshift(@$cmds, "set dgrid3d $dgrid3d;") if ($dgrid3d);
  delete($go{dgrid3d});

  ##-- option: border
  unshift(@$cmds, "set border $go{border};") if (defined($go{border}));
  delete($go{border});

  ##-- option: hidden3d
  unshift(@$cmds, ($go{hidden3d} ? '' : 'un')."set hidden3d;") if (defined($go{hidden3d}));
  delete($go{hidden3d});

  ##-- option: view
  unshift(@$cmds, ($go{view} ? '' : 'un')."set view ".($go{view}||'').";") if (defined($go{view}));
  delete($go{view});

  ##-- option: autoscale
  unshift(@$cmds, ($go{autoscale} ? '' : 'un')."set autoscale ".($go{autoscale}||'').";") if (defined($go{autoscale}));
  delete($go{autoscale});

  ##-- option: ([xyz]|cb|x2|xy|z2)(format|tics|range)
  foreach my $axis (qw(x y z cb x2 y2 z2)) {
    if (defined($go{"${axis}range"})) {
      my $rng = gp_range($go{"${axis}range"});
      $go{"${axis}min"} = $rng->[0] if ($rng->[0] ne '*');
      $go{"${axis}max"} = $rng->[1] if ($rng->[1] ne '*');
    }
    unshift(@$cmds, "set format ${axis} \"".$go{"${axis}format"}."\";") if (defined($go{"${axis}format"}));
    unshift(@$cmds, "set  ${axis}xtics ".$go{"${axis}tics"}.";") if (defined($go{"${axis}tics"}));
    unshift(@$cmds, "set m${axis}xtics ".$go{"m${axis}tics"}.";") if (defined($go{"m${axis}tics"}));
    delete(@go{"${axis}range", "${axis}format", "${axis}tics", "m${axis}tics"});
  }

  ##-- option: gnuplot hardcopy: (hardcopy=>"FILE.gp") or (hardcopy=>$filehandle)
  my ($gpfile,$gpfh,$oldout);
  if ($go{hardcopy} && (ref($go{hardcopy}) || $go{hardcopy} =~ /\.(?:gp|gnuplot)$/)) {
    $go{dump}=1;
    $gpfile = $go{hardcopy};

    $gpfh = ref($gpfile) ? $gpfile : IO::File->new(">$gpfile");
    die(__PACKAGE__ . "::gplot(): could not open file '$gpfile': $!") if (!$gpfh);
    open($oldout, ">&STDOUT")
      or die(__PACKAGE__ . "::gplot(): could not save STDOUT: $!");
    open(STDOUT, ">&", $gpfh)
      or die(__PACKAGE__ . "::gplot(): could not dup STDOUT to $gpfile: $!");
    delete($go{hardcopy});
  }

  ##-- actual plot
  delete($go{noplot});
  if (!$opts->{noplot}) {
    require PDL::Graphics::Gnuplot;
    PDL::Graphics::Gnuplot::plot(%go,@curves);
  }

  ##-- restore old stdout, etc
  if (defined($oldout)) {
    open(STDOUT, '>&', $oldout)
      or die(__PACKAGE__ . "::gplot(): could not restore STDOUT: $!");
    $gpfh->close() if (defined($gpfh) && !ref($gpfile));
  }

  return (\%go,@curves);
}

## @gplot = gplot3d(@args)
sub gplot3d {
  gplot('3d'=>1, @_);
}

##======================================================================
## utils

## (\%globals,@curves) = gp_options(@plotargs)
##  + parses global options from PDL::Graphics::Gnuplot style commands
##  + really just returns everything up to the first reference in @plotargs
sub gp_options {
  my %gopts = qw();
  my %lopts = map {($_=>undef)} qw(with tuplesize legend using);
  my ($opt,$arg);
  while (@_) {
    $opt = shift;
    if (ref($opt) || exists($lopts{$opt})) {
      unshift(@_,$opt);
      last;
    }
    $gopts{$opt}=shift;
  }
  return wantarray ? (\%gopts,@_) : \%gopts;
}

## \@minmax = gp_range($str)
## \@minmax = gp_range([$min,$max])
##  + parses range
sub gp_range {
  my $rng = shift;

  if (!ref($rng)) {
    $rng =~ s/^\s*\[//;
    $rng =~ s/^\s*\]//;
    $rng = [split(/\s*\:\s*/,$rng,2)];
  }
  if (UNIVERSAL::isa($rng,'ARRAY')) {
    $rng = [@$rng];
  }
  $rng->[0] = '*' if (!defined($rng->[0]) || $rng->[0] eq '');
  $rng->[1] = '*' if (!defined($rng->[1]) || $rng->[0] eq '');
  return $rng;
}

## $dgrid3d = gp_dgrid3d( $string )
## $dgrid3d = gp_dgrid3d( \@array )
## $dgrid3d = gp_dgrid3d( \%hash )
##  + @array = ($rows,$cols,$smooth,$dx,$dy)
##  + %hash  = (
##     rows   => $rows,
##     cols   => $cols,
##     smooth => $smooth,
##     norm   => $norm,  ##-- for qnorm
##     dx     => $dx,
##     dy     => $dy,
##    )
##  + "$smooth" is one of qw(splines gauss cauchy exp box hann)
##     - "splines takes no argument
##     - "qnorm" smoothing takes norm (e.g. 2 for Euclidean distance)
##     - other smooth styles take optional $dx,$dy in units of the data itself
##  + maybe i'm misunderstanding, but gnuplot seems to reverse $rows and $cols
sub gp_dgrid3d {
  my $spec = shift;
  return $spec if (!ref($spec));
  my $grid = '';
  if (UNIVERSAL::isa($spec,'ARRAY')) {
    my ($rows,$cols,$smooth,$dx,$dy) = @$spec;
    return (($rows ? $rows : '')
	    .($cols ? ",$cols" : '')
	    .($smooth
	      ? (" $smooth"
		 .($dx ? " $dx" : '')
		 .($dy ? ",$dy" : ''))
	      : ''));
  }
  elsif (UNIVERSAL::isa($spec,'HASH')) {
    return (($spec->{rows} || '')
	    .($spec->{cols} ? ",$spec->{cols}" : '')
	    .($spec->{smooth}
	      ? (" $spec->{smooth}"
		 .($spec->{smooth} eq 'qnorm'
		   ? ($spec->{norm} ? " $spec->{norm}" : '')
		   : (($spec->{dx} ? " $spec->{dx}" : '')
		      .($spec->{dy} ? ",$spec->{dy}" : ''))))
	      : ''));
  }
  return $spec;
}



## $with = gp_with($style,%opts)
##  + parses 'with' option hash for gnuplot
##  + e.g. plot({with=>gp_with('p',linetype=>2,lw=>4,nohidden3d=>1,nosurface=>1)}, $x,$y)
%GP_WITH_OPTS = (map {($_=>undef)}
		 qw(linestyle ls), qw(linetype lt), qw(linewidth lw), qw(linecolor lc),
		 qw(pointtype pt), qw(pointsize ps),
		 qw(fill fs),
		 qw(nohidden3d nocontours nosurface),
		 qw(palette));
sub gp_with {
  my ($style,%opts) = @_;
  return join(' ',
	      $style,
	      map {"$_ $opts{$_}"}
	      grep {exists($GP_WITH_OPTS{$_}) && defined($opts{$_})}
	      sort keys %opts);
}

## \%logscale = gp_logscale_parse( \%which, \@which, $which, $default, ...)
##  + parses a list of logscale specs $which
##  + $which elements can be an ARRAY, HASH-ref or SCALAR
##    - HASH  : (strictest) map of the form $SCALAR=>$base_or_0
##    - ARRAY : (less strict) elements of $SCALAR
##    - SCALAR: matches regex ($axes,$base)=/((x|y|cb)*(=\d+)?)*/ or ($default)=/^\d+$/
##  + in returns a strict HASH of the form ($axis=>$base, ...)
sub gp_logscale_parse {
  my $scale = {};
  my $base_default = 10;
  foreach my $which (@_) {
    if (!defined($which)) {
      next;
    } elsif (UNIVERSAL::isa($which,'HASH')) {
      foreach (keys %$which) {
	my $keyscale = gp_logscale_parse($_);
	@$scale{keys %$keyscale} = map {$which->{$_}} keys %$keyscale;
      }
    } elsif (UNIVERSAL::isa($which,'ARRAY')) {
      foreach (@$which) {
	my $keyscale = gp_logscale_parse($_);
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

## @commands = gp_logscale( $which, ... )
## $commands = gp_logscale( $which, ... )
##  + wrapper for gnuplot logscale commands
sub gp_logscale {
  my $scale = gp_logscale_parse(@_);
  my @cmds  = map {$scale->{$_} ? "set logscale $_ $scale->{$_};" : "unset logscale $_;"} keys %$scale;
  return wantarray ? @cmds : join(" ",@cmds);
}


##======================================================================
## distribution comparison test: quantile-quantile plot (generic)

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
  my @nokeys = (qw(noline unique noplot logfit),keys(%GP_WITH_OPTS));
  delete @gopts{@nokeys};
  delete @popts{@nokeys};
  delete @lopts{@nokeys};

  ##--points
  my %gp = (%{$gopts||{}},%{$popts||{}});
  $gp{lc} = $gp{pc} if ($gp{pc});
  my @plot = ({with=>gp_with('p',%gp), %popts}, $xdata,$ydata);

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
    push(@plot, {with=>gp_with('l',%gl),%lopts}, $xfit,$yfit);
  }

  ##-- actual plot
  if ( !($gopts && $gopts->{noplot}) ) {
    require PDL::Graphics::Gnuplot;
    PDL::Graphics::Gnuplot::plot(%gopts, @plot);
  }
  return @plot;
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


1;
