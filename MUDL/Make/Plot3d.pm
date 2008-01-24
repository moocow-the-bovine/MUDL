##-*- Mode: CPerl -*-

## File: MUDL::Make::Plot3d.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: make administration:
##    config (pseudo-)fields: plot: 3d
##======================================================================

package MUDL::Make::Plot3d;
use MUDL::Make::Plot qw(:utils);
use MUDL::Make::Fields qw(:utils);
use MUDL::Make::PlotData;
use strict;
use Carp;
our @ISA = qw(MUDL::Make::Plot);

##======================================================================
## Globals

##======================================================================
## Constructor(s)

## $obj = $class_or_obj->new(%args)
##  + %args:
##     x=>$x_field_spec,       ##-- a field-specification as per MUDL::Make::Fields
##     y=>$y_field_spec,       ##-- a field-specification as per MUDL::Make::Fields
##     z=>$y_field_spec,       ##-- a field-specification as per MUDL::Make::Fields
##     configs=>\@configs,     ##-- array-ref of MUDL::Make::Config objects
##
##  + Display Options:
##     term  =>$string,        ##-- terminal type (default=undef [not set])
##
##     xlabel=>$string,        ##-- default: $x_field->{title}
##     ylabel=>$string,        ##-- default: $y_field->{title}
##     zlabel=>$string,        ##-- default: $z_field->{title}
##
##     xrange=>$string,        ##-- default: undef (not set)
##     yrange=>$string,        ##-- default: undef (not set)
##     zrange=>$string,        ##-- default: undef (not set)
##
##     view=>$string,          ##-- GNUplot 'set view ...' (default=undef [not set])
##
##     using=>$which,          ##-- default: undef (none ~ 1:2:3)
##     with=>$how,             ##-- default: 'lp'
##     title=>$window_title,   ##-- global plot title
##     smooth=>$how,           ##-- default: undef (none)
##
##     ##-- new 3d options
##     pm3d => $bool,          ##-- whether to 'set pm3d;':    (default: none)
##     hidden3d => $bool,      ##-- whether to 'set hidden3d;' (default: none)
##     isosamples => $string,  ##-- if defined, "set isosamples $string;" is called (default: none)
##
##     ...?
##
##  + I/O Options:
##     outfile=>$file,         ##-- script output file (default=stdout)
##     datadir=>$dir,          ##-- default='.',
##     datasuffix=>$suffix,    ##-- default='.dat'
##
##  + Agglomeration Options:
##     ptitle=>$code_string,   ##-- eval'd wrt each data point (config); should return its title (key)
##                             ##   + vars instantiated:
##                             ##      $_       =  $field_data_hash,
##                             ##      %_       = %$field_data_hash,
##                             ##      $cfg     =  $config,
##                             ##      $configs = \@configs,
##                             ##      $auto    =  $autogenerated_title_string, ##-- lrlab+differentium keys
##     notitle=>$bool,         ##-- if true, no plot title will appear in the plot
##                             ##   + we need a real 'ptitle' as a hash key!
##
##  + Low-level data:
##     plots => \%plots,       ##-- plot data { $title1=>{points=>\@fdata1, ... }, ... }
##     compiled=>$bool,        ##-- true iff plots has been compiled
sub new {
  my $that = shift;
  my $plot =  $that->SUPER::new(
				##-- User Options
				'x'=>'stage',
				'y'=>'emi',
				'z'=>'pr:g',
				#'configs'=>[],

				#'ptitle'=>'"$auto"', ##-- use auto-generated key
				#ptitle=>undef,        ##-- use auto-generated key (more efficient)
				#notitle=>0,

				##-- I/O options
				#outfile=>'-',
				#datadir=>'.',
				#datasuffix=>'.dat',

				##-- Display options
				#term=>undef,   ##-- not set

				#xlabel=>undef, ##-- use field title
				#ylabel=>undef, ##-- use field title
				zlabel=>undef, ##-- use field title

				#xrange=>undef, ##-- not set
				#yrange=>undef, ##-- not set
				zrange=>undef, ##-- not set

				with=>'lp',
				#title=>undef,  ##-- not set
				#smooth=>undef,
				#using=>undef,

				##-- 3d options
				pm3d       => undef,
				hidden3d   => undef,
				isosamples => undef,

				##-- Low-level data
				#mfields=>MUDL::Make::Fields->new(),
				#plots=>{},
				#compiled=>0,

				@_
			      );

  return $plot;
}

## $obj = $class_or_obj->newFull(%args)
##  + creates & compiles

#(inherited)

##======================================================================
## Accessors

##----------------------------------------------------------------------
## Methods: plots (compiled)

## \%plots = $plot->plots()
##   + just returns $plot->{plots} if compiled; otherwise calls compile()

#(inherited)

##======================================================================
## Methods

##----------------------------------------------------------------------
## Methods: compile

## $plot = $plot->compile()
##   + just returns $plot if already compiled; otherwise calls compile()
sub compile {
  my $plot = shift;

  ##-- clear {plots} partitioning
  my $plots = $plot->{plots};
  %$plots = qw();

  ##-- setup $mfields
  my $configs = $plot->{configs};
  my $mfields = $plot->{mfields};
  $mfields->configs($configs);
  $mfields->fields(@$plot{qw(x y)}, 'plotKeyDefault');
  my $xfields    = $mfields->xfields();

  ##-- get auto-fields
  my @autofields   = sort {$a->{name} cmp $b->{name}} @$xfields[2..$#$xfields];
  my @autonames    = map {$_->{name}} @autofields;

  ##-- Get field-data hashes
  my @fdata = $mfields->fieldData(@$configs);

  ##-- Partition plot points (field-data hashes) by 
  my @dimnames = map { $_->{name} } @$xfields[0,1,2]; ##!!!

  do {
    our (%_);
    my ($fdata,$cfg,$auto,$ptitle);
    foreach (@fdata) {
      ##-- get the title of this config
      if (!defined($plot->{ptitle})) {
	##-- default plot title: easy
	$ptitle = $auto = autokey($_, \@autonames);
      } else {
	##-- user-defined plot title: get variables the user might refer to
	%_      = %$_;
	$cfg    = $_->{_};
	$auto   = autokey($_, \@autonames);
	$ptitle = eval $plot->{ptitle};
	warn(ref($plot)."::compile(): error evaluating plot-title code ($plot->{ptitle}): $@") if ($@);
      }

      ##-- setup '__point__' field-data item
      $_->{__point__} = [@$_{@dimnames}];

      ##-- sort this field-hash into the dataset for its title
      $plots->{$ptitle} = MUDL::Make::PlotData->new(
						    ndims=>3,
						    title=>$ptitle,
						    using=>$plot->{using},
						    notitle=>$plot->{notitle},
						    smooth=>$plot->{smooth},
						    with=>$plot->{with},
						    datadir=>$plot->{datadir},
						    datasuffix=>$plot->{datasuffix},
						   )
	if (!$plots->{$ptitle});

      push(@{$plots->{$ptitle}{points}}, $_);
    }
  };

  ##-- sort datasets
  $_->sortPoints() foreach (values(%$plots));


  ##-- get plot options
  if (!defined($plot->{xlabel})) {
    $plot->{xlabel} = $xfields->[0]{title};
    $plot->{xlabel} =~ s/^\s+//;
    $plot->{xlabel} =~ s/\s+$//;
  }
  if (!defined($plot->{ylabel})) {
    $plot->{ylabel} = $xfields->[1]{title};
    $plot->{ylabel} =~ s/^\s+//;
    $plot->{ylabel} =~ s/\s+$//;
  }
  if (!defined($plot->{zlabel})) {
    $plot->{zlabel} = $xfields->[2]{title};
    $plot->{zlabel} =~ s/^\s+//;
    $plot->{zlabel} =~ s/\s+$//;
  }

  ##-- set 'compiled' flag
  $plot->{compiled} = 1;

  return $plot;
}


##----------------------------------------------------------------------
## Methods: GNUplot: datafiles

## @datafiles = $plot->datafiles();
sub datafiles {
  my $plot = shift;
  my $plots = $plot->plots();
  return map { $_->saveData() } @$plots{sort keys %$plots};
}

## @script = $plot->plotCommand(); ##-- no newlines
## $script = $plot->plotCommand(); ##-- newlines (escaped)
sub plotCommand {
  my $plot  = shift;
  my $plots = $plot->plots();
  my @datafiles = $plot->datafiles();
  my @cmds      = map { $_->plotCommand().', ' } @$plots{sort keys %$plots};
  $cmds[$#cmds] =~ s/\,\s*$//;
  unshift(@cmds, 'splot ');
  return wantarray ? @cmds : (join("\\\n  ", @cmds).";\n");
}


##----------------------------------------------------------------------
## Methods: GNUplot script

## @script = $plot->script(); ##-- no newlines
## $script = $plot->script(); ##-- newlines (escaped)
sub script {
  my $plot = shift;
  my @cmds = (
	      ##-- Terminal
	      (defined($plot->{term})   ? "set term $plot->{term};" : qw()),

	      ##-- Plot Title
	      (defined($plot->{title})   ? "set title \"$plot->{title}\";" : qw()),

	      ##-- Labels
	      (defined($plot->{xlabel}) ? "set xlabel \"$plot->{xlabel}\";" : qw()),
	      (defined($plot->{ylabel}) ? "set ylabel \"$plot->{ylabel}\";" : qw()),
	      (defined($plot->{zlabel}) ? "set zlabel \"$plot->{zlabel}\";" : qw()),

	      ##-- Ranges
	      (defined($plot->{xrange}) ? "set xrange $plot->{xrange};" : qw()),
	      (defined($plot->{yrange}) ? "set yrange $plot->{yrange};" : qw()),
	      (defined($plot->{zrange}) ? "set yrange $plot->{zrange};" : qw()),

	      ##-- View
	      (defined($plot->{view})   ? "set view $plot->{view};" : qw()),

	      ##-- 3d options
	      (defined($plot->{pm3d})  ? ($plot->{pm3d} ? "set pm3d;" : "unset pm3d;")     : qw()),
	      (defined($plot->{hidden3d}) ? ($plot->{hidden3d} ? "set hidden3d;" : "unset hidden3d;") : qw()),
	      (defined($plot->{isosamples}) ? "set isosamples $plot->{isosamples};" : qw()),

	      ##-- Plot command
	      scalar($plot->plotCommand()),
	     );
  return wantarray ? @cmds : join("\n", @cmds);
}

##----------------------------------------------------------------------
## Methods: GNUplot script: save

## $outfile = $plot->saveScript();

#(inherited)


##======================================================================
## Functions

##----------------------------------------------------------------------
## Functions: title-generation

## $key = autokey($fieldDataHash,\@activeFieldNames)

#(imported)

1;

__END__

##---------------------------------------------------------------
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