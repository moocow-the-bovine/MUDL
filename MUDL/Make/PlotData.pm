##-*- Mode: CPerl -*-

## File: MUDL::Make::PlotData.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: make administration: config (pseudo-)fields: plots: datasets
##======================================================================

package MUDL::Make::PlotData;
use MUDL::Make::Fields qw(:utils);
use IO::File;
use strict;
use Carp;
our @ISA = qw(MUDL::Object);

##======================================================================
## Globals

##======================================================================
## Constructor(s)

## $obj = $class_or_obj->new(%args)
##  + %args:
sub new {
  my $that = shift;
  my $pdata =  $that->SUPER::new(
				 ##-- Data options
				 'ndims'=>2,      ##-- number of dimensions
				 'title'=>'?',    ##-- plot title
				 'notitle'=>0,    ##-- if true, no actual title will be printed
				 'points'=>[],    ##-- data points (MUDL::Make::FieldData objects)
				                  ##    + contain array-ref '__point__'=>[$x,$y,...]

				 ##-- I/O options
				 'datafile'=>undef,   ##-- datafile name (default: auto-generate)
				 'datadir'=>'.',      ##-- datafile directory
				 'gpdatadir'=>undef,  ##-- datafile directory for script
				 'datasuffix'=>'.dat',

				 ##-- Display Options
				 'using' => undef,  ##-- 'using' spec; default=undef (~1:2)
				 'with'  => 'lp',
				 'smooth'=> undef,

				 ##-- User arguments
				 @_
				);

  return $pdata;
}

##======================================================================
## Methods: datafile

## $datafile = $pdata->datafile()
sub datafile {
  my $pdata = shift;
  if (defined($pdata->{datafile})) {
    return $pdata->{datafile};
  }
  my ($file);
  if ($pdata->{title}) {
    $file = "plot.$pdata->{title}";
  }
  else {
    $file = "plot.$pdata";
  }
  $file =~ s/\,/\./g;
  $file =~ s/[\s\<\>\(\)\[\]\:\/\\]+/_/g;
  $file =~ s/\.\_/\./g;
  return $pdata->{datafile} = $pdata->{datadir}.'/'.$file.$pdata->{datasuffix};
}

## $datafile = $pdata->gpdatafile()
sub gpdatafile {
  my $pdata = shift;
  return $pdata->datafile() if (!defined($pdata->{gpdatadir}) || $pdata->{gpdatadir} eq $pdata->{datadir});
  my $datafile = $pdata->datafile();
  $datafile =~ s/^\Q$pdata->{datadir}\E/$pdata->{gpdatadir}/;
  return $datafile;
}


##======================================================================
## Methods: sort

## $pdata = $pdata->sortPoints()
sub sortPoints {
  my $pdata = shift;
  our ($a,$b);
  my ($apoint,$bpoint,$i,$cmp);
  @{$pdata->{points}} =
    sort {
      ($apoint,$bpoint) = ($a->{__point__}, $b->{__point__});
      for ($i=0; $i <= $#$apoint || $i <= $#$bpoint; $i++) {
	$cmp = (defined($apoint->[$i])
		? (defined($bpoint->[$i])
		   ? $apoint->[$i] <=> $bpoint->[$i]
		   : 1)
		: (defined($bpoint->[$i])
		   ? -1
		   : 0));
	return $cmp if ($cmp);
      }
      return 0;
    } @{$pdata->{points}};

  return $pdata;
}

##======================================================================
## Methods: Generation: data

## @data_lines = $pdata->dataLines()
##  + in scalar context, returns data string
##  + *NO* newline character is appended to data lines
sub dataLines {
  my $pdata = shift;
  my @lines = map { join("\t", @{$_->{__point__}}) } @{$pdata->{points}};
  return wantarray ? @lines : (join("\n", @lines)."\n");
}

## $datafile = $pdata->saveData(%args)
##  + saves data, returns datafile
sub saveData {
  my ($pdata,%args) = @_;

  @$pdata{keys(%args)} = values(%args);
  my $file = $pdata->datafile();
  my $fh = IO::File->new(">$file")
    or die(ref($pdata)."::saveData(): open failed for datafile '$file': $!");
  $fh->print(scalar($pdata->dataLines));
  $fh->close();

  return $file;
}

##======================================================================
## Methods: Generation: plot command

## $plotCommand = $pdata->plotCommand(%args)
##  + returns plot command for this dataset
##  + %args: override %$plot
sub plotCommand {
  my ($pdata,%args) = @_;
  @$pdata{keys(%args)} = values(%args);
  return (''
	  .'"'.$pdata->gpdatafile().'"'
	  .(defined($pdata->{using}) ? " using $pdata->{using}" : '')
	  .(defined($pdata->{smooth}) ? " smooth $pdata->{smooth}" : '')
	  .(defined($pdata->{with})  ? " with $pdata->{with}" : '')
	  .(defined($pdata->{title}) && !$pdata->{notitle} ? " title \"$pdata->{title}\"" : ' notitle')
	  #--.... more options here...
	 );
}

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
