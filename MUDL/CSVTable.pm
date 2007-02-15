##-*- Mode: CPerl -*-

## File: MUDL::Make::CSVTable.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: make administration: CSV tables
##======================================================================

package MUDL::CSVTable;
use MUDL::Object;
use IO::File;
use strict;
use Carp;
our @ISA = qw(MUDL::Object);

##======================================================================
## Globals

##======================================================================
## Constructor(s)

## $obj = $class_or_obj->new(%args)
##  + Options:
##      cmtStr          => $comment_prefix,  ##-- default='#'
##      sepRe           => $separator_regex, ##-- default=/\s+/
##      joinStr         => $join_str,        ##-- default="\t"
##      loadColumnNames => $bool,            ##-- attempt to read column names on load? (default=1)
##                                           ##   + if true, col names are parsed from an initial comment line
##                                           ##     if one is present
##      loadColumnNumbers=> $bool,           ##-- attempt to parse away column numbers on load? (default=1)
##                                           ##   + if true, col names of the format /^\d+\((.*)\)$/
##                                           ##     are parsed as $1
##                                           ##   + also, col names of the format /^\d*((\D.*)\)_\d*$/
##                                           ##     are parsed as $1
##      saveColumnNames => $bool,            ##-- save column names on save? (default=1)
##      saveColumnNumbers=> $bool,           ##-- save column numbers on save? (default=1)
##      sanitizeColumTitles=>$bool,          ##-- sanitize column titles on save? (default=0)
##  + Basic data:
##      data            => \@rows,         ##-- data rows (ARRAY of ARRAYs)
##      ncols           => $ncols,         ##-- maximum number of columns loaded
##  + Optional data:
##      name2col   => \%name2col,          ##-- column-number indexed by column-name (clobbers)
##      col2name   => \@col2name,          ##-- column-name indexed by column-number (safe)
sub new {
  my $that = shift;
  my $csv =  $that->SUPER::new(
			       ##-- Options
			       cmtStr=>'#',
			       sepRe=>'\s+',
			       joinStr=>"\t",
			       loadColumnNames=>1,
			       loadColumnNumbers=>1,
			       saveColumnNames=>1,
			       saveColumnNumbers=>1,
			       sanitizeColumnTitles=>0,

			       ##-- Basic Data
			       data=>[],
			       ncols=>0,

			       ##-- Optional data
			       name2col=>{},
			       col2name=>[],
			       @_
			      );
  return $csv;
}

## $csv = $csv->clearData()
##  + clears data
sub clearData {
  my $csv = shift;
  @{$csv->{data}} = qw();
  return $csv;
}

## $csv = $csv->clear()
##  + clears data & column indices
sub clear {
  my $csv = shift;
  $csv->clearData();
  @{$csv->{name2col}} = qw();
  @{$csv->{col2name}} = qw();
  $csv->{ncols} = 0;
}


##======================================================================
## Methods: Basic Access
##======================================================================

##--------------------------------------------------------------
## Methods: Access: Global

## $nrows = $csv->nrows()
sub nrows { return scalar(@{$_[0]{data}}); }

## $ncols = $csv->ncols()
sub ncols { return $_[0]{ncols}; }

##--------------------------------------------------------------
## Methods: Access: Rows

## (\@row1,...) = $csv->rows(@rownums)
*row = \&rows;
sub rows { return $#_ >= 1 ? @{$_[0]{data}}[@_[1..$#_]] : @{$_[0]{data}}; }

## \%row = $csv->row2hash(\@row)
sub row2hash {
  my ($csv,$row) = @_;
  return { map { (($csv->{col2name}[$_]||$_)=>$row->[$_]) } (0..$#$row) };
}

## \@row = $csv->hash2row(\%row)
sub hash2row {
  my ($csv,$rowh) = @_;
  my $row = [];
  my ($key,$col);
  foreach $key (keys(%$rowh)) {
    next if (!defined($col=$csv->{name2col}{$key}));
    $row->[$col] = $rowh->{$key};
  }
  return $row;
}

## (\%row1,...) = $csv->hrows($rownum)
*hrow = \&hrows;
sub hrows { return map { $_[0]->row2hash($_) } ($_[0]->row(@_[1..$#_])); }

##--------------------------------------------------------------
## Methods: Access: Columns

## (\@col1,...) = $csv->colsByNumber(@colnums)
*col = *cols = *colByNumber = \&colsByNumber;
sub colsByNumber {
  my $csv = shift;
  my ($coli);
  return (
	  map {
	    $coli=$_||0;
	    [map {$_->[$coli]} @{$csv->{data}}]
	  } @_
	 );
}

## (\@col1,...) = $csv->colsByName(@colnames);
*coln = *colsn = *colByName = \&colsByName;
sub colsByName {
  my $csv = shift;
  return $csv->colsByNumber(@{$csv->{name2col}}{@_});
}


##--------------------------------------------------------------
## Methods: Access: Cells

## $val = $csv->at($row,$colNum)
sub at {
  my ($csv,$row,$col) = @_;
  return undef if (!defined($csv->{data})
		   || !exists($csv->{data}[$row])
		   || !exists($csv->{data}[$row][$col]));
  return $csv->{data}[$row][$col];
}

## $val = $csv->atByName($row,$colName)
*atn = \&atByName;
sub atByName {
  my ($csv,$row,$colName) = @_;
  my $col = $csv->{name2col}{$colName};
  return undef if (!defined($col));
  return $csv->at($row,$col);
}


##======================================================================
## Methods: Relational Algebra
##======================================================================

##--------------------------------------------------------------
## Methods: Algebra: Project

## $csv2 = $csv->project(@colNums)
sub project {
  my $csv = shift;
  return $csv->clone()->_project(@_);
}

## $csv2 = $csv->projectByName(@colNames)
*projectn = \&projectByName;
sub projectByName {
  my $csv = shift;
  return $csv->clone()->_projectByName(@_);
}


## $csv = $csv->_project(@colNums)
sub _project {
  my ($csv,@cols) = @_;

  ##-- adjust data
  my ($row);
  foreach $row (@{$csv->{data}}) {
    @$row = @$row[@cols];
  }

  ##-- adjust column indices
  my %name2col_old = %{$csv->{name2col}};
  my @col2name_old = @{$csv->{col2name}};

  @{$csv->{col2name}} = @col2name_old[@cols];
  %{$csv->{name2col}} = map { ($csv->{col2name}[$_]=>$_) } (0..$#cols);
  $csv->{ncols} = scalar(@cols);

  return $csv;
}

## $csv = $csv->_projectByName(@colNames)
*_projectn = \&_projectByName;
sub _projectByName {
  my ($csv,@colNames) = @_;
  my @cols = grep {defined($_)} @{$csv->{name2col}}{@colNames};
  return $csv->_project(@cols);
}


##======================================================================
## Methods: I/O
##======================================================================

## $csvTab = $class_or_object->loadNativeFh($fh,%args)
sub loadNativeFh {
  my ($csv,$fh,%args) = @_;
  $csv = $csv->new(%args) if (!ref($csv));
  #@$csv{keys %args} = values(%args);

  my ($line,@values);
  my $first=1;
  while (defined($line=<$fh>)) {
    chomp($line);
    next if ($line =~ /^\s*$/);
    if ($line =~ m/^\s*\Q$csv->{cmtStr}\E/) {
      ##-- maybe load column names on first comment line
      if ($first && $csv->{loadColumnNames}) {
	$line =~ s/^\s*\Q$csv->{cmtStr}\E*\s*//;
	@values = split(/$csv->{sepRe}/,$line);
	@values = (
		   map {
		     $_=~s/^\s*\d+\((.*)\)$/$1/;
		     $_=~s/^\s*(\S.*)_\d+$/$1/;
		     $_;
		   } @values)
	  if ($csv->{loadColumnNumbers});
	my ($colName,$colNumber);
	my %name2n = qw();
	foreach my $colNumber (0..$#values) {
	  $colName = $values[$colNumber];
	  $colName .= "($name2n{$colName})" if (defined($name2n{$colName}));
	  ++$name2n{$colName};
	  $csv->{col2name}[$colNumber] = $colName;
	  $csv->{name2col}{$colName}   = $colNumber;
	}
	$csv->{ncols} = @values if (@values > $csv->{ncols});
      }
      $first = 0;
      next if ($csv->{cmtStr} ne '');
    }
    $first=0;
    @values = split(/$csv->{sepRe}/,$line);
    push(@{$csv->{data}}, [@values]);
    $csv->{ncols} = @values if (@values > $csv->{ncols});
  }
  return $csv;
}

## $title = $csv->saveColumnTitle($colNum,$colName)
sub saveColumnTitle {
  my ($csv,$num,$name) = @_;
  $name = 'unknown' if (!$name);
  if ($csv->{sanitizeColumnTitles}) {
    $name =~ s/\W+/_/g;
    $name =~ s/\_+/_/g;
    $name =~ s/^\_+//;
    $name =~ s/\_+$//;
    return $name . ($csv->{saveColumnNumbers} ? "_$num" : '');
  }
  return ($csv->{saveColumnNumbers}
	  ? "$num($name)"
	  : $name);
}

## $csvTab = $class_or_object->saveNativeFh($fh,%args)
sub saveNativeFh {
  my ($csv,$fh,%args) = @_;
  #@$csv{keys %args} = values(%args);

  ##-- maybe save column names
  if ($csv->{saveColumnNames} && $csv->{col2name}) {
    $fh->print($csv->{cmtStr},
	       join($csv->{joinStr},
		    map {
		      $csv->saveColumnTitle($_+1,$csv->{col2name}[$_])
		    } (0..$#{$csv->{col2name}})),
	       "\n");
  }

  ##-- save row data
  my ($row);
  $fh->print(
	     map {
	       my $row=$_;
	       join($csv->{joinStr}, map { defined($_) ? $_ : 0 } @$row)."\n"
	     } @{$csv->{data}}
	    );

  return $csv;
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
