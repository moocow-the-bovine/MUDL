##-*- Mode: CPerl -*-

## File: MUDL::Make::Table.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: make administration: config (pseudo-)fields: table
##======================================================================

package MUDL::Make::Table;
use MUDL::Make::Fields;
use strict;
use Carp;
our @ISA = qw(MUDL::Object);

##======================================================================
## Globals

##======================================================================
## Constructor(s)

## $obj = $class_or_obj->new(%args)
##  + %args:
##      mfields => $mfields,            ##-- MUDL::Make::Fields object
##      sortby  => $sbfields,           ##-- sortby field-spec or MUDL::Make::Fields object
##  + Basic data:
##      rows    => \@fdata,             ##-- rows of the table (MUDL::Make::FieldData objects)
##  + Extended data:
##      formatted      => $bool,        ##-- true if table has been extended
##      name2len       => \%fieldName2maxLen,
##      linewd         => $row_line_width,
##      format         => $row_sprintf_format,
##      crows          => \@condensed_table_with_separators, ##-- includes seps: { __hr__=>$how }
sub new {
  my $that = shift;
  my $tab =  $that->SUPER::new(
			       mfields=>undef,
			       sortby=>undef,
			       rows=>[],
			       formatted=>0,
			       @_
			      );

  return $tab;
}

## $obj = $class_or_obj->newFull(%args)
##  + sorts & formats the table immediately
sub newFull {
  my $that = shift;
  my $tab = $that->new(@_)->generateRows()->sortRows()->formatRows();
}

##======================================================================
## Methods

##----------------------------------------------------------------------
## Methods: row-generation

## $tab = $tab->generateRows()
sub generateRows {
  my $tab = shift;
  @{$tab->{rows}} = $tab->{mfields}->fieldData(@{$tab->{mfields}{configs}});
  return $tab;
}

##----------------------------------------------------------------------
## Methods: sort

## $sortby_mf = $tab->sortby()
sub sortby {
  my $tab = shift;
  my $sortby = $tab->{sortby} ? $tab->{sortby} : $tab->{mfields};

  $sortby = $tab->{sortby} = MUDL::Make::Fields->new(fields=>$sortby,
						     configs=>$tab->{mfields}{configs})
    if (!UNIVERSAL::isa($sortby,'MUDL::Make::Fields'));

  ##-- hack for rxcomp: prepend $tab->{mfields}{sortby}
  $sortby->prependFields($tab->{mfields}{sortby}) if (defined($tab->{mfields}{sortby}));

  return $tab->{sortby};
}

## $tab = $tab->sortRows()
sub sortRows {
  my $tab = shift;
  @{$tab->{rows}} = $tab->sortby->sortFieldData($tab->{rows});
  return $tab;
}

##----------------------------------------------------------------------
## Methods: format

## $tab = $tab->formatRows(%args)
##  + %args:
##     force=>$bool, ##-- force re-format
##  + just returns if $tab->{formatted} is true
##  + otherwise creates formatting keys
sub formatRows {
  my ($tab,%args) = @_;

  ##-- Check if we're already formatted (or forcing a re-format)
  return $tab if (!$args{force} && $tab->{formatted});

  ##-- Format: step 1: get visible fields
  my $mf             = $tab->{mfields};
  my $xfields        = $mf->xfields();
  my $visible_fields = $tab->{visible_fields} = [grep {!$_->{hidden}} @$xfields];

  ##-- Format: step 2: get field lengths (all fields)
  my $rows = $tab->{rows};
  my $name2len = $tab->{name2len} = { map { $_->{name}=>length($_->{title}) } @$xfields };
  my ($cf,$field,$fname);
  foreach $cf (@$rows) {
    foreach $field (@$xfields) {
      $fname  = $field->{name};
      $name2len->{$fname} = length($cf->{$fname.":str"})
	if (length($cf->{$fname.":str"}) > $name2len->{$fname});
    }
  }

  ##-- Format: step 3: get sprintf() format (visible only)
  my $fmt    = '';
  my $linewd = 0;
  my ($i,$fwidth);
  foreach $i (0..$#$visible_fields) {
    $field   = $visible_fields->[$i];
    $fwidth  = $name2len->{$field->{name}};
    $fmt    .= "%${fwidth}s";
    $linewd += $fwidth;
    if ($i < $#$visible_fields) {
      if (defined($field->{padright})) {
	$fmt    .= $field->{padright};
	$linewd += length($field->{padright});
      } else {
	$fmt    .= ' ';
	$linewd++;
      }
    }
  }
  $fmt .= "\n";
  @$tab{qw(format linewd)} = ($fmt,$linewd);

  ##-- Format: step 4: insert 'hr' separators
  my %hr2prec = ( major=>30, minor=>20, micro=>10, none=>0 );
  my $crows = $tab->{crows} = [ { %{$rows->[0]} }, ];

  my ($cfprev,$hrhow);
  foreach $i (1..$#$rows) {
    ($cf,$cfprev) = @$rows[$i,$i-1];
    $hrhow        = undef;
    ##-- separate?
    foreach $field (grep {defined($_->{hr})} @$xfields) {
      $fname = $field->{name}.":str";
      if ($cf->{$fname} ne $cfprev->{$fname}) {
	##-- check hr precedence
	$hrhow = $field->{hr} if (!defined($hrhow) || $hr2prec{$hrhow} < $hr2prec{$field->{hr}});
      }
    }
    push(@$crows, ($hrhow ? { __hr__=>$hrhow } : qw()), { %$cf }); ##-- copy rows
  }

  ##-- Format: step 5: condense field values
  my @condense = qw(); ##-- [$nullfieldref, ... ]
  foreach $i (grep { !defined($crows->[$_]{__hr__}) && !defined($crows->[$_-1]{__hr__}) } (1..$#$crows)) {
    ($cf,$cfprev) = @$crows[$i,$i-1];
    foreach $field (grep {$_->{condense}} @$xfields) {
      $fname = $field->{name}.":str";
      if ($cf->{$fname} eq $cfprev->{$fname}) {
	push(@condense, \$cf->{$fname});
      }
    }
  }
  my ($fieldref);
  foreach $fieldref (@condense) {
    $$fieldref = '';
  }

  return $tab;
}

##----------------------------------------------------------------------
## Methods: format: csv

## $tab = $tab->csvRows(%args)
##  + %args:
##     force=>$bool, ##-- force re-format
##  + just returns if $tab->{formatted} is true
##  + otherwise creates formatting keys
sub csvRows {
  my ($tab,%args) = @_;

  ##-- Check if we're already formatted (or forcing a re-format)
  return $tab if (!$args{force} && $tab->{formatted});
  my $sep = $args{csvSeparator} || $tab->{csvSeparator} || "\t";

  ##-- Format: step 1: get visible fields
  my $mf             = $tab->{mfields};
  my $xfields        = $mf->xfields();
  my $visible_fields = $tab->{visible_fields} = [grep {!$_->{hidden}} @$xfields];

  ##-- Format: step 2: get field lengths (all fields): IGNORED for csv
  my $rows = $tab->{rows};
  my ($cf,$field,$fname);

  ##-- Format: step 3: get sprintf() format (visible only)
  my $fmt        = join("\t", map { "%s" } @$visible_fields)."\n";
  $tab->{format} = $fmt;

  ##-- Format: step 4: insert 'hr' separators
  my %hr2prec = ( major=>30, minor=>20, micro=>10, none=>0 );
  my $crows = $tab->{crows} = [ { %{$rows->[0]} }, ];

  my ($cfprev,$hrhow,$i);
  foreach $i (1..$#$rows) {
    ($cf,$cfprev) = @$rows[$i,$i-1];
    $hrhow        = undef;
    ##-- separate?
    foreach $field (grep {defined($_->{hr})} @$xfields) {
      $fname = $field->{name}.":str";
      if ($cf->{$fname} ne $cfprev->{$fname}) {
	##-- check hr precedence
	$hrhow = $field->{hr} if (!defined($hrhow) || $hr2prec{$hrhow} < $hr2prec{$field->{hr}});
      }
    }
    push(@$crows, ($hrhow ? { __hr__=>$hrhow } : qw()), { %$cf }); ##-- copy rows
  }

  ##-- Format: step 5: condense field values [IGNORED]

  return $tab;
}


##----------------------------------------------------------------------
## Methods: format: LaTeX

## $tab = $tab->latexRows(%args)
##  + %args:
##     force=>$bool,      ##-- force re-format
##     env  =>$latexEnv,  ##-- tabular environment (default='tabular')
##  + just returns if $tab->{latexed} is true
##  + otherwise creates LaTeX-formatting keys
sub latexRows {
  my ($tab,%args) = @_;

  ##-- Check if we're already formatted (or forcing a re-format)
  return $tab if (!$args{force} && $tab->{latexed});

  ##-- Latex: Step 0: string-format
  $tab->formatRows() || return undef;

  ##-- Latex: step 1: get latex-visible fields
  my $mf             = $tab->{mfields};
  my $xfields        = $mf->xfields();
  my $visible_fields = $tab->{visible_fields};

  my $latex_fields   = $tab->{latex_fields} = [];

  my ($i,$j,$field,$jfield,$add);
  foreach $i (0..$#$visible_fields) {
    $field = $visible_fields->[$i];
    next if ($field->{latexAttach});
    $field->{latex_fields} = [ undef ]; ##-- flags current field, avoid self-referential structs

    ##-- scan for right-attaching left neighbors
    for ($j = $i-1; $j >= 0; $j--) {
      $jfield = $visible_fields->[$j];
      last if (!$jfield->{latexAttach} || $jfield->{latexAttach} ne 'r');
      unshift(@{$field->{latex_fields}}, $jfield);
    }

    ##-- scan for left-attaching right neighbors
    for ($j = $i+1; $j <= $#$visible_fields; $j++) {
      $jfield = $visible_fields->[$j];
      last if (!$jfield->{latexAttach} || $jfield->{latexAttach} ne 'l');
      push(@{$field->{latex_fields}}, $jfield);
    }

    ##-- store this latex field
    push(@$latex_fields, $field);
  }


  ##-- Latex: step 2: get latex column alignment
  $tab->{latexEnv} = $args{env} ? $args{env} : 'tabular';
  my $lalign = $tab->{latexAlign} = [];
  foreach (@$latex_fields) {
    if ($_->{latexSep}) {
      ##-- explicit latex separator ('&', '|', '||', ...)
      push(@$lalign, $_->{latexAlignHead}) if (defined($_->{latexAlignHead}));
    }
    else {
      ##-- text field(s): get column-align value
      push(@$lalign,
	   (defined($_->{latexAlign})
	    ? $_->{latexAlign}
	    : ($_->{n}
	       ? 'r'
	       : 'c')));
    }
  }


  ##-- Latex: step 3: prune separators & get latex field titles
  @{$tab->{latex_fields}} = grep { !$_->{latexSep} } @{$tab->{latex_fields}};
  foreach (@$latex_fields) {
    $_->{latexTitle} = $_->{title} ? "\\bfseries{$_->{title}}" : ''
      if (!defined($_->{latexTitle}));
  }

  ##-- Latex: step 4: get latex rows (array of hashes):
  ##   $latex_rows = [ \@row1, ..., \@rowN ],
  ##   $row_i      = { row=>$fieldData, latex=>\@latex_column_strings, }
  my $lrows = $tab->{latex_rows} = [];
  my ($row,$lrow);
  foreach $row (@{$tab->{crows}}) {
    if ($row->{__hr__}) {
      push(@$lrows, {row=>$row});
      next;
    }

    $lrow = [];
    for ($i=0; $i <= $#$latex_fields; $i++) {
      $field = $latex_fields->[$i];
      $lrow->[$i] = '';
      for ($j=0; $j <= $#{$field->{latex_fields}}; $j++) {
	$jfield = $field->{latex_fields}[$j];
	$jfield = $field if (!defined($jfield));
	$lrow->[$i] .= $row->{$jfield->{name}.":str"};
      }
    }
    push(@$lrows, {row=>$row, latex=>$lrow});
  }

  ##-- Set 'latexed' flag
  $tab->{latexed} = 1;

  return $tab;
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
