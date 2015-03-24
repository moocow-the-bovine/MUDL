##-*- Mode: CPerl -*-

## File: MUDL::Make::Table.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: make administration: config (pseudo-)fields: table
##======================================================================

package MUDL::Make::Table;
use MUDL::Make::Fields;
use MUDL::CSVTable;
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
##      mfields => $mfields,            ##-- MUDL::Make::Fields object
##      sortby  => $sbfields,           ##-- sortby field-spec or MUDL::Make::Fields object
##  + Basic data:
##      rows    => \@fdata,             ##-- rows of the table (MUDL::Make::FieldData objects)
##  + Extended data:
##      formatted      => $bool,        ##-- true if table has been text-formatted
##      formatted_csv  => $bool,        ##-- true if table has been csv-formatted
##      formatted_latex=> $bool,        ##-- true if table has been latex-formatted
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

  $tab->{formatted} = 1;

  return $tab;
}

##----------------------------------------------------------------------
## Methods: format: csv

## $tab = $tab->csvRows(%args)
##  + %args:
##     force=>$bool,    ##-- force re-format
##     csvSeparator=>$sep_str, ##-- separator string (defualt="\t")
##  + just returns if $tab->{formatted} is true
##  + otherwise creates formatting keys
sub csvRows {
  my ($tab,%args) = @_;

  ##-- Check if we're already formatted (or forcing a re-format)
  return $tab if (!$args{force} && $tab->{formatted_csv});
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

  $tab->{formatted_csv} = 1;

  return $tab;
}


##----------------------------------------------------------------------
## Methods: format: LaTeX

## $tab = $tab->latexRows(%args)
##  + %args:
##     force=>$bool,      ##-- force re-format
##     env  =>$latexEnv,  ##-- tabular environment (default='tabular')
##  + just returns if $tab->{formatted_latex} is true
##  + otherwise creates LaTeX-formatting keys
sub latexRows {
  my ($tab,%args) = @_;

  ##-- Check if we're already formatted (or forcing a re-format)
  return $tab if (!$args{force} && $tab->{formatted_latex});

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

  ##-- Set 'formatted_latex' flag
  $tab->{formatted_latex} = 1;

  return $tab;
}

##======================================================================
## Methods: Conversion
##======================================================================

## $csvTable = $tab->csvTable(%csvArgs);
sub csvTable {
  my $tab = shift;
  my $csv = MUDL::CSVTable->new(@_);
  return $csv->loadNativeString(join('',@{$tab->csvStrings()}));
}

##======================================================================
## Methods: I/O
##======================================================================

##----------------------------------------------------------------------
## Methods: I/O: generic

## $tab = $tab->printGeneric($filename_or_fh,\@strings)
sub printGeneric {
  my ($tab,$file,$strings) = @_;
  $file = \*STDOUT if (!defined($file));
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  die(ref($tab)."::printGeneric(): open failed for file '$file': $!")
    if (!defined($fh));
  $fh->print(@$strings);
  $fh->close() if (!ref($file));
  return $tab;
}

##----------------------------------------------------------------------
## Methods: I/O: Text

## \@strs = $tab->textStrings(%args)
## $tab   = $tab->printText($filename_or_fh,%args)
## + %args:
##     indent => $indent_str
sub printText {
  my ($tab,$file,%args) = @_;
  return $tab->printGeneric($file,$tab->textStrings(%args));
}
sub textStrings {
  my ($tab,%args) = @_;
  my @strings = qw();

  ##-- printText: format
  $tab->formatRows();

  ##-- printText: indent
  my $indent = defined($args{indent}) ? $args{indent} : ' ';

  my $format = $indent.$tab->{format};
  my $linewd = $tab->{linewd};
  my %hr = (
	    begin=>($indent.('-' x $linewd)."\n"),
	    end  =>($indent.('-' x $linewd)."\n"),
	    head =>($indent.('=' x $linewd)."\n"),

	    major=>($indent.('=' x $linewd)."\n"),
	    minor=>($indent.('-' x $linewd)."\n"),
	    micro=>($indent.('·' x $linewd)."\n"),
	    none=>'',
	   );

  ##-- printText: headers
  push(@strings,
       ##-- hrule
       $hr{begin},

       ##-- header
       sprintf($format, map { $_->{title} } @{$tab->{visible_fields}}),

       ##-- hrule
       $hr{head},
      );

  ##-- Summarize: step 5: print table
  my $crows = $tab->{crows};
  my ($cf,$i);
  foreach $i (0..$#$crows) {
    $cf = $crows->[$i];
    push(@strings,
	 (defined($cf->{__hr__})
	  ? $hr{$cf->{__hr__}}    ##-- separator?
	  : sprintf($format,      ##-- data row
		    @$cf{map {$_->{name}.":str"} @{$tab->{visible_fields}}}))
	);
  }
  push(@strings, $hr{end});

  return \@strings;
}


##----------------------------------------------------------------------
## Methods: I/O: CSV

## \@strs = $tab->csvStrings(%args)
## $tab   = $tab->printCSV($filename_or_fh,%args)
## + %args:
##     comment => $comment_str, ##-- defualt='#'
##     sep     => $sep_str,     ##-- default=$tab->{csvSeparator} || "\t"
sub printCSV {
  my ($tab,$file,%args) = @_;
  return $tab->printGeneric($file,$tab->csvStrings(%args));
}
sub csvStrings {
  my ($tab,%args) = @_;
  my @strings = qw();

  ##-- printCSV: format
  $tab->csvRows(csvSeparator=>$args{sep});

  ##-- printCSV: options
  my $comment = defined($args{comment}) ? $args{comment} : '#';
  my $format = $tab->{format};
  my $linewd = 80;
  my %hr = (
	    major=>("\n"
		    .$comment.('=' x $linewd)."\n"),
	    minor=>($comment.('-' x $linewd)."\n"),
	    micro=>($comment.('·' x $linewd)."\n"),
	    none=>'',
	   );

  ##-- printCSV: headers
  my ($title);
  push(@strings,
       ##-- header
       $comment,
       sprintf($format,
	       map {
		 $title = $tab->{visible_fields}[$_]{title};
		 $title =~ s/\s+/_/g;
		 ($_+1)."($title)";
	       }
	       (0..$#{$tab->{visible_fields}})),
      );

  ##-- printCSV: data rows
  my $crows = $tab->{crows};
  my ($cf,$i);
  foreach $i (0..$#$crows) {
    $cf = $crows->[$i];
    push(@strings,
	 (defined($cf->{__hr__})
	  ? $hr{$cf->{__hr__}}    ##-- separator?
	  : sprintf($format,      ##-- data row
		    #@$cf{map {$_->{name}.":str"} @{$tab->{visible_fields}}}
		    @$cf{map {$_->{name}} @{$tab->{visible_fields}}}
		   ))
	);
  }

  return \@strings;
}

##----------------------------------------------------------------------
## Methods: I/O: LaTeX

## \@strs = $tab->latexStrings(%args)
## $tab   = $tab->printLatex($filename_or_fh,%args)
## + %args:
##     indent => $indent_str
sub printLatex {
  my ($tab,$file,%args) = @_;
  return $tab->printGeneric($file,$tab->latexStrings(%args));
}
sub latexStrings {
  my ($tab,%args) = @_;
  my @strings = qw();

  ##-- printLatex: format
  $tab->latexRows();

  ##-- printLatex: options
  my $indent = defined($args{indent}) ? $args{indent} : ' ';
  my %hr = (
	    begin=>($indent."\\hline \%\n"),
	    end  =>($indent."\\hline \%\n"),
	    head =>($indent."\\hline \\hline \%\n"),

	    major=>($indent."\\hline \\hline \%\n"),
	    minor=>($indent."\\hline \%\n"),
	    micro=>($indent."%%-- (hr:micro)\n"),
	    none=>'',
	   );

  ##-- printLatex: table format
  my @latex_fields = @{$tab->{latex_fields}};

  my $tabenv = $tab->{latexEnv}; ##-- TODO: make this available to user!

  ##-- printLatex: headers
  push(@strings,
       ##-- Header
       "%% File auto-generated by ", ref($tab), "::printLatex(): EDIT AT YOUR OWN RISK!\n",

       ##-- Environment & column alignment
       "\\begin{$tabenv}{", @{$tab->{latexAlign}}, "}\n",

       ##-- hrule
       $hr{begin},

       ##-- header
       $indent,
       join(' & ', map { $_->{latexTitle} } @latex_fields),
       "\\\\ \n",

       ##-- hrule
       $hr{head},
      );

  ##-- printLatex: print table
  my $lrows = $tab->{latex_rows};
  my ($lrow,$i);
  foreach $i (0..$#$lrows) {
    $lrow = $lrows->[$i];
    push(@strings,
	 (defined($lrow->{row}{__hr__})
	  ? $hr{$lrow->{row}{__hr__}}          ##-- separator
	  : ($indent,                          ##-- data row
	     join(' & ', @{$lrow->{latex}}),
	     "\\\\ \n",
	    ))
	);
  }
  push(@strings,
       $hr{end},
       "\\end{${tabenv}}\n",
      );

  return \@strings;
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
