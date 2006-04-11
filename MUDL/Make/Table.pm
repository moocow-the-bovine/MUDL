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
##      title2len      => \%fieldTitle2maxLen,
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
    if (!ref($sortby));

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
  my $title2len = $tab->{title2len} = { map { $_->{title}=>length($_->{title}) } @$xfields };
  my ($cf,$field,$ftitle);
  foreach $cf (@$rows) {
    foreach $field (@$xfields) {
      $ftitle  = $field->{title};
      $title2len->{$ftitle} = length($cf->{$ftitle.":str"})
	if (length($cf->{$ftitle.":str"}) > $title2len->{$ftitle});
    }
  }

  ##-- Format: step 3: get sprintf() format (visible only)
  $tab->{format} = join(' ', map { '%'.$title2len->{$_->{title}}.'s' } @$visible_fields)."\n";
  $tab->{linewd} = -1;
  $tab->{linewd} += $title2len->{$_->{title}}+1 foreach (@$visible_fields);

  ##-- Format: step 4: insert 'hr' separators
  my %hr2prec = ( major=>30, minor=>20, micro=>10, none=>0 );
  my $crows = $tab->{crows} = [ { %{$rows->[0]} }, ];

  my ($i,$cfprev,$hrhow);
  foreach $i (1..$#$rows) {
    ($cf,$cfprev) = @$rows[$i,$i-1];
    $hrhow        = undef;
    ##-- separate?
    foreach $field (grep {defined($_->{hr})} @$xfields) {
      $ftitle = $field->{title}.":str";
      if ($cf->{$ftitle} ne $cfprev->{$ftitle}) {
	##-- check hr precedence
	$hrhow = $field->{hr} if (!defined($hrhow) || $hr2prec{$hrhow} < $hr2prec{$field->{hr}});
      }
    }
    push(@$crows, ($hrhow ? { __hr__=>$hrhow } : qw()), { %$cf }); ##-- copy rows
  }

  ##-- Format: step 5: condense field values
  foreach $i (grep { !defined($crows->[$_]{__hr__}) && !defined($crows->[$_-1]{__hr__}) } (1..$#$crows)) {
    ($cf,$cfprev) = @$crows[$i,$i-1];
    foreach $field (grep {$_->{condense}} @$xfields) {
      $ftitle = $field->{title}.":str";
      if ($cf->{$ftitle} eq $cfprev->{$ftitle}) {
	$cf->{$ftitle} = '';
      }
    }
  }

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
