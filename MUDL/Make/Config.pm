##-*- Mode: CPerl -*-

## File: MUDL::Make::Config.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: makefile configuration
##======================================================================

package MUDL::Make::Config;
use MUDL::Object;
use MUDL::Make qw(:all);
use MUDL::Make::Vars;
use IO::File;
use File::Temp qw(tempfile);
use File::Copy qw(copy);
use Carp;
use strict;

our @ISA = qw(MUDL::Object);

##======================================================================
## Globals
our $DEBUG = 0;

##======================================================================
## OBJECT STRUCTURE:
##  + hash:
##     ##
##     ##-- Variables
##     #vars  => $mudl_make_vars,
##     uvars => $user_variables,
##     xvars => \%expanded_vars,
##     ##
##     ##-- Targets
##     targets => $targets_str,  ##-- default: 'all'
##     ##
##     ##-- Key Generation
##     #rs => $record_separator,  ##-- separates ($var,$val) pairs in keys
##     #fs => $field_separator,   ##-- separates $var from $val within ($var,$val) pairs in keys
##     ... ?
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(
				     ##-- Variables
				     #vars=>MUDL::Make::Vars->new(),
				     uvars=>{},
				     xvars=>{},

				     ##-- Targets
				     targets=>'all',

				     ##-- User args
				     @_
				    ), ref($that)||$that;
  return $self;
}

## $mak = $mak->clear;
sub clear {
  my $mak = shift;

  ##-- Variables
  #$mak->{vars}->clear();
  %{$mak->{xvars}} = qw();
  %{$mak->{uvars}} = qw();

  ##-- Targets
  $mak->{targets} = 'all';

  return $mak;
}

##======================================================================
## Identification: key-generation

## $key = $mak->key(\%var2val)
sub key {
  my ($mak,$vars) = @_;
  return join($mak->{rs}, map { $_ . $mak->{fs} . $vars->{$_} } sort(keys(%$vars)));
}

## $ukey = $mak->ukey()
##  + key for user-vars only
sub ukey { return $_[0]->key($_[0]->{uvars}); }

## $xkey = $mak->xkey()
##  + key for expanded variables
##  + requires: $mak->expand()
sub xkey { return $_[0]->key($_[0]->{xvars}); }


##======================================================================
## Expansion: all variables

## $mak = $mak->expand($mudl_mak_vars)
##  + assigns user-variables in $mak->{vars}
##  + expands all variables to $mak->{xvars}
##  + copies $mudl_mak_vars
sub expand { return $_[0]->_expand($_[1]->clone()); }

## $mak = $mak->_expand($mudl_mak_vars)
##  + assigns user-variables in $mak->{vars}
##  + expands all variables to $mak->{xvars}
##  + destructively alters $mudl_mak_vars !
sub _expand {
  my ($mak,$vars) = @_;
  $vars->assign($mak->{uvars});
  $vars->expand(xvars=>$mak->{xvars});
  return $mak;
}


##======================================================================
## Makefile Generation: for user-vars

## $file = $mak->writeUserMakefile()
## $file = $mak->writeUserMakefile($filename_or_fh)
##  + writes variable assignments to a makefile
sub writeUserMakefile {
  my $cfg = shift;
  return MUDL::Make::Vars->new()->assign($cfg->{uvars})->writeMakefile(file=>shift,target=>'');
}

##======================================================================
## Make: Targets

## $bool = $mak->make(%args)
##  + implicitly calls $mak->acquire(%args) on successful completion
##  + %args:
##     targets   => $targets_str, ##-- overrides $mak->{targets}
##     makefiles => \@makefiles,  ##-- makefiles (rules)
##     userfile  => $userfile,    ##-- use $userfile to write variables (default=tmp)
##     makeflags => $str,         ##-- additional flags to $MAKE
sub make {
  my ($mak,%args) = @_;
  my $targets   = $args{targets}   ? $args{targets}      : $mak->{targets};
  my @makefiles = $args{makefiles} ? @{$args{makefiles}} : glob('[Mm]akefile');
  my $userfile  = $mak->writeUserMakefile($args{userfile});
  my $cmd = ($MAKE
	     .' '.join(' ', map { "-f$_" } ($userfile,@makefiles))
	     .($args{makeflags} ? " $args{makeflags}" : '')
	     .' '.$targets
	    );
  print STDERR ref($mak), "::make(): $cmd\n";
  return (system($cmd)==0 && $mak->acquire(%args));
}


##======================================================================
## Make: Acquire Data

## $bool = $mak->acquire(%args)
##  + called after successful $mak->make()
##  + may be implemented in child classes for data acquisition
sub acquire { return 1; }


1;

##======================================================================
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
