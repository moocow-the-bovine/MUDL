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
use MUDL::Utils qw(:sys);
use IO::File;
use Cwd qw(getcwd abs_path);
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
##     dir     => $make_directory, ##-- directory in which to run make (default: '.')
##     dirstack => \@stack,        ##-- directory stack
##     targets => $targets_str,    ##-- default: 'all'
##     userfile => $userfile,      ##-- user config file (default: none)
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(
				     ##-- Variables
				     #vars=>MUDL::Make::Vars->new(),
				     uvars=>{},
				     xvars=>{},

				     ##-- Targets
				     dir=>'.',
				     dirstack=>[],
				     targets=>'all',

				     ##-- User args
				     @_
				    ), ref($that)||$that;
  return $self;
}

## $cfg = $cfg->clear;
sub clear {
  my $cfg = shift;

  ##-- Variables
  #$cfg->{vars}->clear();
  %{$cfg->{xvars}} = qw();
  %{$cfg->{uvars}} = qw();

  ##-- Targets
  $cfg->{dir} = '.';
  $cfg->{targets} = 'all';
  delete($cfg->{userfile});

  return $cfg;
}

##======================================================================
## Constructor: shadow

## $cfg2 = $cfg->copyBrief(\@user_vars_to_keep)
sub copyBrief {
  my ($cfg,$keepvars) = @_;
  my $cfg2 = ref($cfg)->new(%$cfg);
  $cfg2->{uvars} = { map { $_=>$cfg2->{uvars}{$_} } @$keepvars };
  #$cfg2->{xvars} = {};
  return $cfg2;
}


##======================================================================
## Identification: key-generation

## $key = $cfg->key(\%var2val)
sub key {
  my ($cfg,$vars) = @_;
  return join($cfg->{rs}, map { $_ . $cfg->{fs} . $vars->{$_} } sort(keys(%$vars)));
}

## $ukey = $cfg->ukey()
##  + key for user-vars only
sub ukey { return $_[0]->key($_[0]->{uvars}); }

## $xkey = $cfg->xkey()
##  + key for expanded variables
##  + requires: $cfg->expand()
sub xkey { return $_[0]->key($_[0]->{xvars}); }


##======================================================================
## Expansion: all variables

## $cfg = $cfg->expand($mudl_mak_vars)
##  + assigns user-variables in $cfg->{vars}
##  + expands all variables to $cfg->{xvars}
##  + copies $mudl_mak_vars
sub expand { return $_[0]->_expand($_[1]->clone()); }

## $cfg = $cfg->_expand($mudl_mak_vars)
##  + assigns user-variables in $cfg->{vars}
##  + expands all variables to $cfg->{xvars}
##  + destructively alters $mudl_mak_vars !
sub _expand {
  my ($cfg,$vars) = @_;
  $vars->assign($cfg->{uvars});
  $vars->expand(xvars=>$cfg->{xvars});
  return $cfg;
}


##======================================================================
## Makefile Generation: for user-vars

## $file = $cfg->writeUserMakefile()
## $file = $cfg->writeUserMakefile($filename_or_fh)
##  + writes variable assignments to a makefile
sub writeUserMakefile {
  my $cfg = shift;
  my $ufile = shift;
  $ufile = $cfg->{userfile} if (!defined($ufile));
  return MUDL::Make::Vars->new()->assign($cfg->{uvars})->writeMakefile(file=>$ufile,target=>'');
}

##======================================================================
## Make: Directory juggling

## $cwd = $cfg->pushd()
## $cwd = $cfg->pushd($dir)
##  + chdir()s to $cfg->{dir} or $dir, updates $cfg->{dirstack}
sub pushd {
  my ($cfg,$dir) = @_;
  $dir = $cfg->{dir} if (!defined($dir));
  $dir = abs_path($dir);
  my $cwd = getcwd();
  return $dir if ($dir eq $cwd); ##-- no chdir() required

  push(@{$cfg->{dirstack}}, $cwd);
  chdir($dir) or confess(ref($cfg),"::pushd(): could not chdir($dir): $!");
  return $dir;
}

## $cwd = $cfg->popd()
##  + pops the most recent directory from the stack (if any)
sub popd {
  my $cfg = shift;
  return getcwd() if (!@{$cfg->{dirstack}}); ##-- no more directories to pop

  my $dir = pop(@{$cfg->{dirstack}});
  chdir($dir) or confess(ref($cfg),"::popd(): could not chdir($dir): $!");
  return $dir;
}


##======================================================================
## Make: Targets

## $bool = $cfg->make(%args)
##  + implicitly calls $cfg->acquire(%args) on successful completion
##  + %args:
##     dir       => $dir,         ##-- overrides $cfg->{dir}
##     targets   => $targets_str, ##-- overrides $cfg->{targets}
##     makefiles => \@makefiles,  ##-- makefiles (rules) [relative to $dir]
##     userfile  => $userfile,    ##-- use "$dir/$userfile" to write variables (default: temp file)
##     makeflags => $str,         ##-- additional flags to $MAKE
##     dummy     => $bool,        ##-- only print make calls
sub make {
  my ($cfg,%args) = @_;

  ##-- change to make-directory
  $cfg->pushd($args{dir});

  ##-- Get targets, makefiles
  my $targets   = $args{targets}   ? $args{targets}      : $cfg->{targets};
  my @makefiles = $args{makefiles} ? @{$args{makefiles}} : glob('[Mm]akefile');

  ##-- Write user-config file
  my $userfile  = $cfg->writeUserMakefile($args{userfile});

  ##-- Ye Olde Guttes: call $MAKE
  my $cmd = ($MAKE
	     .' '.join(' ', map { "-f '$_'" } ($userfile,@makefiles))
	     .(defined($args{makeflags}) ? " $args{makeflags}" : '')
	     .' '.$targets
	    );
  print STDERR ref($cfg), "::make(): $cmd\n";

  my $rc = $args{dummy} ? $args{dummy} : (system($cmd)==0 && $cfg->acquire(%args));

  ##-- restore last current directory from stack
  $cfg->popd();

  return $rc;
}


##======================================================================
## Make: Acquire Data

## $bool = $cfg->acquire(%args)
##  + called after successful $cfg->make()
##  + may be implemented in child classes for data acquisition
##  + the process is already chdir()d to the $cfg->{dir} when this method is called!
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
