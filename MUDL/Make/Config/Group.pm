##-*- Mode: CPerl -*-

## File: MUDL::Make::Config::Group.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: group of makefile configurations
##======================================================================

package MUDL::Make::Config::Group;
use MUD::Make::Config;
use MUDL::Make qw(:all);
use Carp;
use strict;

our @ISA = qw(MUDL::Make::Config);

##======================================================================
## Globals
our $DEBUG = 0;

##======================================================================
## OBJECT STRUCTURE:
##  + hash:
##    {
##     ##-- Subconfigurations
##     configs => \@subconfigs, ##-- each is a MUDL::Make::Config
##
##    }
sub new {
  my $that = shift;
  my $self = bless $that->MUDL::Object->new
    (
     ##-- Subconfigurations
     configs => [],

     ##-- User args
     @_
    ), ref($that)||$that;

  return $self;
}

## $cfg = $cfg->clear;
sub clear {
  my $cfg = shift;
  $_->clear() foreach (@{$cfg->{subconfigs}});
  return $cfg;
}

## destructor
sub DESTROY { ; }

##======================================================================
## Constructor: shadow

## $cfg2 = $cfg->copyBrief(\@user_vars_to_keep)
sub copyBrief {
  my ($cfg,$keepvars) = @_;
  my $cfg2 = ref($cfg)->new(%$cfg,
			    subconfigs=>[map { $_->copyBrief($keepvars) } @{$cfg->{subconfigs}}],
			   );
  return $cfg2;
}


##======================================================================
## Identification: key-generation

## $key = $cfg->key(\%var2val)
##  + uses '|' to join multiple values
sub key {
  my $cfg = shift;
  confess(ref($cfg), "::key(): not implemented!");
  return undef;
}

## $ukey = $cfg->ukey()
##  + key for user-vars only
sub ukey {
  my $cfg = shift;
  return '('.join(' | ', map { ("(".$_->ukey.")") } @{$cfg->{subconfigs}}).')';
}

## $xkey = $cfg->xkey()
##  + key for expanded variables
##  + requires: $_->expand() on subconfigs
sub xkey {
  my $cfg = shift;
  return '('.join(' | ', map { ("(".$_->xkey.")") } @{$cfg->{subconfigs}}).')';
}

##======================================================================
## Expansion: all variables

## $cfg = $cfg->expand($mudl_mak_vars)
##  + assigns user-variables in $cfg->{vars}
##  + expands all variables to $cfg->{xvars}
##  + copies $mudl_mak_vars
sub expand {
  my $cfg = shift;
  foreach (@{$cfg->{subconfigs}}) {
    if (!$_->expand(@_)) {
      confess(ref($cfg)."::expand() failed!");
      return undef;
    }
  }
  return $cfg;
}

## $cfg = $cfg->_expand($mudl_mak_vars)
##  + assigns user-variables in $cfg->{vars}
##  + expands all variables to $cfg->{xvars}
##  + destructively alters $mudl_mak_vars !
sub _expand {
  my $cfg = shift;
  foreach (@{$cfg->{subconfigs}}) {
    if (!$_->_expand(@_)) {
      confess(ref($cfg)."::_expand() failed!");
      return undef;
    }
  }
  return $cfg;
}


##======================================================================
## Makefile Generation: for user-vars

## $file = $cfg->writeUserMakefile()
## $file = $cfg->writeUserMakefile($filename_or_fh)
##  + writes variable assignments to a makefile
sub writeUserMakefile {
  my $cfg = shift;
  confess(ref($cfg)."::writeUserMakefile(): not implemented!");
  return undef;
}

##======================================================================
## Make: Directory juggling

## $cwd = $cfg->pushd()
## $cwd = $cfg->pushd($dir)
##  + chdir()s to $cfg->{dir} or $dir, updates $cfg->{dirstack}
sub pushd {
  my ($cfg,$dir) = @_;
  my $cfg = shift;
  confess(ref($cfg)."::pushd(): not implemented!");
  return undef;
}

## $cwd = $cfg->popd()
##  + pops the most recent directory from the stack (if any)
sub popd {
  my $cfg = shift;
  confess(ref($cfg)."::popd(): not implemented!");
  return undef;
}


##======================================================================
## Make: Targets

## $bool = $cfg->make(%args)
##  + calls $_->make() on each subconfig
sub make {
  my $cfg = shift;
  foreach (@{$cfg->{subconfigs}}) {
    if (!$_->make(@_)) {
      confess(ref($cfg)."::make() failed!");
      return undef;
    }
  }
  return 1;
}


##======================================================================
## Make: Acquire Data

## $bool = $cfg->acquire(%args)
##  + called after successful $cfg->make()
##  + may be implemented in child classes for data acquisition
##  + the process is already chdir()d to the $cfg->{dir} when this method is called!
sub acquire { return 1; }

## $bool = $cfg->reacquire(%args)
##  + attempts to call reacquire() on each subconfig
sub reacquire {
  my $cfg = shift;
  foreach (@{$cfg->{subconfigs}}) {
    if (!UNVIERSAL::can($_,'reacquire') || !$_->reacquire(@_)) {
      confess(ref($cfg)."::reacquire() failed!");
      return undef;
    }
  }
  return 1;
}


##======================================================================
## MUDL::Make::Fields interface

## $val_or_undef = $cfg->pathValue(\@path)
##  + default version just follows \@path as hash keys
##  + first element of \@path may be the name of an aggregate function
sub pathValue {
  my ($cfg,$path) = @_;

  ##-- apply aggregate functions
  my $afunc = $cfg->aggregateFunction($path->[0]);
  return $afunc->pathValue($cfg,[@$path[1..$#$path]]) if (defined($afunc));

  ##-- no explicit aggregate; use default
  $afunc = $cfg->defaultAggregateFunction();
  return $afunc->pathValue($cfg,$path);
}

##======================================================================
## Aggregate functions: lookup
##  + aggregate functions are fully-qualified classnames

## $afunc = $cfg->defaultAggregateFunction()
sub defaultAggregateFunction {
  return $_[0]->aggregateFunction('min');
}

## $afunc_or_undef = $cfg->aggregateFunction($funcName)
sub aggregateFunction {
  return $_[1]
    if (UNIVERSAL::isa($_[1],
		       'MUDL::Make::Config::GroupFunction'));

  return "MUDL::Make::Config::GroupFunction::$_[1]"
    if (UNIVERSAL::isa("MUDL::Make::Config::GroupFunction::$_[1]",
		       'MUDL::Make::Config::GroupFunction'));

  return undef;
}

##======================================================================
## Aggregate functions: base
package MUDL::Make::Config::GroupFunction;
use Carp;
use strict;
our @ISA = qw();

##------------------------------------------------------
## Aggregate Functions: API

## $val = $afunc->pathValue($groupConfig, \@path)
##  + default calls initialValue(), addValue(), finalValue()
sub pathValue {
  my ($af,$group,$path) = @_;
  my $val = $af->initialValue($group,$path);
  $val = $af->addValue($val,$group,$path) foreach (@{$group->{subconfigs}});
  return $af->finalValue($val,$group,$path);
}

##------------------------------------------------------
## Aggregate Functions: default API

## $val = $af->initialValue($groupConfig,\@path)
sub initialValue { return undef; }

## $val = $af->addValue($val,$groupConfig,\@path)
sub addValue { return $_[1]; }

## $val = $af->finalValue($val,$groupConfig,\@path)
sub finalValue { return $_[1]; }

##------------------------------------------------------
## Aggregate Functions: utilities: comparator

sub compare {
  our ($a,$b);
  return (defined($a)
	  ? (defined($b)
	     ? ($a =~ /^[\+\-]?(?:\d*\.)?\d+(?:[Ee][\+\-]?\d+)?/
		? ($b =~ /^[\+\-]?(?:\d*\.)?\d+(?:[Ee][\+\-]?\d+)?/
		   ? $a <=> $b
		   : $a cmp $b)
		: $a cmp $b)
	     : 1)
	  : (defined($b)
	     ? -1
	     : 0));
}


##======================================================================
## Aggregate functions: min
package MUDL::Make::Config::GroupFunction::min;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($groupConfig, \@path)
sub pathValue {
  my ($af,$group,$path) = @_;
  my $min = undef;
  my ($cfg,$val);
  foreach $cfg (@{$group->{subconfigs}}) {
    next if (!defined($val=$cfg->pathValue($path)));
    $min = $val if (!defined($min) || MUDL::Make::Config::GroupFunction::compare($val,$min) < 0);
  }
  return $min;
}

##======================================================================
## Aggregate functions: max
package MUDL::Make::Config::GroupFunction::max;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($groupConfig, \@path)
sub pathValue {
  my ($af,$group,$path) = @_;
  my $max = undef;
  my ($cfg,$val);
  foreach $cfg (@{$group->{subconfigs}}) {
    next if (!defined($val=$cfg->pathValue($path)));
    $max = $val if (!defined($min) || MUDL::Make::Config::GroupFunction::compare($max,$val) < 0);
  }
  return $max;
}

##======================================================================
## Aggregate functions: sum
package MUDL::Make::Config::GroupFunction::sum;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($groupConfig, \@path)
sub pathValue {
  my ($af,$group,$path) = @_;
  my $sum = 0;
  my ($cfg,$val);
  foreach $cfg (@{$group->{subconfigs}}) {
    next if (!defined($val=$cfg->pathValue($path)));
    $sum += $val;
  }
  return $sum;
}

##======================================================================
## Aggregate functions: prod
package MUDL::Make::Config::GroupFunction::prod;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($groupConfig, \@path)
sub pathValue {
  my ($af,$group,$path) = @_;
  my $prod = 1;
  my ($cfg,$val);
  foreach $cfg (@{$group->{subconfigs}}) {
    next if (!defined($val=$cfg->pathValue($path)));
    $prod *= $val;
  }
  return $prod;
}

##======================================================================
## Aggregate functions: count (all)
package MUDL::Make::Config::GroupFunction::count;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($groupConfig, \@path)
sub pathValue {
  my ($af,$group,$path) = @_;
  return scalar(@{$group->{subconfigs}});
}

##======================================================================
## Aggregate functions: count (distinct)
package MUDL::Make::Config::GroupFunction::nvalues;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($groupConfig, \@path)
sub pathValue {
  my ($af,$group,$path) = @_;
  my ($cfg,$val);
  my %vals = qw();
  foreach $cfg (@{$group->{subconfigs}}) {
    $val = '' if (!defined($val=$cfg->pathValue($path)));
    $vals{$val} = undef;
  }
  return scalar(keys(%vals));
}

##======================================================================
## Aggregate functions: avg
package MUDL::Make::Config::GroupFunction::avg;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($groupConfig, \@path)
sub pathValue {
  my ($af,$group,$path) = @_;
  my $avg = 0;
  my ($cfg,$val);
  foreach $cfg (@{$group->{subconfigs}}) {
    $val  = 0 if (!defined($val=$cfg->pathValue($path)));
    $avg += $val;
  }
  return $avg ? ($avg/scalar(@{$group->{subconfigs}})) : 0;
}

##======================================================================
## Aggregate functions: variance
package MUDL::Make::Config::GroupFunction::var;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($groupConfig, \@path)
sub pathValue {
  my ($af,$group,$path) = @_;
  my $EX = $group->aggregateFunction('avg')->pathValue($group,$path);
  my $VarX = 0;
  my ($cfg,$val);
  foreach $cfg (@{$group->{subconfigs}}) {
    $val   = 0 if (!defined($val=$cfg->pathValue($path)));
    $VarX += $val**2;
  }
  $VarX /= scalar(@{$group->{subconfigs}}) if ($VarX);
  return $VarX - $EX**2;
}

##======================================================================
## Aggregate functions: standard deviation
package MUDL::Make::Config::GroupFunction::stddev;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($groupConfig, \@path)
sub pathValue {
  my ($af,$group,$path) = @_;
  return sqrt($group->aggregateFunction('var')->pathValue($group,$path));
}




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
