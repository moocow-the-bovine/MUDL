##-*- Mode: CPerl -*-

## File: MUDL::Make::Config::Group.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: group of makefile configurations
##======================================================================

package MUDL::Make::Config::Group;
use MUDL::Make::Config;
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
##     gconfigs => \@subconfigs, ##-- key&val pseudo-set of MUDL::Make::Config objects ("$cfg"=>$cfg)
##
##     ##-- Group-by variables
##     #gvars    => \%var2undef,  ##-- group-by variables
##    }
sub new {
  my $that = shift;
  my $self = bless $that->MUDL::Object::new
    (
     ##-- Subconfigurations
     gconfigs => [],

     ##-- Group-by variables:
     #gvars    => {},

     ##-- User args
     @_
    ), ref($that)||$that;

  return $self;
}

## $grp = $grp->clear;
sub clear {
  my $grp = shift;
  $_->clear() foreach (@{$grp->{gconfigs}});
  return $grp;
}

## destructor
sub DESTROY { ; }

##======================================================================
## Constructor: shadow

## $grp2 = $grp->copyBrief(\@user_vars_to_keep)
sub copyBrief {
  my ($grp,$keepvars) = @_;
  my $grp2 = ref($grp)->new(%$grp,
			    gconfigs=> [map {$_->copyBrief($keepvars)} @{$grp->{gconfigs}}],
			   );
  return $grp2;
}

##======================================================================
## Manipulation

## $grp = $grp->addConfig($subcfg)
##  + adds a subconfiguration
sub addConfig {
  my ($grp,$cfg) = @_;
  push(@{$grp->{gconfigs}},$cfg);
  return $grp;
}

##======================================================================
## Compilation

## $grp = $grp->groupCompile()
##  + compiles $grp->{uvars}, $grp->{xvars}
sub groupCompile {
  my $grp = shift;
  return $grp->groupCompileVars('uvars') && $grp->groupCompileVars('xvars');
}

## $grp = $grp->groupCompileVars($varKey)
##  + instantiates $grp->{$varKey} with a HASH containing
##    only those (key,value) pairs shared by all subconfigs' $varKey hashes
sub groupCompileVars {
  my ($grp,$which) = @_;

  ##-- get auxilliary map: ($var=>{$val=>$val})
  my %var2vals = qw();
  my ($cfg,$var,$val);
  foreach $cfg (@{$grp->{gconfigs}}) {
    while (($var,$val)=each(%{$cfg->{$which}})) {
      $var2vals{$var}{$val||''}=$val;
    }
  }

  ##-- compute group variables
  $grp->{$which} = {
		    map  { ($_=>((values(%{$var2vals{$_}}))[0])) }
		    grep { keys(%{$var2vals{$_}})==1 }
		    keys(%var2vals)
		   };

  return $grp;
}


##======================================================================
## Expansion: all variables

## $grp = $grp->expand($mudl_mak_vars)
##  + assigns user-variables in $grp->{vars}
##  + expands all variables to $grp->{xvars}
##  + copies $mudl_mak_vars
sub expand {
  my $grp = shift;
  foreach (@{$grp->{gconfigs}}) {
    if (!$_->expand(@_)) {
      confess(ref($grp)."::expand() failed!");
      return undef;
    }
  }
  return $grp;
}

## $grp = $grp->_expand($mudl_mak_vars)
##  + assigns user-variables in $grp->{vars}
##  + expands all variables to $grp->{xvars}
##  + destructively alters $mudl_mak_vars !
sub _expand {
  my $grp = shift;
  foreach (@{$grp->{gconfigs}}) {
    if (!$_->_expand(@_)) {
      confess(ref($grp)."::_expand() failed!");
      return undef;
    }
  }
  return $grp;
}


##======================================================================
## Makefile Generation: for user-vars

## $file = $grp->writeUserMakefile()
## $file = $grp->writeUserMakefile($filename_or_fh)
##  + writes variable assignments to a makefile
sub writeUserMakefile {
  my $grp = shift;
  confess(ref($grp)."::writeUserMakefile(): not implemented!");
  return undef;
}

##======================================================================
## Make: Directory juggling

## $cwd = $grp->pushd()
## $cwd = $grp->pushd($dir)
##  + chdir()s to $grp->{dir} or $dir, updates $grp->{dirstack}
sub pushd {
  my ($grp,$dir) = @_;
  confess(ref($grp)."::pushd(): not implemented!");
  return undef;
}

## $cwd = $grp->popd()
##  + pops the most recent directory from the stack (if any)
sub popd {
  my $grp = shift;
  confess(ref($grp)."::popd(): not implemented!");
  return undef;
}


##======================================================================
## Make: Targets

## $bool = $grp->make(%args)
##  + calls $_->make() on each subconfig
sub make {
  my $grp = shift;
  foreach (@{$grp->{gconfigs}}) {
    if (!$_->make(@_)) {
      confess(ref($grp)."::make() failed!");
      return undef;
    }
  }
  return 1;
}


##======================================================================
## Make: Acquire Data

## $bool = $grp->acquire(%args)
##  + called after successful $grp->make()
##  + may be implemented in child classes for data acquisition
##  + the process is already chdir()d to the $grp->{dir} when this method is called!
sub acquire { return 1; }

## $bool = $grp->reacquire(%args)
##  + attempts to call reacquire() on each subconfig
sub reacquire {
  my $grp = shift;
  foreach (@{$grp->{gconfigs}}) {
    if (!UNVIERSAL::can($_,'reacquire') || !$_->reacquire(@_)) {
      confess(ref($grp)."::reacquire() failed!");
      return undef;
    }
  }
  return 1;
}


##======================================================================
## MUDL::Make::Fields interface

## $val_or_undef = $grp->pathValue(\@path)
##  + default version just follows \@path as hash keys
##  + first element of \@path may be the name of an aggregate function
sub pathValue {
  my ($grp,$path) = @_;

  ##-- sanity check
  return $grp if (!@$path);

  ##-- apply aggregate functions
  my $afunc = $grp->aggregateFunction($path->[0]);
  return $afunc->pathValue($grp,[@$path[1..$#$path]]) if (defined($afunc));

  ##-- no explicit aggregate; use default
  $afunc = $grp->defaultAggregateFunction();
  return $afunc->pathValue($grp,$path);
}

##======================================================================
## Aggregate functions: lookup
##  + aggregate functions are fully-qualified classnames

## $afunc = $grp->defaultAggregateFunction()
sub defaultAggregateFunction {
  return $_[0]->aggregateFunction('max');
}

## $afunc_or_undef = $grp->aggregateFunction($funcName)
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

## $val = $afunc->pathValue($grpConfig, \@path)
##  + default calls initialValue(), addValue(), finalValue()
sub pathValue {
  my ($af,$grp,$path) = @_;
  my $val = $af->initialValue($grp,$path);
  $val = $af->addValue($val,$grp,$path) foreach (@{$grp->{gconfigs}});
  return $af->finalValue($val,$grp,$path);
}

##------------------------------------------------------
## Aggregate Functions: default API

## $val = $af->initialValue($grpConfig,\@path)
sub initialValue { return undef; }

## $val = $af->addValue($val,$grpConfig,\@path)
sub addValue { return $_[1]; }

## $val = $af->finalValue($val,$grpConfig,\@path)
sub finalValue { return $_[1]; }

##------------------------------------------------------
## Aggregate Functions: utilities: comparator

sub compare($$) {
  our ($a,$b)=@_;
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
## $val = $afunc->pathValue($grpConfig, \@path)
sub pathValue {
  my ($af,$grp,$path) = @_;
  my $min = undef;
  my ($cfg,$val);
  foreach $cfg (@{$grp->{gconfigs}}) {
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
## $val = $afunc->pathValue($grpConfig, \@path)
sub pathValue {
  my ($af,$grp,$path) = @_;
  my $max = undef;
  my ($cfg,$val);
  foreach $cfg (@{$grp->{gconfigs}}) {
    next if (!defined($val=$cfg->pathValue($path)));
    $max = $val if (!defined($max) || MUDL::Make::Config::GroupFunction::compare($max,$val) < 0);
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
## $val = $afunc->pathValue($grpConfig, \@path)
sub pathValue {
  my ($af,$grp,$path) = @_;
  my $sum = 0;
  my ($cfg,$val);
  foreach $cfg (@{$grp->{gconfigs}}) {
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
## $val = $afunc->pathValue($grpConfig, \@path)
sub pathValue {
  my ($af,$grp,$path) = @_;
  my $prod = 1;
  my ($cfg,$val);
  foreach $cfg (@{$grp->{gconfigs}}) {
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
## $val = $afunc->pathValue($grpConfig, \@path)
sub pathValue {
  my ($af,$grp,$path) = @_;
  return scalar(@{$grp->{gconfigs}});
}

##======================================================================
## Aggregate functions: count (distinct)
package MUDL::Make::Config::GroupFunction::nvalues;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($grpConfig, \@path)
sub pathValue {
  my ($af,$grp,$path) = @_;
  my ($cfg,$val);
  my %vals = qw();
  foreach $cfg (@{$grp->{gconfigs}}) {
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
## $val = $afunc->pathValue($grpConfig, \@path)
sub pathValue {
  my ($af,$grp,$path) = @_;
  my $avg = 0;
  my ($cfg,$val);
  foreach $cfg (@{$grp->{gconfigs}}) {
    $val  = 0 if (!defined($val=$cfg->pathValue($path)));
    $avg += $val;
  }
  return $avg ? ($avg/scalar(@{$grp->{gconfigs}})) : 0;
}

##======================================================================
## Aggregate functions: variance
package MUDL::Make::Config::GroupFunction::var;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($grpConfig, \@path)
sub pathValue {
  my ($af,$grp,$path) = @_;
  my $EX = $grp->aggregateFunction('avg')->pathValue($grp,$path);
  my $VarX = 0;
  my ($cfg,$val);
  foreach $cfg (@{$grp->{gconfigs}}) {
    $val   = 0 if (!defined($val=$cfg->pathValue($path)));
    $VarX += $val**2;
  }
  $VarX /= scalar(@{$grp->{gconfigs}}) if ($VarX);
  return $VarX - $EX**2;
}

##======================================================================
## Aggregate functions: standard deviation
package MUDL::Make::Config::GroupFunction::stddev;
use Carp;
use strict;
our @ISA = qw(MUDL::Make::Config::GroupFunction);
##
## $val = $afunc->pathValue($grpConfig, \@path)
sub pathValue {
  my ($af,$grp,$path) = @_;
  return sqrt($grp->aggregateFunction('var')->pathValue($grp,$path));
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
