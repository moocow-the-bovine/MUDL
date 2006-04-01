##-*- Mode: CPerl -*-

## File: MUDL::Make::Collection.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: makefile configurations: collections
##======================================================================

package MUDL::Make::Collection;
use MUDL::Object;
use MUDL::Make qw(:all);
use MUDL::Make::Vars;
use MUDL::Make::Config;
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
##     ##-- Configuration
##     makefiles  => \@makefiles,      ##-- default: [glob('[Mm]akefile')]
##     varfiles   => \@varfiles,       ##-- default: undef (use $obj->{makefiles})
##     vars       => $mudl_make_vars,  ##-- parsed vars: shared
##     uconfigs   => \%ukey2config,    ##-- key=>$config map: user vars
##     xconfigs   => \%xkey2config,    ##-- key=>$config map: expanded vars
##     ##
##     ##-- Key Generation
##     rs => $record_separator,  ##-- separates ($var,$val) pairs in keys
##     fs => $field_separator,   ##-- separates $var from $val within ($var,$val) pairs in keys
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(
				     ##-- Configs
				     makefiles=>[glob('[Mm]akefile')],
				     varfiles =>undef,
				     vars     =>MUDL::Make::Vars->new(),

				     uconfigs  =>{},
				     xconfigs  =>{},

				     ##-- Key Generation (ASCII)
				     #rs=>"\036", ##-- ASCII RS
				     #fs=>"\034", ##-- ASCII FS
				     ##-- Key Generation (Human-readable; may be unsafe)
				     rs=>" && ",
				     fs=>"=>",


				     ##-- User Args
				     @_
				    ), ref($that)||$that;
  return $self;
}

## $mcol = $mcol->clear;
sub clear {
  my $mcol = shift;

  ##-- Variables
  $mcol->{vars}->clear();

  ##-- Configs
  %{$mcol->{uconfigs}} = qw();
  %{$mcol->{xconfigs}} = qw();

  return $mcol;
}

## $mcol2 = $mcol->subcollection(%args)
##  + shared non-configuration elements (including vars)
##  + %args are args to ref($mcol)->new()
sub subcollection {
  my $mcol = shift;
  return ref($mcol)->new(
			 makefiles=>$mcol->{makefiles},
			 varfiles =>$mcol->{varfiles},
			 vars     =>$mcol->{vars},
			 rs       =>$mcol->{rs},
			 fs       =>$mcol->{fs},
			 ##-- User Args
			 @_
			);
}

## DESTROY : hook: clear configs
sub DESTROY { $_[0]->clear(); }

##======================================================================
## Initialization: Parse Makefiles

## $mcol = $mcol->parse(%args)
##  + %args:
##     makefiles=>\@makefiles,   ##-- overrides $cfg->{varfiles}, $cfg->{makefiles}
sub parse {
  my ($mcol,%args) = @_;
  my @makefiles = ($args{makefiles}
		   ? @{$args{makefiles}}
		   : ($mcol->{varfiles}
		      ? @{$mcol->{varfiles}}
		      : @{$mcol->{makefiles}}));
  $mcol->{vars}->parse($_) foreach (@makefiles);
  return $mcol;
}

## $vars = $mcol->parse_p(%args)
##  + %args:
##     makefiles=>\@makefiles,   ##-- overrides $cfg->{varfiles}, $cfg->{makefiles}
sub parse_p {
  my ($mcol,%args) = @_;
  my @makefiles = ($args{makefiles}
		   ? @{$args{makefiles}}
		   : ($mcol->{varfiles}
		      ? @{$mcol->{varfiles}}
		      : @{$mcol->{makefiles}}));
  $mcol->{vars}->parse_p($_) foreach (@makefiles);
  return $mcol;
}


##======================================================================
## Key Generation

## $key = $mcol->key(\%var2val)
sub key {
  my ($mcol,$var2val) = @_;
  return join($mcol->{rs}, map { $_ . $mcol->{fs} . $var2val->{$_} } sort(keys(%$var2val)));
}

## $ukey = $mcol->ukey($cfg)
##  + key for user-vars of $cfg (a MUDL::Make::Config)
sub ukey { return $_[0]->key($_[1]->{uvars}); }

## $xkey = $mcol->xkey($cfg)
##  + key for expanded variables of $cfg (a MUDL::Make::Config)
##  + requires: $cfg->{xvars}
sub xkey { return $_[0]->key($_[1]->{xvars}); }

#sub xkey { return $_[0]->key($_[0]{vars}->clone->assign($_[1]{uvars})->expand()); }



##======================================================================
## Accessors: config: by user vars

## $cfg = $mcol->ufind(\%uvars)
## $cfg = $mcol->ufind($ukey)
##  + no implicit config-creation
sub ufind {
  my ($mcol,$uvars) = @_;
  my $ukey = ref($uvars) ? $mcol->key($uvars) : $uvars;
  return $mcol->{uconfigs}{$ukey};
}

## $cfg = $mcol->uget(\%uvars, %args)
## $cfg = $mcol->uget($ukey,   %args)
##  + implicitly creates config for \%uvars if none exists
##  + implicitly parses $ukey if it is passed as a string (may be dangerous!)
##  + %args:
##     expand=>$bool, ##-- if true, newly created configs will be auto-expanded
sub uget {
  my ($mcol,$uvars,%args) = @_;
  my ($ukey);
  if (ref($uvars)) {
    $ukey = $mcol->key($uvars);
  } else {
    ##-- unsafe: parse user-variables from string
    $ukey  = $uvars;
    $uvars = { map { split(/\Q$mcol->{fs}\E/,$_,2) } split(/\Q$mcol->{rs}\E/, $ukey) };
  }
  return $mcol->{uconfigs}{$ukey} if (defined($mcol->{uconfigs}{$ukey}));

  ##-- auto-create new config
  my $cfg = $mcol->{uconfigs}{$ukey} = MUDL::Make::Config->new(uvars => $uvars,
							      );
  $mcol->expandConfig($cfg) if ($args{expand});

  return $cfg;
}


##======================================================================
## Accessors: config: by expanded vars

## $cfg = $mcol->xfind(\%xvars)
## $cfg = $mcol->xfind($xkey)
##  + no implicit config-creation
sub xfind {
  my ($mcol,$xvars) = @_;
  my $xkey = ref($xvars) ? $mcol->key($xvars) : $xvars;
  return $mcol->{xconfigs}{$xkey};
}

##======================================================================
## Expansion

## $cfg = $mcol->expandConfig($cfg)
##  + expands $cfg according to $mcol->{vars}
##  + adds expanded key to $mcol->{xconfigs}
sub expandConfig {
  my ($mcol,$cfg) = @_;
  $cfg->expand($mcol->{vars});
  return $mcol->{xconfigs}{$mcol->xkey($cfg)} = $cfg;
}

## $mcol = $mcol->expandAll()
##  + re-expands all configs according to $mcol->{vars}
sub expandAll {
  my $mcol = shift;
  my ($ukey);
  foreach $ukey (keys(%{$mcol->{uconfigs}})) {
    $mcol->expandConfig($mcol->{uconfigs}{$ukey});
  }
  return $mcol;
}

## $mcol = $mcol->expandMissing()
##  + expands all configs $cfg with empty %{$cfg->{xvars}}
sub expandMissing {
  my $mcol = shift;
  my ($ukey,$cfg);
  foreach $ukey (keys(%{$mcol->{uconfigs}})) {
    $cfg = $mcol->{uconfigs}{$ukey};
    $mcol->expandConfig($cfg) if (!$cfg->{xvars} || !%{$cfg->{xvars}});
  }
  return $mcol;
}


##======================================================================
## Search

## @configs = $mcol->search(\&criterion_code)
## @configs = $mcol->search( $criterion_str)
##   + CODE ref: returns a list of all configs for which $criterion is true
##     - if $criterion is a CODE ref, it is called with argument $cfg (config to test)
##     - if $criterion is a string  , it is evaluated with variable $_ set to the config to test
sub search {
  my ($mcol,$crit) = @_;
  return grep { $crit->($_) } values(%{$mcol->{uconfigs}}) if (ref($crit));

  my ($rc);
  return
    (grep {
      $rc=eval($crit);
      carp(ref($mcol)."::search(): error evaluating search criteria '$crit': $@") if ($@);
      $rc
    } values(%{$mcol->{uconfigs}}));
}

## @configs = $mcol->vsearch($var_key, \&criterion_code)
## @configs = $mcol->vsearch($var_key,  $criterion_str)
##   + CODE ref: returns a list of all configs for which $criterion is true
##     - if $criterion is a CODE ref, it is called with argument $cfg->{$var_key} (vars of config to test)
##     - if $criterion is a string  , it is evaluated with variable %_ set to %{$cfg->{$var_key}}
sub vsearch {
  my ($mcol,$vkey,$crit) = @_;
  return grep { $crit->($_->{$vkey}) } values(%{$mcol->{uconfigs}}) if (ref($crit));

  my @configs = qw();
  my ($cfg);
  foreach $cfg (values(%{$mcol->{uconfigs}})) {
    $_ = $cfg->{$vkey};
    %_ = %$_;
    push(@configs,$cfg) if (eval $crit);
    carp(ref($mcol)."::vsearch(): error evaluating search criteria '$crit': $@") if ($@);
  }
  return @configs;
}

## @configs = $mcol->usearch(\&criterion_code)
## @configs = $mcol->usearch( $criterion_str)
##  + wrapper for $mcol->vsearch('uvars', $criterion)
sub usearch { return $_[0]->vsearch('uvars', $_[1]); }

## @configs = $mcol->xsearch(\&criterion_code)
## @configs = $mcol->xsearch( $criterion_str)
##  + wrapper for $mcol->vsearch('xvars', $criterion)
sub xsearch { return $_[0]->vsearch('xvars', $_[1]); }


##======================================================================
## Collect (sub-collection creation)

## $subcol = $mcol->collectConfigs(@configs)
sub collectConfigs {
  my ($mcol,@configs) = @_;
  my $subcol = $mcol->subcollection();
  %{$subcol->{uconfigs}} = map { $mcol->ukey($_)=>$_ } @configs;
  %{$subcol->{xconfigs}} = map { $mcol->xkey($_)=>$_ } grep { $_->{xvars} && %{$_->{xvars}} } @configs;
  return $subcol;
}

## $subcol = $mcol->collect(\&criterion_code)
## $subcol = $mcol->collect( $criterion_str)
##   + CODE ref: returns a list of all configs for which $criterion is true
##     - if $criterion is a CODE ref, it is called with argument $cfg (config to test)
##     - if $criterion is a string  , it is evaluated with variable $_ set to the config to test
sub collect { return $_[0]->collectConfigs($_[0]->search($_[1])); }

## $subcol = $mcol->vcollect($var_key, $criterion)
sub vcollect { return $_[0]->collectConfigs($_[0]->vsearch(@_[1,2])); }

## $subcol = $mcol->ucollect($criterion)
sub ucollect { return $_[0]->collectConfigs($_[0]->usearch($_[1])); }

## $subcol = $mcol->xcollect($criterion)
sub xcollect { return $_[0]->collectConfigs($_[0]->xsearch($_[1])); }


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
