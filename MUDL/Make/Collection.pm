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
use MUDL::Make::Config::Group;
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
  $mcol->{vars}->clear() if (defined($mcol->{vars}));

  ##-- Configs
  $mcol->clearConfigs();

  return $mcol;
}

## $mcol = $mcol->clearConfigs()
sub clearConfigs {
  my $mcol = shift;
  %{$mcol->{uconfigs}} = qw() if ($mcol->{uconfigs});
  %{$mcol->{xconfigs}} = qw() if ($mcol->{xconfigs});;
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
sub DESTROY { $_[0]->clearConfigs(); }

##======================================================================
## Manipulation: remove 

## $mcol = $mcol->removeCollection($subCollection)
sub removeCollection {
  my ($mcol,$subcol) = @_;
  delete(@{$mcol->{uconfigs}}{keys(%{$subcol->{uconfigs}})});
  delete(@{$mcol->{xconfigs}}{keys(%{$subcol->{xconfigs}})});
  return $mcol;
}

## $mcol = $mcol->removeConfigs(\@configs)
sub removeConfigs {
  my ($mcol,$configs) = @_;
  delete(@{$mcol->{uconfigs}}{map {$mcol->ukey($_)} @$configs});
  delete(@{$mcol->{xconfigs}}{map {$mcol->xkey($_)} @$configs});
  return $mcol;
}


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
## $key = $mcol->key(\%var2val,\@vars)
sub key {
  my ($mcol,$var2val,$keys) = @_;
  $keys = [keys(%$var2val)] if (!$keys);
  return join($mcol->{rs}, map { $_ . $mcol->{fs} . $var2val->{$_} } sort(@$keys));
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
##     class=>$class, ##-- config class (default: MUDL::Make::Config) [also sets $cfg->{class}]
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
  my $class = $args{class} ? $args{class} : 'MUDL::Make::Config';
  my $cfg = $mcol->{uconfigs}{$ukey} = $class->new(uvars => $uvars);
  $mcol->expandConfig($cfg) if ($args{expand});

  ##-- set config 'class' key
  my $cclass = $class;
  $cclass =~ s/^MUDL::Make::Config(?:\:*)//;
  $cfg->{class} = $cclass;

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
  print STDERR ref($mcol), "::expandConfig(", $mcol->ukey($cfg), ")...\n";
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
## Re-index

## $mcol = $mcol->reindex();
##  + just re-indexes, no re-expansion is performed
sub reindex {
  my ($mcol,%args) = @_;
  my @configs = values(%{$mcol->{uconfigs}});
  $mcol->clearConfigs();
  my ($cfg);
  foreach $cfg (@configs) {
    $mcol->{uconfigs}{$mcol->ukey($cfg)} = $mcol->{xconfigs}{$mcol->xkey($cfg)} = $cfg;
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
  my $tmp = $_;
  foreach $cfg (values(%{$mcol->{uconfigs}})) {
    #$_ = $cfg->{$vkey};
    $_ = $cfg;
    %_ = %{$cfg->{$vkey}};
    push(@configs,$cfg) if (eval $crit);
    carp(ref($mcol)."::vsearch(): error evaluating search criteria '$crit': $@") if ($@);
  }
  $_ = $tmp;
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

##======================================================================
## Grouping (subcollection)

## $grp = $mcol->newGroup()
sub newGroup {
  return MUDL::Make::Config::Group->new();
}

## \%key2grp = $mcol->vgroupby($varkey,$vars)  ##-- scalar context
## @groups   = $mcol->vgroupby($varkey,$vars)  ##-- array context
##  + $vars may be:
##    - a key pseudo-set HASH ref of variable names
##    - an ARRAY ref of var-names [internal]
##    - a (comma+space)-separated list
sub vgroupby {
  my ($col,$vkey,$vars) = @_;

  ##-- parse $vars
  if (!ref($vars)) { $vars = [ grep {defined($_) && $_ ne ''} split(/\,\s*/, $vars) ]; }
  elsif (ref($vars) eq 'HASH') { $vars = [ keys(%$vars) ]; }
  #elsif (ref($vars) eq 'ARRAY') { $vars = { map {($_=>undef)} } @$vars; }

  ##-- get map from ($subKey => $group)
  ##   + also store config-keys for sort (extended)
  my %key2grp = qw();
  my ($xkey,$cfg,$cckey, %cfg2xkey);
  while (($xkey,$cfg)=each(%{$col->{xconfigs}})) {
    $cckey = $col->key($cfg->{$vkey},$vars);
    $key2grp{$cckey} = $col->newGroup() if (!$key2grp{$cckey});
    $key2grp{$cckey}->addConfig($cfg);

    ##-- temporary map
    $cfg2xkey{$cfg}=$xkey;
  }

  ##-- finish groups
  my ($grp);
  foreach $grp (values(%key2grp)) {
    ##-- sort group internally by user-key
    @{$grp->{gconfigs}} = sort { $cfg2xkey{$a} cmp $cfg2xkey{$b} } @{$grp->{gconfigs}};
    ##-- compile group variables
    $grp->groupCompile();
  }

  return wantarray ? values(%key2grp) : \%key2grp;
}

## \%key2grp = $mcol->vgroupover($varkey,$vars)  ##-- scalar context
## @groups   = $mcol->vgroupover($varkey,$vars)  ##-- array context
##  + $vars may be:
##    - a key pseudo-set HASH ref of variable names [internal]
##    - an ARRAY ref of var-names
##    - a (comma+space)-separated list
sub vgroupover {
  my ($col,$vkey,$vars) = @_;

  ##-- parse $vars
  if (!ref($vars)) { $vars = {map {($_=>undef)} grep {defined($_) && $_ ne ''} split(/\,\s*/, $vars)}; }
  #elsif (ref($vars) eq 'HASH') { $vars = [ keys(%$vars) ]; }
  elsif (ref($vars) eq 'ARRAY') { $vars = {map {($_=>undef)} @$vars}; }

  ##-- delegate to $col->groupby(): get 'groupby()' variables
  my %byvars = qw();
  my ($cfg);
  foreach $cfg (values(%{$col->{xconfigs}})) {
    @byvars{grep {!exists($vars->{$_})} keys(%{$cfg->{$vkey}})} = undef;
  }

  return $col->vgroupby($vkey,[keys(%byvars)]);
}

## $subcol = $mcol->ugroupby($vars)
sub ugroupby { return $_[0]->collectConfigs($_[0]->vgroupby('uvars',$_[1])); }

## $subcol = $mcol->xgroupby($vars)
sub xgroupby { return $_[0]->collectConfigs($_[0]->vgroupby('xvars',$_[1])); }

## $subcol = $mcol->ugroupover($vars)
sub ugroupover { return $_[0]->collectConfigs($_[0]->vgroupover('uvars',$_[1])); }

## $subcol = $mcol->xgroupover($vars)
sub xgroupover { return $_[0]->collectConfigs($_[0]->vgroupover('xvars',$_[1])); }

## $subcol = $mcol->ungroup()
##   + pops one grouping level
sub ungroup {
  my $col = shift;
  return $col->collectConfigs(
			       map {
				 (UNIVERSAL::isa($_,'MUDL::Make::Config::Group')
				  ? @{$_->{gconfigs}}
				  : $_)
			       } values(%{$col->{xconfigs}})
			     );
}

##======================================================================
## I/O: Native (perl)

sub saveNativeFh { return $_[0]->savePerlFh(@_[1..$#_]); }
sub loadNativeFh { return $_[0]->loadPerlFh(@_[1..$#_]); }


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
