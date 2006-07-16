##-*- Mode: CPerl -*-

## File: MUDL::Make::Config::MetaProfile.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: makefile configuration: MUDL::Corpus::MetaProfile & descendants
##======================================================================

package MUDL::Make::Config::MetaProfile;
use MUDL::Make::Config;
use MUDL::Corpus::Profile::ITagEval;
use MUDL::CmdUtils qw(load loadModule);
use IO::File;
use Carp;
use strict;

our @ISA = qw(MUDL::Make::Config);

##======================================================================
## Globals
our $DEBUG = 0;

##======================================================================
## OBJECT STRUCTURE: hash
##  + inherited from MUDL::Make::Config
##     ##
##     ##-- Variables
##     #vars  => $mudl_make_vars,
##     uvars => $user_variables,
##     xvars => \%expanded_vars,
##     ##
##     ##-- Targets
##     dir      => $make_directory, ##-- directory in which to run make (default: '.')
##     dirstack => \@stack,        ##-- directory stack
##     targets  => $targets_str,    ##-- default: 'all'
##
##  + new in MUDL::Make::Config::MetaProfile
##     eval_global  => $global_eval_summary, ##-- a MUDL::Corpus::Profile::ITagEval::Summary object
##     eval_targets => $target_eval_summary, ##-- a MUDL::Corpus::Profile::ITagEval::Summary object
##     mpsummary    => \%info,               ##-- as returned by $mp->getSummaryInfo()
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(
				     ##-- Make targets
				     targets  => 'stage-summary-view',
				     userfile => 'user.mak',

				     ##-- acquisition data
				     eval_global  => undef,
				     eval_targets => undef,
				     mpsummary    => undef,

				     ##-- User args
				     @_
				    ), ref($that)||$that;
  return $self;
}

## $cfg = $cfg->clear;
sub clear {
  my $cfg = shift;
  $cfg->SUPER::clear();

  $cfg->{targets} = 'stage-summary-view';
  $cfg->{userfile} = 'user.mak';
  delete(@$cfg{qw(eval_global eval_targets mpsummary)});

  return $cfg;
}


##======================================================================
## Make: Acquire Data

## $bool = $cfg->acquire(%args)
##  + called after successful $cfg->make()
##  + may be implemented in child classes for data acquisition
##  + the process is already chdir()d to the $cfg->{dir} when this method is called!
sub acquire {
  my ($cfg,%args) = @_;

  ##-- get common data
  my $mpdir = $cfg->{xvars}{mpdir};
  my $stage = $cfg->{xvars}{stage};

  ##-- load eval (summary): global
  my ($file);
  $file = "$mpdir/stage${stage}.eval.summary.bin";
  $cfg->{eval_global} = MUDL::Corpus::Profile::ITagEval->loadFile($file)
    or confess(ref($cfg),"::acquire(): could not load global-eval file '$file': $!");

  ##-- load eval (summary): targets
  $file = "$mpdir/stage${stage}.tgs.eval.summary.bin";
  $cfg->{eval_targets} = MUDL::Corpus::Profile::ITagEval->loadFile($file)
    or confess(ref($cfg),"::acquire(): could not load target-eval file '$file': $!");

  ##-- load meta-profile summary
  $file = "$mpdir/stage${stage}.summary.bin";
  $cfg->{mpsummary} = MUDL::Object->loadFile($file)
    or confess(ref($cfg),"::acquire(): could not load MetaProfile summary file '$file': $!");

  return $cfg;
}

##======================================================================
## Re-Acquire Data (call finish())

## $bool = $cfg->reacquire(%args)
##  + may be re-implemented in child classes for data re-acquisition
sub reacquire {
  my ($cfg,%args) = @_;

  ##-- do chdir()
  $cfg->pushd($args{dir});

  ##-- get common data
  my $mpdir = $cfg->{xvars}{mpdir};
  my $stage = $cfg->{xvars}{stage};
  my $tbase = $cfg->{xvars}{tbase};

  ##-- re-acquire: global
  my ($base,$eval,$esum);
  $base = "$mpdir/stage${stage}.t-${tbase}.eval";
  $eval = MUDL::Corpus::Profile::ITagEval->loadFile("$base.bin")
    or confess(ref($cfg),"::reacquire(): could not load global-eval file '$base.bin': $!");
  $eval->finish();
  $esum = $eval->summary();
  $eval->saveFile("$base.bin");
  $esum->saveFile("$base.summary.bin");
  $cfg->{eval_global} = $esum;

  ##-- re-acquire: targets
  $base   = "$mpdir/stage${stage}.t-${tbase}.tgs.eval";
  $eval = MUDL::Corpus::Profile::ITagEval->loadFile("$base.bin")
    or confess(ref($cfg),"::reacquire(): could not load targets-eval file '$base.bin': $!");
  $eval->finish();
  $esum = $eval->summary();
  $eval->saveFile("$base.bin");
  $esum->saveFile("$base.summary.bin");
  $cfg->{eval_targets} = $esum;

  ##-- pop chdir()
  $cfg->popd();

  return $cfg;
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

Copyright (c) 2006, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
