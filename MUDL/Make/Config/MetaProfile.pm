##-*- Mode: CPerl -*-

## File: MUDL::Make::Config::MetaProfile.pm
## Author: Bryan Jurish <moocow@cpan.org>
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

our $MAKE_TARGETS = join(' ',
			 #'stage-eval',
			 #'stage-tgs-eval',
			 #'stage-tgs-k-eval',
			 'stage-summary',
			);

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
##     eval_global    => $global_eval_summary,    ##-- a MUDL::Corpus::Profile::ITagEval::Summary object
##     eval_targets   => $target_eval_summary,    ##-- a MUDL::Corpus::Profile::ITagEval::Summary object
##     eval_targets_k => $targets_k_eval_summary, ##-- a MUDL::Corpus::Profile::ITagEval::Summary object
##     mpsummary      => \%info,                  ##-- as returned by $mp->getSummaryInfo()
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(
				     ##-- Make targets
				     targets  => $MAKE_TARGETS,
				     #userfile => 'user.mak',

				     ##-- acquisition data
				     eval_global  => undef,
				     eval_targets => undef,
				     eval_targets_k => undef,
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

  $cfg->{targets} = $MAKE_TARGETS;
  #$cfg->{userfile} = 'user.mak';
  delete(@$cfg{qw(eval_global eval_targets eval_targets_k mpsummary)});

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
  my $stage    = $cfg->{xvars}{stage};
  my $stagedir = $cfg->{xvars}{stagedir};

  ##-- load eval (summary): global
  my ($file);
  $file = "$stagedir/stage.eval.summary.bin";
  $cfg->{eval_global} = MUDL::Corpus::Profile::ITagEval->loadFile($file)
    or confess(ref($cfg),"::acquire(): could not load global-eval file '$file': $!");

  ##-- load eval (summary): targets
  $file = "$stagedir/stage.tgs.eval.summary.bin";
  $cfg->{eval_targets} = MUDL::Corpus::Profile::ITagEval->loadFile($file)
    or confess(ref($cfg),"::acquire(): could not load target-eval file '$file': $!");

  ##-- load eval (summary): targets_k
  $file = "$stagedir/stage.tgs-k.eval.summary.bin";
  $cfg->{eval_targets_k} = MUDL::Corpus::Profile::ITagEval->loadFile($file)
    or confess(ref($cfg),"::acquire(): could not load targets-k-eval file '$file': $!");

  ##-- load meta-profile summary
  $file = "$stagedir/stage.summary.bin";
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

  ##-- get common data
  my $stagedir = $cfg->{xvars}{stagedir};
  my $stage    = $cfg->{xvars}{stage};
  my $tbase    = $cfg->{xvars}{tbase};

  ##-- force remove: summaries
  $cfg->pushd($args{dir});
  unlink(glob("$stagedir/*.summary*"));
  $cfg->popd();

  ##-- now call make()
  return $cfg->make(%args);
}


1; ##-- make perl happy

