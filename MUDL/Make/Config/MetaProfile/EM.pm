##-*- Mode: CPerl -*-

## File: MUDL::Make::Config::MetaProfile::EM.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: makefile configuration:
##    : MUDL::Corpus::MetaProfile & descendants: EM
##======================================================================

package MUDL::Make::Config::MetaProfile::EM;
use MUDL::Make::Config::MetaProfile;
use MUDL::CmdUtils qw(load loadModule);
use MUDL::Corpus::Profile::PdlProfile::ITagEval;
use IO::File;
use Carp;
use strict;

our @ISA = qw(MUDL::Make::Config::MetaProfile);

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
##     dirstack => \@stack,         ##-- directory stack
##     targets  => $targets_str,    ##-- default: 'all'
##
##  + re-implemented MUDL::Make::Config::MetaProfile::EM from MUDL::Make::Config::MetaProfile
##     eval_global    => $global_eval_summary,    ##-- a MUDL::Corpus::Profile::PdlFile::ITagEval::Summary
##     eval_targets   => $target_eval_summary,    ##-- a MUDL::Corpus::Profile::PdlFile::ITagEval::Summary
##     eval_targets_k => $targets_k_eval_summary, ##-- a MUDL::Corpus::Profile::PdlFile::ITagEval::Summary
##     mpsummary      => \%info,                  ##-- as returned by $mp->getSummaryInfo() [base data]
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(
				     ##-- Make targets
				     targets  => 'em-evals',
				     #userfile => 'user.mak',

				     ##-- acquisition data
				     eval_global    => undef,
				     eval_targets   => undef,
				     eval_targets_k => undef,
				     mpsummary      => undef,

				     ##-- User args
				     @_
				    ), ref($that)||$that;
  return $self;
}

## $cfg = $cfg->clear;
sub clear {
  my $cfg = shift;
  $cfg->SUPER::clear();

  $cfg->{targets} = 'em-global-eval em-tgs-eval em-tgs-k-eval';
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
  my $emdir = $cfg->{xvars}{emdir};
  my $emi   = $cfg->{xvars}{emi};
  my $tbase = $cfg->{xvars}{tbase};

  ##-- acquire Config::MetaProfile eval data
  $cfg->SUPER::acquire(%args);
  @$cfg{qw(mpeval_global mpeval_targets mpeval_targets_k)} = @$cfg{qw(eval_global eval_targets eval_targets_k)};

  ##-- get basename for summary files
  my $filebase = $cfg->{xvars}{emhmm};
  $filebase  =~ s/\.bin$//;
  #$filebase .=  ".t-${tbase}";

  ##-- load eval (summary): global
  my ($file);
  $file = "${filebase}.t-${tbase}.eval.summary.bin";
  $cfg->{eval_global} = MUDL::Corpus::Profile->loadFile($file)
    or confess(ref($cfg),"::acquire(): could not load global-eval file '$file': $!");

  ##-- load eval (summary): targets
  $file = "${filebase}.tgs.t-${tbase}.eval.summary.bin";
  $cfg->{eval_targets} = MUDL::Corpus::Profile->loadFile($file)
    or confess(ref($cfg),"::acquire(): could not load target-eval file '$file': $!");

  ##-- load eval (summary): targets_k
  $file = "${filebase}.tgs-k.t-${tbase}.eval.summary.bin";
  $cfg->{eval_targets_k} = MUDL::Corpus::Profile->loadFile($file)
    or confess(ref($cfg),"::acquire(): could not load targets-k-eval file '$file': $!");

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
  my $emdir = $cfg->{xvars}{emdir};
  my $emi   = $cfg->{xvars}{emi};
  my $tbase = $cfg->{xvars}{tbase};

  ##-- get basename for summary files
  my $filebase = $cfg->{xvars}{emhmm};
  $filebase  =~ s/\.bin$//;

  ##-- re-acquire: global
  my ($base,$eval,$esum);
  $base   = "${filebase}.t-${tbase}.eval";
  $eval = MUDL::Corpus::Profile::ITagEval->loadFile("$base.bin")
    or confess(ref($cfg),"::reacquire(): could not load global-eval file '$base.bin': $!");
  $eval->finish();
  $esum = $eval->summary();
  $eval->saveFile("$base.bin");
  $esum->saveFile("$base.summary.bin");
  $cfg->{eval_global} = $esum;

  ##-- re-acquire: targets
  $base   = "${filebase}.tgs.t-${tbase}.eval";
  $eval = MUDL::Corpus::Profile::ITagEval->loadFile("$base.bin")
    or confess(ref($cfg),"::reacquire(): could not load targets-eval file '$base.bin': $!");
  $eval->finish();
  $esum = $eval->summary();
  $eval->saveFile("$base.bin");
  $esum->saveFile("$base.summary.bin");
  $cfg->{eval_targets} = $esum;

  ##-- re-acquire: targets_k
  $base   = "${filebase}.tgs-k.t-${tbase}.eval";
  $eval = MUDL::Corpus::Profile::ITagEval->loadFile("$base.bin")
    or confess(ref($cfg),"::reacquire(): could not load targets-k-eval file '$base.bin': $!");
  $eval->finish();
  $esum = $eval->summary();
  $eval->saveFile("$base.bin");
  $esum->saveFile("$base.summary.bin");
  $cfg->{eval_targets_k} = $esum;

  ##-- pop chdir()
  $cfg->popd();

  return $cfg;
}


1; ##-- make perl happy
