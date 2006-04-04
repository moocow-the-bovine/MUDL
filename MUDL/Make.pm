##-*- Mode: CPerl -*-

## File: MUDL::Make.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: makefile stuff
##======================================================================

package MUDL::Make;
use Cwd qw(abs_path);
use strict;
use Carp;
our @ISA = qw(MUDL::Object Exporter);

##======================================================================
## Globals
#our $MAKE  = 'env -i make'; ##-- PDL don't like this
our $MAKE  = 'make';

our %EXPORT_TAGS =
  (
   progs=>[qw($MAKE)],
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK    = @{$EXPORT_TAGS{all}};
our @EXPORT       = qw();

##======================================================================
## Constructor (inherited)
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   ##-- Collection
			   col=>undef,
			   colfile=>undef,
			   dir=>'..',
			   varfile=>'../Default.mak',
			   userfile=>'user.mak',

			   ##-- Selection
			   selected=>undef,

			   ##-- Status
			   dirty=>0,
			   loaded=>0,
			   paranoid=>1,

			   ##-- Dummy?
			   dummy=>0,

			   ##-- User args
			   @_,
			  );
}

########################################################################
## ACTIONS: General
########################################################################

##======================================================================
## Actions: alias table
our %ACTIONS = qw();

$ACTIONS{null} = $ACTIONS{dummy} = $ACTIONS{','} =
  { syntax=>'null', help=>'no action', code=>sub { return 1; }, };


##======================================================================
## Actions: perform

## $bool = $mak->perform($action,\@ARGV);
sub perform {
  my ($mak,$act,$argv) = @_;
  my $acth = $act;
  $acth = $ACTIONS{$acth} while (defined($ACTIONS{$acth}));

  my $code = ref($acth) ? $acth->{code} : $mak->can($acth);
  if (!defined($code)) {
    carp(ref($mak),"::perform($act): skipping unknown action");
    return 0;
  }

  ##-- perform the action
  return $code->($mak,$argv);
}

$ACTIONS{which} =
  {
   syntax=>'which',
   help=>'display brief help on all known actions',
   code=>sub {
     my $mak = shift;
     my %syn2act = map { ($ACTIONS{$_}{syntax} ? $ACTIONS{$_}{syntax} : $_)=>$ACTIONS{$_} } keys(%ACTIONS);
     my ($syn,$act);
     foreach $syn (sort(keys(%syn2act))) {
       $act = $syn2act{$syn};
       print STDERR
	 sprintf("  %-18s : %s\n", $syn, ($act->{help} ? $act->{help} : 'no help'));
     }
   },
  };


########################################################################
## ACTIONS: Specific
########################################################################

##======================================================================
## Actions: initialize

## $mak->initCollection();
##  + initializes collection

$ACTIONS{init} = $ACTIONS{initialize} = $ACTIONS{new} =
  {
   syntax=>'init',
   help =>'initialize a new configuration collection',
   code =>\&actInitialize,
  };

sub actInitialize {
  my ($mak,$argv) = @_;

  $mak->{col} = MUDL::Make::Collection->new(varfile=>[$mak->{varfile}],
					    makefiles=>[map {abs_path($_)} glob("$mak->{dir}/[Mm]akefile")],
					   );
  $mak->{col}{vars}->parse($mak->{varfile})
    or confess(ref($mak)."::actInitialize(): could not parse variables file '$mak->{varfile}'");

  $mak->{loaded} = 1;
  $mak->{dirty} = 1;

  return 1;
}

##======================================================================
## Actions: ensure loaded

$ACTIONS{ensure} =
  {
   syntax=>'ensure',
   help=>'ensure collection is loaded',
   code=>sub { $_[0]->ensureLoaded; },
  };


##======================================================================
## Actions: save (explicit)


$ACTIONS{save} =
  {
   syntax=>'save FILE',
   help  =>'save collection to a (new) file FILE',
   code  => \&actSave,
  };
sub actSave {
  my ($mak,$argv) = @_;
  return $mak->ensureLoaded && $mak->{col}->saveFile(shift(@$argv));
}

##======================================================================
## Actions: select

## $subcol = $mak->selected()
##  + uses $mak->{selected} if defined, else $mak->{col}
sub selected {
  my $mak = shift;
  return $mak->{selected} ? $mak->{selected} : $mak->{col};
}

$ACTIONS{select} = $ACTIONS{xselect} = 
  {
   syntax=>'xselect XCRITERIA',
   help  =>'select a subset of the collection configurations',
   code  => \&actSelectExtended,
  };
sub actSelectExtended {
  my ($mak,$argv) = @_;
  ($mak->ensureLoaded()
   && ($mak->{selected} = $mak->{col}->xcollect(shift(@$argv))));
}

$ACTIONS{uselect} = 
  {
   syntax=>'uselect UCRITERIA',
   help  =>'select a subset of the collection configurations',
   code  => \&actSelectUser,
  };
sub actSelectUser {
  my ($mak,$argv) = @_;
  ($mak->ensureLoaded()
   && ($mak->{selected} = $mak->{col}->ucollect(shift(@$argv))));
}

##======================================================================
## Actions: expand

$ACTIONS{expandAll} =
  {
   syntax=>'expandAll',
   help  =>'(re-)expand selected configurations',
   code  => sub { $_[0]->ensureLoaded() && $_[0]->selected()->expandAll(); },
  };

$ACTIONS{expand} = $ACTIONS{expandMissing} =
  {
   syntax=>'expandMissing',
   help  =>'expand unexpanded selected configurations',
   code  => sub { $_[0]->ensureLoaded() && $_[0]->selected()->expandMissing(); },
  };

##======================================================================
## Actions: eval CODE

$ACTIONS{eval} =
  { syntax=>'eval CODE',
    help  =>'eval() some perl CODE',
    code  =>sub {
      my ($mak,$argv) = @_;
      my $code = shift(@$argv);
      return 0 if (!$mak->ensureLoaded);

      my $col = $mak->{col};
      my $mcol = $mak->{col};
      $mak->{dirty} = 1;

      eval $code;
      if ($@) {
	carp(ref($mak)."::eval($code): error: $@");
	return 0;
      }
      return 1;
    },
  };

##======================================================================
## Actions: do FILE

$ACTIONS{do} = $ACTIONS{require} = $ACTIONS{read} = 
  { syntax=>'do FILE',
    help  =>'do() a perl FILE',
    code  =>\&actDoFile,
  };

sub actDoFile {
  my ($mak,$argv) = @_;
  my $file = shift(@$argv);
  return 0 if (!$mak->ensureLoaded);

  my $fh = IO::File->new("<$file")
    or confess(ref($mak)."::do(): open failed for '$file': $!");
  my $code = join('',<$fh>);
  $fh->close();

  my $col = $mak->{col};
  my $rc = eval $code;
  if ($@) {
    carp(ref($mak)."::do($file): error: $@");
    return 0;
  }

  $mak->{dirty} = 1;
  return $rc;
}



##======================================================================
## Actions: make

$ACTIONS{make} = $ACTIONS{acquire} =
  {
   syntax=>'make',
   help=>'call make for selected configurations, acquiring data',
   code=>\&actMake,
  };
sub actMake {
  my $mak = shift;
  return 0 if (!$mak->ensureLoaded);
  my $rc = 1;
  foreach (values(%{$mak->selected()->{uconfigs}})) {
    $rc &&= $_->make( dir=>$mak->{dir}, makefiles=>$mak->{makefiles}, dummy=>$mak->{dummy} );
    if (!$mak->{dummy}) {
      $mak->{dirty} = 1;
      $mak->syncCollection() if ($mak->{paranoid});
    }
  }
  $mak->{dirty} = 1 if (!$mak->{dummy});
  return $rc;
}



########################################################################
## UTILITIES
########################################################################

## $cfg = $mcol->ufind(\%vars)
## $cfg = $mcol->ufind($ukey)
##  + no implicit config-creation
sub ufind {
  my $mak = shift;
  $mak->ensureLoaded || return undef;
  return $mak->{col}->ufind(@_);
}

## $cfg = $mcol->uget(\%uvars, %args)
## $cfg = $mcol->uget($ukey,   %args)
##  + implicitly creates config for \%uvars if none exists
##  + implicitly parses $ukey if it is passed as a string (may be dangerous!)
##  + %args:
##     expand=>$bool, ##-- if true, newly created configs will be auto-expanded
##     class=>$class, ##-- config class (default: MUDL::Make::Config)
sub uget {
  my ($mak,$uvars,%args) = @_;
  $mak->ensureLoaded || return undef;
  my $cfg = $mak->{col}->ufind($uvars);
  if (!defined($cfg)) {
    $cfg = $mak->{col}->uget($uvars,expand=>1,%args);
    $mak->{dirty} = 1;
  }
  return $cfg;
}

## $cfg = $mcol->xfind(\%xvars)
## $cfg = $mcol->xfind($xkey)
##  + no implicit config-creation
sub xfind {
  my $mak = shift;
  $mak->ensureLoaded || return undef;
  return $mak->{col}->xfind(@_);
}


########################################################################
## I/O
########################################################################

## $bool = $mak->ensureLoaded()
sub ensureLoaded {
  my $mak = shift;
  $mak->loadCollection() if (!$mak->{loaded});
  return $mak->{loaded};
}

## $col = $mak->loadCollection()
##  + loads the default collection
sub loadCollection {
  my $mak = shift;
  $mak->{col} = MUDL::Make::Collection->loadFile($mak->{colfile});
  if (!$mak->{col}) {
    confess(ref($mak)."::loadCollectio($mak->{colfile}): load failed!\n");
    $mak->{loaded} = 0;
    $mak->{dirty}  = 0;
    return undef;
  }
  $mak->{loaded} = 1;
  $mak->{dirty}  = 0;

  return $mak->{col};
}

## $col = $mak->syncCollection()
## $col = $mak->syncCollection($file)
##  + saves the default collection

$ACTIONS{sync} =
  {
   syntax=>'sync',
   help  =>'synchonize collection to the default file',
   code  => sub { return $_[0]->syncCollection(); },
  };


sub syncCollection {
  my ($mak,$file) = @_;
  $mak->ensureLoaded();

  if ($mak->{loaded} && $mak->{dirty}) {

    ##-- be paranoid
    if ($mak->{paranoid}) {
      my $ctr = 0;
      $file = $mak->{colfile} if (!$file);
      $ctr++ while (-e sprintf("$file.bak.%03d",$ctr));
      rename($file, sprintf("$file.bak.%03d",$ctr));
    }

    my $rc = $mak->{col}->saveFile($file);
    $mak->{dirty} = 0;

    return $rc;
  }
  return 1;
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
