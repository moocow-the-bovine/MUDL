##-*- Mode: CPerl -*-

## File: MUDL::Make::Vars.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: makefile configuration: variable parser
##======================================================================

package MUDL::Make::Vars;
use MUDL::Object;
use MUDL::Make qw(:all);
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
##  + HASH:
##     $varName => "${assignmentOp} ${varValue}", ...
sub new {
  my $that = shift;
  my $self = bless $that->SUPER::new(
				     @_
				    ), ref($that)||$that;
  return $self;
}

## $vars = $vars->clear;
sub clear {
  my $vars = shift;
  %$vars = qw();
  return $vars;
}

##======================================================================
## Properties: target

## $target = $vars->ctarget($target)
sub ctarget {
  my ($vars,$target) = @_;
  return $target if (defined($target) && $target ne '');
  $target = '__'.ref($vars).'__';
  $target =~ s/\:/_/g;
  return $target;
}


##======================================================================
## Assignment: user vars

## $vars = $vars->assign(\%vars2values)
##  + clobbers existing variables with names in %vars2values
##  + implicitly adds operation ':=' to assigned values
sub assign {
  my ($vars,$uvars) = @_;
  $vars->{$_} = ":= $uvars->{$_}" foreach (keys(%$uvars));
  return $vars;
}

##======================================================================
## Expansion: whole shebang

## \%xvars = $vars->expand(%args)
##  + %args:
##     file=>$makefilename,
##     target=>$target,
##     xvars=>\%xvars, ##-- destination object
##     unlink=>$bool,  ##-- unlink the generated makefile?
sub expand {
  my ($vars,%args) = @_;
  return $vars->expandMakefile($vars->writeMakefile(%args),%args);
}


##======================================================================
## Expansion: parse generated makefile

## \%xvars = $vars->expandMakefile($generated_makefile,%args)
##  + %args:
##     target=>$target,
##     xvars=>\%xvars, ##-- destination object
##     unlink=>$bool,  ##-- unlink the generated makefile?
sub expandMakefile {
  my ($vars,$makefile,%args) = @_;
  my $target = $vars->ctarget($args{target});

  my $makfh = IO::File->new("$MAKE -f $makefile $target |")
    or confess(ref($vars),"::expandMakefile($makefile): could not (make $target): $!");

  ##-- load generated pseudo-code
  my $xvars = $args{xvars} ? $args{xvars} : {};
  my ($vline,$var,$val);
  while (defined($vline=<$makfh>)) {
    chomp($vline);
    ($var,$val) = split(/\=/, $vline, 2);
    $xvars->{$var} = defined($val) ? $val : '';
  }

  ##-- cleanup
  $makfh->close;
  unlink($makefile) if ($args{unlink});

  return $xvars;
}



##======================================================================
## Expansion: generate makefile

## $filename = $vars->writeMakefile(%args)
##  + %args:
##     file   =>$filename_or_fh, ##-- write to $filename_or_fh
##     target =>$config_target,  ##-- pseudo-target (default: '__'.(ref($vars)=~s/\:/_/g).'__'
##                               ##   : use empty string for no pseudo-target
sub writeMakefile {
  my ($vars,%args) = @_;

  ##-- get target
  my $target = defined($args{target}) ? $args{target} : $vars->ctarget($args{target});

  ##-- get filename & fh
  my $file = $args{file};
  my ($fh);
  if (ref($file)) {
    $fh = $file;
  }
  elsif (!defined($file) || $file eq '') {
    ($fh,$file) = tempfile('tmpXXXXX',SUFFIX=>'.mak',UNLINK=>1);
  }

  ##-- write makefile
  $fh->print(
	     ##-- Variables
	     (map { ($_, ' ', $vars->{$_}, "\n") } keys(%$vars)),

	     "\n",

	     ##-- Pseudo-target
	     ($target ne ''
	      ? ("${target}::\n",
		 (map { "\t\@echo \"$_=\$($_)\"\n" } keys(%$vars)),
		 "\n",
		)
	      : qw()),
	    );

  ##-- cleanup
  if (ref($file)) {
    $fh->close();
    return '?';
  }

  return $file;
}


##======================================================================
## Parse: Variables: from (make -p)

## $vars = $vars->parse_p($makefile_or_fh)
## $vars = $vars->parse_p($makefile_or_fh,$dummy_target)
##  + parses output of (make -f$makefile -p)
sub parse_p {
  my ($vars,$makefile,$notarget) = @_;

  if (ref($makefile)) {
    ##-- hack for passed filehandles: copy 'em
    my ($tmpfh,$tmpnam) = tempfile('tempXXXXX',SUFFIX=>'.mak',UNLINK=>1);
    copy($makefile,$tmpfh);
    $tmpfh->close;
    $vars->parse_p($tmpnam, $notarget, makep=>1);
    unlink($tmpnam) if (!$DEBUG);
  }
  else {
    ##-- real file: use (make -p)
    $notarget = '__notarget__' if (!defined($notarget));
    my $makpfh = IO::File->new("$MAKE -f $makefile -p -f/dev/null $notarget 2>/dev/null |")
      or confess(ref($vars),"::parse_p($makefile): could not call ($MAKE -p): $!");
    $vars->parse($makpfh, makep=>1);
    $makpfh->close();
  }
  return $vars;
}


##======================================================================
## Parse: Variables: Guts

## $vars = $vars->parse($makefile_or_fh,%args)
##  + %args:
##      makep=>$bool,    ##-- true iff reading from (make -p)
sub parse {
  my ($vars,$file,%args) = @_;
  my $fh = ref($file) ? $file : IO::File->new("<$file");
  confess(ref($vars)."::parseVariables($file): open failed for '$file': $!") if (!$fh);

  my $buf = '';
  my ($line);
  my ($var,$op,$val);
  while (defined($line=<$fh>)) {
    chomp($line);

    if ($args{makep}) {
      ##-- skip automatic & default variables if reading from 'make -p'
      if ($line =~ /^\# (?:automatic|default)$/) {
	$line = <$fh>; ##-- skip following line
	next;
      }
    }

    ##-- ignore make-internal stuff
    next if ($line =~ /^MAKEFLAGS\s*(?:[\:\?]?)=/);
    next if ($line =~ /^MFLAGS\s*(?:[\:\?]?)=/);
    next if ($line =~ /^MAKEFILE_LIST\s*(?:[\:\?]?)=/);
    next if ($line =~ /^MAKELEVEL\s*(?:[\:\?]?)=/);

    $line =~ s/(?!<\\)\#.*//;      ##-- eliminate comments
    $line =~ s/^\s+/ /;            ##-- trim leading whitespace
    $buf .= $line;                 ##-- append to buffer
    next if ($buf =~ s/\s*\\$/ /); ##-- honor newline escapes

    ##-- parse buffer
    if ($buf =~ /^\s*([^\=]*?\S)\s*([\?\:]?=)\s*(.*)/) {
      ($var,$op,$val) = ($1,$2,$3);
      $vars->{$var}   = $op.' '.$val;
    }
    $buf = '';                   ##-- clear buffer
  }

  $fh->close() if (!ref($file));
  return $vars;
}


##======================================================================
## I/O: Native (Makefile)
##======================================================================

## $obj = $class_or_obj->loadNativeFh($filename_or_fh)
sub loadNativeFh {
  my ($obj,$file) = @_;
  $obj = $obj->new() if (!ref($obj));
  return $obj->parse($file);
}

## $bool = $class_or_obj->saveNativeFh($filename_or_fh)
sub saveNativeFh {
  my ($obj,$file) = @_;
  $obj->writeMakefile(file=>$file,target=>'');
  return $obj;
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
