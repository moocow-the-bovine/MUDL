##-*- Mode: CPerl -*-

## File: MUDL::CmdUtils.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: command-line utilities
##======================================================================

package MUDL::CmdUtils;
use MUDL::Object;
use Exporter;
use Carp;
our @ISA = qw(Exporter);

our %EXPORT_TAGS =
  (
   options=>[qw(parseClassOptions)],
   io=>[qw(load save)],
   plugins=>[qw(loadModule)]
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK = @{$EXPORT_TAGS{all}};
our @EXPORT = @EXPORT_OK;

##======================================================================
## Plugin Utilities

## $fullname = loadModule($name,%args)
##   + Tries to load module $name
##   + %args
##       search=>\@module_prefixes
sub loadModule {
  my ($name,%args) = @_;
  $args{search} =  ['',qw(MUDL::)] if (!$args{search} || !@{$args{search}});

  $name = ref($name) if (ref($name));
  my ($fprefix,$cprefix,$fqfile,$fqname);
  (my $fname = $name) =~ s/::/\//g;

  my @fqfiles = (
		 map  {
		   $cprefix = $_;
		   ($fprefix = $cprefix) =~ s/::/\//g;
		   $fqfile = "${fprefix}${fname}.pm";
		   map {
		     -f "$_/$fqfile" ? [$cprefix.$name,$fqfile] : qw()
		   } @::INC
		 } @{$args{search}}
		);
  if (!@fqfiles) {
    $@ = 'File not found';
    return undef;
  }
  foreach (@fqfiles) {
    if (eval { require "$_->[1]"; }) {
      $fqname = $_->[0];
      last;
    }
  }

  return $fqname;
}


##======================================================================
## Option utilities

## \%opts = parseClassOptions(\%opts)
##   + parses command-line class-options \%opts
##   + leaves all options intact, except for:
##       key => "file:$filename"  # load $filename via load()
sub parseClassOptions {
  my $opts = shift;
  foreach (values(%$opts)) {
    if (s/^file://) {
      $_ = load($_)
	or die(__PACKAGE__, "::parseClassOptions(): load failed for file '$_': $!");
    }
  }
  return $opts;
}

##======================================================================
## I/O utilities

## $obj = load($filespec,%args)
##  + $filespec is one of the following:
##                  $filename : passed to MUDL::Object->loadFile()
##     class:$CLASS:$filename : passed to $CLASS->loadFile
##  + %args
##     may include search=>\@path
sub load {
  my $file = shift;
  if ($file =~ s/^class://) {
    my ($class,$filename) = ($file =~ /^((?:\w|\:\:)+):(.*)$/);
    my $fqclass = $class;
    if (!UNIVERSAL::can($class,'loadFile')) {
      $fqclass = loadModule($class,@_)
	or confess(__PACKAGE__, "::load(): could not find class '$class'!");
    }
    confess(__PACKAGE__, "::load(): no method loadFile() for class '$fqclass'!")
      if (!UNIVERSAL::can($fqclass,'loadFile'));
    return $fqclass->loadFile($filename,@_);
  }
  return MUDL::Object->loadFile($file,@_);
}

## $bool = save($obj,$filespec,%args)
##  + $filespec is one of the following:
##     $filename : passed to MUDL::Object->saveFile()
sub save {
  my ($obj,$file) = (shift,shift);
  if (UNIVERSAL::can($obj,'saveFile')) {
    return $obj->saveFile($file,@_);
  }
  return MUDL::Object::saveFile($obj,$file,@_);
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
