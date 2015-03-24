#-*- Mode: Perl -*-

## File: MUDL::Corpus::Filter::Null.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpus filters: null filter
##======================================================================

package MUDL::Corpus::Filter::Null;
use MUDL::Corpus::Filter;

use strict;
use Carp;
our @ISA = qw(MUDL::Corpus::Filter);

########################################################################
## I/O : Filter : Abstract
########################################################################


##----------------------------------------------------------------------
## Filtering

## undef = $cr->doSentence(\@sentence)
##  + should be overridden by child classes
sub doSentence { return $_[1]; }


##----------------------------------------------------------------------
## Help String

## string = $class_or_obj->helpString()
sub helpString {
  return
    (''
     ."Null filter, useful for corpus format conversion.\n"
     ."Options: (none)\n"
    );
}

##----------------------------------------------------------------------
## AUTOLOAD: pass to reader, writer

##-- don't autoload DESTROY
sub DESTROY { ; }

our $AUTOLOAD;
sub AUTOLOAD {
  my $f = shift;
  return undef if (!defined($f));
  (my $name = $AUTOLOAD) =~ s/.*:://; ##-- strip qualification

  my ($sub);
  if ($sub=UNIVERSAL::can($f->{reader}, $name)) {
    return $sub->($f->{reader},@_);
  }
  elsif ($sub=UNIVERSAL::can($f->{writer}, $name)) {
    return $sub->($f->{writer},@_);
  }
  croak( ref($f) , "::$name() not defined in ", __PACKAGE__ , "::AUTOLOAD.\n");
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
