#-*- Mode: Perl -*-

## File: MUDL::Corpus::Filter::ByEnum.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus filters: by enum (text)
##======================================================================

package MUDL::Corpus::Filter::ByEnum;
use MUDL::Corpus::Filter;
use MUDL::Enum;
use Carp;
our @ISA = qw(MUDL::Corpus::Filter);

########################################################################
## I/O : Filter : Abstract
########################################################################

##----------------------------------------------------------------------
## $cf = $class_or_obj->new(%args)
##  + %args:
##     reader=>$corpusReader,
##     writer=>$corpusWriter,
##     ...
##     enum=>$textEnum,
sub new {
  my $that = shift;
  return $that->SUPER::new(enum=>MUDL::Enum->new(),
			   @_);
}


##----------------------------------------------------------------------
## Filtering

## undef = $cr->doSentence(\@sentence)
##  + should be overridden by child classes
sub doSentence {
  my ($f,$s) = @_;
  @$s = grep { defined($f->{enum}->index($_->text)) } @$s;
  return $s;
}



##----------------------------------------------------------------------
## Help String

## string = $class_or_obj->helpString()
sub helpString {
  return
    (''
     ."Filter corpus content by text lookup in a MUDL::Enum.\n"
     ."Options:\n"
     ."  enum=ENUM           [default=empty]\n"
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

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
