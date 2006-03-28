#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Filter::SafeTags.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus filters:
##  + keep only sentences with "safe" tags
##======================================================================

package MUDL::Corpus::Filter::SafeTags;
use MUDL::Corpus::Filter;

use strict;
use Carp;
our @ISA = qw(MUDL::Corpus::Filter);

##----------------------------------------------------------------------
## $cf = $class_or_obj->new(%args)
##  + %args:
##     reader=>$corpusReader,
##     writer=>$corpusWriter,
##     ...
##     unsafe=>\%key2bool,     ##-- list of unsafe tags: $tag => $true_iff_tag_is_unsafe
sub new {
  my $that = shift;
  my $cf = $that->SUPER::new(unsafe=>{'NIL'=>undef,'nil'=>undef,''=>undef},
			     @_);
  if (exists($cf->{unsafe_str})) {
    my @unsafe = split(/\,/, $cf->{unsafe_str});
    %{$cf->{unsafe}} = map { $_=>undef } @unsafe;
  }
}


##----------------------------------------------------------------------
## Filtering

## undef = $cf->doSentence(\@sentence)
##  + should be overridden by child classes
sub doSentence {
  my ($cf,$sent) = @_;
  my ($tok,$tag);
  foreach $tok (@$sent) {
    $tag = ref($tok) ? $tok->tag : undef;
    if (!defined($tag) || exists($cf->{unsafe}{$tag})) {
      @$sent = qw();
      last;
    }
  }
  return $sent;
}


##----------------------------------------------------------------------
## Help String

## string = $class_or_obj->helpString()
sub helpString {
  return
    (''
     ."Filter out sentences containing \"unsafe\" tags in input corpus.\n"
     ."Options:\n",
     ."  unsafe=>\%set,       ##-- pseudo-set of unsafe tags (default: NIL,nil,'')\n"
     ."  unsafe_str=>$str,    ##-- comma-separated list of unsafe tags\n"
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
