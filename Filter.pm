#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Filter.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus filters
##======================================================================

package MUDL::Corpus::Filter;
use Module::Pluggable search_path=>['MUDL::Corpus::Filter'], require=>1;
use MUDL::Object qw(dummy);
use MUDL::CorpusIO;
use Carp;
our @ISA = qw(MUDL::Object);

########################################################################
## I/O : Filter : Abstract
########################################################################

##----------------------------------------------------------------------
## $cf = $class_or_obj->new(%args)
##  + %args:
##     reader=>$corpusReader,
##     writer=>$corpusWriter,
##     allow_empty_sentences=>$bool,  ##-- default=0
##     ...
sub new {
  my $that = shift;
  return $that->SUPER::new(reader=>'MUDL::CorpusReader',
			   writer=>'MUDL::CorpusWriter',
			   allow_empty_sentences=>0,
			   @_);
}


##----------------------------------------------------------------------
## fileReader(), fileWriter()

## $reader = $obj->fileReader($filename,%args)
sub fileReader {
  my $filter = shift;
  return $filter->{reader} = $filter->{reader}->fileReader(@_);
}

## $writer = $obj->fileWriter($filename,%args)
sub fileWriter {
  my $filter = shift;
  return $filter->{writer} = $filter->{writer}->fileWriter(@_);
}

##----------------------------------------------------------------------
## Abstractions

## $s_or_undef = $cf->doSentence(\@sentence)
##  + should be overridden by child classes
*doSentence = \&dummy('doSentence');

## \@sentence = $cf->processSentence(%args)
##   + calls getSentence(), $cf->doSentence(), putSentence()
sub processSentence {
  my $f = shift;
  my $s = $f->{reader}->getSentence();
  if (defined($s)) {
    $s = $f->doSentence($s);
    $f->{writer}->putSentence($s) if (defined($s));
  }
  return $s;
}

## $bool = $cf->process(%args)
sub process {
  my $f = shift;
  my ($s);
  while (defined($s=$f->{reader}->getSentence())) {
    next if (!@$s && !$f->{allow_empty_sentences});
    $s = $f->doSentence($s);

    next if (!defined($s) || (!@$s && !$f->{allow_empty_sentences}));
    $f->{writer}->putSentence($s);
  }
  return $f;
}

## $bool = $cf->flush(%args)
##  + finish processing, maybe flushing data
sub flush { $_[0]{writer}->flush(@_[1..$#_]); }

##----------------------------------------------------------------------
## Help String

## string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  if ((ref($that) && ref($that) eq __PACKAGE__) || $that eq __PACKAGE__) {
    return "Abstract corpus filter plugin host class.";
  }
  return (ref($that)||$that)."::helpString() not implemented.";
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
  if ($sub=$f->{reader}->can($name)) {
    return $sub->($f->{reader},@_);
  }
  elsif ($sub=$f->{writer}->can($name)) {
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
