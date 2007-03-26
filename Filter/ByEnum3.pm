#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Filter::ByEnum3.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus filters: by enum (trigram text)
##======================================================================

package MUDL::Corpus::Filter::ByEnum3;
use MUDL::Corpus::Filter;
use MUDL::Enum::Nary::Flat;
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
##     bos => $bos,
##     eos => $eos,
##     enum=>$trigramEnum,
sub new {
  my $that = shift;
  return $that->SUPER::new(enum=>MUDL::Enum::Nary::Flat->new(nfields=>3),
			   bos=>'__$',
			   eos=>'__$',
			   @_);
}


##----------------------------------------------------------------------
## Filtering

## undef = $cr->doSentence(\@sentence)
##  + should be overridden by child classes
sub doSentence {
  my ($f,$s) = @_;
  my @txt = map { $f->{bos} } (0..2);
  my @s = @$s;
  @$s = qw();
  my ($tok);
  foreach $tok (@s) {
    shift(@txt);
    push(@txt,$tok->text);
    push(@$s, $tok) if (defined($f->{enum}->lindex(@txt)));
  }
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
     ."  enum=FLAT_ENUM        [default=empty]\n"
     ."  bos=BOS               [default=__\$]\n"
     ."  eos=EOS               [default=__\$]\n"
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
