#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Filter::Map.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus filters: map
##======================================================================

package MUDL::Corpus::Filter::Map;
use MUDL::Corpus::Filter;
use MUDL::Map;
use Carp;
our @ISA = qw(MUDL::Corpus::Filter);

########################################################################
## I/O : Filter : Map
########################################################################

##----------------------------------------------------------------------
## $cf = $class_or_obj->new(%args)
##  + %args:
##     reader=>$corpusReader,
##     writer=>$corpusWriter,
##     ...
##     map     => $map,
##     from    => $fromAttribute, ##-- default='text'
##     to      => $toAttribute,   ##-- default='tag'
##     unknown => $unknownValue,  ##-- default=undef (=keep old value)
sub new {
  my $that = shift;
  return $that->SUPER::new('map'=>MUDL::Map->new(),
			   unknown=>undef,
			   from=>'text',
			   to=>'tag',
			   @_);
}


##----------------------------------------------------------------------
## Filtering

## undef = $f->doSentence(\@sentence)
sub doSentence {
  my ($f,$s) = @_;

  my ($from,$to);
  foreach $tok (@$s) {
    next if (!defined($from = $tok->attribute($f->{from})));
    $to = $f->{'map'}{$from};

    next if (!defined($to) && !defined($to=$f->{unknown}));
    $tok->attribute($f->{to}, $to);
  }

  return $s;
}



##----------------------------------------------------------------------
## Help String

## string = $class_or_obj->helpString()
sub helpString {
  return
    (''
     ."Deterministically map token text to tag.\n"
     ."Options:\n"
     ."  map=MAP           [default=empty]\n"
     ."  from=ATTR         [default=text]\n"
     ."  to=ATTR           [default=tag]\n"
     ."  unknown=VALUE     [default=undef (keep old value)]\n"
    );
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
