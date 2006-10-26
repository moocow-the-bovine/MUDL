#-*- Mode: Perl -*-

## File: MUDL::RSPerl.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: R client (using RSPerl)
##======================================================================

package MUDL::RSPerl;
use R;
use RReferences;

BEGIN {
  our @Rargs = qw(--silent);
  R::startR(@Rargs);
}

##======================================================================
## Vector instantation

## $Rstr = MUDL::RSPerl->_rvector($obj)
##  + returns a (hopefully) r-safe vector string, for $obj
##  + $obj can be:
##     - an array-ref of numeric values
##     - a 1d PDL
##     - a single scalar literal numeric value
##     - an R-safe string
##  + there are better ways to do this
##    - i.e. a full-scale perl-side object-oriented interface system using R variable names
##      possibly indexed by perl reference, etc...
##    - alternatively (better and trickier) a full-fledged R<->PDL interface
##      at the XS/C level
##  + ... but this simple, stupid function ought to suffice for present purposes
##    (significance testing on small objects)
sub _rvector {
  my ($that,$obj) = @_;
  if (!ref($obj)) {
    ##-- literal value or r-safe string
    return $obj;
  }
  elsif (ref($obj) && UNIVERSAL::isa($obj,'ARRAY')) {
    ##-- array-ref
    return 'c('.join(',',@$obj).')';
  }
  elsif (ref($obj) && UNIVERSAL::isa($obj,'PDL')) {
    ##-- PDL (flattened)
    return 'c('.join(',',$obj->flat->list).')';
  }
  carp(__PACKAGE__, "::_rvector(): cannot convert object '$obj' to an R vector!");
  return 'c()';
}

## $Rstr = MUDL::RSPerl->_rvectors(@objs)
##  + list-version of _rvector()
sub _rvectors {
  my $that = shift;
  return map { $that->_rvector($_) } @_;
}

##======================================================================
## Tests: general

## $p_value = $class->genericTest($testName,$testAttribute,$x,$y,@testArgs)
##   + $x and $y are R-vector-safe objects
sub genericTest {
  my ($that,$testName,$testAttr,$x,$y,@args) = @_;

  my $rstr = ($testName
	      .'('
	      .join(',',$that->_rvectors($x,$y),@rargs)
	      .')'
	      .($testAttr ? ('$'.$testAttr) : '')
	     );
  if (0) {
    ##-- DEBUG
    print STDERR __PACKAGE__, "::genericTest(): $rstr\n";
  }

  return R::eval($rstr);
}

## $p_value = $class->tTest($xvals,$yvals,@args)
sub tTest {
  return $_[0]->genericTest('t.test','p.value',@_[1..$#_]);
}

## $p_value = $class->wilcoxTest($xvals,$yvals,@rargs)
##   + $xvals and $yvals are R-safe objects
sub wilcoxTest {
  return $_[0]->genericTest('wilcox.test','p.value',@_[1..$#_]);
}


1; ##-- be happy

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
