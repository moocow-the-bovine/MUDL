#-*- Mode: Perl -*-

## File: MUDL::LogUtils.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: logarithm utilities
##======================================================================

package MUDL::LogUtils;
our $VERSION = 0.01;

our @ISA = qw(Exporter);
our %EXPORT_TAGS = (
		    all=>[qw($LOG_ZERO $LOG_ONE $LOG_BIG plogadd plogsum plogdiff)],
		   );
our @EXPORT_OK = @{$EXPORT_TAGS{all}};
our @EXPORT = @EXPORT_OK;

##-- constants
our $LOG_ZERO = -1e38;
our $LOG_ONE  = 0;
our $LOG_BIG  = 71.38; ##-- log(1e31)

## $log_x_plus_y = plogadd($log_x, $log_y)
sub plogadd {
  my ($x,$y) = @_;
  return $y if ($y-$x > $LOG_BIG);
  return $x if ($x-$y > $LOG_BIG);
  return
    ($x<$y
     ? ($x + log(1 + exp($y-$x)))
     : ($y + log(1 + exp($x-$y))));
}

## $log_sum_args = plogsum(@log_args)
sub plogsum {
  my $x = shift;
  foreach $y (@_) {
    if    ($y-$x > $LOG_BIG) { $x=$y; }
    elsif ($x-$y > $LOG_BIG) { next;  }
    elsif ($x<$y) {
      $x += log(1+exp($y-$x));
    } else {
      $x = $y + log(1+ exp($x-$y));
    }
  }
  return $x;
}


## $log_x_diff_y = plogdiff($log_x, $log_y)
sub plogdiff {
  my ($x,$y) = @_;
  return $y if ($y-$x > $LOG_BIG);
  return $x if ($x-$y > $LOG_BIG);
  return
    ($x>$y
     ? ($x + log(1-exp($y-$x)))
     : ($y + log(1-exp($x-$y))));
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
