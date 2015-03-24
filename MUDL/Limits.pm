## File: MUDL::Limits.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: sets
##======================================================================

package MUDL::Limits;
use Carp;
use Exporter;
use strict;

our @ISA = qw(Exporter);

our %EXPORT_TAGS =
  (
   int  => [qw($INT_BYTES $INT_BITS $INT_MIN $INT_MAX)],
   uint => [qw($UINT_MIN $UINT_MAX)],
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK = @{$EXPORT_TAGS{all}};
our @EXPORT    = @EXPORT_OK;

##======================================================================
## Integer limits

our ($INT_BYTES,$INT_BITS, $INT_MIN,$INT_MAX, $UINT_MIN,$UINT_MAX);
BEGIN {
  $INT_BYTES = length(pack('i',0));
  $INT_BITS  = 8*$INT_BYTES;

  $INT_MAX   = 2**($INT_BITS-1)-1;
  $INT_MIN   = 2**($INT_BITS-1);

  $UINT_MIN   = 0;
  $UINT_MAX   = 2**($INT_BITS)-1;
}

1; ##-- be happy

