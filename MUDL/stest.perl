#!/usr/bin/perl -wd

use lib qw(..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use PDL::CCS;
use Storable qw(freeze thaw store retrieve);
use Benchmark qw(cmpthese timethese);

BEGIN { $, = ' '; }

##----------------------------------------------------------------------
## storable tests
package Storeme;
use PDL;
use PDL::CCS;
use MUDL;
use MUDL::CmdUtils;
use Storable qw(freeze thaw store retrieve);
BEGIN { our @ISA = qw(MUDL::Object); }

sub new {
  my $that=shift;
  my $pdl = random(double, 100,200);
  $pdl *= ($pdl>.9);
  return $that->SUPER::new(foo=>'bar',pdl=>$pdl);
}

sub encode {
  my $self = shift;
  my $p = $self->{pdl};
  my $nnz = $p->flat->nnz;
  @$self{qw(N M)} = $p->dims;
  ccsencodefull($self->{pdl},
		($self->{ptr}=zeroes(long,$self->{N})),
		($self->{rowids}=zeroes(long,$nnz)),
		($self->{nzvals}=zeroes(double,$nnz)));
  delete($self->{pdl});
  return $p;
}

sub decode {
  my $self = shift;
  my ($N,$M,$ptr,$rowids,$nzvals) = @$self{qw(N M ptr rowids nzvals)};
  $self->{pdl} = zeroes(double, $N,$M);
  ccsdecodefull($ptr, $rowids, $nzvals, $self->{pdl});
  delete(@$self{qw(N M ptr rowids nzvals)});
  return ($N,$M,$ptr,$rowids,$nzvals);
}

##-- hooks
sub STORABLE_freeze {
  my ($obj,$cloning) = @_;
  require PDL::IO::Storable;
  return if ($cloning);
  my $p = $obj->encode();
  $obj->{pdl} = $p;
  return '', [map { ($_,$obj->{$_}) } grep { $_ ne 'pdl' } keys(%$obj)];
}

sub STORABLE_thaw {
  my ($obj,$cloning,$serialized,$contents) = @_;
  require PDL::IO::Storable;
  return if ($cloning);
  %$obj = @$contents;
  $obj->decode();
  return $obj;
}

##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

#ltest1;
foreach $i (0..100) {
  print "--dummy[$i]--\n";
}
