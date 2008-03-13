#-*- Mode: CPerl -*-

## File: MUDL::PDL::Ranks.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL: PDL rank utilities
##======================================================================

package MUDL::PDL::Ranks;
use PDL;

package PDL;

##======================================================================
## Ranks

## $rankpdl = $pdl->ranks(%opts)
##  + returns $rankpdl: value-ranks (given 0th dimension of $pdl)
##  + no ranks are shared over 0th dimension
##  + %opts:
##     order=>$asc_or_desc, ##-- one of 'asc' (default) or 'desc'
##     ranks=>$rankpdl,     ##-- pre-allocated output pdl
##     ?
sub ranks {
  my ($pdl,%opts) = @_;

  my $pdl_qs_i   = $pdl->qsorti;
  $pdl_qs_i      = $pdl_qs_i->slice("-1:0") if (defined($opts{order}) && $opts{order} =~ /^d/);
  my $pdl_qs_ndi = $pdl_qs_i->flat->slice("*1,");
  foreach (1..($pdl->ndims-1)) {
    $pdl_qs_ndi  = $pdl_qs_ndi->glue(0,$pdl_qs_i->axisvals($_)->flat->slice("*1,"));
  }

  my $pdl_ranks = $opts{ranks};
  $pdl_ranks    = $pdl->zeroes() if (!defined($pdl_ranks) || $pdl_ranks->isnull);
  $pdl_ranks->indexND($pdl_qs_ndi) .= $pdl_ranks->xvals->flat;

  return $pdl_ranks;
}

##======================================================================
## Ranks: average

## $rankpdl = $pdl->avgranks(%opts)
##  + returns ranks as used e.g. by Spearman's rank correlation
##  + returns $rankpdl: value-ranks (given 0th dimension of $pdl)
##  + %opts:
##     order=>$asc_or_desc, ##-- one of 'asc' (default) or 'desc'
##     ranks=>$rankpdl,     ##-- pre-allocated output pdl
##     ?
sub avgranks {
  my ($pdl,%opts) = @_;

  my $pdl_qs_i   = $pdl->qsorti;
  $pdl_qs_i      = $pdl_qs_i->slice("-1:0") if (defined($opts{order}) && $opts{order} =~ /^d/);
  my $pdl_qs_ndi = $pdl_qs_i->flat->slice("*1,");
  foreach (1..($pdl->ndims-1)) {
    $pdl_qs_ndi  = $pdl_qs_ndi->glue(0,$pdl_qs_i->axisvals($_)->flat->slice("*1,"));
  }
  my $pdl_qs_v = $pdl->indexND($pdl_qs_ndi)->reshape($pdl->dims);
  my ($v_runlen,$v_runvals) = $pdl_qs_v->rle();

  my $v_runlen_dbl  = $v_runlen->double;
  my $v_runlen_cumu = $v_runlen_dbl->cumusumover;
  my $v_runlen_beg  = $v_runlen_cumu->slice("0:-2")->append(0)->rotate(1);

  my $v_rankvals    = $v_runlen_beg + ($v_runlen-1)/2.0;

  my $pdl_ranks = $opts{ranks};
  $pdl_ranks    = $pdl->zeroes() if (!defined($pdl_ranks) || $pdl_ranks->isnull);
  $pdl_ranks->indexND($pdl_qs_ndi) .= rld($v_runlen,$v_rankvals)->flat;

  return $pdl_ranks;
}


package MUDL::Pdl::Ranks;
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
