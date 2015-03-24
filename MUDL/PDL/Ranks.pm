#-*- Mode: CPerl -*-

## File: MUDL::PDL::Ranks.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL: PDL rank utilities
##======================================================================

package MUDL::PDL::Ranks;
use Exporter;
use PDL;

use strict;

our @ISA = qw(Exporter);
our %EXPORT_TAGS =
  (
   'ranks'  => [
		'ranks', 'ranks_asc','ranks_dsc',
		'ranks1', 'ranks1_asc', 'ranks1_dsc',
		'avgranks', 'avgranks_asc', 'avgranks_dsc',
		'avgranks1', 'avgranks1_asc', 'avgranks1_dsc',
	       ],
   'sort'  => [
	       'uniqi', 'qsortr', 'qsortri',
	      ],
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK   = @{$EXPORT_TAGS{all}};
our @EXPORT      = @EXPORT_OK;

##======================================================================
## Sorting

## $rsorted = $pdl->qsortr()
##  + reverse sort, see PDL::Ufunc::qsort()
BEGIN { *PDL::qsortr = \&qsortr; }
sub qsortr { return $_[0]->qsort->slice("-1:0"); }

## $rsortedi = $pdl->qsortri()
##  + reverse index sort, see PDL::Ufunc::qsorti()
BEGIN { *PDL::qsortri = \&qsortri; }
sub qsortri { return $_[0]->qsorti->slice("-1:0"); }

## $uniqi = $pdl->uniqi()
##  + returns (first) unique indices of $pdl
##  + see PDL::Primitive::uniqind()
BEGIN { *PDL::uniqi = *uniqi = \&PDL::uniqind; }

##======================================================================
## Ranks

## $rankpdl = $pdl->ranks(%opts)
##  + returns $rankpdl: value-ranks (given 0th dimension of $pdl)
##  + no ranks are shared over 0th dimension
##  + %opts:
##     order=>$asc_or_desc, ##-- one of 'asc' (default) or 'desc'
##     ranks=>$rankpdl,     ##-- pre-allocated output pdl
##     ?
BEGIN {
  *PDL::ranks = \&ranks;
  *PDL::ranks_asc = \&ranks_asc;
  *PDL::ranks_dsc = \&ranks_dsc;
}
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
  (my $tmp=$pdl_ranks->indexND($pdl_qs_ndi)) .= $pdl_ranks->xvals->flat;

  return $pdl_ranks;
}

## $rankpdl = $pdl->ranks_asc(%opts)    ##-- alias for $pdl->ranks(order=>'asc')
## $rankpdl = $pdl->ranks_dsc(%opts)    ##-- alias for $pdl->ranks(order=>'desc')
sub ranks_asc { ranks(@_,order=>'asc'); }
sub ranks_dsc { ranks(@_,order=>'dsc'); }

## $rank1pdl = $pdl->ranks1(%opts)       ##-- alias for $pdl->ranks(%opts)+1
## $rank1pdl = $pdl->ranks1_asc(%opts)   ##-- alias for $pdl->ranks_asc(%opts)+1
## $rank1pdl = $pdl->ranks1_dsc(%opts)   ##-- alias for $pdl->ranks_asc(%opts)+1
BEGIN {
  *PDL::ranks1 = \&ranks1;
  *PDL::ranks1_asc = \&ranks1_asc;
  *PDL::ranks1_dsc = \&ranks1_dsc;
}
sub ranks1 { ranks(@_)+1; }
sub ranks1_asc { ranks(@_,order=>'asc')+1; }
sub ranks1_dsc { ranks(@_,order=>'dsc')+1; }


##======================================================================
## Ranks: average

## $rankpdl = $pdl->avgranks(%opts)
##  + returns ranks as used e.g. by Spearman's rank correlation
##  + returns $rankpdl: value-ranks (given 0th dimension of $pdl)
##  + %opts:
##     order=>$asc_or_desc, ##-- one of 'asc' (default) or 'desc'
##     ranks=>$rankpdl,     ##-- pre-allocated output pdl
##     ?
BEGIN {
  *PDL::avgranks = \&avgranks;
  *PDL::avgranks_asc = \&avgranks_asc;
  *PDL::avgranks_dsc = \&avgranks_dsc;
}
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
  my $v_runlen_beg  = $v_runlen_cumu->dim(0) > 1 ? $v_runlen_cumu->slice("0:-2")->append(0)->rotate(1) : pdl(double,0);

  my $v_rankvals    = $v_runlen_beg + ($v_runlen-1)/2.0;

  my $pdl_ranks = $opts{ranks};
  $pdl_ranks    = $pdl->zeroes() if (!defined($pdl_ranks) || $pdl_ranks->isnull);
  (my $tmp=$pdl_ranks->indexND($pdl_qs_ndi)) .= rld($v_runlen,$v_rankvals)->flat;

  return $pdl_ranks;
}

## $rankpdl = $pdl->avgranks_asc(%opts)    ##-- alias for $pdl->avgranks(order=>'asc')
## $rankpdl = $pdl->avgranks_dsc(%opts)    ##-- alias for $pdl->avgranks(order=>'desc')
sub avgranks_asc { avgranks(@_,order=>'asc'); }
sub avgranks_dsc { avgranks(@_,order=>'dsc'); }

## $rank1pdl = $pdl->avgranks1(%opts)       ##-- alias for $pdl->ranks(%opts)+1
## $rank1pdl = $pdl->avgranks1_asc(%opts)   ##-- alias for $pdl->ranks_asc(%opts)+1
## $rank1pdl = $pdl->avgranks1_dsc(%opts)   ##-- alias for $pdl->ranks_asc(%opts)+1
BEGIN {
  *PDL::avgranks1 = \&avgranks1;
  *PDL::avgranks1_asc = \&avgranks1_asc;
  *PDL::avgranks1_dsc = \&avgranks1_dsc;
}
sub avgranks1 { avgranks(@_)+1; }
sub avgranks1_asc { avgranks(@_,order=>'asc')+1; }
sub avgranks1_dsc { avgranks(@_,order=>'dsc')+1; }

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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
