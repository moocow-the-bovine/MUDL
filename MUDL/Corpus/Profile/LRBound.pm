#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::LRBound.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R boundaries
##======================================================================

package MUDL::Corpus::Profile::LRBound;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use MUDL::Object;
use MUDL::Set;
use PDL;
use Carp;
our @ISA = qw(MUDL::Dist::Nary MUDL::Corpus::Profile);

##======================================================================
## new
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new(eos=>'__$',
			       bos=>'__$',
			       bounds=>MUDL::Set->new,
			       dobounds=>1,           # whether to profile bounds
			       dononbounds=>1,        # whether to profile non-bounds
			       %args);
  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($pr,$s) = @_;

  my @st = ($pr->{bos}, (map { $_->text } @$s), $pr->{eos});
  my @bi = (0, (grep { exists($pr->{bounds}{$st[$_]}) } (1..$#st-1)), $#st);

  #my ($bii,$sti);
  foreach $bii (1..$#bi) {
    ##-- add bound's own context
    if ($pr->{dobounds} && $bii != $#bi) {
      ++$pr->{nz}{join($pr->{sep}, @st[@bi[$bii-1, $bii, $bii+1]])};
    }

    ##-- add intra-bound context
    if ($pr->{dononbounds}) {
      foreach $sti ($bi[$bii-1]+1..$bi[$bii]-1) {
	++$pr->{nz}{join($pr->{sep}, @st[$bi[$bii-1], $sti, $bi[$bii]])};
      }
    }
  }

  return $pr;
}


##======================================================================
## Conversion: to Enum

## $enum = $lr->toEnumi()
## $enum = $lr->toEnumi($enum)
##  + independent enums
*toEnum = \&toEnumi;
sub toEnumi {
  my ($lr,$enum) = @_;

  my ($el,$ec,$er);
  $el = $er = MUDL::Enum->new();
  #$ec = $lr->{dobounds} ? $el : MUDL::Enum->new();
  $ec = MUDL::Enum->new();
  my $en = MUDL::Enum::Nary->new(enums=>[$el,$ec,$er], sep=>$lr->{sep});
  $lr->SUPER::toEnum($en);

  return $en;
}


##======================================================================
## Conversion: to independent PDL

## ($pdl,$enum) = $lr->toPDLi()
## ($pdl,$enum) = $lr->toPDLi($pdl)
## ($pdl,$enum) = $lr->toPDLi($pdl,$enum)
##   + in scalar context returns $pdl
sub toPDLi {
  my ($lr,$pdl,$en) = @_;

  ##-- enum
  $en = $lr->toEnumi() if (!$en);
  my ($el,$ec,$er) = @{$en->{enums}};
  my $ne = $el->size;

  ##-- pdl
  $pdl = PDL->new() if (!defined($pdl));
  $pdl->reshape($ec->size, $ne * 2);
  $pdl .= 0;

  ##-- data
  my ($k,$v,@fields);
  while (($k,$v)=each(%{$lr->{nz}})) {
    @fields = $lr->split($k);
    $pdl->slice($ec->index($fields[1]) . ',' .  $el->index($fields[0])) += $v;
    $pdl->slice($ec->index($fields[1]) . ',' . ($er->index($fields[2])+$ne)) += $v;
  }

  ##-- normalization
  foreach $k (0..$pdl->dim(0)-1) {
    $v  = $pdl->slice("$k,0:".($ne-1));
    $v /= $v->sum;

    $v  = $pdl->slice("$k,$ne:".($ne*2-1));
    $v /= $v->sum;
  }

  return wantarray ? ($pdl,$en) : $pdl;
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
