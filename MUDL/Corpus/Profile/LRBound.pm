#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::LRBound.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: L-R boundaries
##======================================================================

package MUDL::Corpus::Profile::LRBound;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use MUDL::Object qw(dummy);
use MUDL::Set;
use Carp;
our @ISA = qw(MUDL::Dist::Nary MUDL::Corpus::Profile);

##======================================================================
## new
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new(eos=>'__$',
			       bos=>'__$',
			       bounds=>MUDL::Set->new,
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

  my ($bii,$sti);
  foreach $bii (1..$#bi) {
    ##-- add bound's own context
    $pr->{nz}{join($pr->{sep}, @st[@bi[$bii-1, $bii, $bii+1]])}++ if ($bii != $#bi);

    ##-- add intra-bound context
    foreach $sti ($bi[$bii-1]+1..$bi[$bii]-1) {
      $pr->{nz}{join($pr->{sep}, @st[$bi[$bii-1], $sti, $bi[$bii]])}++;
    }
  }

  return $pr;
}
sub addSentence0 {
  my ($lrp,$s) = @_;
  my ($t);
  my $lb = $lrp->{bos};
  my @cws = qw();
  foreach $t ((map { $_->text } @$s), $lrp->{eos}) {
    if (exists($lrp->{bounds}{$t})) {
      $lrp->{nz}{join($lrp->{sep}, $lb, $_, $t)}++ foreach (@cws);
      @cws = qw();
    }
    push(@cws, $t);
  }
  return $lrp;
}

## undef = $profile->addReader($reader)
sub addReader0 {
  my ($lrp,$cr) = @_;
  my ($s,$t,$lb,@cws);
  while (defined($s=$cr->getSentence)) {
    $lb = $lrp->{bos};
    @cws = qw();
    foreach $t ((map { $_->text } @$s), $lrp->{eos}) {
      if (exists($lrp->{bounds}{$t})) {
	$lrp->{nz}{join($lrp->{sep}, $lb, $_, $t)}++ foreach (@cws);
	@cws = qw();
	$lb = $t;
      }
      push(@cws, $t);
    }
  }
  return $lrp;
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
