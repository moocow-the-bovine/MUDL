#-*- Mode: Perl -*-

## File: MUDL::Lex.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: probabilistic lexicon
##======================================================================

package MUDL::Lex;
use MUDL::Object;
use MUDL::Dist::Partial;
use Carp;
our @ISA = qw(MUDL::Object);

##======================================================================
## Structure
##  + (see MUDL::Dist::Partial)
##  + object structure:
##     counts=>{$tok1=>{''=>$tokcount, $tag1_1=>$count1_1,...}, ..., ''=>$total_count}
sub new {
  my $that = shift;
  return $that->SUPER::new(counts=>{''=>0},@_);
}

##======================================================================
## Conversion: inversion

## $lex2 = $lex->lexTranspose()
## $lex2 = $lex->lexTranspose($lex,%args)
##   + converts lex->{counts} from {$word=>{$tag=>$count,...}} to {$tag=>{$word=>$count}}
sub lexTranspose {
  my ($lx,$lx2) = splice(@_,0,2);
  $lx2 = ref($lx)->new(@_) if (!ref($lx2));
  my ($w,$t,$f,$tcs);
  while (($w,$tcs) = each(%{$lx->{counts}})) {
    next if ($w eq '');
    while (($t,$f) = each(%$tcs)) {
      next if ($t eq '');
      $lx2->{counts}{$t}{$w} += $f;
      $lx2->{counts}{$t}{''} += $f;
      $lx2->{counts}{''}     += $f;
    }
  }
  return $lx2;
}

##======================================================================
## Conversion: to dist

## $naryDist = $lx->toDist()
## $naryDist = $lx->toDist($dist,%args)
##   + %args are passed to MUDL::Dist::Nary->new()
##   + returned dist event structure:
##     ($word,$tag) = $naryDist->split($event)
sub toDist {
  my ($lx,$d) = splice(@_,0,2);
  $d = MUDL::Dist::Nary->new(fields=>2,@_) if (!ref($d));

  my ($w,$tcs,$t,$ct);
  while (($w,$tcs)=each(%{$lx->{counts}})) {
    next if ($w eq '');
    while (($t,$ct)=each(%$tcs)) {
      next if ($t eq '');
      $d->{nz}{$w.$d->{sep}.$t} = $ct;
    }
  }
  return $d;
}


##======================================================================
## Conversion: to map

## $map = $lex->toMap()
## $map = $lex->toMap($map,%args)
##   + %args are passed to MUDL::Map
##   + additional %args:
##      transpose=>$bool,  # if true, map tags to words
sub toMap {
  my ($lx,$map,%args) = @_;
  my $transpose = $args{transpose};
  delete $args{transpose};
  $map = MUDL::Map->new(%args) if (!ref($map));
  my %max = qw();
  my ($w,$tcs,$t,$f);
  while (($w,$tcs)=each(%{$lx->{counts}})) {
    next if ($w eq '');
    while (($t,$f)=each(%$tcs)) {
      next if ($t eq '');
      if (!$transpose) {
	if (!defined($max{$w}) || $f > $max{$w}) {
	  $map->{$w} = $t;
	  $max{$w} = $f;
	}
      } else {
	if (!defined($max{$t}) || $f > $max{$t}) {
	  $map->{$t} = $w;
	  $max{$t} = $f;
	}
      }
    }
  }
  return $map;
}

##======================================================================
## I/O: Native

## $bool = $obj->saveNativeFh($fh,%args)
sub saveNativeFh {
  my ($lx,$fh) = @_;
  my ($tcs);
  foreach $w (grep { $_ ne '' } sort(keys(%{$lx->{counts}}))) {
    $tcs = $lx->{counts}{$w};
    $fh->print(join("\t",
		    $w,
		    $tcs->{''},
		    (map { ($_,$tcs->{$_}) } (grep { $_ ne '' } sort(keys(%$tcs))))),
	       "\n");
  }
}

## $obj = $class_or_obj->saveNativeFh($fh,%args)
sub loadNativeFh {
  my ($lx,$fh) = @_;
  $lx = $lx->new() if (!ref($lx));

  my ($line,$word,$count,@tagcts,$tag,$ct);
  while (defined($line=<$fh>)) {
    next if ($line =~ /^%%/);
    next if ($line =~ /^\s*$/);
    chomp($line);
    ($word,$count,@tagcts) = split(/\s+/, $line);
    while (($tag,$ct) = splice(@tagcts,0,2)) {
      if (!defined($ct)) {
	carp(ref($lx), "::loadNativeFh(): no count for word '$word', tag '$tag'");
	next;
      }
      $lx->{counts}{$word}{$tag} += $ct;
      $lx->{counts}{$word}{''}   += $ct;
      $lx->{counts}{''}          += $ct;
    }
  }
  return $lx;
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
