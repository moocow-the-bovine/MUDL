#-*- Mode: Perl -*-

## File: MUDL::Ngrams.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: N-gram distribution
##  + really just a placeholder for TnT-style I/O
##======================================================================

package MUDL::Ngrams;
use MUDL::Object;
use MUDL::Dist::Partial;
use Carp;
our @ISA = qw(MUDL::Dist::Nary);

##======================================================================
## Constructor
## $ngs = MUDL::Ngrams->new(%args)
##  + (see MUDL::Dist::Nary)
##  + new %args:
##     verbose=>$bool,      ##-- save in verbose mode
sub new {
  my $that = shift;
  return $that->SUPER::new(verbose=>0,@_);
}

##======================================================================
## Utilities: nfields

## $nf = $ngs->nfields();
##  + get (maximum) number of fields
sub nfields {
  my $ngs = shift;
  return $ngs->{nfields} if (defined($ngs->{nfields}) && $ngs->{nfields} != 0);

  my $nf = 0;
  my $nfk;
  foreach $k (keys(%{$ngs->{nz}})) {
    $nfk = scalar($ngs->split($k));
    $nf = $nfk if ($nfk > $nf);
  }

  return $ngs->{nfields} = $nf;
}

##======================================================================
## Sanitization / Expansion

## $ngs = $ngs->sanitize()
##   + throw out all but $nmax-grams
sub sanitize {
  my ($ngs) = @_;
  my $nf = $ngs->nfields;

  my @tmp; ##-- alleviates warning: 'Use of implicit split to @_ is deprecated at ../MUDL/Ngrams.pm line *HERE*+3.'
  delete @{$ngs->{nz}}{
    grep {
      scalar(@tmp=CORE::split(/\Q$ngs->{sep}\E/, $_)) != $nf
    } keys(%{$ngs->{nz}})};

  return $ngs;
}

## $ngs_expanded = $ngs->expand()
##   + add in all non-zero ($k<$nmax)-grams, returns new ngrams
##   + assumes $ngs is sanitized
sub expand {
  my ($ngs) = @_;
  my $ngs2 = $ngs->copy;
  my $sep = $ngs->{sep};
  my ($k,$f,@ng);
  while (($k,$f)=each(%{$ngs->{nz}})) {
    @ng = $ngs->split($k);
    $ngs2->{nz}{join($sep,@ng[0..$_])} += $f foreach (0..($#ng-1));
  }
  $ngs2->{nfields} = 0;
  return $ngs2;
}


##======================================================================
## I/O: Native

## $bool = $obj->saveNativeFh($fh,%args)
sub saveNativeFh {
  my ($ngs,$fh,%args) = @_;

  ##-- plain ol plain ol
  my $ngx = $ngs->expand;
  return $ngx->SUPER::saveNativeFh($fh,%args) if ($ngs->{verbose} || $args{verbose});

  ##-- brief
  my $ngxnz = $ngx->{nz};
  my ($k,$f, @ng,@png);
  foreach $k (sort keys %$ngxnz) {
    $f = $ngxnz->{$k};
    @ng = $ngx->split($k);
    foreach $i (0..($#ng >= $#png ? $#ng : $#png)) {
      last if (!defined($png[$i]) || $ng[$i] ne $png[$i]);
      $ng[$i] = '';
    }
    $fh->print(join("\t", @ng, $f), "\n");
    $png[$_]=$ng[$_] foreach (grep {$ng[$_] ne ''} 0..$#ng);
  }

  return $ngs;
}

## $obj = $class_or_obj->loadNativeFh($fh,%args)
sub loadNativeFh {
  my ($ngs,$fh,%args) = @_;
  $ngs = $ngs->new(%args) if (!ref($ngs));

  my ($line,$f);
  my $sep = $ngs->{sep};
  my $nz = $ngs->{nz};
  my @ngtmp = qw();
  my @ng = qw();
  while (defined($line=$fh->getline)) {
    chomp $line;
    next if ($line =~ /^%%/ || $line =~ /^\s*$/);
    $line =~ s/\s+$//;

    @ngtmp  = CORE::split(/\t/, $line);
    $f      = pop(@ngtmp);

    $ng[$_] = $ngtmp[$_] foreach (grep { $ngtmp[$_] ne '' } (0..$#ngtmp));
    splice(@ng, $#ngtmp+1);

    $nz->{join($sep, @ng)} += $f;
  }

  return $ngs->sanitize();
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
