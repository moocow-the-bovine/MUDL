#-*- Mode: CPerl -*-

## File: MUDL::Map.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: maps
##======================================================================

package MUDL::Map;
use MUDL::Object;
use MUDL::Set;
use IO::File;
use Carp;

our @ISA = qw(MUDL::Object);

##======================================================================
## new()
##  + object structure: hash ($from => $to)


##======================================================================
## Accessors

## $m= $m->clear()
sub clear {
  my $m = shift;
  %$m = qw();
  return $m;
}

## $dom = $m->domain()
##   + returns domain as a set
##   + in list context, returns list of all defined keys
sub domain { return wantarray ? keys(%{$_[0]}) : MUDL::Set->new(keys(%{$_[0]})); }

## $rng = $m->range()
sub range { return wantarray ? values(%{$_[0]}) : MUDL::Set->new(values(%{$_[0]})); }

## $size = $m->size()
sub size { return scalar(values(%{$_[0]})); }


##======================================================================
## I/O: Native
##======================================================================

sub DEFAULT_SORT {
  my $m = shift;
  return sub { $a cmp $b };
}

## $obj = $obj->saveNativeFh($fh,%args)
##  + %args : sort=>\&sortFunc
sub saveNativeFh {
  my ($m,$fh,%args) = @_;
  return undef if (!$m || !$fh);
  my $sortsub = $args{sort} ? $args{sort} : $m->DEFAULT_SORT;
  foreach my $k (sort { &$sortsub($a,$b) } keys(%$m)) {
    $fh->print($k, "\t", $m->{$k}, "\n");
  }
  return $m;
}

## $obj = $class_or_obj->loadNativeFh($fh,%args)
sub loadNativeFh {
  my ($m,$fh) = @_;
  return undef if (!$m || !$fh);
  $m = $m->new if (!ref($m));

  my ($line);
  while (defined($line=$fh->getline)) {
    chomp $line;
    next if ($line =~ /^\s*$/);
    next if ($line =~ /^\s*\#/); ##-- ignore shell-style comments
    if ($line !~ /^(.*\S)\s+(\S.*?)\s*$/) {
      warn( __PACKAGE__ , "::loadNativeFh(): parse error at input line ", $fh->input_line_number);
      next;
    }
    $m->{$1} = $2;
  }

  return $m;
}


##======================================================================
## I/O: XML
##======================================================================

##-- inherited

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
