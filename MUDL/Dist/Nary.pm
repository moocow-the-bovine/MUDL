#-*- Mode: Perl -*-

## File: MUDL::Dist::Nary.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: distributions with complex events
##======================================================================

package MUDL::Dist::Nary;
use MUDL::Dist::Partial;
use Carp;

our @ISA = qw(MUDL::Dist::Partial);

## object structure:
##   + size    : number of possible events
##   + sizes   : array of sizes (per slot)
##   + zmass   : total mass alotted to zero-count events ("missing mass")
##   + nzero   : number of zero-probability entries
##   + sep     : field-separator string (default="\t")
##   + nfields : number of event fields (default=0 [dynamically compute])
sub new {
  my $that = shift;
  my $self = $that->SUPER::new(sep=>"\t", nfields=>0, @_);
  $self->{sizes} = [ map { 0 } (1..$self->{nfields}) ] if (!$self->{sizes});
  return $self;
}

##======================================================================
## Utilities
##======================================================================

## @components = $d->split($event)
##  + splits $event into components
sub split {
  return ($_[0]{nfields}
	  ? CORE::split(/(?:\Q$_[0]->{sep}\E)+/, $_[1], $_[0]{nfields})
	  : CORE::split(/(?:\Q$_[0]->{sep}\E)+/, $_[1]));
}

##======================================================================
## Access
##======================================================================

##----------------------------------------------
## Projection

## $subdist = $d->projectN(@indices)
##   + returns a MUDL::Dist::Nary over @indices
*project = \&projectN;
sub projectN {
  my ($d,@indices) = @_;
  my $sub = ref($d)->new(sep=>$d->{sep}, nfields=>scalar(@indices));
  my ($key,$f);
  while (($key,$f)=each(%{$d->{nz}})) {
    #$sub->{nz}{join($sub->{sep}, (split(/(?:\Q$d->{sep}\E)+/, $key))[@indices])} += $f;
    $sub->{nz}{join($sub->{sep}, ($d->split($key))[@indices])} += $f;
  }
  return $sub;
}

## $subdist = $d->project1($index)
##   + returns a MUDL::Dist for $index
sub project1 {
  my ($d,$i) = @_;
  my $sub = MUDL::Dist->new();
  my ($key,$f);
  while (($key,$f)=each(%{$d->{nz}})) {
    #$sub->{(split(/(?:\Q$d->{sep}\E)+/, $key))[$i]} += $f;
    $sub->{($d->split($key))[$i]} += $f;
  }
  return $sub;
}

##----------------------------------------------
## Selection

## $d2 = $d->select($coderef_or_str)
##  + selects all events for which &$coderef_or_str($key,$val) evaluates to true
##  + $codestr can use variables $key,$val and/or parameters @_[0,1]
##  + OBSOLETE: use prune()
sub select {
  my ($d,$code) = @_;
  $code = eval qq(sub { $code }) if (!ref($code));
  my $d2 = ref($d)->new();
  $d2->{sep} = $d->{sep};
  my ($key,$val);
  while (($key,$val)=each(%{$d->{nz}})) {
    $d2->{nz}{$key} = $val if (&$code($key,$val));
  }
  return $d2;
}


##----------------------------------------------
## clear
sub clear {
  @{$_[0]{sizes}} = qw();
  return $_[0]->SUPER::clear();
}

##----------------------------------------------
## nfields

## $nfields = $d->nFields()
sub nFields {
  return $_[0]{nfields} ? $_[0]{nfields} : ($_[0]{nfields} = $_[0]->getNFields);
}

## $nfields = $d->getNFields
##  + recompute
sub getNFields {
  my $d = shift;
  my $n = 0;
  my @fields;
  foreach (keys(%{$d->{nz}})) {
    #@fields = split(/(?:\Q$d->{sep}\E)+/, $_);
    @fields = $d->split($_);
    $n = @fields if (@fields > $n);
  }
  return $n;
}

##----------------------------------------------
## domains

## \@sets = getDomains()
*domains = \&getDomains;
sub getDomains {
  my $d = shift;
  my @sets = qw();
  my ($k,@fields);
  foreach $k (keys(%{$d->{nz}})) {
    #@fields = split(/(?:\Q$d->{sep}\E)+/, $k);
    @fields = $d->split($k);
    for ($i=0; $i < @fields; $i++) {
      $sets[$i] = MUDL::Set->new() if (!$sets[$i]);
      $sets[$i]{$fields[$i]} = $fields[$i];
    }
  }

  ##-- cache number of fields
  $d->{nfields} = @sets;

  return \@sets;
}

##----------------------------------------------
## size

## \@sizes = $d->sizes()
##   + returns size of each slot
sub sizes {
  return @{$_[0]{sizes}} ? $_[0]{sizes} : ($_[0]->{sizes} = $_[0]->getSizes);
}

## \@sizes = $d->getSizes()
##   + recomputes size per slot
sub getSizes {
  my $d = shift;
  my $sets = $d->getDomains();
  @{$d->{sizes}} = map { scalar(keys(%$_)) } @{$d->getDomains};
  return $d->{sizes};
}

## $size = $d->getSize();
sub getSize {
  my $d = shift;
  my $prod = 1;
  $prod *= $_ foreach (grep { $_ != 0 } @{$d->sizes});
  return $prod;
}


##======================================================================
## Metrics etc.
##======================================================================

## $I = $d->mutualInformation(\@xfields,\@yfields,%args)
##   + get mutual information
##   + %args:
##      px=>$dist_x,
##      py=>$dist_y,
##      xtotal=>$total_x,
##      ytotal=>$total_y,
*I = *mi = \&mutualInformation;
sub mutualInformation {
  my ($d,$xfields,$yfields,%args) = @_;

  my $Px = $args{px} ? $args{px} : $d->projectN(@$xfields);
  my $Py = $args{py} ? $args{py} : $d->projectN(@$yfields);
  my $xtotal = $args{xtotal} ? $args{xtotal} : $Px->total;
  my $ytotal = $args{ytotal} ? $args{ytotal} : $Py->total;

  my $mi = 0;
  my $total = $d->total;
  my ($event,$x,$y,$pxy,$px,$py,@fields);
  while (($event,$pxy)=each(%{$d->{nz}})) {
    next if ($pxy==0);

    @fields = $d->split($event);
    $x = join($Px->{sep}, @fields[@$xfields]);
    $y = join($Py->{sep}, @fields[@$yfields]);

    $px = $Px->{nz}{$x} / $xtotal;
    $py = $Py->{nz}{$y} / $ytotal;
    next if ($px==0 || $py==0);

    $mi += ($pxy/$total) * -log(($pxy/$total)/$px*$py)/log(2);
  }

  return $mi;
}


##======================================================================
## Pruning
##======================================================================

## $d = $d->pruneByEnum(enum=>$enum_nary, ...)
##  + re-implemented as workaround to AUTOLOAD passthrough
#sub pruneByEnum { my $d=shift; $d->prune(which=>$d->pruneByEnumSub(@_), @_); }

## \&prunesub = $d->pruneByEnumSub(enum=>$enum_nary,...)
##   + returns sub to prune events not defined by $enum_nary
sub pruneByEnumSub {
  my ($d,%args) = @_;
  my $enum = $args{enum};
  return sub { !(grep { !defined($_) } $enum->indices($d->split($_[0]))); }
}



##----------------------------------------------
## conditionalization

##-- TODO: implement as another class ?

## $cnd = $nd->conditionalizeN(\@given_fields)
##   + converts $nd->{nz} to { $event => P($event|@given_fields), ... }
sub conditionalize {
  my ($d,$gf) = @_;
  my $gd = $d->projectN(@$gf);
  my ($k,$gk);
  foreach $k (keys(%{$d->{nz}})) {
    #@fields = split(/(?:\Q$d->{sep}\E)+/, $k);
    @fields = $d->split($k);
    $gk = join($d->{sep}, @fields[@$gf]);
    $d->{nz}{$k} /= $gd->{nz}{$gk};
  }
  return $d;
}


##======================================================================
## Conversion: enumeration
##======================================================================

## $enum_nary = $d->toEnum()
## $enum_nary = $d->toEnum($enum_nary)
##   + enumerates events, does not alter dist
sub toEnum {
  my ($d,$e) = @_;
  $e = MUDL::Enum::Nary->new(sep=>$d->{sep}) if (!$e);

  #$e->addSymbols($d->split($_)) foreach (keys(%$d));
  ##--
  #$e->addSymbol($_) foreach (keys(%{$d->{nz}}));
  $d->SUPER::toEnum($e);

  return $e;
}

## $edist = $d->toEDist()
## $edist = $d->toEDist($enum)
## $edist = $d->toEDist($enum)
##   + returns a MUDL::EDist using enumerated events
sub toEDist {
  my ($d,$enum) = @_;
  $enum  = MUDL::Enum::Nary->new(sep=>$d->{sep}) if (!$enum);
  my $ed = MUDL::EDist::Nary->new(sep=>$d->{sep}, enum=>$enum) if (!$edist);

  @{$ed->{nz}}{map { $enum->addSymbol($_) } keys(%{$d->{nz}})} = values(%{$d->{nz}});


  #my $enum = $ed->{enum};
  #@{ $ed->{nz} }{
  #  map {
  #    join($ed->{sep}, $enum->addSymbols($d->split($_)))
  #  } keys(%{$d->{nz}})}
  #  = values(%{$d->{nz}});

  return $ed;
}

##======================================================================
## Conversion: best map
##======================================================================

## $map = $dn->toBestMap(\@keyFields,\@valFields)
sub toBestMap {
  my ($d,$kfi,$vfi) = @_;
  my $map = MUDL::Map->new();
  my %bestf = qw();
  my ($k,$v,@fields,$kf,$vf);
  while (($k,$v) = each(%{$d->{nz}})) {
    @fields = $d->split($k);
    $kf = join($d->{sep}, @fields[@$kfi]);
    if (!defined($bestf{$kf}) || $bestf{$kf} < $v) {
      $bestf{$kf} = $v;
      $map->{$kf} = join($d->{sep}, @fields[@$vfi]);
    }
  }
  return $map;
}

##======================================================================
## Conversion: Lex
##======================================================================

## $lex = $d->toLex()
## $lex = $d->toLex($lex)
sub toLex {
  require MUDL::Lex;
  my ($d,$lex) = splice(@_,0,2);
  $lex = MUDL::Lex->new(@_) if (!ref($lex));
  my ($event,$f,$w,$t);
  while (($event,$f)=each(%{$d->{nz}})) {
    ($w,$t) = $d->split($event);
    $lex->{counts}{$w}{$t} += $f;
    $lex->{counts}{$w}{''} += $f;
    $lex->{counts}{''}     += $f;
  }
  return $lex;
}

##======================================================================
## I/O: Lex
##======================================================================

## $obj = $class_or_obj->loadLexFh($fh,%args)
sub loadLexFh {
  require MUDL::Lex;
  return MUDL::Lex->loadNativeFh($_[1],@_[2..$#_])->toDist($_[0],@_[2..$#_]);
}

## $bool = $class_or_obj->saveLexFh($fh,%args)
sub saveLexFh {
  require MUDL::Lex;
  return $_[0]->toLex(@_[2..$#_])->saveNativeFh($_[1],@_[2..$#_]);
}

__PACKAGE__->registerIOMode('lex',{loadFh=>'loadLexFh',saveFh=>'saveLexFh'});
__PACKAGE__->registerFileSuffix(qr/\.lex$/, 'lex');

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
