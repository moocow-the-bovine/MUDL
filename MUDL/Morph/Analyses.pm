##-*- Mode: CPerl -*-

## File: MUDL::Morph::Analyses.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: morphological editor: analysis map
##======================================================================

package MUDL::Morph::Analyses;
use MUDL::Object;
use MUDL::Enum;

use PDL;
use PDL::EditDistance;

use Encode qw(encode decode);

use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $ma = MUDL::Morph::Analyses->new(%args)
## + %args, structure:
sub new {
  my ($that,%args) = @_;
  my $ma = $that->SUPER::new(
			     dst_encoding=>'ISO-8859-1', ##-- storage encoding; source is always utf8
			     segsep=>'.',                ##-- segment separator
			     w2a=>{},
			     %args,
			     );
  return $ma;
}


##======================================================================
## Manipulators
##======================================================================

## $ma->clear()
##  + clear object content
sub clear {
  my $ma = shift;
  %{$ma->{w2a}} = qw();
}

##======================================================================
## Lookup: check whether a word has been analyzed
##======================================================================

## $bool  = $obj->has_analyses($wtext_dst_encoded)
sub has_analyses {
  return defined($_[0]{w2a}{$_[1]});
}

## $bool = $obj->hasanalyses_utf8($wtext_perl_utf8)
sub has_analyses_utf8 {
  return
    (defined($_[0]{dst_encoding})
     ? (
	$_[0]->has_analyses(encode($_[0]{dst_encoding},$_[1]))
       )
     : $_[0]->has_analyses($_[1]));
}

##======================================================================
## Lookup: get analyses
##======================================================================

## @analyses_dst_encoded  = $obj->analyses($wtext_dst_encoded)
sub analyses {
  my $astrs = $_[0]{w2a}{$_[1]};
  return defined($astrs) ? split(/\t/,$astrs) : qw();
}

## @analyses_decoded  = $obj->analyses_utf8($wtext_perl_utf8)
sub analyses_utf8 {
  return
    (defined($_[0]{dst_encoding})
     ? (
	map { decode($_[0]{dst_encoding},$_) }
	$_[0]->analyses(encode($_[0]{dst_encoding},$_[1]))
       )
     : $_[0]->analyses($_[1]));
}

##======================================================================
## Lookup: text -> segmented_text
##======================================================================

## @segmentations_utf8 = $me->segmentations_utf8($wtext_utf8)
sub segmentations_utf8 {
  return (defined($_[0]{dst_encoding})
	  ? (
	     map {
	       [map { decode($_[0]{dst_encoding},$_) } @$_]
	     } $_[0]->segmentations(encode($_[0]{dst_encoding},$_[1]))
	    )
	  : $_[0]->segmentations($_[1]));
}

## @segmentations_dst_encoded = $me->segmentations($wtext_dst_encoded)
##   + @segmentations = ( \@segment_1, ..., \@segment_n )
##   + \@segment_i    = [ $segmented_text, $edit_distance, $analysis_text, $analysis_weight ]
sub segmentations {
  my ($ma,$wtext) = @_;

  my $analyses_joined = $ma->{w2a}{$wtext};
  return qw() if (!defined($analyses_joined) || $analyses_joined eq '');

  my $wpdl = pdl(byte, [map { ord($_) } split(//,$wtext)]);

  my @segmentations = qw();
  my $segsep = $ma->{segsep};
  my ($analysis,$atext,$aweight, $ahacked);
  my ($apdl, $apdl_sep_i, @costs, $dmat,$amat, $wpath,$apath,$pathlen, $wsegmented);

  foreach $analysis (grep { defined($_) && $_ ne '' } split(/\t/, $analyses_joined)) {
    ##-- parse analysis
    if ($analysis =~ /^(.*)\s*(\<[\d\.eE\+\-]+\>)\s*$/) {
      ($atext,$aweight) = ($1,$2);
    } else {
      ($atext,$aweight) = ($analysis,0);
    }

    ##-- hack analysis
    $ahacked = $segsep . $atext . $segsep;
    $ahacked =~ s/[\(\)\\\#\~\*\|\.]+/$segsep/g;
    $ahacked =~ s/([a-zäöüß])([A-ZÄÖÜ])/$segsep$1/g;
    $ahacked =~ tr/A-ZÄÖÜ/a-zäöü/;
    $ahacked =~ s/\Q$segsep\E+/$segsep/g;

    ##-- align word text with hacked analysis
    $apdl       = pdl(byte, [map {ord($_)} split(//,$ahacked)]);
    $apdl_sep_i = which($apdl==ord($segsep))+1;
    @costs      = edit_costs_static(long, $wpdl->nelem, $apdl->nelem, 0,1,1, @costs);

    $costs[0]->dice_axis(1, $apdl_sep_i) .= 999; ##-- separators don't match anywhere
    #$costs[1]->dice_axis(1, $apdl_sep_i) .= 0;   ##-- separator insertion ain't cheap
    $costs[2]->dice_axis(1, $apdl_sep_i) .= 999; ##-- don't substitute separators anywhere

    ($dmat,$amat) = edit_align_full($wpdl,$apdl,@costs);
    ($wpath,$apath,$pathlen) = edit_bestpath($amat);

    $wsegmented = join('',
		       map {
			 ($wpath->at($_) >= 0
			  ? substr($wtext,$wpath->at($_),1)
			  : ($apath->at($_) >= 0 && $apdl->at($apath->at($_)) == ord($segsep)
			     ? $segsep
			     : qw()))
		       } (0..($pathlen-1)));
    $wsegmented =~ s/\Q$segsep\E+/$segsep/g;
    $wsegmented =~ s/^\Q$segsep\E//;
    $wsegmented =~ s/\Q$segsep\E$//;

    push(@segmentations, [
			  $wsegmented,
			  ($dmat->at($dmat->dim(0)-1,$dmat->dim(1)-1) - $apdl_sep_i->nelem),
			  $atext,
			  $aweight,
			 ]);

  }
  return @segmentations;
}


##======================================================================
## I/O: Native
##======================================================================

## $obj = $class_or_obj->loadNativeFh($fh,%args)
##  + data in $fh should be utf8-encoded!
sub loadNativeFh {
  my ($obj,$fh,%args) = @_;
  $obj = $obj->new(%args) if (!ref($obj));
  @$obj{keys %args} = values %args;

  my $w2a = $obj->{w2a};
  my ($line,$wtext,@analyses);
  while (defined($line=<$fh>)) {
    chomp($line);
    next if ($line=~/^\s*$/ || $line=~/^\s*%/);
    ($wtext,@analyses) = split(/\s*\t+\s*/, $line);
    if (defined($obj->{dst_encoding})) {
      ($wtext,@analyses) = map { encode($obj->{dst_encoding},$_) } ($wtext,@analyses);
    }
    $w2a->{$wtext} = join("\t", @analyses);
  }

  return $obj;
}

## undef = $class_or_obj->saveNativeFh($fh,%args)
##   + saves
sub saveNativeFh {
  my ($obj,$fh,%args) = @_;
  $args{$_} = $obj->{$_} foreach (grep { !exists($args{$_}) } keys(%$obj));

  my $w2a = $obj->{w2a};
  my ($wtext,$analyses_joined,@analyses);
  foreach $wtext (sort keys (%$w2a)) {
    @analyses = split("\t", $w2a->{$wtext});
    if (defined($obj->{dst_encoding})) {
      ($wtext,@analyses) = map { decode($obj->{dst_encoding},$_) } ($wtext,@analyses);
    }
    $fh->print(join("\t", $wtext, @analyses), "\n");
  }

  return $obj;
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
