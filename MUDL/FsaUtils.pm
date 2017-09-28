#-*- Mode: Perl -*-

## File: MUDL::FsaUtils.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL : Fsa utilities
##======================================================================

package MUDL::FsaUtils;
use RWTH::Fsa::Ext;

our $VERSION = 0.02;
our @ISA = qw(Exporter);
our @EXPORT = qw();
our %EXPORT_TAGS =
  (
   weights => [qw(prob2logf logf2prob round)],
   arcs => [qw(getStateArcs setStateArcs)],
   cmp => [qw(lexcmp)],
   strutils => [qw(packed2str sm2str)],
  );
$EXPORT_TAGS{all} = [map { @$_ } values(%EXPORT_TAGS)];
our @EXPORT_OK = @{$EXPORT_TAGS{all}};

##======================================================================
## Sorting utilities
##======================================================================

## $cmp_val = lexcmp($s1,$s2)
##   + compares two strings according to their 'lexicographical order',
##     defined as in Carrasco&Oncina(99).  $s1,$s2 should be strings
##     of packed label-ids (i.e. $s1=$s2=pack('l*',@labelids)).
sub lexcmp {
  return (length($_[0]) < length($_[1])
	  ? -1
	  : (length($_[0]) > length($_[1])
	     ? 1
	     : ($_[0] cmp $_[1])));
}

##======================================================================
## String utilities
##======================================================================

## $str2stateId = packed2str($packedstr)
sub packed2str { return join(',', unpack('l*',$_[0])); }


##======================================================================
## Weight Converters
##======================================================================
sub prob2logf { return -log( $_[0]); }
sub logf2prob { return  exp(-$_[0]); }
sub round { return $_[0]-int($_[0]) < 0.5 ? int($_[0]) : int($_[0])+1; }

##======================================================================
## RWTH::Fsa::Weight
package RWTH::Fsa::Weight;

##--------------------------------------------------------------
## In-place converters
##--------------------------------------------------------------
sub _prob2log {
  $_[0]->setFloat(MUDL::FsaUtils::prob2logf($_[0]->asFloat));
  return $_[0];
}
sub _log2prob {
  $_[0]->setFloat(MUDL::FsaUtils::logf2prob($_[0]->asFloat));
  return $_[0];
}
sub _round {
  $_[0]->setFloat(MUDL::FsaUtils::round($_[0]->asFloat));
  return $_[0];
}

##--------------------------------------------------------------
## Copying converters
##--------------------------------------------------------------
sub prob2log { return _prob2log(RWTH::Fsa::Weight->new($_[0]->asFloat)); }
sub log2prob { return _log2prob(RWTH::Fsa::Weight->new($_[0]->asFloat)); }
sub round { return _round(RWTH::Fsa::Weight->new($_[0]->asFloat)); }

package MUDL::FsaUtils;
## /RWTH::Fsa::Weight
##======================================================================

##======================================================================
## State Utils: arcs
##======================================================================

## \@arcs = getStateArcs($state)
##   + each member of \@ary is an arc-like hash-ref
sub getStateArcs {
  my $q = shift;
  my @arcs = qw();
  my ($ai,$arc);
  for ($ai=0; $ai < $q->nArcs; $ai++) {
    $arc = $q->getArc($ai);
    push(@arcs, {
		 target=>$arc->swig_target_get,
		 input=>$arc->swig_input_get,
		 output=>$arc->swig_output_get,
		 weight=>$arc->swig_weight_get,
		});
  }
  return \@arcs;
}

## \@arcs = setStateArcs($state,\@arcs)
##   + each member of \@ary is an arc-like hash-ref
sub setStateArcs {
  my ($q,$arcs) = @_;
  $q->truncate(0);
  $_->{weight} = $_->{weight}->asFloat foreach (@$arcs);
  foreach my $arc (@$arcs) {
    $q->newArc(int $arc->{target},
	       RWTH::Fsa::Weight->new($arc->{weight}),
	       int $arc->{input},
	       int $arc->{output});
  }
  return $arcs;
}

##======================================================================
## /RWTH::Fsa::*State*
package RWTH::Fsa::State;
*getArcs = \&MUDL::FsaUtils::getStateArcs;
*setArcs = \&MUDL::FsaUtils::setStateArcs;

package RWTH::Fsa::StateRef;
*getArcs = \&MUDL::FsaUtils::getStateArcs;
*setArcs = \&MUDL::FsaUtils::setStateArcs;

package RWTH::Fsa::ConstStateRef;
*getArcs = \&MUDL::FsaUtils::getStateArcs;
#*setArcs = \&MUDL::FsaUtils::setStateArcs;

package MUDL::FsaUtils;
## /RWTH::Fsa::*State*
##======================================================================


##======================================================================
## RWTH::Fsa::StateMap
package RWTH::Fsa::StateMap;

## \@ary = $sm->asArray()
sub asArray {
  my $sm = shift;
  return [map { $sm->get($_) } 0..($sm->size()-1)];
}

## @list = $sm->asList()
sub asList { return @{$_[0]->asArray}; }

## \$packed = $sm->asPacked()
##   + returns reference to packed string representing $sm (packed as 'L')
sub asPacked {
  my $sm = shift;
  my $str = '';
  $str .= pack('L',$sm->get($_)) foreach (0..($sm->size-1));
  return \$str;
}
## $string = $sm->asString()
##  + human-readable string
sub asString {
  my $sm = shift;
  my ($to);
  return
    ('['.join(', ',
	      map {
		$to = $sm->get($_);
		$_.':'.($to==$RWTH::Fsa::InvalidStateId ? '-' : $to)
	      } (0..($sm->size-1)))
     .']');
}

package MUDL::FsaUtils;
## /RWTH::Fsa::StateMap
##======================================================================



##======================================================================
## Automaton Utilities
##======================================================================
package RWTH::Fsa;
use IO::File;

##----------------------------------------------------------------------
## Fsa utils: arc arrays
##----------------------------------------------------------------------

# \@arcs = $fsa->getArcs($stateid)
sub getArcs { return MUDL::FsaUtils::getStateArcs($_[0]->getState($_[1])); }

# \@arcs = $fsa->setArcs($stateid,\@arcs);
sub setArcs { return MUDL::FsaUtils::setStateArcs($_[0]->state($_[1]), $_[2]); }

##----------------------------------------------------------------------
## Fsa utils: drawing
##----------------------------------------------------------------------
# undef = $fsa->drawps($psfilename);
sub drawps {
  my ($fsa,$file) = @_;
  my $fh = IO::File->new("|$DOT -Tps -o $file")
    or die ( __PACKAGE__ , "::drawps(): open failed for pipe to $DOT: $!");

  $fh->print($fsa->draw);
  $fh->close();
}

# undef = drawdot($dotfile);
sub drawdot {
  my ($fsa,$file) = @_;
  my $fh = ref($file) ? $file : IO::File->new(">$file");
  die ( __PACKAGE__ , "::drawps(): open failed for pipe to $DOT: $!") if (!$fh);

  $fh->print($fsa->draw);
  $fh->close() if (!ref($file));
}


##----------------------------------------------------------------------
## Fsa utils: sorting
##----------------------------------------------------------------------

# $staticFsa = fsa_bfsort($constFsa,$doLexSort=1);
#  + returns breadth-first sorted copy of $fsa
sub bfsort {
  my $fsa = shift;
  my $do_lexsort = @_ ? shift : 1;
  $fsa = $fsa->sort($RWTH::Fsa::SortTypeByInput) if ($do_lexsort);

  my $fsa2 = RWTH::Fsa::StaticAutomatonRef->new();
  $fsa2->setSemiring($fsa->semiring);
  $fsa2->setInputAlphabet($fsa->getInputAlphabet);
  $fsa2->setOutputAlphabet($fsa->getOutputAlphabet);
  $fsa2->setType($fsa->type);

  my $noState = $RWTH::Fsa::InvalidStateId;
  my $maxStateId = $fsa->automatonCount->{maxStateId_};
  my $sm = RWTH::Fsa::StateMap->new($maxStateId+1, $noState);
  my @queue = ($fsa->initialStateId);
  my ($qi,$qi2,$q1,$q2,$ai);
  my $i = 0;

  ##-- populate state-map
  while (defined($qi=shift(@queue))) {
    next if ($sm->get($qi) != $noState);
    $sm->set($qi, $i++);
    $q1 = $fsa->getState($qi);
    for ($ai=0; $ai < $q1->nArcs; $ai++) {
      push(@queue, $q1->getArc($ai)->{target});
    }
  }

  ##-- apply state map
  for ($qi=0; $qi < $sm->size(); $qi++) { $fsa2->newState($qi); }
  $fsa2->setInitialStateId($sm->get($fsa->initialStateId));

  for ($qi=0; $qi < $sm->size(); $qi++) {
    $qi2 = $sm->get($qi);
    next if ($qi2 == $noState);

    $q1 = $fsa->getState($qi);
    $q2 = $fsa2->fastState($qi2);

    for ($ai=0; $ai < $q1->nArcs; $ai++) {
      $arc = $q1->getArc($ai);
      $q2->newArc($sm->get($arc->{target}), $arc->{weight}, int $arc->{input}, int $arc->{output});
    }

    $q2->setFinal($q1->{weight}) if ($q1->isFinal);
  }

  return $fsa2;
}


##----------------------------------------------------------------------
## Fsa utils: hashing & comparison
##----------------------------------------------------------------------
*lexcmp = \&MUDL::FsaUtils::lexcmp;

## \%packed2stateid = $fsa->kernel
##    + returns a hash of the form
##        ( $prefixString => $stateId, ... )
##      representing the "kernel" (a la Carrasco&Oncina(99)) of $fsa
##    + Each $prefixString is a packed string of label-ids ('l*') representing
##      a prefix of L($fsa)
sub kernel {
  my $fsa = shift;
  my ($qs,$qi,$qp,$arcs,$arc);
  my $vec = '';
  my @queue = ($fsa->initialStateId,'');
  my %s2q = ();
  while (($qi,$qs) = splice(@queue,0,2)) {
    #next if (exists($s2q{$qs}) || vec($vec,$qi,1));
    next if (exists($s2q{$qs}));
    $s2q{$qs} = $qi;
    next if (vec($vec,$qi,1));

    vec($vec,$qi,1) = 1;
    $qp = $fsa->getState($qi);
    foreach $arc (sort { $a->{input} <=> $b->{input} } @{$fsa->getArcs($qi)}) {
      push(@queue, $arc->{target}, $qs.pack('l',$arc->{input}));
    }
  }
  return \%s2q;
}

*packed2str = \&MUDL::FsaUtils::packed2str;

## \%str2stateId = $fsa->strKernel()
##  + human-readable
sub strKernel {
  my $krn = $_[0]->kernel;
  return { map { packed2str($_) => $krn->{$_} } keys(%$krn) };
}


## \%packed2stateid = $fsa->shortPrefixes
##    + returns a hash of the form
##        ( $prefixString => $stateId, ... )
##      representing the "sort prefixes (Sp())" (a la Carrasco&Oncina(99)) of $fsa
##    + Each $prefixString is a packed string of label-ids ('i*') representing
##      a short prefix of L($fsa)
sub shortPrefixes {
  my $fsa = shift;
  my ($qs,$qi,$qp,$arcs,$arc);
  my $vec = '';
  my @queue = ($fsa->initialStateId,'');
  my %s2q = ();
  while (($qi,$qs) = splice(@queue,0,2)) {
    next if (exists($s2q{$qs}) || vec($vec,$qi,1));
    $s2q{$qs} = $qi;
    vec($vec,$qi,1) = 1;
    $qp = $fsa->getState($qi);
    foreach $arc (sort { $a->{input} <=> $b->{input} } @{$fsa->getArcs($qi)}) {
      push(@queue, $arc->{target}, $qs.pack('l',$arc->{input}));
    }
  }
  return \%s2q;
}

sub strShortPrefixes {
  my $sp = $_[0]->shortPrefixes;
  return { map { packed2str($_) => $sp->{$_} } keys(%$sp) };
}

1;

##======================================================================
## Docs
=pod

=head1 NAME

MUDL::FsaUtils - MUDL : Fsa Utilities

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
