#-*- Mode: CPerl -*-

## File: MUDL::Morph::Goldsmith.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Goldsmith (2001,2004) morphology induction
##======================================================================

package MUDL::Morph::Goldsmith;
use MUDL::Gfsm::FreqTrie;
use MUDL::Corpus::Profile;
use MUDL::Enum;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile);

##======================================================================
## $prof = $class_or_obj->new(%args)
##   + %args:
##     ##-- profiling arguments
##     #bos    => $bos_str,     ##-- bos string
##     #eos    => $eos_str,     ##-- eos string
##     tsplit => $re_or_undef, ##-- split regex for input tokens (undef to profile whole words as symbols)
##     ##-- induction arguments
##     minStemLen1 => $len,              ##-- minimum stem length for succFreqs1() (default=3)
##     maxSuffixLen => $len,             ##-- maximum suffix length (default=5)
##     maxPrefixLen => $len,             ##-- maximum prefix length (default=5)
##     maxNeighborSuccCount1 => $count,  ##-- maximum neighbor successor count for succFreqs1() (default=1)
##     ##-- profiling data
##     abet     => $abet,     ##-- a MUDL::Gfsm::Alphabet
##     pta      => $pta,      ##-- a MUDL::Gfsm::FreqTrie (using $prof->{abet}, reversed=>0)
##     sta      => $sta,      ##-- a MUDL::Gfsm::FreqTrie (using $prof->{abet}, reversed=>0)
##     wenum    => $enum      ##-- enumerates whole-word label-vectors (packed)
##     ##-- model data
##     model    => $model,    ##-- ??? current model: sigs, stems, affixes ???
##     model1   => $model1,   ##-- ??? next model: sigs, stems, affixes ???
##     ##-- useful flags
##     #leftMargin => $int,    ##-- length (in symbols) to allow for BOS
##     #rightMargin => $int,   ##-- length (in symbols) to allow for EOS
sub new {
  my ($that,%args) = @_;

  my $self = $that->SUPER::new
    (
     ##-- profiling arguments
     #bos=>'__$',
     #eos=>'__$',
     tsplit=>undef,
     nosta=>0,
     ##-- induction arguments
     minStemLen1 => 3,
     maxNeighborSuccCount1 => 1,
     maxSuffixLen=>5,
     maxPrefixLen=>5,
     ##-- profiling data
     abet=>MUDL::Gfsm::Alphabet->new(),
     pta=>MUDL::Gfsm::FreqTrie->new(reversed=>0),
     sta=>MUDL::Gfsm::FreqTrie->new(reversed=>0),
     wenum=>MUDL::Enum->new(),
     ##-- model data
     model=>{},
     %args,
    );

  ##-- undefine tsplit if it is the string 'undef'
  $self->{tsplit} = undef if (defined($self->{tsplit}) && $self->{tsplit} eq 'undef');

  ##-- share alphabet
  $self->{pta}{abet} = $self->{sta}{abet} = $self->{abet};
  $self->{pta}->ensureEpsilon();

  ##-- sanitize
  delete($self->{sta}) if ($self->{nosta});

  ##-- compute margins
  #$self->{leftMargin}  = (defined($self->{bos}) ? 1 : 0);
  #$self->{rightMargin} = (defined($self->{eos}) ? 1 : 0);

  return $self;
}

##======================================================================
## Induction: step 1: successor frequencies (#1)

## undef = $morph->successorFrequencies1(%args)
##  + populates initial $morph->{model}
##  + %args:
##    reversed=>$bool, ##-- true iff prefix searc
##    mode=>$mode,     ##-- one of '1', ...

our $DEBUG_SF = 1;

*succFreqs = *successorFreqs = \&successorFrequencies;
sub successorFrequencies {
  my ($morph,%args) = @_;

  my ($trie,$maxAffixLen);
  if (!$args{reversed}) {
    $trie = $morph->{pta};
    $maxAffixLen = $morph->{maxSuffixLen};
  } else {
    $trie = $morph->{sta};
    $maxAffixLen = $morph->{maxPrefixLen};
  }
  my $fsm  = $trie->{fsm};
  my $mode = defined($args{mode}) ? $args{mode} : '1';
  my $cuts = $morph->{cuts} = {};

  my ($vec,@labs,$qid,$i,@sf);
  my $vid = -1;
  foreach $vec (@{$morph->{wenum}{id2sym}}) {
    ++$vid;
    @labs = $args{reversed} ? reverse(unpack('S*',$vec)) : unpack('S*',$vec);
    @sf   = qw();  ##-- maps position index $i to card(alternatives(@labs[0..$i]))
    $qid  = $fsm->root;

    ##-- get successor frequencies for this word
    foreach $i (0..$#labs) {
      $qid = $fsm->find_arc_lower($qid,$labs[$i]);
      croak(ref($morph)."::successorFrequencies(): inconsitent word-enum -- bailing out!")
	if ($qid==$Gfsm::noState);
      #next if ($i < $morph->{minStemLen1});
      push(@sf, $fsm->out_degree($qid));
    }
    #push(@sf,0);

    ##-- attempt to find an acceptable cut (longest stem first)
    for ($i=$#labs -1;
	 $#labs-$i < $maxAffixLen && $i+1 >= $morph->{minStemLen1} && $i >= 0;
	 $i--)
      {
	if ($mode eq '1') {
	  if ($sf[$i-1]    <= $morph->{maxNeighborSuccCount1}
	      && $sf[$i]   >  1
	      && $sf[$i+1] <= $morph->{maxNeighborSuccCount1})
	    {
	      #cutRightBeforeHere($vec,$i)
	      ##-- Linguistica
	      ##    + "use only the smallest suffix: this is wrong in some cases
	      ##      (e.g. assist-an-t, assist-an-ce)"
	      #$cuts->{$vid} = $i;
	      $cuts->{$vid} = $args{reversed} ? ($#labs-$i-1) : $i;
	      last;
	    }
	}
	else {
	  croak(ref($morph)."::successorFrequencies(): unknown mode '$mode' -- bailing out!\n");
	}
      }
  }

  return $morph;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($prof,$sent) = @_;
  my (@labs);
  if (defined($prof->{tsplit})) {
    my ($tok);
    ##-- symbols are characters
    foreach $tok (@$sent) {
      @labs = (map { $prof->{abet}->get_label($_) }
	       (
		#(defined($prof->{bos}) ? $prof->{bos} : qw()),
		split($prof->{tsplit}, ref($tok) ? $tok->text : $tok),
		#(defined($prof->{eos}) ? $prof->{eos} : qw()),
	       ));
      $prof->{pta}->addPathLabels(\@labs,1);
      #$prof->{sta}->addPathLabels([reverse @labs],1);
      $prof->{wenum}->addSymbol(pack('S*',@labs));
    }
  } else {
    ##-- symbols are words
    @labs = (
	     #(defined($prof->{bos}) ? $prof->{bos} : qw()),
	     (map { $prof->{abet}->get_label(ref($_) ? $_->text : $_) } @$sent),
	     #(defined($prof->{eos}) ? $prof->{eos} : qw()),
	    );
    $prof->{pta}->addPathLabels(\@labs,1);
    #$prof->{sta}->addPathLabels([reverse @labs],1);
    $prof->{wenum}->addSymbol(pack('S*',@labs));
  }
}

##======================================================================
## Profiling: Finish

## undef = $prof->finish()
sub finish {
  my $prof = shift;

  ##-- populate STA
  if ($prof->{sta}) {
    my ($vec,@labs);
    foreach $vec (@{$prof->{wenum}{id2sym}}) {
      @labs = unpack('S*',$vec);
      $prof->{sta}->addPathLabels([reverse(@labs)],$prof->{pta}->getFreqLabels(\@labs));
    }
  }
}


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Class for corpus profiles and morphology induction a la Goldsmith (2001,2004).\n)
     .qq( +Profiling Options\n)
     #.qq(  eos=EOS_STRING     [default='__\$']\n)
     #.qq(  bos=BOS_STRING     [default='__\$']\n)
     .qq(  tsplit=RE_OR_UNDEF [default=undef]\n)
     .qq(  nosta=BOOL         [default=0]\n)
     .qq( +Induction Options\n)
     .qq(  (not yet documented)\n)
    );
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
