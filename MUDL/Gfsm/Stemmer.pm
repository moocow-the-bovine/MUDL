##-*- Mode: CPerl -*-

## File: MUDL::Gfsm::Stemmer
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: stemmer
##======================================================================

package MUDL::Gfsm::Stemmer;
use MUDL::Object;
use MUDL::Gfsm::Automaton;
use MUDL::Gfsm::Alphabet;

use Gfsm;
use Carp qw(carp croak);
use strict;

our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $obj = $class_or_obj->new(%args)
##  + %args
##     prefixes   => \@prefix_list,
##     suffixes   => \@suffix_list,
##     stems      => \@stem_list,
##     joinmorphs => \@joinmorph_list,
##     exceptions => \@exception_list,
##  + structure:
##     abet          => $alphabet,
##     prefixTrie    => $trie,
##     suffixTrie    => $trie,
##     stemTrie      => $trie, ##-- known stems only
##     joinmorphTrie => $trie,
##     exceptionTrie => $trie,
sub new {
  my ($that,%args) = @_;
  my $obj = bless {
		   (map { ($_."Trie")=>MUDL::Gfsm::Automaton->newTrie() }
		    qw(prefix suffix stem joinmorph exception)),
		   abet=>MUDL::Gfsm::Alphabet->new(),
		   %args,
		  }, ref($that)||$that;

  $obj->ensureEpsilon();

  return $obj;
}

## undef = $stemmer->ensureEpsilon()
sub ensureEpsilon {
  my $stemmer = shift;
  $stemmer->{abet}->insert('<epsilon>',0);
}

## undef = $stemmer->clear()
sub clear {
  my $stemmer = shift;
  $stemmer->{$_}->clear foreach (qw(prefixTrie suffixTrie stemTrie joinmorphTrie exceptionTrie abet));
  $stemmer->ensureEpsilon;
  return $stemmer;
}

##======================================================================
## Manipulation
##======================================================================

## undef = $stemmer->addChars(@chars)
sub addChars {
  my $stemmer = shift;
  $stemmer->{abet}->insert($_) foreach (@_);
  return $stemmer;
}

## undef = $stemmer->addPrefix($prefix)
sub addPrefix {
  my ($stemmer,$prefix) = @_;
  $stemmer->{prefixTrie}->add_path([ map { $stemmer->{abet}->get_label($_) } split(//,$prefix) ],
				   [], 0.0, 1,0,1);
}

## undef = $stemmer->addSuffix($suffix)
sub addSuffix {
  my ($stemmer,$suffix) = @_;
  $stemmer->{suffixTrie}->add_path([ reverse(map { $stemmer->{abet}->get_label($_) } split(//,$suffix)) ],
				   [], 0.0, 1,0,1);
}

## undef = $stemmer->addStem($stem)
sub addStem {
  my ($stemmer,$stem) = @_;
  $stemmer->{stemTrie}->add_path([ map { $stemmer->{abet}->get_label($_) } split(//,$stem) ],
				 [], 0.0, 1,0,1);
}

## undef = $stemmer->addJoinmorph($joinmorph)
sub addJoinmorph {
  my ($stemmer,$jmorph) = @_;
  $stemmer->{joinmorphTrie}->add_path([ map { $stemmer->{abet}->get_label($_) } split(//,$jmorph) ],
				      [], 0.0, 1,0,1);
}

## undef = $stemmer->addException($exception_surface, $exception_segmented)
sub addException {
  my ($stemmer,$efrom,$eto) = @_;
  $stemmer->{exceptionTrie}->add_path([ map { $stemmer->{abet}->get_label($_) } split(//,$efrom) ],
				      [ map { $stemmer->{abet}->get_label($_) } split(//,$eto)   ],
				      0.0, 1,0,1);
}

##======================================================================
## Extraction: prefixes
##======================================================================

## \@chars = $stemmer->alphabet()
sub alphabet {
  return [map { $_[0]{abet}->find_key($_) } grep { $_ != 0 } @{$_[0]{abet}->labels}];
}

## \@prefixes = $stemmer->prefixes()
sub prefixes {
  return [
	  map { join('', map { $_[0]{abet}->find_key($_) } @{$_->{lo}}) }
	  @{$_[0]{prefixTrie}->paths}
	 ];
}

## \@stems = $stemmer->stems()
sub stems {
  return [
	  map { join('', map { $_[0]{abet}->find_key($_) } @{$_->{lo}}) }
	  @{$_[0]{stemTrie}->paths}
	 ];
}

## \@stems = $stemmer->joinMorphs()
sub joinmorphs {
  return [
	  map { join('', map { $_[0]{abet}->find_key($_) } @{$_->{lo}}) }
	  @{$_[0]{joinmorphTrie}->paths}
	 ];
}

## \@suffixes = $stemmer->suffixes()
sub suffixes {
  return [
	  map { join('', map { $_[0]{abet}->find_key($_) } reverse(@{$_->{lo}})) }
	  @{$_[0]{suffixTrie}->paths}
	 ];
}


## \%exceptions = $stemmer->exceptions()
sub exceptions {
  return {
	  map {
	    (
	     join('', map { $_[0]{abet}->find_key($_) } @{$_->{lo}}),
	     join('', map { $_[0]{abet}->find_key($_) } @{$_->{hi}}),
	    )
	  }
	  @{$_[0]{exceptionTrie}->paths}
	 };
}

##======================================================================
## trie sub-paths
##======================================================================

## \@paths = subpaths($fsm,$root_id)
sub subpaths {
  my ($stemmer,$fsm,$root_id) = @_;
  return [] if (!$fsm->has_state($root_id));
  my $root_id_tmp = $fsm->root;
  $fsm->root($root_id);
  my $paths = $fsm->paths;
  $fsm->root($root_id_tmp);
  return $paths;
}

##======================================================================
## Lookup
##======================================================================

## $segmented_word = $stemmer->parse($word) ##-- scalar context
## #@word_segments  = $stemmer->parse($word) ##-- array context
sub parse {
  my ($stemmer,$word) = @_;

  ##-- 0: get labels
  my @labs = map { $stemmer->{abet}->find_label($_) } split(//,$word);

  ##-- 1: check for exception
  my ($qid,$lo_i);
  ($qid,$lo_i) = $stemmer->{exceptionTrie}->find_prefix(\@labs,[]);
  if ($lo_i == @labs) {
    my $sword = join('',
		     map {
		       $stemmer->{abet}->find_key($_)
		     } @{$stemmer->subpaths($stemmer->{exceptionTrie},$qid)->[0]{hi}});
    return wantarray ? split(/\.+/, $sword) : $sword;
  }

  ##-- 2: get prefixes
  my $i = 0;
  my ($qids,@i_final,$imax);
  my $trie = $stemmer->{prefixTrie};
  my @prefixes = qw();
  while ($i <= $#labs) {
    ($qids,$lo_i) = $trie->find_prefix_states([@labs[$i..$#labs]],[]);
    @i_final      = grep { $trie->is_final($qids->[$_]) } (0..$#$qids);
    last if (!@i_final);
    push(@prefixes, substr($word, $i, $i_final[$#i_final]));
    $i += $i_final[$#i_final];
  }
  my $prefix_len = $i;

  ##-- 3: get suffixes
  my @suffixes = qw();
  $i = 0;
  $trie = $stemmer->{suffixTrie};
  while ($i <= $#labs-$prefix_len) {
    ($qids,$lo_i) = $trie->find_prefix_states([reverse(@labs[($prefix_len)..($#labs-$i)])],[]);
    @i_final      = grep { $trie->is_final($qids->[$_]) } (0..$#$qids);
    last if (!@i_final);
    unshift(@suffixes, substr($word, $#labs-$i-1, $i_final[$#i_final]));
    $i += $i_final[$#i_final];
  }

  ##-- return parsed word
  my @parsed = (@prefixes, substr($word, $prefix_len, @labs-$prefix_len-$i), @suffixes);
  return (wantarray ? @parsed : join('.', @parsed));
}

##======================================================================
## I/O: Native
##======================================================================

## $bool = $obj->saveNativeFh($fh,%args)
sub saveNativeFh {
  my ($obj,$fh) = @_;

  my $except = $obj->exceptions;
  $fh->print(
	     "Alphabet:\n",
	     (join(' ', sort @{$obj->alphabet}), "\n"),
	     "\n",

	     "Prefixes:\n",
	     (join("\n", sort @{$obj->prefixes}), "\n"),
	     "\n",

	     "Suffixes:\n",
	     (join("\n", sort @{$obj->suffixes}), "\n"),
	     "\n",

	     "Stems:\n",
	     (join("\n", sort @{$obj->stems}), "\n"),
	     "\n",

	     "Joinmorphs:\n",
	     (join("\n", sort @{$obj->joinmorphs}), "\n"),
	     "\n",

	     "Exceptions:\n",
	     (map { "$_ -> $except->{$_}\n" } sort(keys(%$except))),
	     "\n"
	    );
  return $obj;
}

## $obj = $class_or_obj->loadNativeFh($fh,%args)
sub loadNativeFh {
  my ($obj,$fh) = @_;
  $obj = $obj->new() if (!ref($obj));

  ##-- data
  my $abet = $obj->{abet};

  my ($line,$key, @charsLo,@charsHi,@labsLo,@labsHi,$efrom,$eto);
  my $mode_default = 'exception';
  my $mode = $mode_default;
  my %known_modes = (map { $_=>undef }
		     qw(prefix suffix stem joinmorph exception alphabet)); #grammar
  while (defined($line=<$fh>)) {
    chomp($line);
    next if ($line =~ /^\s*$/ || $line =~ /^\s*\#/);

    if    ($line eq 'Alphabet:')   { $mode = 'alphabet'; next; }
    elsif ($line eq 'Prefixes:')   { $mode = 'prefix'; next; }
    elsif ($line eq 'Suffixes:')   { $mode = 'suffix'; next; }
    elsif ($line eq 'Stems:')      { $mode = 'stem';   next; }
    elsif ($line eq 'Joinmorphs:') { $mode = 'joinmorph'; next; }
    elsif ($line eq 'Exceptions:') { $mode = 'exception'; next; }
    #elsif ($line eq 'Grammar:')    { $mode = 'grammar'; next; }
    $line =~ s/\\(.)/$1/g;

    if (!exists($known_modes{$mode})) {
      carp(ref($obj)."::loadNativeFh(): unknown mode '$mode' -- assuming '$mode_default'!");
      $mode = $mode_default;
    }

    ##-- alphabet: add non-whitespace strings
    if ($mode eq 'alphabet') {
      $abet->insert($_) foreach (grep { defined($_) && $_ ne '' } split(/\s+/,$line));
      next;
    }
    ##-- exceptions: get lo->hi map
    elsif ($mode eq 'exception') {
      ($efrom,$eto) = split(/\s*->\s*/, $line, 2);

      @charsLo = split(//,$efrom);
      @labsLo  = map { $abet->get_label($_) } @charsLo;

      @charsHi = split(//,$eto);
      @labsHi  = map { $abet->get_label($_) } @charsHi;

      $obj->{exceptionTrie}->add_path(\@labsLo, \@labsHi, 0.0, 1,0,1);
      next;
    }

    ##-- default behavior: input trie (prefix|suffix|stem)
    $key = $mode."Trie";
    @charsLo = split(//,$line);
    @labsLo  = map { $abet->get_label($_) } @charsLo;
    @labsLo = reverse(@labsLo) if ($mode eq 'suffix');
    $obj->{$key}->add_path(\@labsLo,[], 0.0, 1,0,1);
  }

  return $obj;
}


##======================================================================
## I/O: XML
##======================================================================

##-- not implemented

##======================================================================
## Methods: Viewing
##======================================================================

## undef = $stemmer->viewps($key,%opts)
##  + view FSM
sub viewps {
  my ($obj,$key) = @_;
  return $obj->{"${key}Trie"}->viewps(labels=>$obj->{abet},@_);
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
