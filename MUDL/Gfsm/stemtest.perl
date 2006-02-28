#!/usr/bin/perl -wd

use lib '../..';
use MUDL::Gfsm::Stemmer;

sub loadStemmer {
  $stemmer = MUDL::Gfsm::Stemmer->new();
  $stemmer->loadNativeFile('testme.stemmer');
}
#loadStemmer;

##-- debug: viewing
## undef = viewps($fsm,%args)
sub viewps {
  my ($fsm,%args) = @_;
  $fsm->viewps(labels=>$stemmer->{abet},%args);
}


##-- test stemmer: parse
sub testParse {
  our $word = shift;
  $word = 'gegeben' if (!$word);
  our @parsed = $stemmer->parse($word);
  print "parse($word) -> ( ", join(' . ', @parsed), ")\n";
}

##-- test stemmer: exceptions
#loadStemmer;
#testParse('gegeben'); ##-- exception
#testParse('angegeben'); ##-- default parse

##----------------------------------------------------------------------
## Compilation
##----------------------------------------------------------------------

##-- test compilation
sub testcompile {
  our $abet = $stemmer->{abet};

  our $seplab = $abet->get_label('.');
  our $sepfst = Gfsm::Automaton->new();
  $sepfst->root(0);
  $sepfst->add_arc(0,1, 0,$seplab, 0.0);
  $sepfst->is_final(1,1);

  our $sigma = Gfsm::Automaton->new();
  $sigma->root(0);
  $sigma->add_arc(0,1, $_,$_, 1.0) foreach (grep { $_ != 0 } @{$abet->labels});
  $sigma->is_final(1,1);

  our $prefixes = Gfsm::Automaton->new();
  $prefixes->add_path($_->{lo},[], 0.0, 1,0,1) foreach (@{$stemmer->{prefixTrie}->paths});
  $prefixes->_project(1);
  $prefixes->is_transducer(1);

  our $suffixes = Gfsm::Automaton->new();
  $suffixes->add_path($_->{lo},[], 0.0, 1,0,1) foreach (@{$stemmer->{suffixTrie}->paths});
  $suffixes->_project(1);
  $suffixes->is_transducer(1);

  our $min_stem_len = 2;
  our $fullfst =
    $prefixes->concat($sepfst)->closure()
      ->concat( $sigma->n_closure($min_stem_len)->concat($sigma->closure(0)) )
	->concat($sepfst->concat($suffixes->reverse)->closure);

  our $full_ne = $fullfst->rmepsilon->determinize();
}


##-- test compilation
sub testcompile_det {
  our $abet = $stemmer->{abet};

  ##-- separators: FLAT LPAREN RPAREN
  our @seplabs = map { $abet->get_label($_) } ('.', '{', '}');
  our @sepfsts = map { Gfsm::Automaton-new() } @parenlabs;
  foreach (0..$#seplabs) {
    $sepfsts[$_]->root(0);
    $sepfsts[$_]->add_arc(0,1, 0,$seplabs[$_], 0.0);
    $sepfsts[$_]->is_final(1,1);
  }
  our ($seplab,$lparlab,$rparlab) = @seplabs;
  our ($sepfst,$lparfst,$rparfst) = @sepfsts;

  ##-- sigma
  our $sigma = Gfsm::Automaton->new();
  $sigma->root(0);
  $sigma->add_arc(0,1, $_,$_, 0.0) foreach (grep { $_ != 0 } @{$abet->labels});
  $sigma->is_final(1,1);

  ##-- prefixes, suffixes, exceptions
  our @prefixes   = @{$stemmer->prefixes};
  our @suffixes   = @{$stemmer->suffixes};
  our %exceptions = %{$stemmer->exceptions};

  ##-- initial fst: exceptions
  our $fst = Gfsm::Automaton->new;
  

}
loadStemmer;
testcompile_det;


##----------------------------------------------------------------------
## Lookup
##----------------------------------------------------------------------

##-- full lookup: dummy
sub full_lookup {
  my ($fst,$word) = @_;
  our $abet = $stemmer->{abet};
  my @labs = map { $abet->find_label($_) } split(//,$word);
  our $res = $fst->lookup(\@labs);
  our $paths = $res->paths;
  return map {
    (join('', map {$abet->find_key($_)} @{$_->{lo}})
     ." : "
     .join('', map {$abet->find_key($_)} @{$_->{hi}})
     ." <$_->{w}>")
  } @$paths;
}

##-- viterbi lookup: dummy
sub viterbi_lookup {
  my ($fst,$word) = @_;
  our $abet = $stemmer->{abet};
  my @labs = map { $abet->find_label($_) } split(//,$word);

  our $trellis   = $fst->lookup_viterbi(\@labs);
  #our $paths = [$trellis->viterbi_trellis_bestpath()];
  our $paths = $trellis->viterbi_trellis_paths(Gfsm::LSBoth);
  return map {
    (join('', map {$abet->find_key($_)} @{$_->{lo}})
     ." : "
     .join('', map {$abet->find_key($_)} @{$_->{hi}})
     ." <$_->{w}>")
  } @$paths;
}

loadStemmer;
testcompile;
#our @lkp = full_lookup($full_ne,'foo');
our @lkp = viterbi_lookup($full_ne, 'angegeben');

##-- dummy
foreach $i (0..10) {
  print STDERR "--dummy--[$i]\n";
}
