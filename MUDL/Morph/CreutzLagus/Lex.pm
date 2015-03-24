##-*- Mode: CPerl -*-

## File: MUDL::Morph::CreutzLagus::Lex
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: Creutz & Lagus (2002): Lexicon
##======================================================================

package MUDL::Morph::CreutzLagus::Lex;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile);

##======================================================================
## Globals

##-- verbosity levels
our %VLEVELS = (
		none=>0,
		error=>1,
		warn=>2,
		info=>3,
		debug=>10,
		full=>255,

		default=>'debug',
	       );


##======================================================================
## $prof = $class_or_obj->new(%args)
##   + user %args:
##      verbose => $level,            ##-- verbosity level (symbolic or numeric)
##      cw      => $character_width,  ##-- character width (bytes) [default=1]
##      cbits   => $nbits_per_char,   ##-- encoding cost per character [default=5]
##
##   + model data:
##      str2node => \%str2node,  ##-- {$str}=>$lex_node
##      leaves   => \%leaf2leaf, ##-- termninal nodes : TOUGH TO MAINTAIN
##      totalf   => $total_freq, ##-- total leaf-frequency
##
##   + node structure: HASH:
##      $lex_node={
##                 key  =>$key,        ##-- node key-string
##                 freq =>$node_freq,  ##-- total node frequency (so far)
##                 split=>$split_index,##-- split string before char $key[$split_index] (0 => no split)
##                 ldtr =>$left_dtr,   ##-- left daughter
##                 rdtr =>$right_dtr,  ##-- right daughter
##                }
sub new {
  my ($that,%args) = @_;

  my $self = $that->SUPER::new
    (
     ##-- defaults
     cw=>1,
     cbits=>5,

     ##-- model data
     str2node=>{},
     leaves=>{},
     totalf=>0,

     ##-- debug
     verbose => 'default',

     ##-- user args
     %args,
    );

  return $self;
}

## $lex = $lex->clear()
##  + clear lexicon
sub clear {
  my $lex = shift;
  %{$lex->{str2node}} = qw();
  %{$lex->{leaves}} = qw();
  $lex->{totalf} = 0;

  return $lex;
}

##======================================================================
## Utilities: Messages
##======================================================================

## $lex->vmsg($level, @msg)
sub vmsg {
  my ($lex,$level,@msg) = @_;
  $lex->{verbose} = 'default' if (!defined($lex->{verbose}));
  my $olevel = $lex->{verbose};
  $olevel = $VLEVELS{$olevel} while (defined($VLEVELS{$olevel}));
  $level  = $VLEVELS{$level}  while (defined($VLEVELS{$level}));
  return if ($olevel < $level);

  print STDERR ref($lex), ": ", @msg;
}

##======================================================================
## Methods: Properties
##======================================================================

## \@leaf_nodes = $lex->leaves()
sub leaves {
  #return grep {$_->{split}==0} values(%{$_[0]{str2node}});
  return [ values(%{$_[0]{leaves}}) ];
}

## \@nodes = $lex->nodes()
sub nodes {
  my $lex = shift;
  return [ values(%{$lex->{str2node}}) ];
}

## $nbits = $lex->modelDescriptionLength(%args)
##   + description length of the model
##   + %args:
##       leaves=>\@leaves,
sub modelDescriptionLength {
  my ($lex,%args) = @_;

  my $dlen = 0;
  my $cbits = $lex->{cbits};
  my $cw    = $lex->{cw};

  my ($leaf);
  foreach $leaf ($args{leaves} ? @{$args{leaves}} : values(%{$lex->{leaves}})) {
    $dlen += $cbits * length($leaf->{key}) / $cw;
  }
  return $dlen;
}

## $nbits = $lex->corpusDescriptionLength(%args)
##   + description length of the corpus
##   + %args:
##       leaves=>\@leaves,
sub corpusDescriptionLength {
  my ($lex,%args) = @_;
  my $lex = shift;

  my $dlen = 0;
  my $totalf = $lex->{totalf};
  my $log2   = log(2);

  my ($leaf);
  foreach $leaf ($args{leaves} ? @{$args{leaves}} : values(%{$lex->{leaves}})) {
    $dlen -= $leaf->{freq} * log($leaf->{freq}/$totalf) / $log2;
  }
  return $dlen;
}

## $nbits = $lex->totalDescriptionLength(%args)
##   + total descritption length
sub totalDescriptionLength {
  my ($lex,%args) = @_;
  $args{leaves} = $lex->leaves() if (!$args{leaves});
  return $lex->modelDescriptionLength(%args) + $lex->corpusDescriptionLength(%args);
}

##======================================================================
## Methods: Navigation
##======================================================================

## \@descendants = $lex->descendants($node)
##   + returns a list of all descendants of $node (including $node itself)
sub descendants {
  my ($lex,$node) = @_;
  my $descs = [$node];
  my @stack = ($node);
  while (defined($node=pop(@stack))) {
    push(@$descs, $node);
    push(@stack,  @$node{qw(ldtr rdtr)}) if ($node->{split});
  }
  return $descs;
}

##======================================================================
## Methods: Manipulation
##======================================================================

## undef = $lex->removeNode($node)
##  + removes node $node and recursively decreases count of all its descendants
##  + maintains $lex->{leaves}
sub removeNode {
  my ($lex,$node) = @_;
  my $freq  = $node->{freq};
  my @stack = ($node);
  while (defined($node=pop(@stack))) {
    ##-- decrement frequency, checking for removable nodes
    if (($node->{freq} -= $freq) <= 0) {
      delete($lex->{leaves}{$node});
      delete($lex->{str2node}{$node->{key}});
    }

    ##-- investigate daughters, if any
    push(@stack, @$node{qwl(ldtr rdtr)}) if ($node->{split});
  }
}

## $root = $lex->insertString($str,$freq)
##  + get or insert a (new) node for string "$key", adding frequency $freq
##  + no new splitting is performed on the node
##  + if a new node is created, it is inserted as a leaf
##  + otherwise, $freq is added to existing node's descendants
sub insertString {
  my ($lex,$str,$freq) = @_;
  my ($node);
  if (defined($node=$lex->{str2node}{$str})) {
    ##-- node exists: increment counts
    $_->{freq} += $freq foreach (@{$lex->descendants($node)});
  }
  else {
    ##-- new terminal node
    $node = $lex->{str2node}{$key} = {
				      key=>$str,
				      freq=>$freq,
				      split=>0,
				     };
    $lex->{leaves}{node} = $node;
  }
  return $node;
}

## $root = $lex->segmentString($string,$newFreq)
##  + (re-)segments string $string
sub segmentString {
  my ($lex,$str,$freq) = @_;
  my ($node);

  if (defined($node=$lex->{str2node}{$str})=) {
    ##-- node exists already: remove it
    $freq += $node->{freq};
    $lex->removeNode($node);
  }

  ##-- search for best segmentation
  return $lex->_segment($str,$freq);
}

## $root = $lex->_segment($str,$freq)
##  + recursive segmentation
sub _segment {
  my ($str,$freq) = @_;

  ##-- get or insert node for $str
  my $node = $lex->insertString($str,$freq);

  my ($cfg);
  my @configs = qw();
  foreach $i (0..(length($str)/$lex->{cw})-1) {
    $cfg = { split=>$i,
	     nodes=>[ 
	   },

  }
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
