# -*- Mode: Perl -*-

## File: MUDL::Tree.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: trees
##======================================================================

package MUDL::Tree;
use MUDL::Object;
use Carp;
use Exporter;

our @ISA       = qw(MUDL::Object);
our @EXPORT    = qw();
our @EXPORT_OK = qw();

###############################################################
# Globals
###############################################################
our $VERSION = 0.01;
sub VERSION {
  my ($self) = shift;
  return $self->{VERSION} ? $self->{VERSION} : $VERSION;
}

our $Tk_SkipX = 15;
our $Tk_SkipY = 35;
our $Tk_LinePad = 2;
our $Tk_Font = 'helvetica -12 bold';
our $Tk_BoxActive = 1;

###############################################################
# Constructor
#   + $t = MUDL::Tree->new(%args)
#   + structural %args:
#      moms=>{$key=>$momkey,...},       # mothers
#      dtrs =>{$key=>\@daughters,...},  # daughters
#      labels=>{$key=>$label,...},      # node labels
#      epsilon=>$epsilon,               # default=''
#      packfmt=>$pack_id_format,        # for key-generation
#   + extra args:
#      key => $root_key,
#      label => $root_label,
#      mom => $mom_key,
#   + current node identification
#      key => $current_key
#      #index => {$key=>$index,...},
#      #dindex => {$key=>$index},
#      #current=>$node,
###############################################################
sub new {
  my $that = shift;
  my %args = @_;
  my ($key,$label,$mom) = @args{qw(key label mom)};
  delete @args{qw(key label mom)};
  $key = '' if (!defined($key));
  $label = '' if (!defined($label));
  $mom = undef if (!defined($mom));
  return $that->SUPER::new(
			   key => $key,
			   labels => {$key=>$label},
			   moms => {$key=>$mom},
			   dtrs => {},
			   epsilon => '',
			   packfmt => 'C',
			   #dindex => {},
			   #index => {},
			   @_
			  );
}

# ???
sub DESTROY {
  my $self = shift;
  #print STDERR __PACKAGE__, "::DESTROY() called.\n";
  $self->clear();
  %$self = ();
}



## $t = $t->clear()
*clear = \&cleartree;
sub cleartree {
  my $t = shift;
  $t->{key} = '.' if (!defined($t->{key}));
  %{$t->{labels}} = ($t->{key}=>'');
  %{$t->{moms}} = ($t->{key}=>undef);
  %{$t->{dtrs}} = ();
  %{$t->{dindex}} = ();
  return $t;
}

###############################################################
# Basic accessors
###############################################################

# $node = $t->rootNode()
*root = \&rootNode;
sub rootNode {
  my $t = shift;
  return $t->{key};
}

# $lab = $t->rootLabel()
# $lab = $t->rootLabel($lab)
sub rootLabel {
  my $t = shift;
  my $k = $t->root_node();
  $t->{labels}{$k} = shift(@_) if (@_);
  return $t->{labels}{$k};
}

# $lab = $t->label($node)
# $lab = $t->label($node,$lab)
sub label {
  my ($t,$n) = (shift,shift);
  $t->{labels}{$n} = shift(@_) if (@_);
  return $t->{labels}{$n};
}

# @nodes = $t->daughters($node)
*dtrs = *children = \&daughters;
sub daughters {
  my ($t,$n) = (shift,shift);
  return @{$t->{dtrs}{$n}} if (defined($t->{dtrs}{$n}));
  return qw();
}


## $key = $tree->key($node);
##   + gets tree-domain key, returns '' for undefined nodes
*getKey = *get_key = \&key;
sub key {
  my ($tree,$node) = @_;
  return defined($node) ? $node : '';
}

## $str = $tree->key2str($key)
##   + returns human-readable tree-domain key-string
sub key2str {
  return join('.',unpack($_[0]{packfmt}.'*', $_[1]));
}

## $str = $tree->str2key($str)
##   + converts human-readable tree-domain key-string to packed version
sub str2key {
  return pack($_[0]{packfmt}.'*', CORE::split(/\.+/, $_[1]));
}

## $str = $tree->keyString($node)
##   + returns human-readable tree-domain key-string
sub keyString {
  return $_[0]->key2str($_[0]->key($_[1]));
}


###############################################################
# $bool = $tree->isLeafNode($node)
#   + returns true if $node has no children
#   + allows non-method invocation (can call without $self
#     argument)
###############################################################
sub isLeafNode {
  my ($tree,$node) = @_;
  $node = $tree->{key} if (!defined($node));
  return !defined($tree->{dtrs}{$node}) || $#{$tree->{dtrs}{$node}} < 0;
}

###############################################################
# $bool = $tree->isRootNode($node)
#   + returns true if $node's depth is 0
#   + allows non-method invocation (can call without $tree
#     argument)
###############################################################
sub isRootNode {
  my ($tree,$node) = @_;
  $node = $tree->{key} if (!defined($node));
  return !defined($tree->{moms}{$node});
}

###############################################################
# $tree->isCurrentNode($node)
#   + returns true if $handle's key is eq to the key of
#     $self->{handle}
#   + allows method-invocation only!
###############################################################
sub isCurrentNode {
  my ($t,$n) = @_;
  $n = $t->{key} if (!$n);
  return 1 unless (defined($n));
  return defined($t->{current}) && $t->{current} eq $h;
}

###############################################################
# $dtrkey = $tree->addDaughter($label,$mom);
#   + add a new child node under $mom
###############################################################
*addChild = *addChildNode = *addDtr = \&addDaughter;
sub addDaughter {
  my ($t,$lab,$mom) = @_;
  $mom = $t->{key} if (!$mom);
  $t->{dtrs}{$mom} = [] if (!$t->{dtrs}{$mom});
  my $i = @{$t->{dtrs}{$mom}};
  my $k = $mom . pack($t->{packfmt}, $i);
  push(@{$t->{dtrs}{$mom}}, $k);
  $t->{labels}{$k} = $lab;
  $t->{moms}{$k} = $mom;
  return $k;
}

###############################################################
# $t = $t->addDaughters(\@labels,$mom)
###############################################################
*addDtrs = *addChildNodes = *addChildren = \&addDaughters;
sub addDaughters {
  my ($t,$labs,$mom) = @_;
  $mom = $t->{key} if (!$mom);
  foreach (@$labs) { $t->addDaughter($_,$mom); }
  return $t;
}


###############################################################
# $next = $t->nextTopDown($node)
#   + returns the next (top-down, depth-first) node
#   + first checks node's next daughter position; if
#     that fails, there are no more child positions
#     available under $node.
#   + otherwise, checks mother instead -- if undef is
#     returned, $node is the root of the tree, and we
#     can return undef ourselves.
#   + otherwise, the last 2 steps are repeated until some
#     defined next-daughter is found
#     - An exception is the case where the next defined
#       child node's value is $self->{epsilon} -- this causes
#       resumption of the loop just as if no more children
#       were available.
#   + If some defined daughter is found, she is returned.
###############################################################
sub nextTopDown {
  my ($t,$node) = @_;
  $node = $t->{key} if (!$node);
  my ($pos,$next);
 FINDKID: {
    do {
      # try getting next daughter
      $pos = $t->{dindex}{$node}++;
      if (defined($pos)
	  && defined($t->{dtrs}{$node})
	  && defined($next = $t->{dtrs}{$node}[$pos])) {
	# found an open child...
	$node = $next;
	if ($t->{labels}{$next} eq $t->{epsilon}) {
	  # ...bummer, it's epsilon.
	  ;
	} else {
	  # ...it's good
	  last FINDKID;
	}
      }
      # no more children: look at mother-node, if we have one
      return undef unless($node = $t->{moms}{$node});
    } while (1);
  }
  return $node;
}

###############################################################
# $next = $tree->expand(\@labels,$node)
#   + adds children in @$values and calls next_topdown($node)
###############################################################
sub expand {
  my ($t,$labels,$node) = @_;
  $node = $tree->{key} if (!$node);
  $t->add_children($labels,$node);
  return $t->next_topdown($node);
}

###############################################################
# \@leaves = $tree->leaves($node)
# \@leaves = $tree->leaves()
#   + returns a listref of leaves dominated by $node,
#     which defaults to $tree.
###############################################################
sub leaves {
  my ($t,$node) = @_;
  $node = $t->{key} if (!defined($node));
  my $ll = [];
  $t->traverse(node => $node,
	       sub => \&_leaf_list_node,
	       leaflist => $ll);
  return $ll;
}
sub _leaf_list_node {
  my (%args) = @_;
  if ($args{tree}->is_leaf_node($args{node})) {
    push(@{$args{leaflist}}, $args{tree}{labels}{$args{node}});
  }
}

###############################################################
# $tree->traverse(%args)
#    + abstract method for tree traversal, based on the example
#      code in the documentation of the Tree::MultiNode module.
#    + %args is a hash which may have the following keys:
#        tree    => $tree (caller)
#        sub     => \&sub,
#        after   => \&sub_after
#        node    => $node,
#        depth   => $depth,
#        first   => \&first,
#        last    => \&last,
#        butlast => \&butlast
#   + recursively descends the tree from $node (default is
#     $tree, calling &$sub(%args) for each
#     node.
#   + $sub_after(%args) is called after processing all children
#   + &first(%args) is called (if specified) for the first of 
#     a node's children, before calling traverse(%args) for that
#     child.
#   + &last(%args) is called (if specified) for the last of a 
#     node's children, after having called traverse(%args) for
#     that child
#   + &butlast(%args) is called (if specified) for all but the
#     last of a node's children, after having called
#     traverse(%args) for that child.
#   + The values returned by each call of any of the code
#     references \&sub, \&first, \&last, and \&butlast are.
#     gathered into a list which is returned by traverse
###############################################################
sub traverse {
  my ($tree,%args) = @_;
  # initialization
  $args{node} = $tree->{key} if (!defined($args{node}));
  $args{depth} = -1 if (!defined($args{depth}));
  $args{tree} = $tree if (!defined($args{tree}));
  ++$args{depth};
  my $node = $args{node};
  my (@list,$nkids,$i);

  # current node
  @list = &{$args{sub}}(%args) if (defined($args{sub}));
  $nkids = defined($tree->{dtrs}{$node}) ? $#{$tree->{dtrs}{$node}} : -1;
  for ($i = 0; $i <= $nkids; $i++) {
    # recursive transversal
    $args{node} = $tree->{dtrs}{$node}[$i];
    push(@list,
	 ($i == 0 && defined($args{first}) ? &{$args{first}}(%args) : qw()),
	 ($tree->traverse(%args)),
	 ($i != $nkids && defined($args{butlast}) ? &{$args{butlast}}(%args) : qw()),
	 ($i == $nkids && defined($args{last}) ? &{$args{last}}(%args) : qw()),
	);
  }
  --$args{depth};
  $args{node} = $node;
  # post-processing
  push(@list, &{$args{after}}(%args)) if (defined($args{after}));
  return @list;
}

###############################################################
# $tree->dumpTree();
# $tree->dumpTree($filehandle);
# $tree->dumpTree($filehandle,$node);
#   + top-level decomposed version of the example code in the
#     docs for Tree::MultiNode
#   + $node defaults to $tree
###############################################################
sub dumpTree { 
  my ($tree,$fh,$node) = @_;
  $fh = STDOUT if (!defined($fh));
  $node = $tree->{key} if (!$node);
  $fh->print($tree->dumpTreeList($node));
}

###############################################################
# $tree->dumpTreeList($node)
# $node->dumpTreeList()
#   + decomposed version of the example code in the
#     docs for Tree::MultiNode (depth-passing, undef checking)
#   + returns a list of strings which can be passed to "print"
#   + $node defaults to $tree
###############################################################
sub dumpTreeList {
  my ($tree,$node) = @_;
  $node = $tree->{key} if (!$node);
  return $tree->traverse(sub => \&dump_tree_node,
			 node => $node,
			 depth => -1);
}
sub dump_tree_node {
  my (%args) = @_;
  my $lead = ' ' x ($args{depth}*2);
  my $label = $args{tree}{labels}{$args{node}};
  #my ($key,$val) = $args{handle}->get_data();
  my @l = ($lead, "label:   ", defined($label) ? $label : "undef", "\n",
	   #$lead, "key:   ", defined($key) ? $key : "undef", "\n",
	   #$lead, "val:   ", defined($val) ? $val : "undef", "\n",
	   $lead, "depth: ", defined($depth) ? $args{depth} : "undef", "\n$lead--\n");
  return @l;
}


###############################################################
# $tree->string($string)
# $tree->string()
#   + sets tree structure to reflect that of $string, if
#     specified,
#   + otherwise, returns string rep of tree
###############################################################
sub string {
  my $tree = shift;
  return $tree->canonical_string() if (!@_);
  my $str = shift;
  $tree->clear;
  my $node = $tree->{key};
  while ($str) {
    if ($str =~ s/^([^<(,)>]+)//) { # symbol
      $tree->{labels}{$node} = $1;
    }
    #elsif ($str =~ s/^<([^<(,)>]+)>//) { # focussed node
    #  $node->{label} = $1;
    #  $self->{handle} = $handle->new($handle);  # hack!
    #}
    elsif ($str =~ s/^\(//) { # first daughter
      $node = $tree->add_child($node);
    }
    elsif ($str =~ s/^,//) {  # next daughter
      $node = $tree->{moms}{$node} if (defined($tree->{moms}{$node}));
      $node = $tree->add_child($node);
    }
    elsif ($str =~ s/^\)//) { # last daughter
      $node = $tree->{moms}{$node} if (defined($tree->{moms}{$node}));
    } else {
      warn ("cannot interpret tree-string from `$str'\n");
      last;
    }
  }
}

###############################################################
# I/O: Native
###############################################################
# $obj = $obj_or_class->loadNativeFh($fh)
sub loadNativeFh {
  my ($t,$fh) = @_;
  $t = $t->new() if (!$t);
  $fh->input_record_separator(undef);
  my $s = $fh->getline;
  $t->string($s);
  return $t;
}
# $bool = $obj_or_class->saveNativeFH($fh)
sub saveNativeFh {
  my ($t,$fh) = @_;
  $fh->print($t->string, "\n");
  return $t;
}

###############################################################
# $tree->canonical_string($node)
# $node->canonical_string()
#   + prints the list returned by tree_ascii_list as a string,
#     which constitutes an ASCII representation of the
#     (sub)tree rooted at $handle.
###############################################################
*canonical_string = \&canonicalString;
sub canonicalString { 
  my ($tree,$node) = @_;
  return join('',$tree->treeAsciiList($node||$tree->{key}));
}

###############################################################
# $tktree = $tree->toTk(%args)
#  + creates a MUDL::Tk::Tree from $tree
sub toTk {
  require MUDL::Tk::Tree;
  return MUDL::Tk::Tree->new(tree=>$_[0], enum=>$_[0]{enum}, @_[1..$#_]);
}

# $tktree = $tree->view(%args)
#  + creates a MUDL::Tk::Tree from $tree and views it
sub view {
  return $_[0]->toTk(@_[1..$#_])->view(@_[1..$#_]);
}



###############################################################
# undef = $node->print_tree_ascii()
# undef = $node->print_tree_ascii($filehandle)
# undef = $tree->print_tree_ascii($filehandle, $node])
#   + prints the list returned by tree_ascii_list, which
#     constitute an ASCII representation of the (sub)tree
#     rooted at $node
###############################################################
sub printTreeAscii { 
  my ($tree,$fh,$node) = @_;
  $node = $tree->{key} if (!defined($node));
  $fh = STDOUT unless (defined($fh));
  $fh->print($tree->treeAsciiList($node));
}

###############################################################
# @strings = $tree->treeAsciiList($node)
# @strings = $node->treeAsciiList()
#   + returns a list of strings which can be passed to "print"
#     and which constitute an ASCII representation of $handle,
#     and which bear a remarkable resemblance to a PROLOG term.
###############################################################
sub treeAsciiList {
  my ($tree,$node) = @_;
  $node = $tree if (!defined($node));
  return $tree->traverse(sub => \&_tree_ascii_list_node,
			 first => \&_tree_ascii_list_first,
			 last => \&_tree_ascii_list_last,
			 butlast => \&_tree_ascii_list_butlast,
			 node => $node,
			 #current => $self->{current},
			);
}
sub _tree_ascii_list_node {
  my %args = @_;
  my $lab = $args{tree}{labels}{$args{node}};
  $lab = 'undef' unless (defined($lab));
  if (defined($args{current}) && $args{node} eq $args{current}) {
    return "<$lab>";
  }
  return $lab;
}
sub _tree_ascii_list_first { return '('; }
sub _tree_ascii_list_last { return ')'; }
sub _tree_ascii_list_butlast { return ','; }


###############################################################
# I/O: XML
###############################################################

# $obj = $obj->saveXMLNode(%args)
sub saveXMLNode {
  my $t = shift;
  (my $nodename=ref($t)) =~ s/::/./g;
  my $node = XML::LibXML::Element->new($nodename);
  my ($tn,$xn,$xl);

  foreach $tn (keys(%{$t->{moms}})) {
    $node->appendChild($xn = XML::LibXML::Element->new('node'));
    $xn->setAttribute('key', $t->key2str($tn));
    if (defined($t->{moms}{$tn})) {
      $xn->setAttribute('mom', $t->key2str($t->{moms}{$tn}));
    } else {
      $xn->setAttribute('root', "true");
    }
    $xn->appendChild($xl = XML::LibXML::Element->new('label'));
    $xl->appendChild(MUDL::Object::saveXMLNode($t->{labels}{$tn}));
  }
  return $node;
}

# $obj = $class_or_obj->loadXMLNode($node,%args)
sub loadXMLNode {
  my ($t,$node) = @_;
  $t = $t->new() if (!ref($t));

  my ($xn,$xl,$xid,$xroot,$xmom, $tid,$tmom);
  foreach $xn ($node->getChildrenByTagName('node')) {
    $xid   = $xn->getAttribute('key');
    $xmom  = $xn->getAttribute('mom');
    $xroot = $xn->getAttribute('root');
    $xl    = ($xn->getChildrenByTagName('label'))[0];
    $lab   = MUDL::Object->loadXMLNode($xl->firstChild);

    $tid   = $t->str2key($xid);
    $t->{labels}{$tid} = $lab;
    if ($xroot || !defined($xmom)) {
      $t->{key} = $tid;
    } else {
      $tmom = $t->str2key($xmom);
      $t->{moms}{$tid} = $tmom;
      $t->{dtrs}{$tmom} = [] if (!$t->{dtrs}{$tmom});
      push(@{$t->{dtrs}{$tmom}}, $tid);
    }
  }
  @$_ = sort(@$_) foreach (values(%{$t->{dtrs}}));

  return $t;
}

###############################################################
# Conversion: from cluster
###############################################################

# $t = $class_or_obj->fromClusters($cluster_tree,%args)
#   + %args:
#      enum=>$enum,
#      dists=>$dists,
#      %other_tree_new_args
sub fromClusters {
  my ($t,$ct,%args) = @_;
  $t = $t->new(packfmt=>'I',%args) if (!ref($t));

  my @queue = ('',$#$ct);
  my ($tid,$ctid, $ctn,@tdids,$i);
  while (($tid,$ctid)=splice(@queue,0,2)) {
    $ctn = $ct->[$ctid];

    ##-- leaves?
    foreach $i (0,1) {
      $tdids[$i] = $tid.pack($t->{packfmt}, $i);
      $t->{moms}{$tdids[$i]} = $tid;
      if ($ctn->[$i] >= 0) {
	$t->{labels}{$tdids[$i]} = $ctn->[$i];
      } else {
	$t->{moms}{$tdids[$i]} = $tid;
	push(@queue, $tdids[$i], -($ctn->[$i]+1));
      }
    }
    $t->{dtrs}{$tid}   = [@tdids];
    $t->{labels}{$tid} = 'n'.$ctid;
  }

  return $t;
}


###############################################################
# Autoload function allows
#  "$mytree->key" to get an arbitrary key value, and 
#  "$mytree->key(val)" to set the value of 'key' to 'val'.
###############################################################
sub AUTOLOAD {
  my $self = shift;
  my $type = ref($self) || croak "$self is not an object";
  my $name = $AUTOLOAD;
  $name =~ s/.*:://; # strip package-part of name
  if (exists($self->{$name})) {
    if (@_) { return $self->{$name} = shift; } # set value
    else { return $self->{$name}; }            # get value
  }
  carp(__PACKAGE__ , "::$name() method not found in AUTOLOAD");
  return undef;
}

1;
