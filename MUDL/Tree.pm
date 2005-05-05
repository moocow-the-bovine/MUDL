## -*- Mode: Perl -*-

## File: MUDL::Tree.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: trees
##======================================================================

package MUDL::Tree;
use MUDL::Object;
use Storable;
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

###############################################################
# Constructor
#   + $t = MUDL::Tree->new(%args)
#   + structural %args:
#      labels=>{$id=>$label,...},         # node labels
#      root  =>$current_root_id,          # current root node
#      dtrs  =>{$id=>\@dtr_ids,...}       # daughter nodes
#      moms  =>{$id=>$momid,...}          # mother node
#      _ctr  =>$id,                       # id-counter for key-allocation
#   + extra args:
#      root  => $root_key,
#      label => $root_label,
#      mom   => $root_momid,
#   + current node identification
#      root  => $current_key,
#      label => $current_label,
#      #index => {$key=>$index,...},
#      #dindex => {$key=>$index},
#      #current=>$node,
#   + viewing
#      encoding => $encoding
###############################################################
sub new {
  my $that = shift;
  my %args = @_;
  my ($root,$label,$mom) = @args{qw(root label mom)};
  delete @args{qw(root label mom)};

  $root = 0 if (!defined($root));
  my $t = $that->SUPER::new(
			    root => $root,
			    labels => {},
			    dtrs=>{},
			    moms=>{$root=>$mom},
			    _ctr=>1,
			    @_
			   );
  $t->{labels}{$root} = $label if (defined($label) && !defined($t->{labels}{$root}));
  return $t;
}

# ???
sub DESTROY {
  my $self = shift;
  #print STDERR __PACKAGE__, "::DESTROY() called.\n";
  $self->clear();
  %$self = ();
}

## $t = $t->clear()
*cleartree = \&clear;
sub clear {
  my $t = shift;
  my $root = 0;
  $t->{root} = $root;
  %{$t->{labels}} = ($root=>'');
  %{$t->{labels}} = ($root=>'');
  %{$t->{moms}}   = ($root=>undef);
  %{$t->{dtrs}}   = ();
  return $t;
}

###############################################################
# Low-level accessors
###############################################################

## $key = $tree->nodeId($node);
##   + gets tree-domain key, returns '' for undefined nodes
##   + probably obsolete
*getKey = *get_key = *key = *id = *nodeid = *nodeKey = \&nodeId;
sub nodeId {
  return defined($_[1]) ? $_[1] : '';
}

## $noderef = $t->nodeDefault($node_or_ref)
##   + destructively alters $node_or_ref to $t->{root} if undefined
sub nodeDefault {
  return
    (defined($_[1]) ? $_[1] : ($_[1]=$_[0]->{root}));
}

###############################################################
# Basic accessors
###############################################################

# $node = $t->rootNode()
*root = \&rootNode;
sub rootNode { return $_[0]->{root}; }

# $lab = $t->label($node)
# $lab = $t->label($node,$lab)
sub label {
  my ($t,$n) = (shift,shift);
  $t->nodeDefault($n);
  $t->{labels}{$n} = shift(@_) if (@_);
  return $t->{labels}{$n};
}

# @nodes = $t->nodes()
#  + returns list of all nodes, unordered
#  + in scalar context returns array-ref
sub nodes {
  return wantarray ? keys(%{$_[0]{moms}}) : [keys(%{$_[0]{moms}})];
}

# $size = $t->size()
sub size { return scalar(keys(%{$_[0]{moms}})); }

###############################################################
# Structural Manipulators
###############################################################

## $dtr = $tree->addDaughter($mom,$label);
##   + add a new child node under $mom
*addChild = *addChildNode = *addDtr = \&addDaughter;
sub addDaughter {
  my ($t,$mom,$lab) = @_;
  return undef if (!defined($t->nodeDefault($mom)));
  my $did = $t->{_ctr}++;
  push(@{$t->{dtrs}{$mom}}, $did);
  $t->{moms}{$did} = $mom;
  $t->{labels}{$did} = $lab if (defined($lab));
  return $did;
}

## @dtrs = $t->addDaughters($mom,@labels)
##   + in scalar context returns array-ref
*addDtrs = *addChildNodes = *addChildren = \&addDaughters;
sub addDaughters {
  return
    (wantarray
     ? (map { $_[0]->addDaughter($_[1],$_) } @_[2..$#_])
     : [(map { $_[0]->addDaughter($_[1],$_) } @_[2..$#_])]);
}


###############################################################
# Structural Accessors: Daughters
###############################################################

# $ndtrs = $t->nDaughters($node)
#  + get number of daughters
*nDtrs = *ndtrs = *nChildren = *nchildren = *nChildNodes = \&nDaughters;
sub nDaughters {
  my ($t,$mom) = @_;
  $t->nodeDefault($mom);
  my $dtrs = $t->{dtrs}{$mom};
  return defined($dtrs) ? scalar(@$dtrs) : 0;
}

# $nth_dtr = $t->nthDaughter($mom,$n)
*daughter = *dtr = *child = *childNode = *nthChild = *nthChildNode = *nthDtr = \&nthDaughter;
sub nthDaughter {
  my ($t,$mom,$n) = @_;
  return undef if (!defined($t->nodeDefault($mom)));
  my $dtrs = $t->{dtrs}{$mom};
  return defined($dtrs) ? $dtrs->[$n] : undef;
}

# @nodes = $t->daughters($mom)
*dtrs = *children = \&daughters;
sub daughters {
  my ($t,$mom) = @_;
  my ($dtrs);
  return qw() if (!defined($t->nodeDefault($mom)) || !defined($dtrs=$t->{dtrs}{$mom}));
  return @$dtrs;
}

###############################################################
# Structural Accessors: Mother
###############################################################

# $mom = $t->mother($node);
*mom = *mother = *parentNode = *parent = \&mother;
sub mother { return $_[0]{moms}{$_[1]}; }


###############################################################
# Readability / Node-key stringification
#  + OBSOLETE
###############################################################

## $str = $tree->key2str($id)
##   + returns human-readable tree-domain key-string
sub key2str { return $_[1]; }

## $id = $tree->str2key($str)
##   + converts human-readable tree-domain key-string to packed key
sub str2key { return $_[1]; }

## $str = $tree->keyString($id)
##   + returns human-readable tree-domain key-string
sub keyString { return $_[1]; }


###############################################################
# Debugging
###############################################################

# $t = $class_or_tree->random($nnodes,%args)
sub random {
  my ($that,$nnodes,%args) = @_;
  my $t = $that->new(label=>'root',%args);
  my @nodes = ($that->root);
  my ($mom,$dtr);
  foreach $i (2..$nnodes) {
    $mom = $nodes[rand(@nodes)];
    push(@nodes, $t->addDtr($mom,''));
  }
  return $t;
}


###############################################################
# Predicates
###############################################################

## $bool = $tree->isLeafNode($node)
##   + returns true if $node has no children
*isLeaf = \&isLeafNode;
sub isLeafNode {
  return $_[0]->ndtrs($_[1]) == 0;
}

## $bool = $tree->isRootNode($node)
##   + returns true if $node's depth is 0
*isRoot = \&isRootNode;
sub isRootNode {
  return !defined($_[0]->mother($_[1]));
}

## $bool = $tree->isInteriorNode($node)
##   + returns true if $node has no children
*isInterior = *isNonterm = *isNonterminal = *isNonterminalNode = \&isInteriorNode;
sub isInteriorNode {
  return $_[0]->ndtrs($_[1]) > 0;
}


## $bool = $tree->isCurrent($node)
#   + returns true if $node key is eq to the key of
*isCurrent = \&isCurrentNode;
sub isCurrentNode {
  my ($t,$n) = @_;
  return undef if (!defined($t->nodeDefault($n)));
  return defined($t->{current}) && $t->{current} eq $n;
}

###############################################################
# Traversal
###############################################################

# $tree->traverse(\%args)
#    + abstract method for tree traversal, based on the example
#      code in the documentation of the Tree::MultiNode module.
#    + \%args is a hashref which may have the following keys:
#        tree    => $tree (caller)
#        sub     => \&sub,
#        after   => \&sub_after
#        node    => $node,
#        depth   => $depth,
#        first   => \&first,
#        last    => \&last,
#        butlast => \&butlast,
#        order   => 'bfs'  | 'dfs',  # traversal order
#   + recursively descends the tree from $node (default is
#     $tree, calling $sub->(\%args) for each node.
#   + $sub_after->(\%args) is called after processing all children
#   + &first(\%args) is called (if specified) for the first of 
#     a node's children, before calling traverse(\%args) for that
#     child.
#   + &last(\%args) is called (if specified) for the last of a 
#     node's children, after having called traverse(\%args) for
#     that child
#   + &butlast(\%args) is called (if specified) for all but the
#     last of a node's children, after having called
#     traverse(\%args) for that child.
#   + The values returned by each call of any of the code
#     references \&sub, \&first, \&last, and \&butlast are.
#     gathered into a list which is returned by traverse
#   + in scalar context, returns array-ref
sub traverse {
  my $tree = shift;
  my $args = ref($_[0]) && ref($_[0]) eq 'HASH' ? shift : {@_};

  ##-- initialization
  $tree->nodeDefault($args->{node});
  $args->{tree} = $tree if (!defined($args->{tree}));
  $args->{order} = 'dfs' if (!defined($args->{order}));
  my (@output,$ndtrs,$i,$node,$act,$dtr,$depth,@dqueue);

  my @queue = ($args->{sub},$args->{node},0);
  while (($act,$node,$depth)=splice(@queue,0,3)) {

    #print STDERR "act=$act ; node=$node ; depth=$depth\n";

    ##-- current node
    @$args{qw(node depth)} = ($node,$depth);
    push(@output, $act->($args)) if (defined($act));
    next if ($act ne $args->{sub});

    ##-- enqueue daughters
    $ndtrs = $tree->ndtrs($node)-1;

    if ($args->{order} eq 'bfs') {
      ##-- breadth-first
      for ($i=0; $i <= $ndtrs; $i++) {
	$dtr = $args->{node} = $tree->{dtrs}{$node}[$i];
	push(@queue,
	     ($i==0 && defined($args->{first}) ? ($args->{first},$dtr,$depth+1) : qw()),
	     ($args->{sub},$dtr,$depth+1),
	     ($i != $ndtrs && defined($args->{butlast}) ? ($args->{butlast},$dtr,$depth+1) : qw()),
	     ($i == $ndtrs && defined($args->{last})    ? ($args->{last},$dtr,$depth+1)    : qw()),
	    );
      }
      $args->{depth} = $depth;
      $args->{node} = $node;
      # post-processing
      push(@queue, $args->{after},$node,$depth) if (defined($args->{after}));
    }
    else {
      ##-- depth-first
      @dqueue = qw();
      for ($i=0; $i <= $ndtrs; $i++) {
	$dtr = $args->{node} = $tree->{dtrs}{$node}[$i];
	push(@dqueue,
	     ($i==0 && defined($args->{first}) ? ($args->{first},$dtr,$depth+1) : qw()),
	     ($args->{sub},$dtr,$depth+1),
	     ($i != $ndtrs && defined($args->{butlast}) ? ($args->{butlast},$dtr,$depth+1) : qw()),
	     ($i == $ndtrs && defined($args->{last})    ? ($args->{last},$dtr,$depth+1)    : qw()),
	    );
      }
      $args->{depth} = $depth;
      $args->{node} = $node;
      # post-processing
      push(@dqueue, $args->{after},$node,$depth) if (defined($args->{after}));
      unshift(@queue, @dqueue);
    }
  }
  return wantarray ? @output : \@output;
}

###############################################################
# Search
###############################################################

# @nodes = $t->descendants($root,\%args)
#  + gets all nodes dominated by $root, in default traversal order
#  + returned value should always include $root itself.
#  + in scalar context, returns array \@nodes
sub descendants {
  my ($t,$n) = (shift,shift);
  $t->nodeDefault($n);
  my @nodes = $t->traverse(node=>$n, sub=>sub { $_[0]{node}; },@_);
  return wantarray ? @nodes : \@nodes;
}

# @nodes = $t->ancestors($node)
#  + gets all nodes properly dominating $node, in frontier-to-root order
#  + in scalar context, returns array \@nodes
sub ancestors {
  my ($t,$n) = @_;
  $t->nodeDefault($n);
  my @nodes = qw();
  my $mom = $n;
  while (defined($mom=$t->mother($mom))) {
    push(@nodes, $mom);
  }
  return wantarray ? @nodes : \@nodes;
}

## @leafnodes = $tree->leaves($node)
## @leafnodes = $tree->leaves()
##   + returns a list leaves dominated by $node,
##   + retuzrns array-ref in scalar context
*leafNodes = \&leaves;
sub leaves {
  my ($t,$node) = (shift,shift);
  $t->nodeDefault($node);
  my @ll = $t->traverse(sub=>sub {
			  return $t->isLeafNode($_[0]{node}) ? $_[0]{node} : qw()
			},
			node=>$node,
			@_);
  return wantarray ? @ll : \@ll;
}

## @leaflabs = $tree->leafLabels($node)
## @leaflabs = $tree->leafLabels()
##   + returns a list leaves dominated by $node,
##   + returns array-ref in scalar context
sub leafLabels {
  my ($t,$node) = (shift,shift);
  $t->nodeDefault($node);
  my @ll =
    (grep { defined($_) && $_ ne '' }
     $t->traverse(
		  sub=>sub {
		         $t->isLeafNode($_[0]{node}) ? $t->label($_[0]{node}) : qw()
		       },
		  node=>$node,
		  @_));
  return wantarray ? @ll : \@ll;
}



###############################################################
# $str = $tree->dump($node)
#   + decomposed version of the example code in the
#     docs for Tree::MultiNode (depth-passing, undef checking)
#   + returns a list of strings which can be passed to "print"
sub dump {
  my ($tree,$node) = @_;
  $tree->nodeDefault($node);
  return join('',
	      $tree->traverse(sub => \&dump_tree_node,
			      node => $node,
			      depth => 0));
}
sub dump_tree_node {
  my $args = shift;
  my $lead = ' ' x ($args->{depth}*2);
  my $label = $args->{tree}{labels}{$args->{node}};
  #my ($key,$val) = $args->{handle}->get_data();
  my @l = ($lead, "label:   ", defined($label) ? $label : "undef", "\n",
	   #$lead, "key:   ", defined($key) ? $key : "undef", "\n",
	   #$lead, "val:   ", defined($val) ? $val : "undef", "\n",
	   $lead, "depth: ", defined($args->{depth}) ? $args->{depth} : "undef", "\n$lead--\n");
  return @l;
}


###############################################################
# $str = $tree->string($string)
# $str = $tree->string()
#   + sets tree structure to reflect that of $string, if specified
#   + otherwise, returns string rep of tree
sub string {
  my $tree = shift;
  return $tree->canonical_string() if (!@_);
  my $str = shift;
  my $str_orig = $str;
  $tree->clear;
  my $node = $tree->{root};
  while ($str) {
    last if ($str =~ /^\s*$/ && $node eq $tree->{root});

    if ($str =~ s/^([^<(,)>]+)//) { # symbol
      $tree->{labels}{$node} = $1;
    }
    elsif ($str =~ s/^\(//) { # first daughter
      $node = $tree->addDtr($node);
    }
    elsif ($str =~ s/^,//) {  # next daughter
      $node = $tree->{moms}{$node} if (defined($tree->{moms}{$node}));
      $node = $tree->addDtr($node);
    }
    elsif ($str =~ s/^\)//) { # last daughter
      $node = $tree->{moms}{$node} if (defined($tree->{moms}{$node}));
    } else {
      warn (__PACKAGE__ , "::string(): cannot interpret tree-string from `$str'\n");
      last;
    }
  }
  return $str_orig;
}

###############################################################
# I/O: Native
###############################################################
# $obj = $obj_or_class->loadNativeFh($fh)
sub loadNativeFh {
  my ($t,$fh) = @_;
  $t = $t->new() if (!ref($t));
  #local $/ = undef;
  my $s = $fh->getline;
  chomp($s);
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
  $tree->nodeDefault($node);
  return join('',$tree->treeAsciiList($node));
}

###############################################################
# $tktree = $tree->toTk(%args)
#  + creates a MUDL::Tk::Tree from $tree
sub toTk {
  require MUDL::Tk::Tree;
  return MUDL::Tk::Tree->new(tree=>$_[0],
			     enum=>$_[0]{enum},
			     (defined($_[0]{encoding}) ? (encoding=>$_[0]{encoding}) : qw()),
			     @_[1..$#_]);
}

# $tktree = $tree->view(%args)
#  + creates a MUDL::Tk::Tree from $tree and views it
sub view {
  return $_[0]->toTk(@_[1..$#_])->view(@_[1..$#_]);
}


###############################################################
# @strings = $tree->treeAsciiList($node)
# @strings = $node->treeAsciiList()
#   + returns a list of strings which can be passed to "print"
#     and which constitute an ASCII representation of $handle,
#     and which bear a remarkable resemblance to a PROLOG term.
###############################################################
sub treeAsciiList {
  my ($tree,$node) = (shift,shift);
  $tree->nodeDefault($node);
  return $tree->traverse({sub => \&_tree_ascii_list_node,
			  first => sub { '(' },
			  last => sub { ')' },
			  butlast => sub { ',' },
			  node => $node,
			  #current => $self->{current},
			  @_,
			 });
}
sub _tree_ascii_list_node {
  my ($args) = @_;
  my $lab = $args->{tree}{labels}{$args->{node}};
  $lab = 'undef' unless (defined($lab));
  if (defined($args->{current}) && $args->{node} eq $args->{current}) {
    return "<$lab>";
  }
  return $lab;
}

###############################################################
# I/O: XML
###############################################################

##-- inherited

# $obj = $obj->HashEntry2XMLNode($key,$val)
sub HashEntry2XMLNode_hmm {
  my ($t,$k,$v) = @_;
  return undef if ($k eq 'labels' || $k eq 'moms');
  return $t->SUPER::HashEntry2XMLNode($k,$v) if ($k ne 'dtrs');

  my ($tn,$xn,$xl,$tdtrs);
  my %xnodes =
    map {
      $xn = XML::LibXML::Element->new('node');
      $xn->appendChild($xl = XML::LibXML::Element->new('label'));
      $xl->appendChild(MUDL::Object::saveXMLNode($t->{labels}{$_}));
      ($_ => $xn)
    } $t->nodes;

  while (($tn,$xn)=each(%xnodes)) {
    next if (!defined($tdtrs=$t->{dtrs}{$tn}));
    $xn->appendChild($xnodes{$_}) foreach (@$tdtrs);
  }

  my $xtnode = XML::LibXML::Element->new('MUDL.Tree.Nodes');
  $xtnode->appendChild($xnodes{$t->{root}});
  return $xtnode;
}

# undef = $obj->xmlNode2HashEntry($xmlnode)
sub xmlNode2HashEntry_hmm {
  my ($t,$node) = @_;
  return $t->SUPER::XMLNode2HashEntry($node) if ($node->nodeName ne 'MUDL.Tree.Nodes');

  my @queue = ($t->{root},$node->firstChild);
  my ($tn,$xn, $xln,$xl, $xd);
  while (($tn,$xn)=splice(@queue,0,2)) {
    $xln = ($xn->getChildrenByTagName('label'))[0];
    $xl = defined($xln) ? MUDL::Object->loadXMLNode($xln->firstChild) : undef;
    $t->label($tn,$xl) if (defined($xl));

    foreach $xd ($xn->getChildrenByTagName('node')) {
      push(@queue, $t->addDaughter($tn,undef), $xd);
    }
  }
  return $t;
}


###############################################################
# Conversion: from cluster (array: OBSOLETE)
###############################################################

# $t = $class_or_obj->fromClusterArray($cluster_tree,%args)
#   + %args:
#      enum=>$enum,
#      dists=>$dists,
#      %other_tree_new_args
sub fromClusterArray {
  my ($t,$ct,%args) = @_;
  $t = $t->new(%args) if (!ref($t));
  my $cdists = $t->{dists};
  $t->{dists} = {};             ##-- maps from (interior) node-ids to distance

  my @queue = ($t->{root},$#$ct);
  $t->label($t->{root},-($#$ct+1));

  my ($tid,$ctid, $ctn,@tdids,$i);
  while (($tid,$ctid)=splice(@queue,0,2)) {
    $ctn = $ct->[$ctid];

    foreach $i (0,1) {
      $tdids[$i] = $t->addDtr($tid,undef);

      if ($ctn->[$i] >= 0) {
	##-- Leaf
	$t->label($tdids[$i], $ctn->[$i]);
      } else {
	##-- Non-Leaf
	$t->label($tdids[$i], -($ctn->[$i]+1));
	push(@queue, $tdids[$i], -($ctn->[$i]+1));
      }
    }

    if (defined($cdists)) {
      $t->{dists}{$tid} = $cdists->[$ctid];
    }
  }

  return $t;
}

###############################################################
# Conversion: from cluster (PDL)
###############################################################

# $t = $class_or_obj->fromClusterPDL($cluster_tree_pdl,%args)
#   + %args:
#      enum=>$enum,
#      dists=>$dists_pdl,
#      %other_tree_new_args

*fromClusters = \&fromClusterPDL;
sub fromClusterPDL {
  my ($t,$ct,%args) = @_;
  $t = $t->new(%args) if (!ref($t));
  my $cdists   = $t->{dists};
  my $cgroups  = $t->{groups};
  $t->{dists}  = {};             ##-- maps from (interior) node-ids to distance
  $t->{groups} = {};             ##-- maps from leaf-ids to group-ids

  my @queue = ($t->{root}, $ct->dim(1)-2);
  $t->label($t->{root}, $ct->dim(1)-1);

  my ($tid,$ctid, $tlabi,$tlabs, $ctn,@tdids,$i);
  while (($tid,$ctid)=splice(@queue,0,2)) {
    $ctn = $ct->slice(",($ctid)");

    foreach $i (0,1) {
      $tdids[$i] = $t->addDtr($tid,undef);

      if ($ctn->at($i) >= 0) {
	##-- Leaf
	#$t->label($tdids[$i], ($tlabi=$ctn->at($i)));
	$tlabi = $ctn->at($i);
	$tlabs  = defined($t->{enum}) ? $t->{enum}->symbol($tlabi) : undef;
	$tlabs  = $tlabi if (!defined($tlabs));
	$t->label($tdids[$i], $tlabs);
	if (defined($cgroups)) {
	  $t->{groups}{$tdids[$i]} = $cgroups->at($tlabi);
	}
      } else {
	##-- Non-Leaf
	#$t->label($tdids[$i], -($ctn->at($i)+1)); #OLD

	$tlabi = -($ctn->at($i)+1);
	$tlabs = $tlabi;
	$t->label($tdids[$i], $tlabs);
	push(@queue, $tdids[$i], $tlabi);
      }
    }

    if (defined($cdists)) {
      $t->{dists}{$tid} = $cdists->at($ctid);
    }
  }

  return $t;
}


###############################################################
# Conversion: to Dendogram
###############################################################
sub toDendogram {
  my $t = shift;
  require MUDL::Tk::Dendogram;
  return MUDL::Tk::Dendogram->new(tree=>$t,
				  (defined($t->{dists}) ? (dists=>$t->{dists}) : qw()),
				  (defined($t->{enum})  ? (enum=>$t->{enum}) : qw()),
				  (defined($t->{dmult}) ? (dmult=>$t->{dmult}) : qw()),
				  @_,
				 );
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

