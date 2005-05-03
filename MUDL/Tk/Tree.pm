##-*- Mode: Perl -*-

## File: MUDL::Tk::Tree.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Tk trees
##======================================================================

package MUDL::Tk::Tree;
use MUDL::Object;
use Tk;
use utf8;

our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $tkt = MUDL::Tk::Tree->new(%args)
##   + %args:
##      tree=>$tree,   # a MUDL::Tree object
##      enum=>$enum,   # for leaf enumeration
##      ...
##      font=>$tkfont,  # for node labels
##      xskip=>$pixels,
##      yskip=>$pixels,
##      boxactive=>$bool,
##      node=>$root_node,
##      linepad=>$pixels,
##  + Description:
#       xskip     : min horizontal space (pixels) between nodes
#       yskip     : vertical space (pixels) between nodes
#       linepad   : vertical space between node-center and edges
#       font      : text font
#       boxactive : boolean - whether to box the current handle
sub new {
  my $that = shift;
  my $self = $that->SUPER::new(
			       tree=>undef,
			       enum=>undef,

			       canvasWidth => 640,
			       canvasHeight => 480,
			       xskip=>15,
			       yskip=>35,
			       linepad=>2,
			       font=>'helvetica -12 bold',
			       boxactive=>1,
			       ##-- User options
			       @_,
			      );
  return $self;
}

##======================================================================
## view
## undef = $t->view(%args)
##   + %args:
##       loop => $bool, # do/don't enter main loop (default=do)
sub view {
  my ($tkt,%args) = @_;
  @$tkt{keys(%args)} = values(%args);

  my $w = Tk::MainWindow->new();
  $w->title('MUDL::Tk::Tree');
  $w->bind("<KeyPress-q>", sub { $w->destroy });

  my $c = $tkt->{canvas} =
    $w->Scrolled('Canvas',
		       -background => 'white',
		       -cursor => 'crosshair',
		       -borderwidth => 2,
		       -scrollbars => 'se',
		       -confine=>1,
		       -width=>$tkt->{canvasWidth},
		       -height=>$tkt->{canvasHeight},
		      );
  $c->pack(qw(-fill both expand 1));

  $c->CanvasBind("<ButtonPress-2>",
		 [sub {
		    $_[0]->configure(-cursor=>'target');
		    $_[0]->scanMark($_[1],$_[2]);
		  },
		  Ev('x'), Ev('y')]);

  $c->CanvasBind("<Button2-Motion>",  [$c, 'scan', 'dragto', Ev('x'), Ev('y')] );

  $c->CanvasBind("<ButtonRelease-2>",
		 [sub {
		    $_[0]->configure(-cursor=>'crosshair')
		  }
		 ]);
  #$c->CanvasBind("<Button-1>", [\&dendogram_canvas_center, Ev('x'), Ev('y')] );



  $tkt->toCanvas($c);
  Tk::MainLoop if (!defined($args{loop}) || $args{loop});
}


#======================================================================
# toCanvas
# $t = $t->toCanvas($canvas,%args)
#   + %args keys: see $tree
#   + draws the the parse tree rooted at $handle 
#     on a Tk::Canvas object $canvas
sub toCanvas {
  my ($tkt,$canvas,%args) = @_;
  unless (defined($canvas)) {
    warn(__PACKAGE__ , "::toCanvas(): no canvas object!\n");
    return undef;
  }
  @$tkt{keys(%args)} = values(%args);

  my $pred = undef;
  $tkt->{tree}->traverse({tkt => $tkt,
			  canvas=> $canvas,
			  sub => \&dtk_do_node,
			  after => \&dtk_on_up,
			  nodeinfo => {},
			  ancestors => [],
			  pred => \$pred,
			  tags => ["$tkt->{tree}"],
			 });


  my @bbox = $canvas->bbox("$tkt->{tree}");
  $canvas->configure(-scrollregion => [
				       $bbox[0]-$tkt->{xskip},
				       $bbox[1]-$tkt->{yskip},
				       $bbox[2]+$tkt->{xskip},
				       $bbox[3]+$tkt->{yskip},
				      ]);

  return $tkt;
}


#======================================================================
# Drawing guts
sub dtk_do_node {
  my ($args) = @_;

  #print STDERR "dtk_do_node_called: args=", map { "'$_'" } @_, "\n"; ##-- DEBUG

  $args->{nodeinfo}{$args->{node}} =
    {
     key => $args->{node},
     str => ('n'.$args->{node}),
     label => $args->{tree}->label($args->{node}),
     depth => $args->{depth},
     ancestors => [ @{$args->{ancestors}} ],
     pred => defined($args->{pred}) ? ${$args->{pred}} : undef,
    };
  push(@{$args->{ancestors}}, "d".$args->{nodeinfo}{$args->{node}}{str});
  # interior nodes are handled by dtk_on_up
  return qw() if (scalar($args->{tree}->children($args->{node})));
  dtk_draw_node($args);
}

sub dtk_on_up { 
  my ($args) = @_;
  ${$args->{pred}} = $args->{node};
  pop(@{$args->{ancestors}});
  return qw() if ($args->{tree}->isLeafNode($args->{node}));
  dtk_draw_node(@_);
}

sub dtk_draw_node {
  my ($args) = @_;
  my $nodei = $args->{nodeinfo}->{$args->{node}};
  my $canvas = $args->{canvas};
  my $keystr = 'n'.$args->{node};
  my $id = $canvas->createText(0, 0, 
			       -tags => ['node',
					 $keystr,
					 @{$nodei->{ancestors}},
					 @{$args->{tags}},
					],
			       -justify => 'center',
			       -anchor => 'c',
			       -text => $nodei->{label},
			       -font => $args->{tkt}{font} || '',
			      );
  $nodei->{id} = $id;

  # --- do the node placement ---
  # - preliminary node placement
  if (scalar($args->{tree}->children($args->{node}))) {
    # nonterminal node
    my @kbbox = $canvas->bbox("d" . $keystr);
    $canvas->coords($id, ($kbbox[2] + $kbbox[0]) / 2, 0);
  }

  # - make sure it was ok -- catch terminals, too
  my @bbox = $canvas->bbox($keystr, "d".$keystr);

  my ($xborder);
  my @pbbox = (defined($nodei->{pred})
	       ? $canvas->bbox( 'n'.$nodei->{pred},
			       'dn'.$nodei->{pred})
	       : qw());

  if (@pbbox) {
    $xborder = $pbbox[2] + $args->{tkt}{xskip};
    #print STDERR 
    #  "Huh?? node->value: $node->{value}, pred: $node->{pred}, xskip: $args->{xskip}, pbbox[2] = $pbbox[2]\n";
  } else {
    $xborder = $args->{tkt}{xskip};
  }

  # x placement/adjustment
  if ($bbox[0] < $xborder) {
    # houston, we have a problem
    $canvas->move($keystr, $xborder - $bbox[0], 0);
    $canvas->move("d".$keystr, $xborder - $bbox[0], 0);
  }

  # y placement/adjustment
  $canvas->move($nodei->{id}, 0, $args->{tkt}{yskip} + ($nodei->{depth} * $args->{tkt}{yskip}));

  # --- final height & width calculation ---
  @bbox = $canvas->bbox($keystr);
  $nodei->{height} = $bbox[3] - $bbox[1];
  @bbox = $canvas->bbox($keystr, "d".$keystr);
  $nodei->{width} = $bbox[2] - $bbox[0];

  # --- do box ? ---
  # include box ?
  if (defined($args->{tkt}{boxactive}) && defined($args->{current}) &&
      $args->{tkt}{boxactive} && $args->{node} eq $args->{current}) {
    $canvas->createRectangle($canvas->bbox($id),
			     -tags => [$keystr,
				       @{$nodei->{ancestors}},
				       @{$args->{tags}}]);
  }

  # --- connect the daughters ---
  my @dtrs = $args->{tree}->children($args->{node});
  my ($dtrkey,$dtri,$dx,$dy,$mx,$my);
  foreach $dtrkey (@dtrs) {
    $dtri = $args->{nodeinfo}{$dtrkey};
    ($dx,$dy) = $canvas->coords($dtri->{id});
    ($mx,$my) = $canvas->coords($nodei->{id});
    $canvas->createLine($mx,
			$my + $args->{tkt}{linepad} + ($nodei->{height} / 2),
			$dx,
			$dy - ($args->{tkt}{linepad} + ($dtri->{height} / 2)),
			-tags => ["d".$keystr, @{$nodei->{ancestors}}, @{$args->{tags}}]);
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

Bryan Jurish E<lt>jurish@ling.uni-potsdam.deE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
