#-*- Mode: Perl -*-

## File: MUDL::Tk::Dendogram.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Tk Dendograms
##======================================================================

package MUDL::Tk::Dendogram;
use MUDL::Object;
use Tk;

our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $dg = MUDL::Tk::Dendogram->new(%args)
##   + %args:
##      tree=>$tree,   # a MUDL::Tree object
##      ...
##      font=>$tkfont  # for leaves
##      ilfont=>$info_label_font,
##      ivfont=>$info_value_font,
##      xpad=>$pixels
##      ypad=>$pixels
##      dmult=>$distance_multiplier # default: 1
##   + Assumptions:
##     - interior nodes of $tree are integer-labelled
##     - $tree may have {dists} member: used for computing distances
##     - $tree may have {enum} member: used for computing enums
##     - $tree is strictly binary branching
sub new {
  my $that = shift;
  my $self = $that->SUPER::new(
			       tree=>undef,
			       #enum=>undef,
			       #dists=>undef,

			       ##-- Tk options
			       xpad=>5,
			       ypad=>5,
			       dmult=>1,

			       font=>'helvetica -12',
			       ivfont=>'helvetica -12',
			       ilfont=>'helvetica -12 bold',
			       canvasWidth=>800,
			       canvasHeight=>600,

			       ##-- User options
			       @_,
			      );
  $self->{tree} = MUDL::Tree->new() if (!$self->{tree});

  return $self;
}

##======================================================================
## View
## undef = $dg->view(%args)
##  + %args:
##      loop=>$bool,  # do/don't enter main loop
##      file=>$file,  # initial tree file
sub view {
  my ($dg,%args) = @_;

  #--------------------------------------
  # Main Window
  my $w = $dg->{main} = Tk::MainWindow->new();
  $w->title('MUDL::Tk::Dendogram');

  #--------------------------------------
  # Menus
  my $mb = $dg->{menu}{frame} = $w->Frame(-relief=>'raised',-bd=>2,Name=>'menubar');

  #--------------------
  # Menus: File
  my $mbf = $dg->{menu}{file}  = $mb->Menubutton(-text=>'File', Name=>'file', -underline=>0);
  $mbf->pack(-side=>'left');

  $mbf->menu->add('command',
		  -label=>'Open',
		  -accelerator=>'Ctrl+o',
		  -command=>sub {$dg->ddg_menu_open});
  $w->bind('all', '<Control-KeyPress-o>', sub { $dg->ddg_menu_open });

  $mbf->menu->add('command',
		  -label=>'Save',
		  -accelerator=>'Ctrl+s',
		  -command=>sub {$dg->ddg_menu_save});
  $w->bind('all', '<Control-KeyPress-s>', sub { $dg->ddg_menu_save });

  $mbf->menu->add('separator');

  $mbf->menu->add('command',
		  -label=>'Clear',
		  -accelerator=>'Ctrl+c',
		  -command=>sub {$dg->ddg_menu_clear});
  $w->bind('all', '<Control-KeyPress-c>', sub { $dg->ddg_menu_clear });


  $mbf->menu->add('separator');

  $mbf->menu->add('command',
		  -label=>'Export Postscript (all)',
		  -accelerator=>'Ctrl+x',
		  -command=>sub {$dg->ddg_export_ps(tag=>'all')});
  $w->bind('all', '<Control-KeyPress-x>', sub { $dg->ddg_export_ps(tag=>'all') });

  $mbf->menu->add('command',
		  -label=>'Export Postscript (selection)',
		  -accelerator=>'Ctrl+e',
		  -command=>sub {$dg->ddg_export_ps(tag=>'selected')});
  $w->bind('all', '<Control-KeyPress-e>', sub { $dg->ddg_export_ps(tag=>'selected') });

  $mbf->menu->add('separator');
  $mbf->menu->add('command',
		  -label=>'Quit',
		  -accelerator=>'Ctrl+q',
		  -command=>sub {$w->destroy});

  #--------------------
  # Menus: Window
  my $mbw = $dg->{menu}{window}  = $mb->Menubutton(-text=>'Window', Name=>'window', -underline=>0);
  $mbw->pack(-side=>'left');

  $mbw->menu->add('checkbutton',
		  -label=>'Info',
		  -accelerator=>'Ctrl+i',
		  -variable=>\$dg->{info}{state},
		  -command=>sub{$dg->ddg_menu_showinfo},
		 );
  $w->bind('all', '<Control-KeyPress-i>', sub { $dg->ddg_menu_showinfo });


  #--------------------
  # Menus: Top, Bindings
  $mb->pack(-side=>'top', -fill=>'x');
  $w->bind('all', '<KeyPress-q>', sub { $w->destroy });


  #--------------------------------------
  # Info Frame
  my $iw = $dg->{info}{top} = $w->Toplevel(Name=>'infobox');
  $iw->title('MUDL::Tk::Dendogram Info');

  ##-- infopair($label,\$variable)
  my $iwrow = 0;
  my $iwlf = $iw->Frame();
  my $iwpair = sub {
    my ($label,$varref) = @_;
    my $pl = $iwlf->Label(-text=>$label,-font=>$dg->{ilfont}, -justify=>'right', -anchor=>'e');
    my $pv = $iwlf->Label(-textvariable=>$varref,-font=>$dg->{ivfont}, -justify=>'left', -anchor=>'w');
    $pl->grid(-row=>$iwrow,-column=>0,-sticky=>'nsew');
    $pv->grid(-row=>$iwrow,-column=>1,-sticky=>'nsew');
    ++$iwrow;
  };
  $iwpair->('Node Id:', \$dg->{info}{nodeid});
  $iwpair->('Node Distance:', \$dg->{info}{nodedist});
  $iwpair->('Log Distance:', \$dg->{info}{logdist});


  my $iwdismiss = $iw->Button(Name=>'dismiss',-text=>'Dismiss',-command=>[sub { $iw->withdraw(); }]);
  $iwlf->pack(-side=>'top',-fill=>'both',-expand=>1);
  $iwdismiss->pack(-side=>'bottom',fill=>'x');
  #$iwdismiss->grid(-row=>$iwrow,-column=>0,-columnspan=>2,-sticky=>'nsew');

  $iw->withdraw();

  #--------------------------------------
  # Canvas
  my $c = $dg->{canvas} = $w->Scrolled('Canvas',
				       -background => 'white',
				       -cursor => 'crosshair',
				       -borderwidth => 2,
				       -scrollbars => 'se',
				       -confine=>1,
				       -width=>$dg->{canvasWidth},
				       -height=>$dg->{canvasHeight},
				      );
  $c->pack(qw(-fill both expand 1));

  #--------------------------------------
  # Bindings
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

  $c->CanvasBind("<ButtonPress-1>", [\&ddg_canvas_select,
				     $dg,
				     Ev('x'), Ev('y'),
				    ]);

  $c->CanvasBind("<ButtonPress-3>", [\&ddg_show_info,
				     $dg,
				     Ev('x'), Ev('y'),
				    ]);

  #$c->CanvasBind("<Button-1>", [\&ddg_canvas_center, Ev('x'), Ev('y')] );


  #--------------------------------------
  # Guts
  $dg->loadTreeFile($args{file},tocanvas=>0) if (defined($args{file}));
  $dg->toCanvas($c,%args);
  Tk::MainLoop if (!defined($args{loop}) || $args{loop});
}

##======================================================================
## toCanvas

# undef = $dg->toCanvas($c);
sub toCanvas {
  my ($dg,$c,%args) = @_;

  $dg->{cid2nid} = {}; ##-- maps canvas to node-ids
  $dg->{tree}->traverse({dg=>$dg,
			 canvas=>$c,
			 sub=>\&_ddg_do_node,
			 after=>\&_ddg_on_up,
			 info=>{},
			 texty=>0,
			 ancestors=>[],
			 tags=>[],
			});

  ##-- recenter
  my @bbox = $c->bbox('all');
  $c->configure(-scrollregion => [
				  $bbox[0]-$dg->{xpad},
				  $bbox[1]-$dg->{ypad},
				  $bbox[2]+$dg->{xpad},
				  $bbox[3]+$dg->{ypad},
				 ]);
}

sub _ddg_do_node {
  my ($args) = @_;
  push(@{$args->{ancestors}}, 'dn'.$args->{node});

  # interior nodes are handled by dtk_on_up
  return qw() if (!$args->{tree}->isLeafNode($args->{node}));

  _ddg_draw_node($args);
}


sub _ddg_draw_node {
  my ($args) = @_;
  my ($dg,$tree,$node,$canvas) = @$args{qw(dg tree node canvas)};
  my $keystr = $node;

  my ($lab,$labtxt,$cid);

  if ($tree->isLeafNode($node)) {
    ##--------------------------
    ## Leaf Drawing
    $labtxt = $lab = $tree->label($node);
    $labtxt = $tree->{enum}->symbol($lab) if (defined($tree->{enum}));
    $labtxt = $lab if (!defined($labtxt));
    $labtxt = $keystr if (!defined($labtxt));

    #print STDERR "LEAF: node=$node ; lab=$lab ; keystr=$keystr\n";
    #print STDERR "    : tags=n$keystr", @{$args->{ancestors}}, @{$args->{tags}}, "\n";

    $cid = $canvas->createText(0, $args->{texty},
			       -tags => ['node',
					 ($tree->isLeafNode($node) ? 'leaf' : qw()),
					 ('n'.$keystr),
					 @{$args->{ancestors}},
					 @{$args->{tags}},
					],
			       -justify => 'left',
			       -anchor => 'w',
			       -text => $labtxt,
			       -font => $args->{dg}{font} || '',
			      );
    $args->{texty} = $dg->{ypad} + ($canvas->bbox($cid))[3];
    $args->{dg}{cid2nid}{$cid} = $node;
  }
  else {
    ##--------------------------
    ## Interior Node Drawing

    ##-- get label
    $lab  = $tree->label($node);
    my $di = $lab;         ##-- interior nodes assumed integer-labelled!

    my @dtrs  = $tree->daughters($node);
    my @sdtrs = map { 'n'.$_ } @dtrs;

    ##-- get daughter coords
    my ($x0,$y0) = $canvas->coords($sdtrs[0]);
    my ($x1,$y1) = $canvas->coords($sdtrs[1]);

    #print STDERR "NONTERM: node=$node ; lab=$lab ; keystr=$keystr\n";
    #print STDERR "       : sdtrs=", @sdtrs, "\n";
    #print STDERR "       : (x0,y0)=($x0,$y0) ; (x1,y1)=($x1,$y1)\n";
    #print STDERR "       : tags=n$keystr", @{$args->{ancestors}}, @{$args->{tags}}, "\n";

    ##-- get distance
    my $dist = defined($tree->{dists}) && defined($tree->{dists}{$node}) ? $tree->{dists}{$node} : 0;

    ##-----------------------
    ## new node
    my $y  = ($y0 + $y1) / 2;
    my $x  = ($x0 < $x1 ? $x0 : $x1) - $dg->{xpad} - ($dg->{dmult}*$dist);

    $cid = $canvas->createText($x, $y,
			       -anchor=>'c',
			       -justify=>'c',
			       -text=>('n'.$keystr),
			       -state=>'hidden',
			       -fill=>'red',
			       -tags=>['node',
				       ('n'.$keystr),
				       @{$args->{ancestors}},
				       @{$args->{tags}}]);
    $args->{dg}{cid2nid}{$cid} = $node;

    $cid = $canvas->createLine($x0, $y0,
			       $x,  $y0,
			       #$x,  $y,
			       $x,  $y1,
			       $x1, $y1,
			       -tags=>['line',
				       ('n'.$keystr),
				       @{$args->{ancestors}},
				       @{$args->{tags}}]);
    $args->{dg}{cid2nid}{$cid} = $node;
  }

  return qw();
}


sub _ddg_on_up {
  my $args = shift;
  pop(@{$args->{ancestors}});
  return qw() if ($args->{tree}->isLeafNode($args->{node}));
  _ddg_draw_node($args);
}


##======================================================================
## Centering
sub ddg_canvas_center {
  my ($canvas,$sx,$sy) = @_;
  my ($centerx, $centery) = ( $canvas->width() / 2.0, $canvas->height() / 2.0 );
  $canvas->scan('mark', $centerx, $centery);
  $canvas->scan('dragto',
		$centerx + (($centerx - $sx) / 10.0),
		$centery + (($centery - $sy) / 10.0));
}


##======================================================================
## Selection
sub ddg_canvas_select {
  my ($canvas,$dg,$sx,$sy) = @_;
  my ($cx,$cy) = ($canvas->canvasx($sx), $canvas->canvasy($sy));
  #print "select: canvas=$canvas ; args=@_\n";

  my @current = $canvas->find(withtag=>'current');
  #print "current=@current\n";

  ##-- delete old selection
  $canvas->itemconfigure('selected', -fill=>'black');
  $canvas->delete('selbox');
  $canvas->dtag('selected');

  ##-- add new selection
  #print "selected=", join(' ', $canvas->find(withtag=>'selected')), "\n";
  foreach (@current) {
    #print
    #  ("current=$_ ; tags=", join(' ', $canvas->itemcget($_, '-tags')),
    #   "; id=", $dg->{tree}->key2str($dg->{cid2nid}{$_}), "\n");
    $canvas->addtag('selected', withtag=>('dn'.$dg->{cid2nid}{$_}));
  }
  $canvas->addtag('selected',withtag=>'current');

  ##-- highlight in blue
  #$canvas->itemconfigure('selected', -fill=>'blue');

  ##-- box it
  my @bbox = $canvas->bbox('selected');
  if (@bbox) {
    #print "bbox=@bbox\n";
    $canvas->createRectangle(@bbox, -outline=>'blue', -tags=>['selbox']);
  }

  ##-- set info variables
  my ($cid,$nodeid);
  if (defined($cid=$current[0])) {
    $nodeid = $dg->{info}{nodeid} = $dg->{cid2nid}{$cid};

    my $distr = \$dg->{info}{nodedist};
    $$distr = $dg->{tree}{dists}{$nodeid};
    if (defined($$distr)) {
      $dg->{info}{logdist} = log($$distr);
      $$distr = sprintf("%e", $$distr);
    } else {
      $dg->{info}{nodedist} = $dg->{info}{logdist} = '(undef)';
    }
  }

  return;
}

##======================================================================
## Selection
sub ddg_show_info {
  my ($canvas,$dg,$sx,$sy) = @_;
  ddg_canvas_select(@_);

  ##-- get info
  ## ??

  ##-- show info window
  $dg->{info}{top}->deiconify;
  return;
}



##======================================================================
## Tk: open
sub ddg_menu_open {
  my $dg = shift;
  my $file = $dg->{main}->getOpenFile(
				      -title=>'Open Tree File',
				      -defaultextension=>'.xml',
				      -filetypes=>
				      [
				       ['ASCII Tree Files',  '.tree'],
				       ['XML Files',         '.xml'],
				       ['Binary Files',      '.bin'],
				       ['All Files',         '*'],
				      ]
				     );
  return if (!defined($file));
  $dg->loadTreeFile($file);
}

##======================================================================
## undef = $dg->loadtreeFile($filename,%args)
##   + loads and displays $filename
##   + %args
##      tocanvas->$bool
sub loadTreeFile {
  my ($dg,$file,%args) = @_;
  $args{tocanvas} = 1 if (!defined($args{tocanvas}));

  $dg->{tree}->clear;
  $dg->{tree} = $dg->{tree}->loadFile($file)
    or warn( __PACKAGE__ , ": open failed for file '$file': $!");

  if ($args{tocanvas}) {
    $dg->view(loop=>0) if (!defined($dg->{canvas}));
    $dg->{canvas}->delete('all');
    $dg->toCanvas($dg->{canvas})
  }
}


##======================================================================
## Tk: save
sub ddg_menu_save {
  my $dg = shift;
  my $file = $dg->{main}->getSaveFile(
				      -title=>'Save Tree File',
				      -defaultextension=>'.xml',
				      -filetypes=>
				      [
				       ['ASCII Tree Files',  '.tree'],
				       ['XML Files',         '.xml'],
				       ['Binary Files',      '.bin'],
				       ['All Files',         '*'],
				      ]);
  return if (!defined($file));

  $dg->{tree}->saveFile($file)
    or warn( __PACKAGE__ , ": save failed for file '$file': $!");
}

##======================================================================
## Tk: clear
sub ddg_menu_clear {
  my $dg = shift;
  $dg->{canvas}->delete('all');
  %{$dg->{cid2nid}} = qw();
}


##======================================================================
## Tk: show info
sub ddg_menu_showinfo {
  my $dg = shift;
  if ($dg->{info}{state}) {
    $dg->{info}{top}->deiconify();
  } else {
    $dg->{info}{top}->withdraw();
  }
};

##======================================================================
## Tk: export
sub ddg_export_ps {
  my ($dg,%args) = @_;
  my $file = $dg->{main}->getSaveFile(-title=>'Export PostScript file',
				      -defaultextension=>'.ps',
				      -filetypes=>
				      [
				       ['PostScript Files',  ['.ps', '.eps']],
				       ['All Files',         '*'],
				      ]
				     );
  return if (!defined($file));

  $dg->savePs($file,%args);
}



##======================================================================
## Tk: export
## undef = $dg->savePs($filename, tag=>$which);
sub savePs {
  my ($dg,$file,%args) = @_;

  $dg->view(loop=>0) if (!defined($dg->{canvas}));

  $args{tag} = 'all' if (!defined($args{tag}));
  my @bb = $dg->{canvas}->bbox($args{tag});
  if (!@bb) {
    warn( __PACKAGE__ , ": no nodes to save!");
    return undef;
  }
  $dg->{canvas}->itemconfigure('selbox',-state=>'hidden');

  $dg->{canvas}->update();
  $dg->{canvas}->postscript(-file=>$file,
			    -height=>($bb[3]-$bb[1]),
			    -width=>($bb[2]-$bb[0]),
			    '-x'=>$bb[0],
			    '-y'=>$bb[1],
			   );

  $dg->{canvas}->itemconfigure('selbox',-state=>'normal');
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
