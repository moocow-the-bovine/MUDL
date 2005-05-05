##-*- Mode: Perl -*-

## File: MUDL::Tk::Dendogram.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Tk Dendograms
##======================================================================

package MUDL::Tk::Dendogram;
use MUDL::Object;
use Tk;
use Tk::ROText;
use Encode;
use Text::Wrap;
use PDL;

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
##      gcolor=>$groupHlColor,
##      gdepth=>$groupDepth,
##   + additional args
##      encoding=>$display_label_encoding,  # default: ISO-8859-1
##   + Assumptions:
##     - interior nodes of $tree are integer-labelled
##     - $tree may have {dists} member: used for computing distances: { $nonterm_nodid => $distance_at_nodid, ... }
##     - $tree may have {enum} member: used for computing enums --- OBSOLETE!
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

			       gcolor=>'blue',
			       gdepth=>50,
			       gxpad=>2,
			       gypad=>2,

			       ##-- encoding
			       encoding=>'ISO-8859-1',

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
  $w->title(ref($dg));

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
  # Menus: Options
  my $mbo = $dg->{menu}{options}  = $mb->Menubutton(-text=>'Options', Name=>'options', -underline=>0);
  $mbo->pack(-side=>'left');

  #$mbo->menu->add('checkbutton',
	#	  -label=>'Show Node Details',
	#	  -accelerator=>'Ctrl+i',
	#	  -variable=>\$dg->{info}{state},
	#	  -command=>sub{$dg->ddg_menu_showinfo},
	#	 );

  $mbo->menu->add('checkbutton',
		  -label=>'Show Leaf Groups',
		  -accelerator=>'Ctrl+g',
		  -variable=>\$dg->{info}{gstate},
		  -command=>sub{$dg->ddg_menu_showgroups},
		 );

  $mbo->menu->add('checkbutton',
		  -label=>'Regex Search Mode',
		  -accelerator=>'Ctrl+r',
		  -variable=>\$dg->{info}{searchregex},
		 );


  $w->bind('all', '<Control-KeyPress-g>',
	   sub { $dg->{info}{gstate} = !$dg->{info}{gstate}; $dg->ddg_menu_showgroups });
  $w->bind('all', '<Control-KeyPress-r>',
	   sub { $dg->{info}{searchregex} = !$dg->{info}{searchregex} });

  #--------------------
  # Menus: Top, Bindings
  $mb->pack(-side=>'top', -fill=>'x');
  $w->bind('all', '<KeyPress-q>', sub { $w->destroy });


  #--------------------------------------
  # Info Panel / info frame
  my $iw = $dg->{info}{top} = $w->Frame(Name=>'infobox',relief=>'groove',-bd=>2);

  ##-- infopair($label,\$variable)
  my $iwrow = 0;
  my $iwcf = $iw->Frame(-relief=>'groove',-bd=>2);
  my $iwpair = sub {
    my ($parent,$label,$varref,$width) = @_;
    $width = 20 if (!defined($width));
    my $pl = $parent->Label(-text=>$label,-font=>$dg->{ilfont}, -justify=>'right', -anchor=>'ne');
    my $pv = $parent->Label(-textvariable=>$varref,-font=>$dg->{ivfont}, -justify=>'left', -anchor=>'nw',-width=>$width);
    $pl->grid(-row=>$iwrow,-column=>0,-sticky=>'nsew');
    $pv->grid(-row=>$iwrow,-column=>1,-sticky=>'nsew');
    ++$iwrow;
  };
  $iwpair->($iwcf, 'Max Group Size:', \$dg->{info}{maxgsize},5);
  $iwpair->($iwcf, 'Avg Group Size (NS):', \$dg->{info}{avggsize},5);

  $iwrow = 0;
  my $iwlf = $iw->Frame(-relief=>'groove',-bd=>2);
  $iwpair->($iwlf, 'Node Id:', \$dg->{info}{nodeid});
  $iwpair->($iwlf, 'Group Id:', \$dg->{info}{groupid});
  $iwpair->($iwlf, 'Node Distance:', \$dg->{info}{nodedist});
  $iwpair->($iwlf, 'Log Distance:', \$dg->{info}{logdist});
  $iwpair->($iwlf, 'Node Size:', \$dg->{info}{nodesize});

  ##-- search-entry for leaves
  my $iwsf = $iw->Frame(-relief=>'groove',-bd=>2,-height=>'10p');
  my $pl = $iwsf->Label(-text=>'Search:',-font=>$dg->{ilfont}, -justify=>'right', -anchor=>'ne');
  my $st = $dg->{info}{searchentry} = $iwsf->Entry(-font=>$dg->{ivfont},-width=>20,
						   -textvariable=>\$dg->{info}{searchtext});
  $st->bind("<KeyPress-Return>", sub { $dg->ddg_search });
  $st->bindtags([grep { $_ ne 'all' } $st->bindtags]);
  $pl->pack(-side=>'left');
  $st->pack(-side=>'right',-fill=>'x',-expand=>1);

  my $iwtf = $iw->Frame(-bd=>2);
  my $istpair = sub {
    my ($label,$tref,$width,$height) = @_;
    $width = 20 if (!defined($width));
    $height = 5 if (!defined($height));
    my $pf = $iwtf->Frame(-relief=>'groove',-bd=>2);
    my $pl = $pf->Label(-text=>$label,-font=>$dg->{ilfont}, -justify=>'center', -anchor=>'c');
    my $pv = $pf->Scrolled('Text', -font=>$dg->{ivfont}, -wrap=>'word', -width=>$width,-height=>$height);
    $pl->pack(-side=>'top',-fill=>'x');
    $pv->pack(-side=>'top',-fill=>'both',-expand=>1);
    $$tref = $pv;
    return $pf;
  };
  $istpair->('Node Tags:', \$dg->{info}{tagstext},20,5)->pack(-side=>'top',-fill=>'x'); #\$dg->{info}{nodetags}
  $istpair->('Leaves:', \$dg->{info}{leavestext}, 20,10)->pack(-side=>'top',-fill=>'both',-expand=>1);


  #my $iwdismiss = $iw->Button(Name=>'dismiss',
	#		      -text=>'Dismiss',
	#		      -command=>[sub { $iw->withdraw();$dg->{info}{state}=0; }]);
  $iwcf->pack(-side=>'top',-fill=>'x',-expand=>0);
  $iwlf->pack(-side=>'top',-fill=>'x',-expand=>0);
  $iwsf->pack(-side=>'top',-fill=>'both',-expand=>0);
  $iwtf->pack(-side=>'top',-fill=>'both',-expand=>1);
  #$iwdismiss->pack(-side=>'bottom',fill=>'x');
  #$iw->withdraw();

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
  $iw->pack(-side=>'left',-fill=>'y',-expand=>0,ipadx=>5,ipady=>5);
  $c->pack(-side=>'left',-fill=>'both',expand=>1,ipadx=>5,ipady=>5);

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

  $c->CanvasBind("<ButtonPress-1>", sub { $dg->ddg_canvas_select('current') });
				     #$dg,
				     #'current'
				     #Ev('x'), Ev('y'),
				    #]);

  $c->CanvasBind("<ButtonPress-3>", [\&ddg_display_info,
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
  $dg->{tree}{groups}  = { map { $_=>0 } ($dg->{tree}->leaves) }
    if (!defined($dg->{tree}{groups}) || !%{$dg->{tree}{groups}});
  $dg->{tree}->traverse({dg=>$dg,
			 encoding=>$dg->{encoding},
			 canvas=>$c,
			 sub=>\&_ddg_do_node,
			 after=>\&_ddg_on_up,
			 info=>{},
			 texty=>0,
			 ancestors=>[],
			 tags=>[],
			});

  ##-- groups?
  $dg->ddg_menu_showgroups if ($dg->{info}{gstate});
  $dg->ddg_menu_showinfo if ($dg->{info}{state});

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
    #$labtxt = $tree->{enum}->symbol($lab) if (defined($tree->{enum}));
    #$labtxt = $lab if (!defined($labtxt));
    $labtxt = "key:".$keystr if (!defined($labtxt));

    ##-- encode
    $labtxt = Encode::encode($args->{encoding},$labtxt) if (defined($args->{encoding}));
    $labtxt = wrap('',"\t",$labtxt);

    #print STDERR "LEAF: node=$node ; lab=$lab ; keystr=$keystr ; labtxt='$labtxt'\n";
    #print STDERR "    : tags=n$keystr", @{$args->{ancestors}}, @{$args->{tags}}, "\n";

    $cid = $canvas->createText(0, $args->{texty},
			       -tags => ['node', 'leaf',
					 ('n'.$keystr),
					 ('al'.$keystr),
					 'g'.(defined($tree->{groups}{$keystr}) ? $tree->{groups}{$keystr} : '(none)'),
					 @{$args->{ancestors}},
					 @{$args->{tags}},
					],
			       -justify => 'left',
			       -anchor => 'w',
			       -text => $labtxt,
			       -font => ($args->{dg}{font} || ''),
			      );
    $args->{texty} = $dg->{ypad} + ($canvas->bbox($cid))[3];
    $args->{dg}{cid2nid}{$cid} = $node;

    ##-- add 'leaves' nodeinfo
    my ($ancid);
    foreach (@{$args->{ancestors}}) {
      (my $ancid=$_) =~ s/^dn//;
      push(@{$args->{nodeinfo}{$ancid}{leaves}}, ('al'.$keystr));
    }
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
    my @dcoords = map { [$canvas->coords($_)] } @sdtrs;
    #my ($x0,$y0) = $canvas->coords($sdtrs[0]);
    #my ($x1,$y1) = $canvas->coords($sdtrs[1]);

    ##print STDERR "NONTERM: node=$node ; lab=$lab ; keystr=$keystr\n";
    ##print STDERR "       : sdtrs=", @sdtrs, "\n";
    ##print STDERR "       : (x0,y0)=($x0,$y0) ; (x1,y1)=($x1,$y1)\n";
    ##print STDERR "       : tags=n$keystr", @{$args->{ancestors}}, @{$args->{tags}}, "\n";

    ##-- get distance
    my $dist = defined($tree->{dists}) && defined($tree->{dists}{$node}) ? $tree->{dists}{$node} : 0;

    ##-----------------------
    ## new node
    my $dcpdl  = pdl(\@dcoords);
    my $dcpdlx = $dcpdl->slice("0");
    my $dcpdly = $dcpdl->slice("1");
    my $y      = $dcpdly->avg;
    my $x      = $dcpdlx->min - $dg->{xpad} - ($dg->{dmult}*$dist);

    ##--OLD
    #my $y  = ($y0 + $y1) / 2;
    #my $x  = ($x0 < $x1 ? $x0 : $x1) - $dg->{xpad} - ($dg->{dmult}*$dist);

    $cid = $canvas->createText($x, $y,
			       -anchor=>'c',
			       -justify=>'c',
			       -text=>('n'.$keystr),
			       -state=>'hidden',
			       -fill=>'red',
			       -tags=>['node',
				       ('n'.$keystr),
				       @{$args->{ancestors}},
				       @{$args->{nodeinfo}{$keystr}{leaves}},
				       @{$args->{tags}}]);
    $args->{dg}{cid2nid}{$cid} = $node;

    ##-- crossbar
    $cid = $canvas->createLine($x, $dcpdly->max,
			       $x, $dcpdly->min,
			       -tags=>['line',
				       ('n'.$keystr),
				       @{$args->{ancestors}},
				       @{$args->{nodeinfo}{$keystr}{leaves}},
				       @{$args->{tags}}]);
    $args->{dg}{cid2nid}{$cid} = $node;

    ##-- daughter connector lines
    my ($dx,$dy);
    foreach $dcoords (@dcoords) {
      ($dx,$dy) = @$dcoords;
      $cid = $canvas->createLine($dx, $dy,
				 $x,  $dy,
				 -tags=>['line',
					 ('n'.$keystr),
					 @{$args->{ancestors}},
					 @{$args->{nodeinfo}{$keystr}{leaves}},
					 @{$args->{tags}}]);
      $args->{dg}{cid2nid}{$cid} = $node;
    }

    #$cid = $canvas->createLine($x0, $y0,
	#		       $x,  $y0,
	#		       #$x,  $y,
	#		       $x,  $y1,
	#		       $x1, $y1,
	#		       -tags=>['line',
	#			       ('n'.$keystr),
	#			       @{$args->{ancestors}},
	#			       @{$args->{nodeinfo}{$keystr}{leaves}},
	#			       @{$args->{tags}}]);
    #$args->{dg}{cid2nid}{$cid} = $node;
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
## Search
sub ddg_search {
  my $dg = shift;
  my $canvas = $dg->{canvas};
  my $stxt = $dg->{info}{searchtext};

  $canvas->dtag('search');
  my ($txt);
  foreach (
	   grep {
	     $txt=$canvas->itemcget($_,'-text');
	     $dg->{info}{searchregex} ? ($txt =~ $stxt) : $txt eq $stxt
	   } $canvas->find(withtag=>'node'))
    {
      print STDERR "--- search($stxt): found cid=$_ ---\n";
      $dg->{canvas}->addtag('search', withtag=>$_);
    }
  $dg->ddg_canvas_select('search');
}


##======================================================================
## Selection
sub ddg_canvas_select {
  my ($dg,$stag) = @_;
  my $canvas = $dg->{canvas};

  #my ($canvas,$dg,$sx,$sy) = @_;
  #my ($cx,$cy) = ($canvas->canvasx($sx), $canvas->canvasy($sy));
  #print "select: canvas=$canvas ; args=@_\n";

  my @current = $canvas->find(withtag=>$stag);
  #print "current=@current\n";

  ##-- delete old selection
  $canvas->itemconfigure('selected', -fill=>'black');
  $canvas->itemconfigure('selected&&goutline', -fill=>'white');
  $canvas->delete('selbox');
  $canvas->dtag('selected');

  ##-- delete old group
  $canvas->itemconfigure('gselected', -fill=>'black');
  $canvas->itemconfigure('gselected&&goutline', -fill=>'white');
  $canvas->dtag('gselected');

  ##-- add new selection
  #print "selected=", join(' ', $canvas->find(withtag=>'selected')), "\n";
  foreach (@current) {
    ##-- ignore non-nodes
    next if (!defined($dg->{cid2nid}{$_}));

    #print
    #  ("current=$_ ; tags=", join(' ', $canvas->itemcget($_, '-tags')),
    #   "; id=", $dg->{tree}->key2str($dg->{cid2nid}{$_}), "\n");
    $canvas->addtag('selected', withtag=>('dn'.$dg->{cid2nid}{$_}));
  }
  $canvas->addtag('selected',withtag=>'current');

  ##-- info variables: defaults
  $dg->{info}{nodeid} =
    $dg->{info}{nodedist} =
      $dg->{info}{logdist} =
	$dg->{info}{nodesize} =
	  $dg->{info}{groupid} = '(undef)';


  ##-- info variables
  my ($cid,$nodeid);
  if (defined($cid=$current[0])) {
    ##-- node tags
    my @tags = $canvas->gettags($cid);
    $dg->{info}{tagstext}->delete('0.0','end');
    $dg->{info}{tagstext}->insert('end', join(' ', @tags));

    my ($gid,$distr);
    if (defined($dg->{cid2gid}) && defined($gid=$dg->{info}{groupid}=$dg->{cid2gid}{$cid})) {
      ##-- info variables: group
      $canvas->addtag('selected', withtag=>"g$gid");

      my @gleaves = map { $dg->{cid2nid}{$_} } $canvas->find(withtag=>"g$gid&&leaf");
      $cid = (sort { $a <=> $b }
	      $canvas->find(withtag=>join("&&", map { 'al'.$_ } @gleaves)))[0];

      $canvas->addtag('selected', withtag=>"n$dg->{cid2nid}{$cid}||dn$dg->{cid2nid}{$cid}");
    }


    if (defined($cid) && defined($nodeid=$dg->{info}{nodeid}=$dg->{cid2nid}{$cid})) {
      ##-- set info variables: node

      ##-- distance
      $distr = \$dg->{info}{nodedist};
      $$distr = $dg->{tree}{dists}{$nodeid};
      if (defined($$distr)) {
	$dg->{info}{logdist} = sprintf("%11.4e", log($$distr));
	$$distr = sprintf("%11.4e", $$distr);
      } else {
	$dg->{info}{nodedist} = $dg->{info}{logdist} = '(undef)';
      }

      ##-- group-id from leaf
      if ($dg->{tree}->isLeafNode($nodeid) && defined($dg->{tree}{groups})) {
	if (defined($gid=$dg->{info}{groupid}=$dg->{tree}{groups}{$nodeid})) {
	  ##-- highlight group members
	  $canvas->addtag('gselected', withtag=>("g$gid"));
	  $canvas->itemconfigure('gselected', -fill=>$dg->{gcolor});
	}

      }
      $dg->{info}{groupid} = '(none)' if (!defined($dg->{info}{groupid}));

      ##-- node size, leaves
      my @leaves = $canvas->find(withtag=>"leaf&&dn$nodeid");
      $dg->{info}{nodesize} = scalar(@leaves);
      $dg->{info}{leavestext}->delete('0.0','end');
      $dg->{info}{leavestext}->insert('end',
				      join(' ',
					   "g$dg->{info}{groupid}:",
					   map {
					     $canvas->itemcget($_,'-text')
					   } @leaves));
      $dg->{info}{leavestext}->tagAdd('sel', '0.0','end');
    }
  }


  ##-- selection: fill in blue
  $canvas->itemconfigure('selected', -fill=>'blue');

  ##-- selection: box
  my @bbox = $canvas->bbox('selected');
  if (@bbox) {
    #print "bbox=@bbox\n";
    $canvas->createRectangle(@bbox, -outline=>'blue', -tags=>['selbox']);
  }

  return;
}

##======================================================================
## Selection
sub ddg_display_info {
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
				       ['Tree Files', [qw(.tree .tree.xml .tree.bin .tree.asc)]],
				       ['All Files',  '*'],
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

  $dg->{main}->title(ref($dg)." - ".$file);
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
## Tk: show groups
sub ddg_menu_showgroups {
  my $dg = shift;
  my $canvas = $dg->{canvas};
  my $tree = $dg->{tree};

  my $groups = $dg->{tree}{groups};
  my $cid2gid = $dg->{cid2gid} = {};
  %$groups = { map { ($_=>0) } $tree->leaves } if (!$groups || !%$groups);

  ##-- delete old groups
  $dg->{canvas}->delete('group');

  if ($dg->{info}{gstate}) {
    $dg->{gdepth} = 50 if (!defined($dg->{gdepth}));

    my @lbbox = $canvas->bbox('leaf');
    my $gxLo  = $lbbox[2] + $dg->{gxpad};
    my $gxHi  = $gxLo + $dg->{gdepth} + $dg->{gxpad};
    my ($gy,@gbbox,$cid);

    my %gids = qw();
    foreach (keys(%$groups)) {
      $gids{$groups->{$_}}++;
    }

    my $maxgsize=0;
    my $totalgsize=0;
    foreach (values(%gids)) {
      $totalgsize += $_ if ($_ > 1);
      $maxgsize = $_ if ($_ > $maxgsize);
    }

    my $ngroups = scalar(keys(%gids));
    foreach $gid (keys(%gids)) {
      ##-- draw
      @gbbox = $canvas->bbox("g$gid");
      $gy = ($gbbox[1]+$gbbox[3])/2;

      $cid = $canvas->createText($gxHi, $gy,
				 -anchor=>'w',
				 -justify=>'l',
				 -text=>$gid,
				 -tags=>['group', 'glabel', "g$gid"],
				);
      $cid2gid->{$cid} = $gid;

      $cid = $canvas->createPolygon($gxHi-$dg->{gxpad}, $gy,
				    $gxLo, $gbbox[1]+$dg->{gypad},
				    $gxLo, $gbbox[3]-$dg->{gypad},
				    -outline=>'black',
				    -fill=>'white',
				    -tags=>['group', 'goutline', "g$gid"],
				   );
      $cid2gid->{$cid} = $gid;
    }

    $dg->{info}{avggsize} = sprintf("%.2f", $totalgsize/$ngroups);
    $dg->{info}{maxgsize} = $maxgsize;
  }

  my @cbbox = $canvas->bbox('all');
  $canvas->configure(-scrollregion => [
				       $cbbox[0]-$dg->{xpad},
				       $cbbox[1]-$dg->{ypad},
				       $cbbox[2]+$dg->{xpad},
				       $cbbox[3]+$dg->{ypad},
				      ]);
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
