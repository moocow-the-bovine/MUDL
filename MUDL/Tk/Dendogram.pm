##-*- Mode: Perl -*-

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
##      tree=>\@tree,  # as returned by Algorithm::Cluster::treecluster
##      dist=>\@dist,  # distance vector as returned by Algorithm::Cluster::treecluster
##      enum=>$enum,   # for leaf enumeration
##      ...
##      font=>$tkfont  # for leaves
##      xpad=>$pixels
##      ypad=>$pixels
##      dmult=>$distance_multiplier # default: 1
##      #dist2pix=>$sub,
sub new {
  my $that = shift;
  my $self = $that->SUPER::new(
			       tree=>[],
			       dist=>[],
			       enum=>undef,

			       ##-- Tk options
			       xpad=>5,
			       ypad=>5,
			       dmult=>1,

			       font=>'helvetica -12',
			       canvasWidth=>800,
			       canvasHeight=>600,

			       ##-- User options
			       @_,
			      );
  return $self;
}

##======================================================================
## View
sub view {
  my $dg = shift;
  my $w = Tk::MainWindow->new();
  $w->title('MUDL::Tk::Dendogram');
  $w->bind("<KeyPress-q>", sub { $w->destroy });

  my $c = $w->Scrolled('Canvas',
		       -background => 'white',
		       -cursor => 'crosshair',
		       -borderwidth => 2,
		       -scrollbars => 'se',
		       -confine=>1,
		       -width=>$dg->{canvasWidth},
		       -height=>$dg->{canvasHeight},
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

  $c->CanvasBind("<ButtonPress-1>", [\&dendogram_canvas_select,
				     $dg,
				     Ev('x'), Ev('y'),
				     #Ev('@')
				    ]);
  #$c->CanvasBind("<Button-1>", [\&dendogram_canvas_center, Ev('x'), Ev('y')] );

  $dg->toCanvas($c);

  Tk::MainLoop;
}

##======================================================================
## toCanvas
sub toCanvas {
  my ($dg,$c) = @_;

  ##-- common arguments: text
  my @txtargs = (qw(-anchor w -justify left),
		 (defined($dg->{font}) ? ('-font',$dg->{font}) : qw()),
		);

  ##-- locals
  my ($d0,$d1,$t0,$t1, $id0,$id1, $d0done,$d1done, $txt);

  my ($x,$y, $x0,$y0,$x1,$y1);
  my ($textx,$texty) = (0,0);

  my @queue = ('n'.$#{$dg->{tree}});
  my ($nodid,$node,$t,$i, $cid);
  my %done = (n=>pack('b'.scalar(@{$dg->{tree}})),
	      l=>pack('b'.scalar(@{$dg->{tree}}+1)));

  ##-- $node => \@ancestors;
  my $ancs = $dg->{ancs} =  {};
  my $cid2nid = $dg->{cid2nid} =  {};

  while (defined($nodid=shift(@queue))) {

    #print STDERR "checking: $nodid: ";
    $ancs->{$nodid} = [] if (!defined($ancs->{$nodid}));

    ($t,$i) = ($nodid =~ /^(.)(\d+)/);
    if (vec($done{$t}, $i, 1)) {
      ##-- already plotted
      #print STDERR " already done!.\n";
      next;
    }

    if ($t eq 'l') {
      ##--------------------------
      ## Leaf plotting
      #print STDERR "leaf.\n";

      ($x,$y) = (0, $texty);
      $txt = defined($dg->{enum}) ? $dg->{enum}->symbol($i) : $i;
      $txt = $i if (!defined($txt));
      $cid = $c->createText($x, $y,
			    -text=>$txt,
			    @txtargs,
			    -tags=>['node', 'leaf', $nodid, @{$ancs->{$nodid}}],
			   );
      $texty = $dg->{ypad} + ($c->bbox($cid))[3];

      vec($done{$t}, $i, 1) = 1;
      $cid2nid->{$cid} = $nodid;
    }
    else {
      ##--------------------------
      ## Nonterminal plotting
      #print STDERR "nonterm ";

      ($d0,$d1) = @{$dg->{tree}[$i]};
      if ($d0 < 0) { $t0='n'; $d0 = -($d0+1); } else { $t0='l'; }
      if ($d1 < 0) { $t1='n'; $d1 = -($d1+1); } else { $t1='l'; }

      $id0 = $t0.$d0;
      $id1 = $t1.$d1;

      if (!($d0done = vec($done{$t0}, $d0, 1))) {
	$ancs->{$id0} = [@{$ancs->{$nodid}}, ('a'.$nodid)];
      }
      if (!($d1done = vec($done{$t1}, $d1, 1))) {
	$ancs->{$id1} = [@{$ancs->{$nodid}}, ('a'.$nodid)];
      }

      if (!$d0done || !$d1done) {
	#print STDERR " re-enqueueing.\n";
	unshift(@queue,
		($d0done ? qw() : $id0),
		($d1done ? qw() : $id1),
		$nodid);
	next;
      }

      #print STDERR " plotting.\n";
      vec($done{$t}, $i, 1) = 1;

      ##-- get daughter coords
      ($x0,$y0) = $c->coords($id0);
      ($x1,$y1) = $c->coords($id1);

      ##-- get distance
      $dist = defined($dg->{dist}) && defined($dg->{dist}[$i]) ? $dg->{dist}[$i] : 0;


      ##-----------------------
      ## new node
      $y  = ($y0 + $y1) / 2;
      $x  = ($x0 < $x1 ? $x0 : $x1) - $dg->{xpad} - ($dg->{dmult}*$dist);
      $cid = $c->createText($x, $y,
			    -anchor=>'c',
			    -justify=>'c',
			    -text=>$nodid,
			    -state=>'hidden',
			    -fill=>'red',
			    -tags=>['node', $nodid, @{$ancs->{$nodid}}]);
      $cid2nid->{$cid} = $nodid;

      $cid = $c->createLine($x0, $y0,
			    $x,  $y0,
			    #$x,  $y,
			    $x,  $y1,
			    $x1, $y1,
			    -tags=>['line', $nodid, @{$ancs->{$nodid}}]);
      $cid2nid->{$cid} = $nodid;
    }
  }

  ##---------------------------------------------------------
  ## recenter
  ##---------------------------------------------------------
  my @bbox = $c->bbox('node','line');
  $c->configure(-scrollregion => [
				  $bbox[0]-$dg->{xpad},
				  $bbox[1]-$dg->{ypad},
				  $bbox[2]+$dg->{xpad},
				  $bbox[3]+$dg->{ypad},
				 ]);

  return;
}


##======================================================================
## Centering
sub dendogram_canvas_center {
  my ($canvas,$sx,$sy) = @_;
  my ($centerx, $centery) = ( $canvas->width() / 2.0, $canvas->height() / 2.0 );
  $canvas->scan('mark', $centerx, $centery);
  $canvas->scan('dragto',
		$centerx + (($centerx - $sx) / 10.0),
		$centery + (($centery - $sy) / 10.0));
}


##======================================================================
## Selection
sub dendogram_canvas_select {
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
    #print "current=$_ ; tags=", join(' ', $canvas->itemcget($_, '-tags')), "; id=$dg->{cid2nid}{$_}\n";
    $canvas->addtag('selected', withtag=>('a'.$dg->{cid2nid}{$_}));
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

  return;
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
