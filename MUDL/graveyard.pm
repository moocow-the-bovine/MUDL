#########################################################################
# from: Bigrams.pm
#########################################################################

##======================================================================
## conditionalize

## $bg = $bg->conditionalize()
## $bg = $bg->conditionalize($unigrams)
## $bg = $bg->conditionalize($unigrams,$totalunigrams)
##   + converts $bg->{nz} to { "$w1$fs$w2" => P($w2|$w1), ... }
sub conditionalize {
  my ($bg,$ug,$total) = @_;
  $ug = $bg->unigrams if (!$ug);
  $total = $ug->total if (!$total);
  my ($k,$f1,$w1,$w2);
  foreach $k (keys(%{$bg->{nz}})) {
    ($w1,$w2) = split($bg->{fs}, $k, 2);
    $f1 = $ug->{$w1};
    if ($f1) {
      $bg->{nz}{$k} /= $f1;
    } else {
      carp ( __PACKAGE__ , "::conditionalize(): no unigram probability for '$w1': set to zero.\n");
      $bg->{nz}{$k} = 0;
    }
  }
  return $bg;
}

##======================================================================
## metrics, etc

## $H = $bg->conditionalEntropy($unigrams)
## $H = $bg->conditionalEntropy($unigrams,$ugtotal)
## $H = $bg->conditionalEntropy($unigrams,$ugtotal,$bgtotal)
sub conditionalEntropy {
  my ($bg,$ug,$ugtotal,$bgtotal) = @_;
  $ug = $bg->unigrams if (!$ug);
  my $Huni   = $ug->entropy($ugtotal);
  my $Hjoint = $bg->entropy($bgtotal);
  return $Hjoint - $Huni;
}
sub conditionalEntropy0 {
  my ($bg,$ug,$ugtotal,$bgtotal) = @_;
  $ug = $bg->unigrams if (!$ug);
  $ugtotal = $ug->total if (!$ugtotal);
  $bgtotal = $bg->total if (!$bgtotal);
  my ($k,$p1,$p12,$w1,$w2);
  my $H = 0;
  foreach $k (keys(%{$bg->{nz}})) {
    ($w1,$w2) = split($bg->{fs}, $k, 2);
    #$p1 = $ug->{$w1} / $ugtotal;
    $p2 = $ug->{$w2} / $ugtotal;
    $p12 = $bg->{nz}{$k} / $bgtotal;
    if ($p12 && $p2) {
      $H += $p12 * log($p2/$p12)/log(2);
    } else {
      carp ( __PACKAGE__ , "::conditionalEntropy(): missing data for '$w1,$w2': ignoring.\n");
    }
  }
  my $nz = $bg->nZero;
  $H += $bg->{zmass} * log($bgtotal/($bg->{zmass}/$nz))/log(2) if ($bg->{zmass} && $nz);
  return $H;
}



#########################################################################
# from: Tk::Dendogram.pm
#########################################################################

##-- OLD
sub view0 {
  my $dg = shift;
  my $w = Tk::MainWindow->new();
  $w->title('MUDL::Tk::Dendogram');
  $w->bind("<KeyPress-q>", sub { $w->destroy });

  my $c = $w->Scrolled('Canvas',
		       -background => 'white',
		       -cursor => 'target',
		       -borderwidth => 2,
		       -scrollbars => 'se',
		       -confine=>1
		      );
  $c->pack(qw(-fill both expand 1));
  $c->CanvasBind("<Button-2>",        [$c, 'scan', 'mark',   Ev('x'), Ev('y')] );
  $c->CanvasBind("<Button2-Motion>", [$c, 'scan', 'dragto', Ev('x'), Ev('y')] );
  $c->CanvasBind("<Button-1>", [\&dendogram_canvas_center, Ev('x'), Ev('y')] );


  ##-- common arguments: text
  my @txtargs =
    (qw(-anchor w -justify left),
     (defined($c->{font}) ? ('-font',$c->{font}) : qw()),
    );

  ##-- common arguments: nodes (hide these eventually)
  my ($wd,$ht) = ($dg->{xpad}/2, $dg->{ypad}/2);

  my ($tmom, $d0, $d1, $d, $d0txt,$d1txt);

  my ($x,$y, $x0,$y0,$x1,$y1);
  my ($textx,$texty) = (0,0);
  my $nodeid = 1;

  foreach $i (0..$#{$dg->{tree}}) {
    $tmom = $dg->{tree}[$i];
    $dist = $dg->{dist}[$i];
    $dist = 0 if (!defined($dist));

    ##-----------------------
    ## first daughter
    if (($d0=$tmom->[0]) >= 0) {
      ##-- first daughter is a leaf
      ($x0,$y0) = (0, $texty);
      $d0txt = defined($dg->{enum}) ? $dg->{enum}->symbol($d0) : $d0;
      $d0txt = $d0 if (!defined($d0txt));
      $d = $c->createText($x0, $y0,
			  -text=>$d0txt,
			  @txtargs,
			  -tags=>['node',
				  'leaf',
				  "l$d0"],
			 );
      $texty = $dg->{ypad} + ($c->bbox($d))[3];
    }
    else {
      ##-- first daughter is a non-leaf
      ($x0,$y0) = $c->coords('n'.(-$d0));
    }

    ##-----------------------
    ## second daughter
    if (($d1=$tmom->[1]) >= 0) {
      ##-- first daughter is a leaf
      ($x1,$y1) = (0, $texty);

      $d1txt = defined($dg->{enum}) ? $dg->{enum}->symbol($d1) : $d1;
      $d1txt = $d1 if (!defined($d1txt));

      $d = $c->createText($x1, $y1,
			  -text=>$d1txt,
			  -tags=>['node',
				  'leaf',
				  "l$d1"],
			  @txtargs);

      $texty = $dg->{ypad} + ($c->bbox($d))[3];
    }
    else {
      ##-- 2nd daughter is a non-leaf
      ($x1,$y1) = $c->coords('n'.(-$d1));
    }

    ##-----------------------
    ## new node
    $y  = ($y0 + $y1) / 2;
    $x  = ($x0 < $x1 ? $x0 : $x1) - $dg->{xpad} - ($dg->{dmult}*$dist);
    #c->createRectangle(($x-$wd/2), ($y-$ht/2),
	#		($x+$wd/2), ($y+$ht/2),
	#		tags=>['node', "n$nodeid"]);
    $c->createText($x, $y,
		   -text=>"n$nodeid",
		   -state=>'hidden',
		   -tags=>['node', "n$nodeid"]);

    $c->createLine($x0, $y0,
		   $x,  $y0,
		   #$x,  $y,
		   $x,  $y1,
		   $x1, $y1,
		   -tags=>['line']);

    $nodeid++;
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

  Tk::MainLoop();
  return;
}
