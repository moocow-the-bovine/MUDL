#!/usr/bin/perl -w

use Gtk2;
use Gtk2::SimpleList;
Gtk2->init;

our $data = [
	     [qw(this is   a test .)],
	     [qw(is   only a test .)],
	    ];
our $datai = 2;
our $ncols = $#{$data->[0]}+1;

##--------------------------------------
## Main Window
$w = Gtk2::Window->new('toplevel');
$w->signal_connect( 'destroy', sub { exit( 0 ); } );
$w->set_title($0);
$w->set_default_size(640,480);

##--------------------------------------
## Main Window: Vbox
$vbox = $w->{vbox} = Gtk2::VBox->new(0,0);
$w->add($vbox);

##--------------------------------------
## Button Bar
$bbox = Gtk2::HButtonBox->new;
$bbox->set_layout('spread'); # 'default-style' 'spread' 'edge' 'start' 'end'

$btest = Gtk2::Button->new('Test');
$btest->signal_connect('clicked',
		       sub {
			 #$sel = $sl->get_selection;
			 #@rows = $sel->get_selected_rows();
			 #print "rows=", join(', ', @rows), "\n";
			 #@rdata = map { $sl->get_row_data_from_path($_) } @rows;
			 #print "rdata=", join(', ', @rdata), "\n";
			 @rowis = $sl->get_selected_indices;
			 print "selected_indices=", join(', ', @rowis), "\n";
		       });
$bbox->add($btest);

$bquit = Gtk2::Button->new('Quit');
$bquit->signal_connect('clicked', sub { exit(0); });
$bbox->add($bquit);

$vbox->pack_end($bbox, 0,0,5);

##--------------------------------------
## Fonts
#$fontname_kwic_ctx  = "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-iso8859-1";
#$fontname_kwic_key = "-*-helvetica-bold-r-normal-*-14-*-*-*-*-*-iso8859-1";



##--------------------------------------
## SimpleList2: Utils
##  + adapted from code by muppet <scott asofyet org>
use Carp;
sub set_column_renderer_property {
  my ($slist, $index, $property, $value) = @_;
  my $column = $slist->get_column ($index);
  croak "set_column_renderer_property: invalid column index $index" unless defined $column;
  my ($cell) = $column->get_cell_renderers;
  $cell->set ($property => $value);
}
sub get_column_renderer_property {
  my ($slist, $index, $property) = @_;
  my $column = $slist->get_column ($index);
  croak "get_golumn_renderer_property: invalid column index $index" unless defined $column;
  my ($cell) = $column->get_cell_renderers;
  return $cell->get ($property);
}

sub set_column_alignment { return set_column_renderer_property(@_[0,1],'xalign',$_[2]); }
sub get_column_alignment { return set_column_renderer_property(@_[0,1],'xalign'); }

##--------------------------------------
## SimpleList2
sub doSimpleList2 {
  $sl = Gtk2::SimpleList->new( (map { ("col$_"=>'text') } (0..($ncols-1))), );

  ##--------------------------------------
  ## SimpleList: formatting
  set_column_alignment($sl, $_, 1) foreach (0..($datai-1));
  set_column_alignment($sl, $datai, 0.5);
  set_column_alignment($sl, $_, 0) foreach (($datai+1)..($ncols-1));

  set_column_renderer_property($sl, $datai,
			       weight=>2*(get_column_renderer_property($sl, $datai-1, 'weight')));
  set_column_renderer_property($sl, $datai, foreground=>'blue');

  ##--------------------------------------
  ## SimpleList: properties
  $sl->set_reorderable(0);

  ##-- test
  foreach (qw(row-activated)) {  #cursor-changed select-cursor-row
    my $signal = $_;
    $sl->signal_connect($signal,
			sub { print "$signal: ", join(',', @_), "\n"; });
  }

  ##-- column sorting
  $sl->set('headers-clickable'=>1);
  @cols = $sl->get_columns;
  foreach (0..$#cols) {
    my $i = $_;
    $cols[$i]->set_sort_column_id($i);
  }

  ##-- alternating colors
  $sl->set('rules-hint'=>1);

  ##--------------------------------------
  ## SimpleList: data
  @{$sl->{data}} = @$data;
  $sl->columns_autosize();

  ##--------------------------------------
  ## SimpleList: scroll it
  $scrolled = Gtk2::ScrolledWindow->new(undef,undef);
  $scrolled->set_policy('automatic', 'automatic');
  $scrolled->add($sl);

  ##--------------------------------------
  ## SimpleList: selection
  $sel = $sl->get_selection;
  $sel->set_mode('multiple');
  $sel->unselect_all;


  ##--------------------------------------
  ## SimpleList: pack it
  $vbox->pack_start($scrolled, 1,1,5);
}
doSimpleList2;



##--------------------------------------
## Main
$w->show_all;
Gtk2->main;
