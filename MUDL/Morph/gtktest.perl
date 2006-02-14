#!/usr/bin/perl -w

use Gtk;
Gtk->init;

$data = [
	 [qw(this is   a test .)],
	 [qw(is   only a test .)],
	];
$datai = 2;
$ncols = $#{$data->[0]}+1;

##--------------------------------------
## Main Window
$w = Gtk::Window->new('toplevel');
$w->signal_connect( 'destroy', sub { Gtk->exit( 0 ); } );
$w->set_title($0);
$w->set_default_size(640,480);

##--------------------------------------
## Main Window: Vertical box
$vbox = $w->{vbox} = Gtk::VBox->new(0,0);
$vbox->show;
$w->add($vbox);


##--------------------------------------
## Column list
#$clist = Gtk::CList->new_with_titles(map { sprintf("col%.02d", $_) } (0..($ncols-1)));
$clist = Gtk::CList->new($ncols);
$clist->reorderable(0);
$clist->set_column_justification($_,'right') foreach (0..($datai-1));
$clist->set_column_justification($datai,'center');
$clist->set_column_justification($_,'left') foreach (($datai+1)..($ncols-1));
$clist->set_selection_mode('extended');

$clistSW = $clist->{scroller} = Gtk::ScrolledWindow->new(undef,undef);
$clistSW->set_policy('automatic','automatic');
$clistSW->add($clist);

$vbox->pack_start($clistSW, 1,1,0);

##--------------------------------------
## CList: signals
$clist->signal_connect('select_row', sub { clist_select_row(1,@_); });
$clist->signal_connect('unselect_row', sub { clist_select_row(0,@_); });

sub clist_select_row {
  my ($selected,$clist,$rowi,$coli,$event) = @_;
  my $row = $clist->get_row_data($rowi);
  print STDERR "$0: select_row($selected, row[$rowi]=(", join(' ', @$row), "))\n";
  #print STDERR "$0: select_row($selected, [", join(',', @$row), "])\n";

  my @selection = $clist->selection;
  print STDERR
    "$0: selection=(", join(',', @selection), ")\n";
}

##--------------------------------------
## Styles: CList
#$fontname_clist_med  = "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-iso8859-1";
#$fontname_clist_bold = "-*-helvetica-bold-r-normal-*-14-*-*-*-*-*-iso8859-1";
##--
$fontname_clist_med  = "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-iso8859-1";
$fontname_clist_bold = "-*-helvetica-bold-r-normal-*-14-*-*-*-*-*-iso8859-1";


$font_clist_med  = Gtk::Gdk::Font->load($fontname_clist_med);
$style_clist_med = Gtk::Style->new;
$style_clist_med->font($font_clist_med);

$font_clist_bold = Gtk::Gdk::Font->load($fontname_clist_bold);
$style_clist_bold = Gtk::Style->new;
$style_clist_bold->font($font_clist_bold);



##--------------------------------------
## Fill column list
foreach $rowi (0..$#$data) {
  $row = $data->[$rowi];
  $clist->append(@$row);
  $clist->set_row_data($rowi,$row);

  ##--------------------------------------
  ## set cell style
  foreach $coli (0..$#$row) {
    if ($coli==$datai) {
      $clist->set_cell_style($rowi,$coli,$style_clist_bold);
    } else {
      $clist->set_cell_style($rowi,$coli,$style_clist_med);
    }
  }
}
$clist->columns_autosize();

##--------------------------------------
## Main
$w->show_all;
Gtk->main;
