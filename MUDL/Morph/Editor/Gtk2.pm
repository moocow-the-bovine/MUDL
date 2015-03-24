##-*- Mode: CPerl -*-

## File: MUDL::Morph::Editor::Gtk2.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: Gtk morphological editors
##======================================================================

package MUDL::Morph::Editor::Gtk2;
use MUDL::Object;
use MUDL::Morph::Editor;
use Encode;
use Gtk2;
use Gtk2::SimpleList;
use Gtk2::SimpleMenu;

use PDL;
use PDL::EditDistance;

use Carp;
use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $gui = MUDL::Morph::Editor::Gtk2->new(%args)
## + %args, structure:
##
##    ## Underlying data
##    data => $data,      ## MUDL::Morph::Editor object
##
##    ## GUI behavior
##    noinit => $bool,    ## don't automatically initialize
##    encoding => $enc,
sub new {
  my $that = shift;
  my $gui = $that->SUPER::new(
			      ##-- underlying data
			      data=>MUDL::Morph::Editor->new(), ##-- editor data

			      ##-- GUI stuff
			      default_size=>[1000,700],
			      kwicn=>4,

			      ##-- text encoding
			      encoding=>'ISO-8859-1',

			      ##-- chosen files
			      files=>{},

			      ##-- user options
			      @_,
			     );

  ##-- initialize
  $gui->initialize() if (!$gui->{noinit});

  return $gui;
}

##======================================================================
## Initialization
##======================================================================

## $gui = $gui->initialize();
sub initialize {
  my $gui = shift;

  ##--------------------------------------
  ## Top level
  my $w = $gui->{main} = Gtk2::Window->new('toplevel');

  ##--------------------------------------
  ## Main Window
  $w->signal_connect( 'destroy', sub { Gtk2->main_quit; } );
  $w->set_title(ref($gui));
  #$w->set_size_request(@{$gui->{default_size}});
  $w->set_default_size(@{$gui->{default_size}});

  ##--------------------------------------
  ## Main Window: VBox
  my $vbox = $w->{vbox} = Gtk2::VBox->new(0,0);
  $vbox->show;
  $w->add($vbox);

  ##--------------------------------------
  ## Menu bar
  $gui->create_menubar();
  $vbox->pack_start($gui->{menu}{widget},0,1,0);

  ##--------------------------------------
  ## Main Frame: pack logic
  my $vhp = $vbox->{hpaned} = Gtk2::HPaned->new();
  $vbox->pack_start($vhp, 1,1,0);

  ##--------------------------------------
  ## Word-Type Frame
  my $wf = $gui->{wordf} = Gtk2::Frame->new('Word Types');
  $wf->set_border_width(5);
  $wf->set_shadow_type("etched-in");
  $vhp->add1($wf);

  $gui->create_word_frame();

  ##--------------------------------------
  ## Detail Frame
  my $df = $gui->{detailf} = Gtk2::Frame->new();
  $df->set_border_width(0);
  $df->set_shadow_type("none");
  $vhp->add2($df);

  $gui->create_detail_frame;

  ##--------------------------------------
  ## Show all
  $gui->{main}->show_all;

  return $gui;
}


##--------------------------------------------------------------
## GUI: Initialization: Menu bar

## $menu = $gui->create_menubar()
sub create_menubar {
  my $gui = shift;

  ##----------------------------
  ## menu tree for Gtk2::SimpleMenu
  my $menu_tree =
    [
     ##-------------------------
     ## Menu: File
     _File =>
     {
      item_type=>'<Branch>',
      children=>[
		 ##-- File: Open
		 '_Open Project' => {
				     callback => sub { $gui->menu_project_open; },
				     accelerator=>'<ctrl>O',
				    },

		 ##-- File: Save
		 '_Save Project' => {
				     callback => sub { $gui->menu_project_save; },
				     accelerator=>'<ctrl>S',
				    },

		 ##-- File: Save
		 '_Clear Project' => {
				      callback => sub { $gui->menu_project_clear; },
				      accelerator=>'<ctrl>C',
				     },

		 ##-- File: Separator
		 Separator => { item_type=>'<Separator>' },

		 ##-- File: Quit
		 _Quit => {
			   callback => sub { Gtk2->main_quit; },
			   accelerator=>'<ctrl>Q',
			  },
		],
     },

     ##-------------------------
     ## Menu: Corpus
     _Corpus =>
     {
      item_type=>'<Branch>',
      children=>[
		##-- Corpus: Load
		 _Load => {
			   callback => sub { $gui->menu_corpus_load; },
			   accelerator=>'<ctrl>L',
			  },

		 ##-- Corpus: Save
		 _Save => {
			   callback => sub { $gui->menu_corpus_save; },
			   #accelerator=>'<ctrl>S',
			  },

		 ##-- Corpus: Separator
		 Separator => { item_type=>'<Separator>' },

		 ##-- Corpus: Clear
		 _Clear => {
			    callback => sub { $gui->menu_corpus_clear; },
			    #accelerator=>'<ctrl>C',
			   },
		],
     },

     ##-------------------------
     ## Menu: Analyses
     '_Analyses' =>
     {
      item_type=>'<Branch>',
      children=>[
		 ##-- Analyses: Load
		 '_Load' => {
			    callback => sub { $gui->menu_analyses_load; },
			    #accelerator=>'<ctrl>T',
			   },

		 ##-- Analyses: Save
		 '_Save' => {
			     callback => sub { $gui->menu_analyses_save; },
			     #accelerator=>'<ctrl>T',
			    },

		 ##-- Analyses: Separator
		 Separator => { item_type=>'<Separator>' },

		 ##-- Analyses: Clear
		 Clea_r => {
			    callback => sub { $gui->menu_analyses_clear; },
			    #accelerator=>'<ctrl>R',
			   },
		],
     },

    ];


  my $menu = $gui->{menu} = Gtk2::SimpleMenu->new(menu_tree=>$menu_tree);

  #return $menu;
}

##--------------------------------------------------------------
## GUI: Initialization: Word Frame
sub create_word_frame {
  my $gui = shift;

  my $wf = $gui->{wordf};
  my $wvb = $wf->{vbox} = Gtk2::VBox->new(0,0);
  $gui->create_word_list;

  ##-- notebook (info, filters, etc.)
  my $nb = $wf->{notebook} = Gtk2::Notebook->new;
  $nb->set_tab_pos('top');
  $nb->set_show_tabs(1);
  $nb->set_scrollable(1);
  $nb->set_border_width(5);
  $wvb->pack_end($nb, 0,0,0);

  ##-- word frame: notebook: info
  $gui->create_word_info;

  ##-- word frame: notebook: filters
  $gui->create_word_filters;


  #$wf->set_usize(256,0);
  $nb->set_size_request(256,256);
  $wf->add($wvb);
}

##--------------------------------------------------------------
## GUI: Initialization: Word Frame: List: (re-)creation
sub create_word_list {
  my $gui = shift;

  my $wf = $gui->{wordf};

  ##-- destroy old objects if any
  $wf->{list}->destroy() if ($wf->{list});
  $wf->{scrolled}->destroy() if ($wf->{scrolled});

  ##-- Word list: create
  my $wl = $wf->{list} = Gtk2::SimpleList->new(
					       'Word'=>'text',
					       'Freq'=>'int',
					       'An?'=>'bool',
					       #'App?'=>'bool',
					       'ID'=>'hidden',
					      );
  ##--------------------------------------
  ## Word List: size
  $wl->set_size_request(256,100);

  ##--------------------------------------
  ## Word List: sorting
  $wl->set_reorderable(0);
  my @cols = $wl->get_columns;
  $cols[$_]->set_sort_column_id($_) foreach (0..$#cols);

  ##--------------------------------------
  ## Word List: column properties
  set_column_alignment($wl, 0, 0.0); ##-- word
  set_column_alignment($wl, 1, 1.0); ##-- freq
  set_column_alignment($wl, 2, 0.5); ##-- analyzed?
  #set_column_alignment($wl, 3, 0.5); ##-- applied?

  foreach (0..$#cols) {
    #$cols[$_]->set('expand'=>($_==0 ? 1 : 0));
    $cols[$_]->set('expand'=>0);
    $cols[$_]->set('min-width'=>0);
    $cols[$_]->set('resizable'=>1);
    set_column_renderer_property($wl, $_, 'mode', 'inert');
  }

  ##-- alternating colors for each row
  $wl->set('rules-hint'=>1);

  ##-- enable search (?)
  $wl->set_enable_search(1);
  $wl->set_search_column(0); ##-- word

  ##--------------------------------------
  ## Word List: Data
  #@{$wl->{data}} = @$data;
  #$wl->columns_autosize;

  ##--------------------------------------
  ## Word List: selection
  my $sel = $wl->get_selection;
  $sel->set_mode('single');
  $sel->unselect_all;

  #$wl->signal_connect('select-row', sub { $gui->select_word(@_); }); #?
  #$wl->signal_connect('row-activated', sub { $gui->select_word(@_); }); #?
  $sel->signal_connect('changed', sub { $gui->select_word(@_); });

  ##--------------------------------------
  ## Word List: scrolling
  my $scrolled = $wf->{scrolled} = Gtk2::ScrolledWindow->new(undef,undef);
  $scrolled->set_shadow_type('in');
  $scrolled->set_border_width(5);
  $scrolled->set_policy('automatic','automatic');
  $scrolled->add($wl);

  ##--------------------------------------
  ## Word List: packing
  $wf->{vbox}->pack_start($scrolled, 1,1,0);
}

##--------------------------------------------------------------
## GUI: Initialization: Word Frame: Notebook: Info
sub create_word_info {
  my $gui = shift;

  my $wf   = $gui->{wordf};
  my $wfnb = $wf->{notebook};
  my $wfi  =  $gui->{wordinfo} = Gtk2::Frame->new();
  $wfi->set_border_width(5);
  $wfi->set_shadow_type('none');

  my $vbox = $wfi->{vbox} = Gtk2::VBox->new;
  $vbox->set_border_width(0);
  $wfi->add($vbox);

  ##-- Info: Table
  my $tab = $wfi->{tab} = Gtk2::Table->new(5,2);
  $vbox->pack_start($tab, 1,1,0);

  ##-- Info: Table contents
  @$wfi{qw(nwordsLab nwordsVal)} = $gui->label_value_pair($tab, 0, '# Words:', '0');
  @$wfi{qw(wordLab wordVal)} = $gui->label_value_pair($tab, 1, 'Word:', '-undef-');
  @$wfi{qw(idLab   idVal)}   = $gui->label_value_pair($tab, 2, 'ID:', '-undef-');
  @$wfi{qw(freqLab freqVal)} = $gui->label_value_pair($tab, 3, 'Freq:', '-undef-');
  @$wfi{qw(anLab   anVal)} = $gui->label_value_pair($tab, 4, 'Analyzed:', '-undef-');

  ##-- Info: Scrolled
  $gui->scroll_widget($wfi,scroll_policy=>[qw(automatic never)]);
  $wfnb->append_page($wfi->{scrolled},'Info');
}

##--------------------------------------------------------------
## GUI: Initialization: Word Frame: Notebook: Filters
sub create_word_filters {
  my ($gui) = shift;

  my $wf  = $gui->{wordf};
  my $wfnb = $wf->{notebook};
  my $wff =  $gui->{filters} = Gtk2::Frame->new();
  $wff->set_shadow_type('none');
  $wff->set_border_width(5);

  my $vbox = $wff->{vbox} = Gtk2::VBox->new();
  $vbox->set_border_width(0);
  $wff->add($vbox);

  my $wfftab = $wff->{tab} = Gtk2::Table->new(4, 2);
  $vbox->pack_start($wfftab, 1,1,0);

  ##-- Minimum Frequency
  my $wff_min_adj  = $wff->{min_adj}  = Gtk2::Adjustment->new(0.0, 0.0, 1e38, 1.0, 10.0, 0.0);
  my $wff_min_spin = $wff->{min_spin} = Gtk2::SpinButton->new($wff_min_adj, 1.0, 0);
  $wff_min_spin->set_width_chars(0);
  $wff_min_spin->set_wrap(0);
  $wff_min_spin->set_update_policy('always'); ##???
  $gui->labelled_widget($wfftab, 0, "Min F:", $wff_min_spin);
  #$wff_min_adj->signal_connect(value_changed=>sub { print "value changed: ", $_[0]->get_value, "\n"; });

  ##-- Maximum Frequency
  my $wff_max_adj  = $wff->{max_adj}  = Gtk2::Adjustment->new(0.0, 0.0, 1e38, 1.0, 10.0, 0.0);
  my $wff_max_spin = $wff->{max_spin} = Gtk2::SpinButton->new($wff_max_adj, 1.0, 0);
  $wff_max_spin->set_width_chars(0);
  $wff_max_spin->set_wrap(0);
  $wff_max_spin->set_update_policy('always'); ##???
  $gui->labelled_widget($wfftab, 1, "Max F:", $wff_max_spin);

  ##-- Regex
  my $wff_regex = $wff->{regex} = Gtk2::Entry->new();
  $wff_regex->set_width_chars(0);
  $gui->labelled_widget($wfftab, 2, 'Regex:', $wff_regex);

  ##-- Analyzed?
  my $wff_anl_adj  = $wff->{anl_adj}  = Gtk2::Adjustment->new(-1, -1, 1, 1, 1, 0);
  my $wff_anl_spin = $wff->{anl_spin} = Gtk2::SpinButton->new($wff_anl_adj, 1.0, 0);
  $wff_anl_spin->set_width_chars(0);
  $wff_anl_spin->set_wrap(1);
  $wff_anl_spin->set_update_policy('always'); ##???
  $gui->labelled_widget($wfftab, 3, "Analyzed?:", $wff_anl_spin);

  ##-- ButtonBox
  my $bbox = $wff->{bbox} = Gtk2::HButtonBox->new();
  $bbox->set_layout('spread');
  $vbox->pack_start($bbox,0,0,0);

  my $b_reset = $bbox->{reset} = Gtk2::Button->new('Reset');
  $b_reset->signal_connect('clicked',
			   sub {
			     $wff_min_adj->set_value(0.0);
			     $wff_max_adj->set_value(0.0);
			     $wff_regex->set_text('');
			     $gui->populateWordList();
			   });
  $bbox->add($b_reset);

  my $b_apply = $bbox->{apply} = Gtk2::Button->new('Apply');
  $b_apply->signal_connect('clicked',
			   sub { $gui->populateWordList(); });
  $bbox->add($b_apply);

  ##-- Scrolled With Viewport (manual)
  $gui->scroll_widget($wff,scroll_policy=>[qw(automatic never)]);
  $wfnb->append_page($wff->{scrolled},'Filters');
}

##--------------------------------------------------------------
## GUI: Initialization: Detail Frame
sub create_detail_frame {
  my $gui = shift;

  ##-- detail frame: vertical separator
  my $df = $gui->{detailf};
  my $dfvp = $df->{vpaned} = Gtk2::VPaned->new();
  $df->add($dfvp);

  ##-- detail frame: kwic area
  $gui->create_detail_context_area();

  ##-- detail frame: edit area
  $gui->create_detail_edit_area();

  ##-- detail frame: notebook
  $gui->create_detail_edit_notebook_area();
}

##--------------------------------------------------------------
## GUI: Initialization: Detail: Context Area
sub create_detail_context_area {
  my $gui = shift;

  my $cf = $gui->{contexts} = Gtk2::Frame->new('Contexts');
  $cf->set_shadow_type('etched-in');
  $cf->set_border_width(5);

  my $vbox = $cf->{vbox} = Gtk2::VBox->new();
  $cf->add($vbox);

  my $kwicn = $gui->{kwicn};

  ##-- KWIC List : create
  my $kwic = $cf->{kwic} =
    Gtk2::SimpleList->new(
			  (map { ("i-".($kwicn-$_+1))=>'text' } (1..$kwicn)),
			  ('i'=>'text'),
			  (map { ("i+$_"=>'text') } (1..$kwicn)),
			  'sentidx'=>'hidden',
			  'occidx'=>'hidden',
			 );
  $kwic->set_reorderable(0);

  ##-- KWIC List : properties
  set_column_alignment($kwic, $_,     1.0) foreach (0..($kwicn-1));
  set_column_alignment($kwic, $kwicn, 0.5);
  set_column_alignment($kwic, $_,     0.0) foreach (($kwicn+1)..(2*$kwicn));

  set_column_renderer_property($kwic, $kwicn,
			       weight=>2*(get_column_renderer_property($kwic, $kwicn-1, 'weight')));
  set_column_renderer_property($kwic, $kwicn, foreground=>'blue');

  $kwic->set('headers-clickable'=>0);
  $kwic->set('rules-hint'=>1);

  ##-- KWIC List : selection properties
  $kwic->get_selection->set_mode('multiple');

  ##-- KWIC CList: scroll it
  my $scrolled = $cf->{kwicScrolled} = Gtk2::ScrolledWindow->new(undef,undef);
  $scrolled->set_shadow_type('in');
  $scrolled->set_border_width(5);
  $scrolled->set_policy('automatic','automatic');
  $scrolled->add($kwic);
  $vbox->pack_start($scrolled, 1,1,0);

  ##-- pack it
  $cf->set_size_request(200,200);
  $gui->{detailf}{vpaned}->add1($cf);
}

##--------------------------------------------------------------
## GUI: Initialization: Detail: Edit Area
sub create_detail_edit_area {
  my $gui = shift;

  ##--------------------------------------
  ## Basic
  my $frame = $gui->{editf} = Gtk2::Frame->new;
  $frame->set_shadow_type('none');
  $frame->set_border_width(5);

  my $vbox = $frame->{vbox} = Gtk2::VBox->new;
  $frame->add($vbox);

  ##--------------------------------------
  ## Edit area
  my $eframe = $frame->{manEditFrame} = Gtk2::Frame->new('Edit');
  $eframe->set_shadow_type('etched-in');
  $eframe->set_border_width(0);
  $vbox->pack_start($eframe, 0,1,0);

  my $evbox  = $eframe->{vbox}     = Gtk2::VBox->new;
  my $ehbox  = $eframe->{hbox}     = Gtk2::HBox->new;
  $eframe->add($evbox);
  $evbox->pack_start($ehbox, 1,1,5);

  my $elab   = $frame->{manEditLabel} = Gtk2::Label->new('<b>Segments:</b>');
  $elab->set_use_markup(1);
  my $eentry = $frame->{manEditEntry} = Gtk2::Entry->new();
  my $keyval_enter  = Gtk2::Gdk->keyval_from_name('Enter');
  my $keyval_return = Gtk2::Gdk->keyval_from_name('Return');
  $eentry->signal_connect('key-press-event',
			  sub {
			    my $keyval = $_[1]->keyval;
			    $gui->edit_apply_clicked()
			      if ($keyval==$keyval_enter || $keyval==$keyval_return);
			  });

  my $apply = $frame->{manEditApply} = Gtk2::Button->new('  Apply  ');
  $apply->signal_connect('clicked', sub { $gui->edit_apply_clicked(); });

  my $revoke = $frame->{manEditRevoke} = Gtk2::Button->new('  Revoke  ');
  $revoke->signal_connect('clicked', sub { $gui->edit_revoke_clicked(); });

  $ehbox->pack_end($revoke, 0,0,5);
  $ehbox->pack_end($apply, 0,0,5);
  $ehbox->pack_start($elab, 0,0,5);
  $ehbox->pack_start($eentry, 1,1,5);

  ##-- pack it
  $gui->{detailf}{vpaned}->add2($frame);
}

##--------------------------------------------------------------
## GUI: Initialization: Detail: Notebook area
sub create_detail_edit_notebook_area {
  my $gui = shift;

  ##-- edit notebook
  my $enb = $gui->{detailnb} = Gtk2::Notebook->new;
  $enb->set_tab_pos('top');
  $enb->set_show_tabs(1);
  $enb->set_scrollable(1);
  $enb->set_border_width(5);

  ##-- edit notebook: analyses
  $gui->create_edit_detail_analyses_page();

  ##-- edit notebook: Morphs
  $gui->create_detail_edit_trie_page('Morphs');

  ##-- edit notebook: Prefixes
  #$gui->create_detail_edit_trie_page('Prefixes');

  ##-- edit notebook: Stems
  #$gui->create_detail_edit_trie_page('Stems');

  ##-- edit notebook: Suffixes
  #$gui->create_detail_edit_trie_page('Suffixes');

  ##-- pack it
  $enb->set_size_request(200,200);
  #$gui->{detailf}{vpaned}->add2($enb);
  $gui->{editf}{vbox}->pack_end($enb, 1,1,0);
}

##--------------------------------------------------------------
## GUI: Initialization: Detail: Edit: Analyses
sub create_edit_detail_analyses_page {
  my $gui = shift;

  ##--------------------------------------
  ## Basic
  my $frame = $gui->{editAnalyses} = Gtk2::Frame->new;
  $frame->set_shadow_type('none');
  $frame->set_border_width(5);

  my $vbox = $frame->{vbox} = Gtk2::VBox->new;
  $frame->add($vbox);

  ##--------------------------------------
  ## Analysis list
  my $alist = $frame->{alist} = Gtk2::SimpleList->new('Segments'=>'text',
						      'Distance'=>'int',
						      'Analysis'=>'text',
						      'Weight'=>'double',
						      ''=>'text');
  #$alist->set_size_request(100,100);
  $alist->set_headers_visible(1);
  $alist->set_reorderable(0);

  ##--------------------------------------
  ## Analysis List: sorting
  $alist->set_reorderable(0);
  my @cols = $alist->get_columns;
  $cols[$_]->set_sort_column_id($_) foreach (0..$#cols);

  ##-- alternating colors for each row
  $alist->set('rules-hint'=>1);

  ##--------------------------------------
  ## Analysis List: column properties
  set_column_alignment($alist, 0, 0.0); ##-- segments
  set_column_alignment($alist, 1, 1.0); ##-- distance
  set_column_alignment($alist, 2, 0.0); ##-- analysis
  set_column_alignment($alist, 3, 1.0); ##-- weight

  foreach (0..$#cols) {
    #$cols[$_]->set('expand'=>($_<=1 ? 1 : 0));
    $cols[$_]->set('expand'=>0);
    $cols[$_]->set('min-width'=>0);
    $cols[$_]->set('resizable'=>1);
    set_column_renderer_property($alist, $_, 'mode', 'inert');
  }

  ##--------------------------------------
  ## Analysis List: selection
  my $sel = $alist->get_selection;
  $sel->set_mode('single');

  #$wl->signal_connect('select-row', sub { $gui->select_word(@_); }); #?
  #$wl->signal_connect('row-activated', sub { $gui->select_word(@_); }); #?
  $sel->signal_connect('changed', sub { $gui->select_analysis(@_); });


  ##--------------------------------------
  ## Analysis List: scrolling
  my $scrolled = $alist->{scrolled} = Gtk2::ScrolledWindow->new(undef,undef);
  $scrolled->set_shadow_type('in');
  $scrolled->set_border_width(5);
  $scrolled->set_policy('automatic','automatic');
  $scrolled->add($alist);

  ##--------------------------------------
  ## Analysis List: packing
  $vbox->pack_start($scrolled, 1,1,0);

  ##--------------------------------------
  ## ButtonBox
  #my $box = $frame->{bbar} = Gtk2::HButtonBox->new;
  #$bbox->set_layout('spread');
  #$vbox->pack_end($bbox,0,0,0);

  ##--------------------------------------
  ## notebook page (scrolled)
  $gui->scroll_widget($frame, scroll_policy=>[qw(automatic automatic)]);
  $gui->{detailnb}->append_page($frame->{scrolled}, 'Analyses');
}

##--------------------------------------------------------------
## GUI: Initialization: Detail: Edit: Trie page(s)

## undef = $gui->create_edit_trie_page($key,%args)
##  %args:
##   + title=>$title  ## default: $key
##   + ...
sub create_detail_edit_trie_page {
  my ($gui,$key,%args) = @_;

  my $title = $args{title} ? $args{title} : $key;
  my $frame = $gui->{"edit$key"} = Gtk2::Frame->new;
  $frame->set_shadow_type('none');
  $frame->set_border_width(5);

  ##-- pack it
  $gui->scroll_widget($frame,scroll_policy=>[qw(automatic automatic)]);
  $gui->{detailnb}->append_page($frame->{scrolled},$key);
}


##======================================================================
## GUI: Utilities: Notebook
##======================================================================

## undef = $gui->scroll_widget($widget,%args)
##  + %args:
##    scroll_policy => [$xpolicy,$ypolicy],
##  + adds keys 'viewport', 'scrolled' to $widget
sub scroll_widget {
  my ($gui,$widget,%args) = @_;
  my $scroll_policy = $args{scroll_policy} ? $args{scroll_policy} : [qw(automatic automatic)];

  ##-- viewport
  my $viewport = $widget->{viewport} = Gtk2::Viewport->new;
  $viewport->set_shadow_type('none');
  $viewport->add($widget);

  ##-- scrolled
  my $scrolled = $widget->{scrolled} = Gtk2::ScrolledWindow->new;
  $scrolled->set_shadow_type('none');
  $scrolled->set_policy(@$scroll_policy);
  $scrolled->add($viewport);
}


##======================================================================
## GUI: Utilities: SimpleList
##  + adapted from code by muppet <scott asofyet org>
##======================================================================

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


##======================================================================
## Gtk Utilities: Labelled Widgets (using Gtk2::Table)
##======================================================================

## ($label,$value) = $wg->label_value_pair($table,$row,$label_text,$value_text,%args)
sub label_value_pair {
  my ($gui,$table,$row,$labtxt,$valtxt, %args) = @_;

  my $val = Gtk2::Label->new($valtxt);
  $val->set_justify('left');
  $val->set_alignment(0.0, 0.5);
  $val->set_ellipsize($args{'valellipsize'} ? $args{'valellipsize'} : 'none')
    if ($val->can('set_ellipsize'));

  return $gui->labelled_widget($table,$row,$labtxt,$val,
			       labstyle=>'label',
			       labxopts=>[qw(fill expand)],
			       labyopts=>[qw(fill expand)],
			       %args,
			      );
};

## ($label,$widget) = $gui->labelled_widget($table,$row,$label_text,$value_widget,%args)
##  + packs $label as label on $widget into $table at row $row
sub labelled_widget {
  my ($gui,$table,$row,$labtxt,$val, %args) = @_;
  $args{labstyle} = 'label' if (!defined($args{labstyle}));
  $args{labcol} = 0 if (!defined($args{labcol})); ##-- label-column start
  $args{valcol} = 1 if (!defined($args{valcol})); ##-- value column start
  $args{labwd}  = 1 if (!defined($args{labwd}));   ##-- label width (#/columns)
  $args{valwd}  = 1 if (!defined($args{valwd}));   ##-- value width (#/columns)

  $args{labxopts}  = [qw(fill)] if (!defined($args{labxopts}));
  $args{labyopts}  = [qw(fill)] if (!defined($args{labyopts}));
  $args{labxpad}   = 5 if (!defined($args{labpad}));
  $args{labypad}   = 5 if (!defined($args{labpad}));

  $args{valxopts}  = [qw(fill expand)] if (!defined($args{valyopts}));
  $args{valyopts}  = [qw(fill expand)] if (!defined($args{valyopts}));
  $args{valxpad}   = 5 if (!defined($args{valxpad}));
  $args{valypad}   = 5 if (!defined($args{valypad}));
  #my @how = (@args{qw(xopts yopts xpad ypad)});

  my $lab = Gtk2::Label->new("<b>".$labtxt."</b>");
  $lab->set_use_markup(1);
  $lab->set_ellipsize($args{'labellipsize'} ? $args{'labellipsize'} : 'none')
    if ($lab->can('set_ellipsize'));

  ## gtk_misc_set_alignment(label, xalign, yalign)
  ##  + the align parameter is a float [0.0, 1.0], where 0.0 is top/left, 1.0 is
  ##    bottom/right, and 0.5 is centered.
  $lab->set_justify('right');
  $lab->set_alignment(1.0, 0.5);
  $lab->show;
  $table->attach($lab, $args{labcol}, $args{labcol}+$args{labwd}, $row, $row+1,
		 @args{qw(labxopts labyopts labxpad labypad)});

  $val->show;
  $table->attach($val, $args{valcol}, $args{valcol}+$args{valwd}, $row, $row+1,
		 @args{qw(valxopts valyopts valxpad valypad)});

  return ($lab,$val);
};


##======================================================================
## GUI: Guts: populateWordList
##======================================================================

## undef = $gui->populateWordList
#*populateWordList = apidummy('populateWordList');
sub populateWordList {
  my $gui = shift;

  ##-- data objects
  my $me = $gui->{data};
  my $wenum = $me->{wenum};
  my $wfreq = $me->{wfreq};
  #my $analyses = $me->{analyses};
  my $w2seg = $me->{w2seg};

  ##-- clear gui
  $gui->create_word_list();
  $gui->{wordf}{scrolled}->show_all;

  ##-- gui objects
  my $wl  = $gui->{wordf}{list};
  my $fmin  = $gui->{filters}{min_adj}->get_value;
  my $fmax  = $gui->{filters}{max_adj}->get_value;
  my $fre   = $gui->{filters}{regex}->get_text;
  my $fan   = $gui->{filters}{anl_adj}->get_value;

  ##-- set sort-column
  my @cols = $wl->get_columns;

  ##-- add all words (filtering)
  my ($wid,$word_utf8,$word,$wordfreq,$wordanalyzed);
  foreach $wid (0..$#{$wenum->{id2sym}}) {
    $word_utf8 = $wenum->{id2sym}[$wid];
    $word      = Encode::encode($gui->{encoding},$word_utf8) if (defined($gui->{encoding}));
    $wordfreq  = $wfreq->{$wid};

    #$wordanalyzed = defined($analyses) && $analyses->has_analyses_utf8($word_utf8) ? 1 : 0;
    $wordanalyzed = defined($w2seg->[$wid]) && $w2seg->[$wid] ne '' ? 1 : 0;

    next if ( ($fmin > 0 && $wordfreq < $fmin)
	      ||
	      ($fmax > 0 && $wordfreq > $fmax)
	      ||
	      ($fre ne '' && $word !~ m/$fre/)
	      ||
	      ($fan >= 0  && $wordanalyzed != $fan)
	    );

    push(@{$wl->{data}}, [ $word, $wordfreq, $wordanalyzed, $wid, ]);
  }

  ##-- update info: number of matching words
  $gui->{wordinfo}{nwordsVal}->set_text(scalar(@{$wl->{data}}));

  $gui->select_word();
}

##======================================================================
## Guts: Word Selection
##======================================================================

sub select_word {
  my $gui = shift;
  my $wl = $gui->{wordf}{list};

  my ($i) = $wl->get_selected_indices;

  ##-- clear kwic list
  @{$gui->{contexts}{kwic}{data}} = qw();

  if (!defined($i) || $i > $#{$wl->{data}}) {
    ##-- no data: clear info frame
    $gui->{wordinfo}{wordVal}->set_text('-undef-');
    $gui->{wordinfo}{idVal}->set_text('-undef-');
    $gui->{wordinfo}{freqVal}->set_text('-undef-');
    $gui->{wordinfo}{anVal}->set_text('-undef-');

    return;
  }

  ##-- Get data
  my ($word,$freq,$an,$wid) = @{$wl->{data}[$i]};

  ##-- Update info frame
  $gui->{wordinfo}{wordVal}->set_text($word);
  $gui->{wordinfo}{idVal}->set_text($wid);
  $gui->{wordinfo}{freqVal}->set_text($freq);
  $gui->{wordinfo}{anVal}->set_text($an ? 'yes' : 'no');

  ##-- Update KWIC list
  my $kwicn = $gui->{kwicn};
  my $kwic = $gui->{contexts}{kwic};
  my $corpus = $gui->{data}{corpus};
  my $wenum = $gui->{data}{wenum};
  my @occs = (defined($gui->{data}{woccs}{$wid})
	      ? unpack('(LS)*', $gui->{data}{woccs}{$wid})
	      : qw());

  my ($sentid,$occid,@sent,@kwic);
  while (@occs) {
    ($sentid,$occid) = splice(@occs,0,2);
    @sent = $corpus->{sents}[$sentid]->list;

    @kwic = (
	     ##-- visible columns
	     (map {
	       ($_ < 0 || $_ > $#sent
		? ''
		: ($gui->{encoding}
		   ? Encode::encode($gui->{encoding}, $wenum->{id2sym}[$sent[$_]])
		   : $wenum->{id2sym}[$sent[$_]]))
	     } (($occid-$kwicn)..($occid+$kwicn))),
	     ##-- hidden columns
	     ($sentid,$occid),
	    );

    push(@{$kwic->{data}},[@kwic]);
  }

  ##-- now resize kwic columns
  $kwic->columns_autosize;

  ##-- populate analysis list
  my $alist = $gui->{editAnalyses}{alist};
  if ($gui->{data}{analyses}) {
    my $wtext         = Encode::decode($gui->{encoding},$word);
    my @segmentations = sort {
      $a->[1] <=> $b->[1] || $a->[3] <=> $b->[3] || $a->[0] cmp $b->[0] || $a->[2] cmp $b->[2]
    } $gui->{data}{analyses}->segmentations_utf8($wtext);

    ##-- add to data
    @{$alist->{data}} = map {
      [
       Encode::encode($gui->{encoding},$_->[0]),
       $_->[1],
       Encode::encode($gui->{encoding},$_->[2]),
       $_->[3],
       '',
      ]
    } @segmentations;
  }

  ##-- populate analysis edit-entry
  my ($wseg);
  if (defined($wseg=$gui->{data}{w2seg}[$wid])) {
    $wseg = Encode::encode($gui->{encoding},$wseg);
  } elsif (@{$gui->{editAnalyses}{alist}{data}}) {
    $wseg = $gui->{editAnalyses}{alist}{data}[0][0];
  } else {
    $wseg = Encode::encode($gui->{encoding},$word);
  }
  $gui->{editf}{manEditEntry}->set_text($wseg);
}

##======================================================================
## Guts: Analysis Selection
##======================================================================

sub select_analysis {
  my $gui = shift;
  my $alist = $gui->{editAnalyses}{alist};
  my ($i) = $alist->get_selected_indices;
  my $adata = defined($i) ? $alist->{data}[$i] : undef;

  ##-- set edit entry value
  $gui->{editf}{manEditEntry}->set_text($adata ? $adata->[0] : '');
}

##======================================================================
## GUI: Apply segmentation edits
##======================================================================

sub edit_apply_clicked {
  my $gui  = shift;

  my $wseg = $gui->{editf}{manEditEntry}->get_text();
  if (!defined($wseg) || $wseg eq '') {
    $gui->error_dialog(undef, "Apply segmentation:\n", "enter segmented text first!");
    return;
  }
  $wseg = Encode::decode($gui->{encoding},$wseg);

  ##-- add morphs
  my @morphs = split(/\Q$gui->{data}{analyses}{segsep}\E+/, $wseg);
  @{$gui->{data}{morphs}}{@morphs} = qw();


  ##-- set segmented word
  my $wl = $gui->{wordf}{list};
  my ($i) = $wl->get_selected_indices;
  return if (!defined($i));

  my ($word,$freq,$an,$wid) = @{$wl->{data}[$i]};
  $gui->{data}{w2seg}[$wid]  = $wseg;

  $wl->{data}[$i][2] = 1; ##-- mark as analyzed

  ##-- jump to next
  $gui->select_next_word();
  $gui->{editf}{manEditEntry}->activate();
}

sub select_next_word {
  my $gui = shift;
  my $wl  = $gui->{wordf}{list};
  my ($i) = $wl->get_selected_indices();
  if ($i < $#{$wl->{data}}) {
    $wl->unselect($i);
    $wl->select($i+1);
  }
}

sub edit_revoke_clicked {
  my $gui  = shift;

  ##-- set segmented word
  my $wl = $gui->{wordf}{list};
  my ($i) = $wl->get_selected_indices;
  return if (!defined($i));

  my ($word,$freq,$an,$wid) = @{$wl->{data}[$i]};
  $gui->{data}{w2seg}[$wid] = undef;
  $wl->{data}[$i][2] = 0; ##-- mark as un-analyzed
}


##======================================================================
## GUI: Dialogs
##======================================================================

##--------------------------------------------------------------
## GUI: Dialogs: Error
##--------------------------------------------------------------

## undef = $gui->error_dialog($title_or_undef, @message)
sub error_dialog {
  my ($gui,$title,@message) = @_;

  $title = (ref($gui).": Error") if (!$title);

  my $dlg = Gtk2::Dialog->new($title,
			      $gui->{main},
			      'destroy-with-parent',
			      'gtk-ok'=>'none');

  $dlg->vbox->pack_start(Gtk2::Label->new(join('',@message)), 1,1,10);
  $dlg->signal_connect(response=>sub { $_[0]->destroy; });

  $dlg->show_all;
}


##--------------------------------------------------------------
## GUI: Dialogs: Confirm
##--------------------------------------------------------------

## $gui->confirm_dialog($title,$callback,@message)
sub confirm_dialog {
  my ($gui,$title,$callback,@message) = @_;

  $title = (ref($gui).": Confirm") if (!$title);

  my $dlg = Gtk2::Dialog->new($title,
			      $gui->{main},
			      'destroy-with-parent',
			      'gtk-ok'=>'ok',
			      'gtk-cancel'=>'cancel');

  $dlg->vbox->pack_start(Gtk2::Label->new(join('',@message)), 1,1,10);
  $dlg->signal_connect(response=>sub {
			 $callback->() if ($_[1] eq 'ok');
			 $_[0]->destroy;
		       });

  $dlg->show_all;
}

##--------------------------------------------------------------
## GUI: Dialogs: File: Open
##--------------------------------------------------------------

## undef = $gui->file_open_dialog($key,$title,\&load_sub,@loadargs)
## + args:
##   $key   : key for the dialog
##   $title : title of the dialog
##   \&load_sub : sub ref for actual load operation
##   @loadargs  : args for loading
## + implied args:
##   $me->{files}{$key} : stores last filename selected by dialog with key $key
sub file_open_dialog {
  my ($gui,$key,$title,$loadsub,@loadargs) = @_;

  $title = (ref($gui).": $key: Open") if (!$title);
  my $dlg = Gtk2::FileChooserDialog->new($title,
					 $gui->{main},
					 'open',
					 'gtk-ok'=>'ok',
					 'gtk-cancel'=>'cancel');

  ##-- properties
  $dlg->set_filename($gui->{files}{$key}) if ($gui->{files}{$key});

  my ($file);
  $dlg->signal_connect(response=>sub {
			 if ($_[1] eq 'ok') {
			   $file = $_[0]->get_filename;
			   $gui->{files}{$key} = $file;
			   $loadsub->($file, @loadargs);
			 }
			 $_[0]->destroy;
		       });

  $dlg->show_all;
}

##--------------------------------------------------------------
## GUI: Dialogs: File: Save
##--------------------------------------------------------------

## undef = $gui->file_save_dialog($key,$title,$force,\&save_sub,@saveargs)
## + args:
##   $key   : key for the dialog
##   $title : title of the dialog
##   $force : clobber existing files without confirm?
##   \&save_sub : sub ref for actual save operation
##   @saveargs  : args for saving
## + implied args:
##   $me->{"${key}_filename"} : stores last filename selected by dialog with key $key
sub file_save_dialog {
  my ($gui,$key,$title,$force,$savesub,@saveargs) = @_;

  $title = (ref($gui).": $key: Save") if (!$title);
  my $dlg = Gtk2::FileChooserDialog->new($title,
					 $gui->{main},
					 'save',
					 'gtk-ok'=>'ok',
					 'gtk-cancel'=>'cancel');

  ##-- properties
  $dlg->set_filename($gui->{files}{$key}) if ($gui->{files}{$key});

  my ($file);
  $dlg->signal_connect(response=>sub {
			 if ($_[1] eq 'ok') {
			   $file = $_[0]->get_filename;
			   if (!$force && -f $file) {
			     $gui->confirm_dialog("${title}: Overwrite File?",
						  sub {
						    $gui->{files}{$key} = $file;
						    $savesub->($file,@saveargs);
						  },
						  "Overwrite old file\n\`$file' ?",
						 );
			   } else {
			     $gui->{files}{$key} = $file;
			     $savesub->($file,@saveargs);
			   }
			 }
			 $_[0]->destroy;
		       });
  $dlg->show_all;
}


##======================================================================
## Guts: I/O: Editor Project Data
##======================================================================

## undef = $gui->menu_project_open()
sub menu_project_open {
  my $gui = shift;

  $gui->file_open_dialog('project',
			(ref($gui)."::Open Project Data File"),
			sub {
			  my ($loaded);
			  if ( ($loaded = $gui->{data}->loadFile($_[0])) ) {
			    $gui->{data} = $loaded;
			  } else {
			    $gui->error_dialog(undef,"Error loading project data file\n", "'$_[0]'");
			  }
			  $gui->populateWordList();
			});
}

## undef = $gui->menu_project_save()
sub menu_project_save {
  my $gui = shift;
  $gui->file_save_dialog('project',
			 (ref($gui)."::Save Project Data File"),
			 0,
			 sub {
			   if (!$gui->{data}->saveFile($_[0])) {
			     $gui->error_dialog(undef,"Error saving project data file\n", "'$_[0]'");
			   }
			 });
}

## undef = $gui->menu_project_clear()
sub menu_project_clear {
  my $gui = shift;
  $gui->{data}->clear;
  $gui->{data}{analyses}->clear;
  $gui->populateWordList;
}

##======================================================================
## Guts: I/O: Analysis Map
##======================================================================

## undef = $gui->menu_analyses_load()
sub menu_analyses_load {
  my $gui = shift;

  $gui->file_open_dialog('analyses ',
			(ref($gui)."::Open Analysis Map File"),
			sub {
			  my ($loaded);
			  if ( ($loaded=$gui->{data}{analyses}->loadFile($_[0])) ) {
			    @{$gui->{data}{analyses}}{keys %$loaded} = values %$loaded;
			    $gui->populateWordList;
			  } else {
			    $gui->error_dialog(undef,"Error loading analysis map file\n", "'$_[0]'");
			  }
			});
}

## undef = $gui->menu_analyses_save()
sub menu_analyses_save {
  my $gui = shift;
  $gui->file_save_dialog('analyses',
			 (ref($gui)."::Save Analysis Map File"),
			 0,
			 sub {
			   if (!$gui->{data}{analyses}->saveFile($_[0])) {
			     $gui->error_dialog(undef,"Error saving analysis map file\n", "'$_[0]'");
			   }
			 });
}

## undef = $gui->menu_analyses_clear()
sub menu_analyses_clear {
  my $gui = shift;
  $gui->{data}{analyses}->clear;
}


##======================================================================
## Guts: I/O: Corpus
##======================================================================

## undef = $gui->menu_corpus_load();
sub menu_corpus_load {
  my $gui = shift;
  $gui->file_open_dialog('corpus_in',
			(ref($gui)."::Load Corpus File"),
			sub {
			  $gui->{data}->loadCorpus($_[0]);
			  $gui->populateWordList();
			});
}

## undef = $gui->menu_corpus_save();
*menu_corpus_save = apidummy('menu_corpus_save');

## undef = $gui->menu_corpus_clear()
sub menu_corpus_clear {
  my $gui = shift;
  $gui->{data}->clearCorpus;
  $gui->populateWordList;
}


##======================================================================
## Utilities: dummy API functions
##======================================================================
sub apidummy {
  my $name = shift;
  return sub {
    my $gui = shift;
    $gui->error_dialog(undef,
		       (ref($gui), "\n",
			"Function not yet implemented:\n",
			$name));
  };
}

1;

