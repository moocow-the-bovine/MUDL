##-*- Mode: CPerl -*-

## File: MUDL::Morph::Editor::Gtk.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Gtk morphological editors
##======================================================================

package MUDL::Morph::Editor::Gtk;
use MUDL::Object;
use MUDL::Morph::Editor;
use Encode;
use Gtk;

use strict;
our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $gui = MUDL::Morph::Editor::Gtk->new(%args)
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
			      default_size=>[640,480],
			      kwicn=>4,

			      ##-- Fonts
			      fonts=>
			      {
			       wordlist=>"-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-iso8859-1",
			       kwic_ctx=>"-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-iso8859-1",
			       kwic_key=>"-*-helvetica-bold-r-normal-*-14-*-*-*-*-*-iso8859-1",
			       label   =>"-*-helvetica-bold-r-normal-*-14-*-*-*-*-*-iso8859-1",
			       value   =>"-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-iso8859-1",
			      },

			      gdkfonts=>{}, ##-- fonts: low-level Gtk::Gdk::Font objects
			      styles=>{},   ##-- fonts: Gtk::Style objects

			      ##-- text encoding
			      encoding=>'ISO-8859-1',

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
  my $w = $gui->{main} = Gtk::Window->new('toplevel');

  ##--------------------------------------
  ## Styles, fonts, etc.
  $gui->create_fonts;
  $gui->create_colors;
  $gui->create_styles;

  ##--------------------------------------
  ## Main Window
  $w->signal_connect( 'destroy', sub { Gtk->exit( 0 ); } );
  $w->set_title(ref($gui));
  $w->set_default_size(@{$gui->{default_size}});

  ##--------------------------------------
  ## Main Window: VBox
  my $vbox = $w->{vbox} = Gtk::VBox->new(0,0);
  $vbox->show;
  $w->add($vbox);

  ##--------------------------------------
  ## Menu bar
  $gui->create_menubar();
  $vbox->pack_start($gui->{menubar},0,1,0);

  ##--------------------------------------
  ## Main Frame: pack logic
  my $vhp = $vbox->{hpaned} = Gtk::HPaned->new();
  $vbox->pack_start($vhp, 1,1,0);

  ##--------------------------------------
  ## Word-Type Frame
  my $wf = $gui->{wf} = Gtk::Frame->new;
  $wf->set_shadow_type("in");
  $wf->set_usize(200,0);
  $vhp->add1($wf);

  my $wvb = $wf->{vbox} = Gtk::VBox->new(0,0);
  $gui->create_word_clist();
  #$gui->create_word_filters();
  $wf->add($wvb);

  ##--------------------------------------
  ## Details Frame
  my $df = $gui->{df} = Gtk::Frame->new;
  $df->set_shadow_type("in");
  $vhp->add2($df);

  ##--------------------------------------
  ## Details Frame: notebook
  $gui->create_details_notebook;

  ##--------------------------------------
  ## Show all
  $gui->{main}->show_all;

  return $gui;
}

##--------------------------------------------------------------
## GUI: Initialization: Fonts, Colors, Styles

## undef = $gui->create_fonts()
##  + must precede create_styles()
sub create_fonts {
  my $gui = shift;
  foreach (keys(%{$gui->{fonts}})) {
    $gui->{gdkfonts}{$_} = Gtk::Gdk::Font->load($gui->{fonts}{$_});
  }
}

##--------------------------------------------------------------
## GUI: Initialization: Colors

## undef = $gui->create_colors()
sub create_colors { ; }

##--------------------------------------------------------------
## GUI: Initialization: Styles

## undef = $gui->create_styles()
##  + must have been preceeded by create_fonts()
sub create_styles {
  my $gui = shift;
  my ($style);
  foreach (keys(%{$gui->{gdkfonts}})) {
    $style = $gui->{styles}{$_} = Gtk::Style->new;
    $style->font($gui->{gdkfonts}{$_});
  }
}

##--------------------------------------------------------------
## GUI: Initialization: Menu bar

## $menu = $gui->create_menubar()
sub create_menubar {
  my $gui = shift;

  # This is the GtkItemFactoryEntry structure used to generate new menus.
  # Item 1: The menu path. The letter after the underscore indicates an
  #         accelerator key once the menu is open.
  # Item 2: The accelerator key for the entry
  # Item 3: The callback function.
  # Item 4: The callback action.  This changes the parameters with
  #         which the function is called.  The default is 0.
  # Item 5: The item type, used to define what kind of an item it is.
  #         Here are the possible values:
  #          NULL               -> "<Item>"
  #          ""                 -> "<Item>"
  #          "<Title>"          -> create a title item
  #          "<Item>"           -> create a simple item
  #          "<CheckItem>"      -> create a check item
  #          "<ToggleItem>"     -> create a toggle item
  #          "<RadioItem>"      -> create a radio item
  #          <path>             -> path of a radio item to link against
  #          "<Separator>"      -> create a separator
  #          "<Branch>"         -> create an item to hold sub items (optional)
  #          "<LastBranch>"     -> create a right justified branch 
  my @menu_items = 
    (
     ##-----------------------------------
     ## File
     {
      path => '/_File/_Open',
      accelerator => '<control>O',
      callback    => sub { $gui->menu_file_open; },
     },
     {
      path        => '/_File/_Save',
      accelerator => '<control>S',
      callback    => sub { $gui->menu_file_save(); },
     },
     { path=>'/File/sep1', type=>'<Separator>', },
     {
      path        => '/_File/_Quit',
      callback    => sub { Gtk->exit( 0 ); },
      accelerator => '<control>Q',
     },

     ##-----------------------------------
     ## Corpus
     {
      path => '/_Corpus/_Load',
      #accelerator => ''
      callback    => sub { $gui->menu_corpus_load; },
     },
     {
      path        => '/_Corpus/_Save',
      #accelerator => '',
      callback    => sub { $gui->menu_corpus_save; },
     },
     { path=>'/Corpus/sep1', type=>'<Separator>', },
     {
      path        => '/_Corpus/_Clear',
      callback    => sub { $gui->menu_corpus_clear },
      accelerator => '<control>C',
     },
    );

  ##-- get the main menubar
  my ($menubar,$ifactory,$agroup) = get_main_menubar($gui->{main},@menu_items);
  $gui->{menubar}        = $menubar;
  $menubar->{menu_items} = [@menu_items];
  $menubar->{factory}    = $ifactory;

  return $menubar
}

## ($menubar,$item_factory,$accel_group) = get_main_menbar($window, @menu_items)
sub get_main_menubar {
  my ($window,@menu_items) = @_;

  my $menubar;
  my $item_factory;
  my $accel_group;

  $accel_group = new Gtk::AccelGroup();

  # This function initializes the item factory.
  # Param 1: The type of menu - can be 'Gtk::MenuBar', 'Gtk::Menu',
  #          or 'Gtk::OptionMenu'.
  # Param 2: The path of the menu.
  # Param 3: The accelerator group.  The item factory sets up
  #          the accelerator table while generating menus.
  $item_factory = new Gtk::ItemFactory( 'Gtk::MenuBar',
					'<main>',
					$accel_group );

  # This function generates the menu items. Pass the item factory,
  # the number of items in the array, the array itself, and any
  # callback data for the the menu items.
  $item_factory->create_items( @menu_items );

  # Attach the new accelerator group to the window.
  $window->add_accel_group( $accel_group );

  # Finally, return the actual menu bar created by the item factory.
  #*menubar = gtk_item_factory_get_widget (item_factory, "<main>");
  return ( $item_factory->get_widget('<main>'), $item_factory, $accel_group );
}

##--------------------------------------------------------------
## GUI: Initialization: Word CList
sub create_word_clist {
  my $gui = shift;

  my $wf = $gui->{wf};
  my $wcl = $wf->{clist} = Gtk::CList->new_with_titles('Word','Freq');
  $wcl->reorderable(0);
  $wcl->set_column_justification(0,'left');
  $wcl->set_column_justification(1,'right');
  $wcl->set_column_width(0,128);
  $wcl->set_column_width(1,24);
  $wcl->set_selection_mode('single');

  ##-- word selection stuff
  $wcl->signal_connect('select_row', sub { $gui->select_word(@_); });

  ##-- sort stuff
  $wcl->{sort_column}=0;
  $wcl->{sort_type}='ascending';
  $wcl->set_auto_sort(0);
  $wcl->set_sort_column($wcl->{sort_column});
  $wcl->set_sort_type($wcl->{sort_type});
  $wcl->set_compare_func(\&word_clist_compare_func,$wcl->{sort_reverse},0);
  $wcl->signal_connect('click-column',
		       sub {
			 my ($wcl,$col) = @_;
			 if ($col==$wcl->{sort_column}) {
			   $wcl->{sort_type} = ($wcl->{sort_type} eq 'ascending'
						? 'descending'
						: 'ascending');
			 } else {
			   $wcl->{sort_column} = $col;
			 }
			 $wcl->set_sort_column($wcl->{sort_column});
			 $wcl->set_sort_type($wcl->{sort_type});
			 $wcl->set_compare_func(\&word_clist_compare_func,
						($col==1 ? 1 : 0));
			 $wcl->sort;
		       });

  my $scrolled = $wf->{clistScrolled} = Gtk::ScrolledWindow->new(undef,undef);
  $scrolled->set_policy('automatic','automatic');
  $scrolled->add($wcl);

  $wf->{vbox}->pack_start($scrolled, 1,1,0);
}

sub word_clist_compare_func {
  #my ($wcl,$a,$b,$is_numeric) = @_;
  return
    #($is_reversed
    # ? ($is_numeric ?  $b <=> $a : $b cmp $a)
    # : ($is_numeric ?  $a <=> $b : $a cmp $b));
    #($is_numeric ?  $a <=> $b : $a cmp $b)
    ($_[3] ? $_[1] <=> $_[2] : $_[1] cmp $_[2])
      ;
}


##--------------------------------------------------------------
## GUI: Initialization: Details notebook
sub create_details_notebook {
  my $gui = shift;
  my $df  = $gui->{df};
  my $nb  = $df->{nb} = Gtk::Notebook->new;
  $nb->set_tab_pos('top');
  $nb->border_width(10);
  $nb->set_show_tabs(1);
  $nb->set_scrollable(1);
  #$nb->realize;
  $df->add($nb);

  ##-- filters
  $nb->append_page($gui->create_word_filters,
		   Gtk::Label->new('Filters'));

  ##-- contexts
  $nb->append_page($gui->create_word_contexts(),
		   Gtk::Label->new('Contexts'));
}

##--------------------------------------------------------------
## GUI: Initialization: Word Filters
sub create_word_filters {
  my ($gui) = shift;

  my $wff = $gui->{filters} = Gtk::Frame->new();
  $wff->border_width(5);
  $wff->set_shadow_type('none');

  my $vbox = $wff->{vbox} = Gtk::VBox->new();
  $vbox->border_width(5);
  $wff->add($vbox);

  my $wfftab = $wff->{tab} = Gtk::Table->new(3, 2);
  $vbox->pack_start($wfftab, 1,1,0);

  ##-- Minimum Frequency
  my $wff_min_adj  = $wff->{min_adj}  = Gtk::Adjustment->new(0.0, 0.0, 1e38, 1.0, 10.0, 0.0);
  my $wff_min_spin = $wff->{min_spin} = Gtk::SpinButton->new($wff_min_adj, 1.0, 0);
  $wff_min_spin->set_wrap(0);
  $wff_min_spin->set_usize(100,0);
  $wff_min_spin->set_update_policy('always'); ##???
  $gui->labelled_widget($wfftab, 0, "Min Freq:", $wff_min_spin);
  #$wff_min_adj->signal_connect(value_changed=>sub { print "value changed: ", $_[0]->get_value, "\n"; });

  ##-- Maximum Frequency
  my $wff_max_adj  = $wff->{max_adj}  = Gtk::Adjustment->new(0.0, 0.0, 1e38, 1.0, 10.0, 0.0);
  my $wff_max_spin = $wff->{max_spin} = Gtk::SpinButton->new($wff_max_adj, 1.0, 0);
  $wff_max_spin->set_wrap(0);
  $wff_max_spin->set_usize(100,0);
  $wff_max_spin->set_update_policy('always'); ##???
  $gui->labelled_widget($wfftab, 1, "Max Freq:", $wff_max_spin);

  ##-- Regex
  my $wff_regex = $wff->{regex} = Gtk::Entry->new();
  $gui->labelled_widget($wfftab, 2, 'Regex', $wff_regex);

  ##-- ButtonBox
  my $bbox = $wff->{bbox} = Gtk::HButtonBox->new();
  $bbox->set_layout_default('end');
  $vbox->pack_start($bbox,0,0,5);

  my $b_apply = $bbox->{apply} = Gtk::Button->new('Apply');
  $b_apply->signal_connect('clicked', sub { $gui->apply_word_filters(@_); });
  $bbox->add($b_apply);

  return $wff;
}

##--------------------------------------------------------------
## GUI: Initialization: Word Contexts
sub create_word_contexts {
  my $gui = shift;

  my $cf = $gui->{contexts} = Gtk::Frame->new();
  $cf->border_width(5);
  $cf->set_shadow_type('none');

  my $vbox = $cf->{vbox} = Gtk::VBox->new();
  $vbox->border_width(5);
  $cf->add($vbox);

  my $kwicn = $gui->{kwicn};

  ##-- create KWIC CList
  #my $kwic = $cf->{kwic} = Gtk::CList->new($kwicn*2 + 1);
  my $kwic = $cf->{kwic} = Gtk::CList->new_with_titles(
						       (map { "i-".($kwicn-$_+1) } (1..$kwicn)),
						       "i",
						       (map { "i+$_" } (1..$kwicn)),
						      );

  $kwic->reorderable(0);
  $kwic->set_column_justification($_,'right') foreach (0..($kwicn-1));
  $kwic->set_column_justification($kwicn,'center');
  $kwic->set_column_justification($_,'left') foreach (($kwicn+1)..($kwicn*2));
  $kwic->set_selection_mode('extended');

  ##-- ... scroll the KWIC CList
  my $scrolled = $cf->{kwicScrolled} = Gtk::ScrolledWindow->new(undef,undef);
  $scrolled->set_policy('automatic','automatic');
  $scrolled->add($kwic);
  $vbox->pack_start($scrolled, 1,1,5);

  return $cf;
}

##======================================================================
## Gtk Utilities: Labelled Widgets
##======================================================================

## ($label,$value) = $wg->label_value_pair($table,$row,$label_text,$value_text,%args)
sub label_value_pair {
  my ($gui,$table,$row,$labtxt,$valtxt, %args) = @_;

  my $val = Gtk::Label->new($valtxt);
  $val->set_style($gui->{styles}{value});
  $val->set_justify('left');
  $val->set_alignment(0.0, 0.5);

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

  my $lab = Gtk::Label->new($labtxt);
  $lab->set_style($gui->{styles}{$args{labstyle}});

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
## Gtk Utilities: Dialogs
##======================================================================

##--------------------------------------------------------------
## Callback: Destroy Dialog
##--------------------------------------------------------------

## undef = $gui->destroy_dialog(@window_refs_and_gui_keys)
sub destroy_dialog {
  my $gui = shift;
  my ($ref,$key);
  foreach (@_) {
    next if (!defined($_));
    if (ref($_)) { ($ref,$key) = ($_,undef); }
    else         { ($ref,$key) = (\$gui->{$_},$_); }

    #print "destroy: ref=$ref ; key=", (defined($key) ? $key : '(undef)'), "\n";

    delete($gui->{$key}) if (defined($key));
    %{$$ref} = qw() if (ref($$ref) && UNIVERSAL::isa($$ref,'HASH'));
    undef($$ref) if (defined($$ref));
  }
  0;
}

##--------------------------------------------------------------
## Callback: Generic Dialog
##--------------------------------------------------------------

## $dlg = $gui->generic_dialog($key,%args)
##  + $key = $gui_key,
##  + %args:
##    title => $title,
##    ref => $scalar_reference,
##    class => $dialog_class,
##    newargs=>$args_to_new()
sub generic_dialog {
  my ($gui,$key,%args) = @_;

  if (!defined($gui->{$key})) {
    $args{class} = 'Gtk::Dialog' if (!$args{class});
    $gui->{$key} = $args{class}->new($args{newargs} ? @{$args{newargs}} : qw());
    $gui->{$key}->signal_connect("destroy",
				 sub { $gui->destroy_dialog($args{ref},$key); });
    $gui->{$key}->signal_connect("delete_event",
				 sub { $gui->destroy_dialog($args{ref},$key); });
    $gui->{$key}->set_title($args{title} ? $args{title} : $args{class});
  }
  #if (!$gui->{$key}->visible) {
  #  $gui->{$key}->show;
  #} else {
  #  $gui->{$key}->destroy;
  #}

  ${$args{ref}} = $gui->{$key} if (defined($args{ref}) && ref($args{ref}));
  return $gui->{$key};
}

##--------------------------------------------------------------
## Callback: Error Dialog
##--------------------------------------------------------------

## $gui->error_dialog($title,@message)
sub error_dialog {
  my ($gui,$title,@message) = @_;

  my ($dlg);
  $gui->generic_dialog('error_dialog', ref=>\$dlg,
		      title=>($title ? $title : (ref($gui).'::Error'))
		     );
  $dlg->set_default_size(300,100);
  $dlg->border_width(5);
  $dlg->show;

  my ($b);
  $b = $dlg->{dismiss} = Gtk::Button->new("Dismiss");
  $b->signal_connect("clicked", sub { $dlg->destroy });
  $b->can_default(1);
  $b->show;
  $dlg->action_area->add($b);

  ##-- hbox
  my $hbox = $dlg->{hbox} = Gtk::HBox->new(0,5);
  $hbox->show;
  $dlg->vbox->pack_start($hbox,1,1,5);

  ##-- pixmap
  #my $pix = $dlg->{pix} = Gtk::Pixmap->new(@{$gui->{gdk_pixmaps}{error}});
  #$pix->show;
  #$hbox->pack_start($pix,0,0,5);

  ##-- subbox
  my $labbox = $dlg->{labbox} = Gtk::HBox->new(0,5);
  $labbox->show;
  $hbox->pack_start($labbox, 1,1,5);

  ##-- error message
  my $lab = Gtk::Label->new(join('',@message));
  $lab->set_line_wrap(1);
  $lab->show;
  $labbox->pack_start($lab, 1,1,5);

  ##-- select the default button
  $dlg->{dismiss}->signal_emit('focus',1);
  $dlg->show_all;

  return 0;
}

##--------------------------------------------------------------
## Callback: Confirm Dialog
##--------------------------------------------------------------

## $gui->confirm_dialog($key,$title,$callback,@message)
sub confirm_dialog {
  my ($gui,$key,$title,$callback,@message) = @_;

  my ($dlg);
  $gui->generic_dialog($key,
		      ref=>\$dlg,
		      title=>($title ? $title : (ref($gui).'::Confirm'))
		     );
  $dlg->set_default_size(300,100);
  $dlg->border_width(5);
  $dlg->show;

  my ($b);
  ##-- button: cancel
  $b = $dlg->{cancel} = Gtk::Button->new("Cancel");
  $b->signal_connect("clicked", sub { $dlg->destroy });
  $b->can_default(1);
  $b->show;
  $dlg->action_area->add($b);

  ##-- button: ok
  $b = $dlg->{ok} = Gtk::Button->new("OK");
  $b->signal_connect("clicked", sub { &$callback(); $dlg->destroy; });
  $b->can_default(1);
  $b->show;
  $dlg->action_area->add($b);

  ##-- hbox
  my $hbox = $dlg->{hbox} = Gtk::HBox->new(0,5);
  $hbox->show;
  $dlg->vbox->pack_start($hbox,1,1,5);

  ##-- pixmap
  #my $pix = $dlg->{pix} = Gtk::Pixmap->new(@{$gui->{gdk_pixmaps}{question}});
  #$pix->show;
  #$hbox->pack_start($pix,0,0,5);

  ##-- subbox
  my $labbox = $dlg->{labbox} = Gtk::HBox->new(0,5);
  $labbox->show;
  $hbox->pack_start($labbox, 1,1,5);

  ##-- error message
  my $lab = Gtk::Label->new(join('',@message));
  $lab->set_line_wrap(1);
  $lab->show;
  $labbox->pack_start($lab, 1,1,5);

  ##-- select the default button
  $dlg->{ok}->signal_emit('focus',1);
  $dlg->show_all;
}

##--------------------------------------------------------------
## Callback: File Open Dialog
##--------------------------------------------------------------

## undef = $gui->file_open_dialog($key,$title,\&load_sub,@loadargs)
## + args:
##   $key   : key for the dialog
##   $title : title of the dialog
##   \&load_sub : sub ref for actual load operation
##   @loadargs  : args for loading
## + implied args:
##   $me->{"${key}_filename"} : stores last filename selected by dialog with key $key
sub file_open_dialog {
  my ($gui,$key,$title,$loadsub,@loadargs) = @_;

  ##-- dialog
  my ($fs);
  $gui->generic_dialog($key,
		      ref=>\$fs,
		      class=>'Gtk::FileSelection',
		      title=>$title,
		      newargs=>[''],
		     );

  ##-- properties
  my $filekey = "${key}_filename";
  $fs->set_filename($gui->{$filekey}) if ($gui->{$filekey});

  ##-- buttons
  my $file = undef;
  $fs->cancel_button->signal_connect('clicked', sub { $fs->destroy; });
  $fs->ok_button->signal_connect('clicked',
				 sub {
				   $file = $fs->get_filename;
				   $gui->{$filekey} = $file;
				   $loadsub->($file,@loadargs);
				   $fs->destroy;
				 });

  $fs->show;
}

## undef = $gui->file_save_dialog($key,$title,$do_force,\&savesub,@saveargs)
## + args:
##   $key   : key for the dialog
##   $title : title of the dialog
##   $do_force  : force overwrite?
##   \&save_sub : sub ref for actual save operation
##   @saveargs  : additional args for \&savesub
## + implied args:
##   $gui->{"${key}_filename"} : stores last filename selected by dialog with key $key
sub file_save_dialog {
  my ($gui,$key,$title,$force,$savesub,@saveargs) = @_;
  ##-- dialog
  my ($fs);
  $gui->generic_dialog($key,
		      ref=>\$fs,
		      class=>'Gtk::FileSelection',
		      title=>$title,
		      newargs=>[''],
		     );

  ##-- properties
  my $filekey = "${key}_filename";
  $fs->set_filename($gui->{$filekey}) if ($gui->{$filekey});

  ##-- buttons
  my $file = undef;
  $fs->cancel_button->signal_connect('clicked', sub { $fs->destroy; });
  $fs->ok_button->signal_connect('clicked',
				 sub {
				   $file = $fs->get_filename;
				   $gui->{$filekey} = $file;
				   if (!$force && -f $file) {
				     $gui->confirm_dialog("${key}_confirm",
							 "${title}: Overwrite File?",
							 sub { $savesub->($file,@saveargs); },
							 "Overwrite old file\n",
							 "\`$file\' ?",
							);
				   } else {
				     $savesub->($file,@saveargs);
				   }
				   $fs->destroy;
				 });

  $fs->show;
}

##======================================================================
## Guts: populate word list
##======================================================================

## undef = $gui->populateWordList()
sub populateWordList {
  my $gui = shift;

  ##-- gui objects
  my $wcl = $gui->{wf}{clist};
  my $fmin  = $gui->{filters}{min_adj}->get_value;
  my $fmax  = $gui->{filters}{max_adj}->get_value;
  my $fre   = $gui->{filters}{regex}->get_text;

  ##-- data objects
  my $me = $gui->{data};
  my $wenum = $me->{wenum};
  my $wfreq = $me->{wfreq};

  ##-- clear gui
  $wcl->clear;
  $gui->{contexts}{kwic}->clear;

  ##-- add all words (filtering)
  my ($wid,$word,$wordfreq,$rowi);
  foreach $wid (0..$#{$wenum->{id2sym}}) {
    my $widr = $wid;
    $word = $wenum->{id2sym}[$wid];
    $word = Encode::encode($gui->{encoding}, $word) if (defined($gui->{encoding}));
    $wordfreq = $wfreq->{$wid};

    next if ( ($fmin > 0 && $wordfreq < $fmin)
	      ||
	      ($fmax > 0 && $wordfreq > $fmax)
	      ||
	      ($fre ne '' && $word !~ m/$fre/) );

    $rowi = $wcl->append($word, $wfreq->{$wid});
    $wcl->set_row_data($rowi, \$widr);
  }

  ##-- sort clist
  $wcl->sort;
}

##======================================================================
## Guts: select word (populate KWIC)
##======================================================================

sub select_word {
  my ($gui,$wcl,$rowi) = @_;

  my $wid = ${$wcl->get_row_data($rowi)};
  my $corpus = $gui->{data}{corpus};
  my $wenum  = $gui->{data}{wenum};
  my @occs = unpack('(LS)*', $gui->{data}{woccs}{$wid});

  ##-- clear kwic table
  my $kwic = $gui->{contexts}{kwic};
  my $kwicn = $gui->{kwicn};
  $kwic->clear;

  ##-- get styles
  my $style_ctx = $gui->{styles}{kwic_ctx};
  my $style_key = $gui->{styles}{kwic_key};

  ##-- build new kwic table
  my ($word,$sentid,$occid,@sent,@kwic,$kwicid);
  while (@occs) {
    ($sentid,$occid) = splice(@occs,0,2);
    @sent = $corpus->{sents}[$sentid]->list;

    @kwic = map {
      ($_ < 0 || $_ > $#sent
       ? ''
       : ($gui->{encoding}
	  ? Encode::encode($gui->{encoding}, $wenum->{id2sym}[$sent[$_]])
	  : $wenum->{id2sym}[$sent[$_]]))
    } (($occid-$kwicn)..($occid+$kwicn));

    $kwicid = $kwic->append(@kwic);
    $kwic->set_cell_style($kwicid, $_, ($_==$kwicn ? $style_key : $style_ctx))
      foreach (0..$#kwic);

    my $rowdata = pack('LS', $sentid, $occid);
    $kwic->set_row_data($kwicid,\$rowdata);
  }

  ##-- now resize kwic columns
  $kwic->columns_autosize;

  ##-- ... & select all of 'em
  $kwic->select_all;
}



##======================================================================
## Guts: View: filter word list
##======================================================================

## undef = $gui->apply_word_filters(@event_args)
sub apply_word_filters {
  my $gui = shift;
  $gui->populateWordList();
}


##======================================================================
## guts: I/O: Editor Data
##======================================================================

## undef = $gui->menu_file_open()
*menu_file_open = apidummy('menu_file_open');

## undef = $gui->menu_file_save()
*menu_file_save = apidummy('menu_file_save');

##======================================================================
## Guts: I/O: Corpus
##======================================================================

## undef = $gui->menu_corpus_load();
sub menu_corpus_load {
  my $gui = shift;
  $gui->file_open_dialog('dlg_corpus_load',
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
		       (ref($gui).": $name(): not yet implemented!"));
  };
}
