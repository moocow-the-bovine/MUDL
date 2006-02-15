##-*- Mode: CPerl -*-

## File: MUDL::Morph::Editor::Gtk2.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
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
  $w->set_size_request(@{$gui->{default_size}});

  ##--------------------------------------
  ## Main Window: VBox
  my $vbox = $w->{vbox} = Gtk2::VBox->new(0,0);
  $vbox->show;
  $w->add($vbox);

  ##--------------------------------------
  ## Menu bar
  $gui->create_menubar();
  #$vbox->pack_start($gui->{menubar},0,1,0);
  $vbox->pack_start($gui->{menu}{widget},0,1,0);

  ##--------------------------------------
  ## Main Frame: pack logic
  my $vhp = $vbox->{hpaned} = Gtk2::HPaned->new();
  $vbox->pack_start($vhp, 1,1,5);

  ##--------------------------------------
  ## Word-Type Frame
  my $wf = $gui->{wordf} = Gtk2::Frame->new('Word Types');
  $wf->set_shadow_type("etched-in");
  $vhp->add1($wf);

  #$gui->create_word_frame();

  ##--------------------------------------
  ## Details Frame
  my $df = $gui->{detailsf} = Gtk2::Frame->new;
  $df->set_shadow_type("none");
  $vhp->add2($df);

  ##--------------------------------------
  ## Details Frame
  #$gui->create_details_frame;

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
		 _Open => {
			   callback => sub { $gui->menu_file_open; },
			   accelerator=>'<ctrl>O',
			  },

		 ##-- File: Save
		 _Save => {
			   callback => sub { $gui->menu_file_save; },
			   accelerator=>'<ctrl>S',
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
			    accelerator=>'<ctrl>C',
			   },
		],
     },

     ##-------------------------
     ## Menu: Morphology
     _Morphology =>
     {
      item_type=>'<Branch>',
      children=>[
		##-- Morphology: Load FST
		 '_Load FST' => {
				 callback => sub { $gui->menu_morph_fst_load; },
				 #accelerator=>'<ctrl>L',
				},

		 ##-- Morphology: Load Labels
		 '_Load Labels'  => {
				     callback => sub { $gui->menu_morph_labs_load; },
				     #accelerator=>'<ctrl>S',
				    },

		 ##-- Morphologys: Separator
		 Separator => { item_type=>'<Separator>' },

		 ##-- Morphlogy: Clear
		 _Clear => {
			    callback => sub { $gui->menu_morph_clear; },
			    #accelerator=>'<ctrl>Q',
			   },
		],
     },

    ];


  my $menu = $gui->{menu} = Gtk2::SimpleMenu->new(menu_tree=>$menu_tree);

  #return $menu;
}

##======================================================================
## GUI: Guts: populateWordList
##======================================================================

*populateWordList = apidummy('populateWordList');

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
## guts: I/O: Editor Data
##======================================================================

## undef = $gui->menu_file_open()
sub menu_file_open {
  my $gui = shift;

  $gui->file_open_dialog('data',
			(ref($gui)."::Open Data File"),
			sub {
			  if (!($gui->{data} = $gui->{data}->loadFile($_[0]))) {
			    $gui->error_dialog(undef,"Error loading data file\n", "'$_[0]'");
			  }
			  $gui->populateWordList();
			});
}

## undef = $gui->menu_file_save()
sub menu_file_save {
  my $gui = shift;
  $gui->file_save_dialog('data',
			 (ref($gui)."::Save Data File"),
			 0,
			 sub {
			   if (!$gui->{data}->saveFile($_[0])) {
			     $gui->error_dialog(undef,"Error saving data file\n", "'$_[0]'");
			   }
			 });
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
## Guts: I/O: Morphology
##======================================================================

sub menu_morph_fst_load {
  my $gui = shift;

  my $fst = $gui->{data}{fst};
  $fst = $gui->{data}{fst} = MUDL::Gfsm::Automaton->new() if (!$fst);

  $gui->file_open_dialog('morph_fst',
			(ref($gui)."::Load Morphology FST"),
			sub {
			  if (!$fst->load($_[0])) {
			    $gui->error_dialog(undef, "Error loading morphology FST file:\n", $_[0]);
			  }
			});
}

sub menu_morph_labs_load {
  my $gui = shift;
  my $labs = $gui->{data}{labs};

  $labs = $gui->{data}{labs} = MUDL::Gfsm::Alphabet->new() if (!$labs);

  $gui->file_open_dialog('morph_labs',
			(ref($gui)."::Load Morphology Labels"),
			sub {
			  if (!$labs->load($_[0])) {
			    $gui->error_dialog(undef, "Error loading morphology labels file:\n", $_[0]);
			  }
			});
}

sub menu_morph_clear {
  my $gui = shift;
  $gui->{data}{fst}->clear if ($gui->{data}{fst});
  $gui->{data}{labs}->clear if ($gui->{data}{labs});
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
