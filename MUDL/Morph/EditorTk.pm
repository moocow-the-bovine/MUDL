##-*- Mode: CPerl -*-

## File: MUDL::Morph::Editor.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: Tk morphological editors
##======================================================================

package MUDL::Morph::Editor;
use MUDL::Object;
use MUDL::Corpus::Buffer::Pdl;
use MUDL::Enum;
use MUDL::EDist;
use PDL;

use Tk;
use utf8;
use Encode;

use strict;

our @ISA = qw(MUDL::Object);

##======================================================================
## Constructor
## $me = MUDL::Morph::Editor->new(%args)
## + %args, structure:
##    ##-- menu bar
##    menu => {
##             frame=>$mbar_frame,
##             file=>$file_menu,
##             ##...
##             help=>$help_menu,
##            },
##
sub new {
  my $that = shift;
  my $me = $that->SUPER::new(
			     ##-- look & feel
			     font=>'helvetica -14',
			     labfont=>'helvetica -14 bold',
			     kwicfont=>'courier -14',
			     encoding=>'ISO-8859-1',
			     kwicn=>4,

			     ##-- data
			     wenum=>MUDL::Enum->new(),   ##-- word enum
			     wfreq=>MUDL::EDist->new(),  ##-- word-id unigram frequencies
			     woccs=>{},                  ##-- maps word ids to occurrence indices, as
			                                 ##  $occs = pack('(LS)*', $sentidx,$occidx, ...)
			     corpus=>undef, ##-- a MUDL::Corpus::Buffer::Pdl

			     ##-- User options
			     @_,
			    );

  ##-- generate corpus
  $me->{corpus} = MUDL::Corpus::Buffer::Pdl->new(txtenum=>$me->{wenum},
						 dobos=>0,
						 doeos=>0,
						 dobash=>0,
						);

  return $me;
}

##======================================================================
## View
## undef = $me->view(%args)
##  + %args:
##      loop=>$bool,  # do/don't enter main loop
##      #file=>$file,  # initial tree file
sub view {
  my ($me,%args) = @_;

  #--------------------------------------
  # Main Window
  my $w = $me->{main} = Tk::MainWindow->new();
  $w->title(ref($me));

  #--------------------------------------
  # Menus
  my $mb = $me->{menu}{frame} = $w->Frame(-relief=>'raised',-bd=>2,Name=>'menubar');

  #--------------------
  # Menus: File
  my $mbf = $me->{menu}{file}  = $mb->Menubutton(-text=>'File', Name=>'file', -underline=>0);
  $mbf->pack(-side=>'left');

  ##-- Menus: File: Open
  $mbf->menu->add('command',
		  -label=>'Open',
		  -accelerator=>'Ctrl+o',
		  -command=>sub {$me->menu_open},
		 );
  $w->bind('all', '<Control-KeyPress-o>', sub { $me->menu_open });

  ##-- Menus: File: Save
  $mbf->menu->add('command',
		  -label=>'Save',
		  -accelerator=>'Ctrl+s',
		  -command=>sub {$me->menu_save},
		 );
  $w->bind('all', '<Control-KeyPress-s>', sub { $me->menu_save });

  ##-- Menus: File: Quit
  $mbf->menu->add('separator');
  $mbf->menu->add('command',
		  -label=>'Quit',
		  -accelerator=>'Ctrl+q',
		  -command=>sub {$w->destroy});

  #--------------------
  # Menus: Options
  #my $mbo = $me->{menu}{options}  = $mb->Menubutton(-text=>'Options', Name=>'options', -underline=>0);
  #$mbo->pack(-side=>'left');

  #--------------------
  # Menus: Corpus
  my $mbcorpus = $me->{menu}{corpus}  = $mb->Menubutton(-text=>'Corpus', Name=>'corpus', -underline=>0);
  $mbcorpus->pack(-side=>'left');

  ##-- Menus: Corpus: Load
  $mbcorpus->menu->add('command',
		       -label=>'Load',
		       -accelerator=>'Ctrl+L',
		       -command=>sub {$me->menu_corpus_load},
		      );
  $w->bind('all', '<Control-KeyPress-l>', sub { $me->menu_corpus_load });

  ##-- Menus: Corpus: Save
  $mbcorpus->menu->add('command',
		       -label=>'Save',
		       -command=>sub {$me->menu_corpus_save},
		      );

  ##-- Menus: Corpus: Clear
  $mbcorpus->menu->add('command',
		       -label=>'Clear',
		       -command=>sub {$me->clearCorpus},
		      );


  #--------------------
  # Menus: Top, Bindings
  $mb->pack(-side=>'top', -fill=>'x');

  #--------------------
  # Main frame
  my $mframe = $me->{mframe} = $w->Frame(-relief=>'solid',
					 #-bg=>'red',
					);
  $mframe->pack(-side=>'top',-fill=>'both',-expand=>1);

  #--------------------
  # Word-Frame
  my $wframe = $me->{wframe} = $mframe->Frame(-relief=>'groove',
					      #-bg=>'blue',
					     );
  #--------------------
  # Word-Frame: Caption
  my $wlabel = $me->{wframe}{label} = $wframe->Label(-relief=>'groove',
						    -text=>'Words',
						    -font=>$me->{labfont},
						   );
  $wlabel->pack(-side=>'top',-fill=>'x',-expand=>0);


  #--------------------
  # Word-Frame: Word-list
  my $wlist = $me->{wlist} = $wframe->Scrolled('Listbox',
					       -bg=>'white',
					       -fg=>'black',
					       -width=>30,
					       -height=>20,
					       -scrollbars=>'se',
					       -font=>$me->{font},
					       -takefocus=>1,
					      );

  $wlist->bind('<Button-1>', sub { $me->selectWord($wlist->curselection) });
  $wlist->bind('<KeyPress-space>', sub { $me->selectWord($wlist->curselection) });
  $wlist->pack(-side=>'top', -fill=>'both', -expand=>1);

  #--------------------
  # Word-Frame: Pack
  $wframe->pack(-side=>'left',-fill=>'y', -expand=>0);

  #--------------------
  # Details Frame
  my $dframe = $me->{dframe} = $mframe->Frame(-relief=>'groove',
					      #-bg=>'green',
					     );
  #--------------------
  # Details Frame: KWIC
  my $kwic = $me->{kwic} = $dframe->Scrolled('Listbox',
					      -bg=>'white',
					      -fg=>'black',
					      -width=>60,
					      -height=>10,
					      -scrollbars=>'se',
					      -font=>$me->{kwicfont},
					      -takefocus=>1,
					     );
  $me->{kwic}{label} = $dframe->Label(-text=>'KWIC',-relief=>'groove',-font=>$me->{labfont});
  $me->{kwic}{label}->pack(-side=>'top',-fill=>'x');
  $kwic->pack(-side=>'top', -fill=>'both', -expand=>1);


  #--------------------
  # Details Frame: pack
  $dframe->pack(-side=>'left',-fill=>'both',-expand=>1);

  #--------------------------------------
  # Guts
  Tk::MainLoop if (!defined($args{loop}) || $args{loop});
}

##======================================================================
## $me->redraw()
##  + to be called on corpus change
sub redraw {
  my $me = shift;

  ##-- clear & re-fill word list
  $me->{wlist}->delete(0,'end');
  $me->{wlist}{i2id} = [] if (!$me->{wlist}{i2id});
  @{$me->{wlist}{i2id}} = qw();
  foreach (sort @{$me->{wenum}{id2sym}}) {
    push(@{$me->{wlist}{i2id}}, $me->{wenum}{sym2id}{$_});
    $me->{wlist}->insert('end', Encode::encode($me->{encoding},$_));
  }
}

##======================================================================
## Word list
##======================================================================

## $me->selectWord($wlist_index)
sub selectWord {
  my ($me,$idx) = @_;
  my $wid = $me->{wlist}{i2id}[$idx];

  ##-- clear & re-fill KWIC table
  $me->{kwic}->delete(0,'end');
  my (@cl,@cr,$word,$sentid,$occid,@sent,$lenl,$lenr);
  my @occs = unpack('(LS)*', $me->{woccs}{$wid});
  my @kwics = qw(); ##-- list of [ @left_context_strings, $word_string, @right_context_strings ]

  while (@occs) {
    ($sentid,$occid) = splice(@occs,0,2);
    @sent = $me->{corpus}{sents}[$sentid]->list;

    @cl = map { ($_ < 0      ? '' : Encode::encode($me->{encoding},$me->{wenum}{id2sym}[$sent[$_]])) }
      (($occid-$me->{kwicn})..($occid-1));
    $word = Encode::encode($me->{encoding},$me->{wenum}{id2sym}[$sent[$occid]]);
    @cr = map { ($_ > $#sent ? '' : Encode::encode($me->{encoding},$me->{wenum}{id2sym}[$sent[$_]])) }
      (($occid+1)..($occid+$me->{kwicn}));

    push(@kwics, [@cl,$word,@cr]);
  }

  ##-- get lengths
  my @lens = qw();
  my ($kwic,$i);
  foreach $kwic (@kwics) {
    foreach $i (0..$#$kwic) {
      $lens[$i] = length($kwic->[$i]) if (!defined($lens[$i]) || length($kwic->[$i]) > $lens[$i]);
    }
  }

  ##-- add it
  foreach $kwic (@kwics) {
    $me->{kwic}->insert('end',
			join(' ',
			     (map { sprintf("%${lens[$_]}s", $kwic->[$_]) } (0..($me->{kwicn}-1))),
			     sprintf(" *%-".$lens[$me->{kwicn}]."s* ", $kwic->[$me->{kwicn}]),
			     (map { sprintf("%-${lens[$_]}s", $kwic->[$_]) } (($me->{kwicn}+1)..$#$kwic)),
			    ));
  }

}

##======================================================================
## I/O: Corpus
##======================================================================

## $me->menu_corpus_load()
sub menu_corpus_load {
  my $me = shift;
  my $file = $me->{main}->getOpenFile(
				      -title=>'Load Corpus File',
				      -defaultextension=>'.t',
				      -filetypes=>
				      [
				       ['Corpus Files', [qw(.t .tt .t.xml .tt.xml)]],
				       ['All Files', '*'],
				      ],
				     );
  return if (!defined($file));
  $me->loadCorpus($file);
}

## $me->loadCorpus($file,%args)
##  + load corpus into the object
##  + populates $me->{wenum}, $me->{wfreq}, ...?
sub loadCorpus {
  my ($me,$file,%args) = @_;
  my $cr = MUDL::CorpusIO->fileReader($file);
  my $wenum = $me->{wenum};
  my $wfreq = $me->{wfreq};
  my $csents = $me->{corpus}{sents};
  my $woccs = $me->{woccs};
  my ($sent,$tok,@tokids);
  my $sentid = $#$csents+1;
  while (defined($sent=$cr->getSentence)) {
    @tokids = map { $wenum->addSymbol(ref($_) ? $_->text : $_) } @$sent;
    $wfreq->{$_}++ foreach (@tokids);
    push(@$csents, pdl(long,@tokids));
    $woccs->{$tokids[$_]} .= pack('LS', $sentid, $_) foreach (0..$#tokids);
    ++$sentid;
  }
  $me->redraw();
}

## $me->clearCorpus($file,%args)
##  + clear object contents
sub clearCorpus {
  my $me = shift;
  $me->{wenum}->clear;
  $me->{wfreq}->clear;
  %{$me->{woccs}} = qw();
  $me->{corpus}->clear;
  $me->{kwic}->delete(0,'end');
  $me->redraw;
}
