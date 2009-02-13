##-*- Mode: CPerl -*-

## File: MUDL::Make.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: makefile stuff
##======================================================================

package MUDL::Make;
BEGIN { print STDERR __PACKAGE__  , " loading...\n"; }
use Cwd qw(abs_path);
use Digest::MD5 qw(md5);
use MUDL::Object;
use MUDL::Make::Fields qw(:all);
use MUDL::Make::Table;
use MUDL::Make::Plot;
use MUDL::Make::Plot3d;
use Text::Wrap qw();
use strict;
use Carp;
our @ISA = qw(MUDL::Object Exporter);

##======================================================================
## Globals

##---------------------------------------------------------------
## Globals: Action Table
##  %ACTIONS = { $actName => \%actHash, ... }
##   + %actHash keys:
##       syntax=>$help_syntax,
##       help=>$one_line_help_string,
##       code=>\&code,                  ##-- called as $code->($mak,$actName,$actArgStr)
##       ...
##   + data: see below
our %ACTIONS = qw();

##---------------------------------------------------------------
## Globals: programs

#our $MAKE  = 'env -i make'; ##-- PDL doesn't like empty environment at all (kira)
our $MAKE  = 'make';

##---------------------------------------------------------------
## Globals: Verbosity levels

our %VLEVELS =
  (
   'silent'=>0,
   'error'=>1,
   'warn'=>2,
   'info'=>3,
   'debug'=>255,

   'DEFAULT'=>3,
  );

##---------------------------------------------------------------
## Globals: Exporter
our %EXPORT_TAGS =
  (
   'programs'=>[qw($MAKE)],
   'progs'=>[qw($MAKE)],   ##-- backwards-compatibility

   'vlevels'=>[qw(%VLEVLES)],
   #'fields'=>[qw(%FIELDS)],
  );
$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK    = @{$EXPORT_TAGS{all}};
our @EXPORT       = qw();


##======================================================================
## Constructor (inherited)
sub new {
  my $that = shift;
  return $that->SUPER::new(
			   ##-- Collection
			   col=>undef,
			   colfile=>undef,
			   coldigest=>'',
			   dir=>'..',
			   varfile=>'../Default.mak',
			   #userfile=>"user.mak", ##-- override default user file (tmp)
			   colmd5=>'',  ##-- MD5 digest for collection
			   automd5=>1,  ##-- whether to automatically acquire collection MD5 digest on load()

			   ##-- Selection (subcollection)
			   selected=>undef,

			   ##-- Field-specifications
			   sortby => 'sortDefault',         ##-- sort
			   collect => 'collectDefault',     ##-- Partitioning / best-value search : ??!!
			   summarize => 'summarizeDefault', ##-- Summarization


			   ##-- Status
			   changed=>0,  ##-- true if changes are KNOWN to have been made to collection
			   loaded=>0,   ##-- true if collection is loaded
			   paranoid=>0, ##-- set to true to backup old collection files
			   backup=>'~', ##-- backup suffix

			   ##-- Dummy?
			   dummy=>0,
			   verbose=>$VLEVELS{DEFAULT},

			   ##-- User args
			   @_,
			  );
}

##======================================================================
## Verbose messages

# undef = $mak->vmsg($level,@msg)
#  + print @msg to STDERR if $mak->{verbose} >= $level
sub vmsg {
  my ($mak,$level,@msg) = @_;
  $level = $VLEVELS{$level} while (exists($VLEVELS{$level}));
  print STDERR @msg if (
			(ref($mak) && $mak->{verbose} >= $level)
			||
			(!ref($mak) && $VLEVELS{DEFAULT} >= $level)
		       );
}


##======================================================================
## ACTIONS: General
##======================================================================

##---------------------------------------------------------------
## Actions: General: parse action

## ($act,$argstr) = $mak->parseAction($action)
##  + basically just an alias for split(/[\,\:\=\s]+/,$action,2)
sub parseAction {
  my ($mak,$action) = @_;
  return split(/[\s\,\:\=]+/,$action,2);
  #my ($actstr,$argstr) = split(/[\s\,\:\=]+/,$action,2);
  #my @args = defined($argstr) && $argstr ne '' ? eval qq($argstr) : qw();
  #return ($actstr,defined($argstr) ? $argstr : '');
}


##---------------------------------------------------------------
## Actions: General: perform action

## $bool = $mak->perform($actName,$actArgStrOrUndef);
sub perform {
  my ($mak,$actName,$actArg) = @_;
  my $acth = $actName;
  $acth = $ACTIONS{$acth} while (defined($ACTIONS{$acth}));

  my $code = ref($acth) ? $acth->{code} : $mak->can($acth);
  if (!defined($code)) {
    $mak->vmsg('error', ref($mak), ": unknown action: '$actName' -- skipping!\n");
    return 0;
  }

  ##-- perform the action
  $mak->vmsg('info', ref($mak), ": perform($actName", (defined($actArg) ? ", '$actArg'" : ''), ")\n");
  return $code->($mak,$actArg);
}

##---------------------------------------------------------------
## Actions: help

$ACTIONS{which} = $ACTIONS{help} =
  {
   syntax=>'help',
   help=>'display brief help on all known actions',
   code=>\&actHelp,
  };

sub actHelp {
  my $mak = shift;
  my %syn2act = map { ($ACTIONS{$_}{syntax} ? $ACTIONS{$_}{syntax} : $_)=>$ACTIONS{$_} } keys(%ACTIONS);
  my $tw_columns = $Text::Wrap::columns;
  $Text::Wrap::columns = 100;
  my ($syn,$act);
  foreach $syn (sort(keys(%syn2act))) {
    $act = $syn2act{$syn};
    print STDERR
      Text::Wrap::wrap((' ' x 2), (' ' x 29),
		       sprintf("%-24s : %s\n", $syn, ($act->{help} ? $act->{help} : 'no help')));
  }
  $Text::Wrap::columns = $tw_columns;
  return 1;
}


##======================================================================
## ACTIONS: Specific
##======================================================================

##---------------------------------------------------------------
## Actions: dummy

$ACTIONS{null} = $ACTIONS{dummy} = $ACTIONS{','} =
  { syntax=>'null', help=>'no action', code=>sub { return 1; }, };


##---------------------------------------------------------------
## Actions: initialize

## $mak->initCollection();
##  + initializes collection

$ACTIONS{init} = $ACTIONS{initialize} = $ACTIONS{new} =
  {
   syntax=>'init',
   help =>'initialize a new configuration collection',
   code =>\&actInitialize,
  };

sub actInitialize {
  my ($mak,$argv) = @_;

  $mak->{col} = MUDL::Make::Collection->new(varfile=>[$mak->{varfile}],
					    makefiles=>[map {abs_path($_)} glob("$mak->{dir}/[Mm]akefile")],
					   );
  $mak->{col}{vars}->parse($mak->{varfile})
    or confess(ref($mak)."::actInitialize(): could not parse variables file '$mak->{varfile}'");

  $mak->{loaded} = 1;

  return 1;
}

##---------------------------------------------------------------
## Actions: reparse variables

$ACTIONS{reparse} = $ACTIONS{reparseVariables} =
  {
   syntax=>'reparse',
   help =>'reparse configuration collection variables (does NOT re-expand!)',
   code =>\&actReparse,
  };

sub actReparse {
  my ($mak,$argv) = @_;
  return 0 if (!$mak->ensureLoaded());

  my $col = $mak->{col};
  $mak->{col}{vars}->parse($mak->{varfile})
    or confess(ref($mak)."::actReparse(): could not parse variables file '$mak->{varfile}'");

  return 1;
}



##---------------------------------------------------------------
## Actions: ensure loaded

$ACTIONS{ensure} =
  {
   syntax=>'ensure',
   help=>'ensure collection is loaded',
   code=>\&ensureLoaded,
  };


##---------------------------------------------------------------
## Actions: save (explicit)


$ACTIONS{save} =
  {
   syntax=>'save [FILE]',
   help  =>'save collection to a (new) file FILE, defaults to current file',
   code  => \&actSave,
  };
sub actSave {
  my ($mak,$file) = @_;
  $file = $mak->{colfile} if (!$file);
  my $rc = $mak->ensureLoaded && $mak->saveSafe($mak->{col},$file);
  $mak->{changed} = 0 if ($rc && $file eq $mak->{colfile});
  return $mak;
}

##---------------------------------------------------------------
## Actions: save: selection


$ACTIONS{saveSelection} = $ACTIONS{saveSel} =
  {
   syntax=>'saveSel FILE',
   help  =>'save selection as a collection to a (new) file FILE',
   code  => \&actSaveSelection,
  };
sub actSaveSelection {
  my ($mak,$file) = @_;
  return $mak->ensureLoaded && $mak->saveSafe($mak->selected(),$file);
}

##---------------------------------------------------------------
## Utilties: save: safe

## $mak->safeSave($col,$filename)
*safeSave = \&saveSafe;
sub saveSafe {
  my ($mak,$col,$file) = @_;
  ##
  ##-- maybe make a bacup
  if ($mak->{backup} && -e $file) {
    my $bak = $file.$mak->{backup};
    $mak->vmsg('info', ref($mak), "::saveSafe(): backing up old '$file' to '$bak'\n");
    if (-e $bak) {
      unlink($bak)
	or die(ref($mak), "::saveSafe(): could not unlink old backup '$bak'");
    }
    rename($file,$bak)
      or die(ref($mak), "::saveSafe(): could not create backup file '$bak'");
  }
  return $col->saveFile($file);
}


##---------------------------------------------------------------
## Actions: select

$ACTIONS{xselect} = $ACTIONS{selectx} =
  {
   syntax=>'xselect XCRITERIA',
   help  =>'select a subset of the collection configurations',
   code  => \&actSelectExtended,
  };
sub actSelectExtended {
  my ($mak,$crit) = @_;
  ($mak->ensureLoaded()
   && ($mak->{selected} = $mak->{col}->xcollect($crit)));
}

$ACTIONS{select} = $ACTIONS{uselect} = $ACTIONS{selectu} =
  {
   syntax=>'uselect UCRITERIA',
   help  =>'select a subset of the collection configurations',
   code  => \&actSelectUser,
  };
sub actSelectUser {
  my ($mak,$crit) = @_;
  ($mak->ensureLoaded()
   && ($mak->{selected} = $mak->{col}->ucollect($crit)));
}

$ACTIONS{oselect} = $ACTIONS{selecto} =
  {
   syntax=>'oselect OBJCRITERIA',
   help  =>'select a subset of the collection configurations',
   code  => \&actSelectObject,
  };
sub actSelectObject {
  my ($mak,$crit) = @_;
  ($mak->ensureLoaded()
   && ($mak->{selected} = $mak->{col}->collect($crit)));
}

$ACTIONS{unselect} = $ACTIONS{selectAll} = $ACTIONS{clearSelection} =
  {
   syntax=>'unselect',
   help  =>'Clears current selection, if any',
   code  => \&actUnselect,
  };
*actSelectAll = \&actUnselect;
sub actUnselect {
  my $mak = shift;
  ($mak->ensureLoaded()
   && delete($mak->{selected}));
}


##---------------------------------------------------------------
## Actions: group

##-- groupby
$ACTIONS{groupby} = $ACTIONS{xgroupby} =
  {
   syntax=>'xgroupby VARS',
   help  =>'group selected collection configurations by shared values for VARS',
   code  => \&actXGroupBy,
  };
sub actXGroupBy {
  my ($mak,$vars) = @_;
  ($mak->ensureLoaded()
   && ($mak->{selected} = $mak->selected()->xgroupby($vars)));
}


##-- groupover
$ACTIONS{group} = $ACTIONS{groupover} = $ACTIONS{ugroup} = $ACTIONS{ugroupover} =
  {
   syntax=>'ugroupover VARS',
   help  =>'group selected collection configurations over distinct values for VARS',
   code  => \&actUGroupOver,
  };
sub actUGroupOver {
  my ($mak,$vars) = @_;
  ($mak->ensureLoaded()
   && ($mak->{selected} = $mak->selected()->ugroupover($vars)));
}


##-- ungroup
$ACTIONS{ungroup} = $ACTIONS{nogroup} =
  {
   syntax=>'ungroup',
   help  =>'ungroup selection',
   code  => \&actUngroup,
  };
sub actUngroup {
  my ($mak) = @_;
  ($mak->ensureLoaded()
   && (!$mak->{selected} || ($mak->{selected} = $mak->{selected}->ungroup())));
}



##---------------------------------------------------------------
## Actions: expand

$ACTIONS{expand} = $ACTIONS{expandAll} = $ACTIONS{expandSelected} = $ACTIONS{expandSel} =
  {
   syntax=>'expandSelected',
   help  =>'(re-)expand selected configurations',
   code  => \&actExpandAll,
  };
sub actExpandAll {
  $_[0]->ensureLoaded() && $_[0]->selected()->expandAll( userfile=>$_[0]{userfile} );
}

$ACTIONS{expandMissing} =
  {
   syntax=>'expandMissing',
   help  =>'expand unexpanded selected configurations',
   code  => \&actExpandMissing,
  };
sub actExpandMissing {
  $_[0]->ensureLoaded() && $_[0]->selected()->expandMissing( userfile=>$_[0]{userfile} );
}

##---------------------------------------------------------------
## Actions: union

$ACTIONS{union} = $ACTIONS{add} = $ACTIONS{addCollection} =
  {
   syntax=>'union FILE',
   help=>'add configurations from FILE (clobbering any that were already here)',
   code=>\&actUnion,
  };
sub actUnion {
  my ($mak,$colfile) = @_;
  return 0 if (!$mak->ensureLoaded());
  my $col2 = MUDL::Make::Collection->loadFile($colfile)
    or die(ref($mak),"::actUnion(): load failed for '$colfile': $!");
  return $mak->{col}->addCollection($col2);
}


##---------------------------------------------------------------
## Actions: remove

$ACTIONS{rm} = $ACTIONS{rmSel} = $ACTIONS{rmSelected} =
  {
   syntax=>'rmSelected',
   help=>'remove selected configurations from collection',
   code=>\&actRmSelected,
  };
sub actRmSelected {
  my ($mak) = shift;
  return 0 if (!$mak->ensureLoaded());
  $mak->{col}->removeCollection($mak->selected);
  return 1;
}


##---------------------------------------------------------------
## Actions: refresh

$ACTIONS{refreshVars} =
  {
   syntax=>'refreshVars',
   help  =>'refresh make variables (implies expandAll)',
   code  => \&actRefreshVars,
  };

sub actRefreshVars {
  my $mak = shift;
  return 0 if (!$mak->ensureLoaded());

  $mak->{col}{vars}->clear;
  $mak->{col}{vars}->parse($mak->{varfile})
    or confess(ref($mak)."::actRefreshVars(): could not parse variables file '$mak->{varfile}'");

  return $mak->perform('expandAll',@_);
}


##---------------------------------------------------------------
## Actions: eval CODE

$ACTIONS{eval} =
  { syntax=>'eval CODE',
    help  =>'eval() some perl CODE',
    code  => \&actEval,
  };
sub actEval {
  my ($mak,$code) = @_;
  return 0 if (!$mak->ensureLoaded);

  my $col = $mak->{col};
  my $mcol = $mak->{col};

  eval "{ no strict; $code; }";
  if ($@) {
    carp(ref($mak)."::eval($code): error: $@");
    return 0;
  }
  return 1;
}


##---------------------------------------------------------------
## Actions: do FILE

$ACTIONS{do} = $ACTIONS{require} = $ACTIONS{read} = 
  { syntax=>'do FILE',
    help  =>'do() a perl FILE',
    code  =>\&actDoFile,
  };

sub actDoFile {
  my ($domak,$file) = @_;

  my ($rc);
  if (! -e "$file") {
    $@ = "'$file' does not exist";
    $rc=0;
  }
  elsif (! -r "$file") {
    $@ = "no read permission for '$file'";
    $rc=0;
  }
  else {
    return 0 if (!$domak->ensureLoaded);
    {
      no strict;
      local $col = $mak->{col};
      local $mak = $domak;
      $rc = do "$file";
    }
  }
  if ($@) {
    carp(ref($domak)."::do($file): error: $@");
    return 0;
  }
  return $rc;
}
sub actDoFile0 {
  my ($mak,$file) = @_;
  return 0 if (!$mak->ensureLoaded);

  my $fh = IO::File->new("<$file")
    or confess(ref($mak)."::do(): open failed for '$file': $!");
  my $code = join('',<$fh>);
  $fh->close();

  my $col = $mak->{col};
  my $rc = eval $code;
  if ($@) {
    carp(ref($mak)."::do($file): error: $@");
    return 0;
  }

  return $rc;
}

##---------------------------------------------------------------
## Actions: default sort priority

$ACTIONS{sortby} = $ACTIONS{priority} =
  {
   syntax=>'sortby [FIELD,...] ',
   help=>'set sort-priority.  Flags: "n"=numeric, ...?',
   code=>\&actSortBy,
  };
sub actSortBy {
  my ($mak,@ufields) = @_;
#  $mak->{sortby} = join(',',grep { defined($_) && $_ ne '' } @ufields);
  $mak->{sortby} = [@ufields];
  return 1;
}

##---------------------------------------------------------------
## Actions: partitioning: by collecting values

$ACTIONS{collect} =
  {
   syntax=>'collect [FIELD(s),...] ',
   help=>'collect variant values on FIELD(s) ',
   code=>\&actCollect,
  };
sub actCollect {
  my ($mak,@ufields) = @_;
  $mak->{partition} = $mak->collect(\@ufields);
  return $mak->{partition};
}



##---------------------------------------------------------------
## Actions: list: brief

$ACTIONS{list} = $ACTIONS{ls} =
  {
   syntax=>'list [FIELD,...]',
   help=>'list selected configurations (brief)',
   code=>\&actList,
  };

sub actList {
  my ($mak,$fields) = @_;
  return 0 if (!$mak->ensureLoaded);

  my $configs = $mak->selectedConfigs;
  if (!@$configs) {
    print "  (no configurations selected)\n";
    return 1;
  }

  $fields     = 'listDefault' if (!$fields);
  my $mf_ls   = $mak->fields($fields, configs=>$configs);
  my $xfields = $mf_ls->xfields;

  my @listKeys = 
    (
     map { $mak->listKey($_, (map { $_->{title} } @$xfields)) }
     $mf_ls->fieldData(
		       $mak->fields($mak->{sortby}, configs=>$configs)->sortConfigs($configs)
		      )
    );

  my ($lkey);
  my %listed = qw();
  foreach $lkey (@listKeys) {
    next if (exists($listed{$lkey}));
    print "  ", $lkey, "\n";
    $listed{$lkey} = undef;
  }

  return 1;
}


## $listKey = $mak->listKey($fieldData)
## $listKey = $mak->listKey($fieldData, @fieldTitles)
sub listKey {
  my ($mak,$fdata,@ftitles) = @_;
  @ftitles = grep { $_ ne '_' } sort(keys(%$fdata)) if (!@ftitles);
  return join(' ', map { ($_
			  .'='
			  .(defined($fdata->{$_}) ? $fdata->{$_} :'-undef-')
			 )
		       } @ftitles);
}

##---------------------------------------------------------------
## Actions: Summarize (ASCII table format)

$ACTIONS{summarize} = $ACTIONS{summary} = $ACTIONS{table} = $ACTIONS{tab} =
  {
   syntax=>'summarize [FIELD,...]',
   help=>'summarize selected configurations (ASCII table-format)',
   code=>\&actSummary,
  };

*actTable = \&actSummary;
sub actSummary {
  my ($mak,$ufields) = @_;
  return 0 if (!$mak->ensureLoaded);

  my $configs = $mak->selectedConfigs;
  if (!@$configs) {
    $mak->vmsg('warn', ref($mak),"::actSummary(): no configurations selected\n");
    return 1;
  }

  $ufields   = 'summarizeDefault' if (!$ufields);
  my $mf = $mak->fields($ufields,configs=>$configs);
  my $tab = MUDL::Make::Table->newFull(mfields=>$mf, sortby=>$mak->{sortby});

  return $tab->printText(\*STDOUT, indent=>' ');
}

##---------------------------------------------------------------
## Actions: Summarize (CSV table format)

$ACTIONS{csvtable} = $ACTIONS{csvtab} = $ACTIONS{csv} = $ACTIONS{dat} =
  {
   syntax=>'csvtable [FIELD,...]',
   help=>'summarize selected configurations (CSV table-format)',
   code=>\&actCSVTable,
  };

sub actCSVTable {
  my ($mak,$ufields) = @_;
  return 0 if (!$mak->ensureLoaded);

  my $configs = $mak->selectedConfigs;
  if (!@$configs) {
    $mak->vmsg('warn', ref($mak),"::actCSVTable(): no configurations selected\n");
    return 1;
  }

  $ufields   = 'summarizeDefault' if (!$ufields);
  my $mf = $mak->fields($ufields,configs=>$configs);
  my $tab = MUDL::Make::Table->newFull(mfields=>$mf, sortby=>$mak->{sortby});

  return $tab->printCSV(\*STDOUT, comment=>'#');
}

##---------------------------------------------------------------
## Actions: Summarize (R-dat format)

$ACTIONS{rtab} = $ACTIONS{rdat} =
  {
   syntax=>'rdat [FIELD,...]',
   help=>'summarize selected configurations (R read.delim() format)',
   code=>\&actRdat,
  };

sub actRdat {
  my ($mak,$ufields) = @_;
  return 0 if (!$mak->ensureLoaded);

  my $configs = $mak->selectedConfigs;
  if (!@$configs) {
    $mak->vmsg('warn', ref($mak),"::actRdat(): no configurations selected\n");
    return 1;
  }

  $ufields   = 'rtabDefault' if (!$ufields);
  my $mf = $mak->fields($ufields,configs=>$configs);
  my $tab = MUDL::Make::Table->newFull(mfields=>$mf, sortby=>$mak->{sortby});

  my $csv = $tab->csvTable();
  $csv->{cmtStr} = '';
  $csv->{saveColumnNumbers} = 0;
  $csv->{sanitizeColumnTitles} = 1;
  $csv->saveNativeFh(\*STDOUT);

  return 1;
}

##---------------------------------------------------------------
## Actions: Summarize (LaTeX table format)

$ACTIONS{lsummarize} = $ACTIONS{lsummary} = $ACTIONS{ltable} = $ACTIONS{ltab} =
  {
   syntax=>'lsummary [FIELD,...]',
   help=>'summarize selected configurations (LaTeX table-format)',
   code=>\&actLatexSummary,
  };

sub actLatexSummary {
  my ($mak,$ufields) = @_;
  return 0 if (!$mak->ensureLoaded);

  my $configs = $mak->selectedConfigs;
  if (!@$configs) {
    $mak->vmsg('warn', ref($mak),"::actLatexSummary(): no configurations selected\n");
    return 1;
  }

  $ufields   = 'latexDefault' if (!$ufields);
  my $mf = $mak->fields($ufields,configs=>$configs);
  my $tab = MUDL::Make::Table->newFull(mfields=>$mf, sortby=>$mak->{sortby});

  return $tab->printLatex(\*STDOUT, indent=>' ');
}


##-- Utilities: latexify (UNUSED!)

## $lstr = latexify($str)
sub latexify {
  my $str = shift;
  $str =~ s/([\%\_\&\\])/\\$1/g;
  return $str;
}

##---------------------------------------------------------------
## Actions: make (safe)

$ACTIONS{make} = $ACTIONS{acquire} =
  {
   syntax=>'make',
   help=>'call make for selected configurations, acquiring data',
   code=>\&actMake,
  };
sub actMake {
  my $mak = shift;
  return 0 if (!$mak->ensureLoaded);
  my $rc = 1;
  my ($cfg);
  foreach $cfg ($mak->sortSelection()) {
    $rc &&= $cfg->make( dir=>$mak->{dir}, userfile=>$mak->{userfile}, makefiles=>$mak->{makefiles}, dummy=>$mak->{dummy} );
    if (!$rc) {
      warn(ref($mak),"::actMake() failed -- bailing out");
      exit(1);
    }
    if (!$mak->{dummy}) {
      $mak->syncCollection() if ($mak->{paranoid});
    }
  }
  return $rc;
}

##---------------------------------------------------------------
## Actions: make-k (unsafe)

$ACTIONS{'make-k'} = $ACTIONS{'makek'} =
  {
   syntax=>'make-k',
   help=>'greedily call make for selected configurations (don\'t die)',
   code=>\&actMakeK,
  };
sub actMakeK {
  my $mak = shift;
  return 0 if (!$mak->ensureLoaded);
  my $rc = 1;
  my ($cfg);
  foreach $cfg ($mak->sortSelection()) {
    $rc &&= $cfg->make( dir=>$mak->{dir}, userfile=>$mak->{userfile}, makefiles=>$mak->{makefiles}, dummy=>$mak->{dummy} );
    if (!$mak->{dummy}) {
      $mak->syncCollection() if ($mak->{paranoid});
    }
  }
  return $rc;
}

##---------------------------------------------------------------
## Actions: re-acquire

$ACTIONS{reacquire} =
  {
   syntax=>'reacquire',
   help=>'re-acquire data for selected configurations (may be faster than "acquire")',
   code=>\&actReacquire,
  };
sub actReacquire {
  my $mak = shift;
  return 0 if (!$mak->ensureLoaded);
  my $rc = 1;
  my ($cfg);
  my $fspec = 'stage,emi,auto';
  my %makargs = (dir=>$mak->{dir}, userfile=>$mak->{userfile}, makefiles=>$mak->{makefiles}, dummy=>$mak->{dummy});
  my $mf_ls = $mak->fields($fspec, configs=>$mak->selectedConfigs);
  my $xfields = $mf_ls->xfields;
  my @xtitles = map { $_->{title} } @$xfields;
  my ($fdata,$lskey);
  foreach $cfg ($mak->sortSelection(sortby=>$fspec)) {
    ($fdata) = $mf_ls->fieldData($cfg);
    $lskey = $mak->listKey($fdata, @xtitles);
    $mak->vmsg('info', ref($mak), "::actReacquire(): $lskey\n");
    $rc &&= ($cfg->can('reacquire')
	     ? $cfg->reacquire(%makargs)
	     : $cfg->make(%makargs));
    if (!$mak->{dummy}) {
      $mak->syncCollection() if ($mak->{paranoid});
    }
  }
  return $rc;
}


##---------------------------------------------------------------
## Actions: plot

$ACTIONS{plot} =
  {
   syntax=>'plot %args=(x=>"FIELD",y=>"FIELD",...)',
   help=>'create and print a 2d GNUplot script for selected data',
   code=>\&actPlot,
  };
sub actPlot {
  my ($mak,$args) = @_;
  return 0 if (!$mak->ensureLoaded);
  my $rc = 1;
  my %args = defined($args) ? eval "($args)" : qw();
  warn(ref($mak)."::actPlot($args): error evaluating argument string: $@") if ($@);

  my $configs = $mak->selectedConfigs();
  my $plot    = MUDL::Make::Plot->newFull(configs=>$configs,%args);
  my $gpfile  = $plot->saveScript();

  $mak->vmsg('info', ref($mak), "::actPlot(): saved GNUplot script to '$gpfile'.\n");

  return $rc;
}

##---------------------------------------------------------------
## Actions: plot3d

$ACTIONS{plot3d} = $ACTIONS{splot} = 
  {
   syntax=>'plot3d %args=(x=>"FIELD",y=>"FIELD",z=>"FIELD",...)',
   help=>'create and print a 3d GNUplot script for selected data',
   code=>\&actPlot3d,
  };
sub actPlot3d {
  my ($mak,$args) = @_;
  return 0 if (!$mak->ensureLoaded);
  my $rc = 1;
  my %args = defined($args) ? eval "($args)" : qw();
  warn(ref($mak)."::actPlot3d($args): error evaluating argument string: $@") if ($@);

  my $configs = $mak->selectedConfigs();
  my $plot    = MUDL::Make::Plot3d->newFull(configs=>$configs,%args);
  my $gpfile  = $plot->saveScript();

  $mak->vmsg('info', ref($mak), "::actPlot3d(): saved GNUplot script to '$gpfile'.\n");

  return $rc;
}



##======================================================================
## UTILITIES
##======================================================================

##---------------------------------------------------------------
## Utilities: selection

## $subcol = $mak->selected()
##  + uses $mak->{selected} if defined, else $mak->{col}
sub selected {
  my $mak = shift;
  return $mak->{selected} ? $mak->{selected} : $mak->{col};
}

## \@configs = $mak->selectedConfigs()
sub selectedConfigs {
  my $mak = shift;
  return [values(%{$mak->selected->{uconfigs}})];
}

## $key2ConfigSet = $mak->partition()
##  + uses $mak->{partition} if present, else id(selection)
sub partition {
  my $mak = shift;
  return ($mak->{partition}
	  ? $mak->{partition}
	  #: {''=>{map {$_=>$_} values(%{$mak->selected->{uconfigs}})}}
	  : $mak->collect(['auto'])
	 );
}


##---------------------------------------------------------------
## Utilities: search

## $cfg = $mcol->ufind(\%vars)
## $cfg = $mcol->ufind($ukey)
##  + no implicit config-creation
sub ufind {
  my $mak = shift;
  $mak->ensureLoaded || return undef;
  return $mak->{col}->ufind(@_);
}

## $cfg = $mcol->uget(\%uvars, %args)
## $cfg = $mcol->uget($ukey,   %args)
##  + implicitly creates config for \%uvars if none exists
##  + implicitly parses $ukey if it is passed as a string (may be dangerous!)
##  + %args:
##     expand=>$bool, ##-- if true, newly created configs will be auto-expanded
##     class=>$class, ##-- config class (default: MUDL::Make::Config)
sub uget {
  my ($mak,$uvars,%args) = @_;
  $mak->ensureLoaded || return undef;
  my $cfg = $mak->{col}->ufind($uvars);
  if (!defined($cfg)) {
    $cfg = $mak->{col}->uget($uvars,expand=>1,%args);
  }
  return $cfg;
}

## $cfg = $mcol->xfind(\%xvars)
## $cfg = $mcol->xfind($xkey)
##  + no implicit config-creation
sub xfind {
  my $mak = shift;
  $mak->ensureLoaded || return undef;
  return $mak->{col}->xfind(@_);
}

##---------------------------------------------------------------
## Utilities: fields (object)


## $makFieldsObj = $mak->fields(\@field_hashes_or_strings, %new_args)
##  + %args:
##     configs=>\@mudl_make_configs,   ##-- default: selected configs
##     alias  =>\%field_alias_hash,    ##-- pre-emptive field aliases
##     noauto =>\%fieldTitles,         ##-- ignore some auto fields
sub fields {
  my ($mak,$ufields,%args) = @_;
  return qw() if (!$mak->ensureLoaded());

  ##-- get configs
  my $configs = $args{configs} ? $args{configs} : [values(%{$mak->selected()->{uconfigs}})];
  my $alias   = $args{alias} ? {%FIELDS, %{$args{alias}}} : \%FIELDS;

  return MUDL::Make::Fields->new(%args,configs=>$configs,alias=>$alias,fields=>$ufields);
}


##---------------------------------------------------------------
## Utilities: partitioning & collection

## \%differntiaFieldConfigKey_to_ConfigList = $mak->collect(\@fieldsToCollect, %args)
##   + %args:
##       configs => \@configs,
sub collect {
  my ($mak,$fields,%args) = @_;

  ##-- get configs
  return {} if (!$mak->ensureLoaded);
  my $configs = $args{configs} ? $args{configs} : [values(%{$mak->selected->{uconfigs}})];

  ##-- get collection-fields
  my @fields_col = $mak->fields([grep { defined($_) && $_ ne '' } @$fields]);
  my %col2field  = map { $mak->fieldTitle($_)=>$_ } @fields_col;

  ##-- get all possible user-fields
  my @vars_all   = $mak->userVariables($configs);
  my @fields_all = $mak->fields([map {"xvars->$_"} @vars_all]);
  my %all2field  = map { $mak->fieldTitle($_)=>$_ } @fields_all;

  ##-- find differentia fields
  my %diff2field  = %all2field;
  delete(@diff2field{ keys(%col2field)} );
  delete(@diff2field{ map { $_->{alt} ? @{$_->{alt}} : qw() } @fields_col });
  my @fields_diff = values(%diff2field);

  ##-- get sub-collection map: $subukey=>{$cfg=>$cfg,...}
  my ($cfg,$diffcf,$diffkey);
  my $key2clist = {};
  foreach $cfg (@$configs) {
    $diffcf  = $mak->configFieldHash($cfg,\@fields_diff);
    delete($diffcf->{_});
    $diffkey = $mak->{col}->key($diffcf);
    $key2clist->{$diffkey} = [] if (!$key2clist->{$diffkey});
    push(@{$key2clist->{$diffkey}}, $cfg);
  }

  return $key2clist;
}


##---------------------------------------------------------------
## Utilities: sort: high-level

## $sortby_mfields = $mak->sortby()
## $sortby_mfields = $mak->sortby($sortby)
## $sortby_mfields = $mak->sortby($sortby,$configs)
sub sortby {
  my ($mak,$sortby,$configs) = @_;
  $sortby = $mak->{sortby} if (!$sortby);
  $configs = $mak->selectedConfigs if (!$configs);
  my $mf = ref($sortby) ? $sortby : MUDL::Make::Fields->new(fields=>[$sortby],configs=>$configs);
  $mak->{sortby} = $mf if (!ref($mak->{sortby}) && $mak->{sortby} eq $sortby);
  return $mf;
}

## @sorted_selected_configs = $mak->sortSelection(%args)
##  + %args:
##     sortby=>$fieldSpec,
##  + sorts currently selected configs by the field-spec in $sortby (default=$mak->{sortby})
sub sortSelection {
  my ($mak,%args) = @_;
  return qw() if (!$mak->ensureLoaded());
  my $selconfigs = $mak->selectedConfigs;
  return $mak->sortby($args{sortby},$selconfigs)->sortConfigs($selconfigs);
}

## @sorted_configs = $mak->sortConfigs(\@configs, %args)
##  + %args:
##     sortby=>$fieldSpec,
##  + sorts @configs by the field-spec in $sortby (default=$mak->{sortby})
sub sortConfigs {
  my ($mak,$configs,%args) = @_;
  return $mak->sortby($args{sortby}, $configs)->sortConfigs($configs);
}



##======================================================================
## I/O
##======================================================================

## $bool = $mak->ensureLoaded()
sub ensureLoaded {
  my $mak = shift;
  $mak->loadCollection() if (!$mak->{loaded});
  return $mak->{loaded};
}

## $col = $mak->loadCollection()
##  + loads the default collection
sub loadCollection {
  my $mak = shift;
  $mak->vmsg('info', ref($mak),": loadCollection($mak->{colfile})...\n");
  $mak->{col} = MUDL::Make::Collection->loadFile($mak->{colfile});
  if (!$mak->{col}) {
    confess(ref($mak), "::loadCollection($mak->{colfile}): load failed!\n");
    $mak->{loaded} = 0;
    $mak->{coldigest} = '';
    return undef;
  }
  $mak->{loaded} = 1;
  $mak->{coldigest} = $mak->{automd5} ? $mak->collectionDigest() : '';

  $mak->vmsg('info', ref($mak),": loadCollection($mak->{colfile}): loaded.\n");
  return $mak->{col};
}

## $digest_str = $mak->collectionDigest()
##  + gets MD5 digest string (binary) from $mak->{collection}->savePerlString()
sub collectionDigest {
  my $mak = shift;
  return '' if (!$mak->{loaded});
  my $tmp = $Storable::canonical;
  $Storable::canonical = 1;
  my $digest = Digest::MD5::md5(Storable::freeze($mak->{col}));
  $Storable::canonical = $tmp;
  return $digest;
}

## $bool              = $mak->{changed} = $mak->changed(); ##-- scalar context
## ($bool,$newdigest) = $mak->{changed} = $mak->changed(); ##-- array context
##  + returns TRUE iff collection is loaded and has been changed
sub changed {
  my $mak = shift;
  my ($changed,$digest);

  if (!$mak->{loaded}) {
    ($changed,$digest) = (0,'');
  }
  elsif ($mak->{changed}) {
    ($changed,$digest) = (1,$mak->collectionDigest());
  }
  else {
    $digest  = $mak->collectionDigest();
    $changed = ($digest ne $mak->{coldigest});
  }
  return wantarray ? ($changed,$digest) : $changed;
}

## $col = $mak->syncCollection()
## $col = $mak->syncCollection($file)
##  + saves the default collection

$ACTIONS{sync} =
  {
   syntax=>'sync [FILE]',
   help  =>'synchonize collection to the default file',
   code  => \&syncCollection,
  };

*actSync = \&syncCollection;
sub syncCollection {
  my ($mak,$file) = @_;
  $file = $mak->{colfile} if (!$file);
  return 0 if (!$mak->ensureLoaded());

  my ($changed,$digest) = $mak->changed();
  if ($changed) {

    ##-- be paranoid ?
    if ($mak->{paranoid}) {
      my $ctr = 0;
      $ctr++ while (-e sprintf("$file.bak.%03d",$ctr));
      rename($file, sprintf("$file.bak.%03d",$ctr));
    }

    #my $rc = $mak->{col}->saveFile($file);
    my $rc = $mak->saveSafe($mak->{col},$file);
    $mak->{changed} = 0;
    $mak->{coldigest} = $digest;
    return $rc;
  }

  return 1;
}

BEGIN { print STDERR __PACKAGE__  , " loaded.\n"; }
1;

__END__

##---------------------------------------------------------------
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
