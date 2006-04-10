##-*- Mode: CPerl -*-

## File: MUDL::Make.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: makefile stuff
##======================================================================

package MUDL::Make;
use Cwd qw(abs_path);
use Digest::MD5 qw(md5);
use Text::Balanced qw(extract_bracketed);
use strict;
use Carp;
our @ISA = qw(MUDL::Object Exporter);

##======================================================================
## Globals

##---------------------------------------------------------------
## Globals: Field Aliases

## %FIELDS: ( $fieldName => \%fieldData, ..., $aliasName=>$fieldName, )
##  + %fieldData keys:
##     path => \@path,       ##-- nested MUDL::Make::Config key-path
##     n    => $bool,        ##-- true iff numeric
##     fmt  => $how,         ##-- sprintf template for tabular formatting (default='auto')
##     title => $title,      ##-- field title (for tab)
##     evaltitle=>$str,      ##-- field title, eval'd (with variable $field set to full field)
##     alt   => \@titles,    ##-- alternative titles
##     eval  => $eval,       ##-- eval() for value adjustment
##     hr    => $how,        ##-- summarize(): separator type for changed values qw(major minor micro)
##     condense=>$bool,      ##-- summarize(): condense consecutive duplicate values?
##     xcode =>\&xcode,      ##-- dynamic alias: calls $code->($mak,$field,configs=>\@configs,...) to expand
##     hidden=>$bool,        ##-- if true, field is not displayed
##
##     ##-- TODO--
##     vcode =>\&vcode,      ##-- dynamic value: calls $code->($mak,$cfg,$field,configs=>\@configs,...) for val
##     ....
our %FIELDS =
  (
   ##-- Pseudo-fields
   'all'  => undef,   ##-- all user-defined fields in selection
   'auto' => undef,   ##-- all user-defined fields in selection which actually vary

   ##-- Action-specific field aliases
   'listDefault' => [ qw(auto) ],
   'sortDefault' => [
		     qw(stage emi corpus),
		      #qw(pr:g rc:g pr:t rc:t)
		    ],
   'collectDefault' => [
			  qw(corpus stage),
			  #qw(emi),
			 ],
   'summarizeDefault' => [
			  'stage.emi',
			  'corpus',
			  #'lang',
			  'lrlabel',
			  #':',
			  'auto',
			  '|',
			  'pr:g',
			  #'rc:g',
			  #'F:g',
			  #qw(apr:g arc:g aF:g),
			  'ar:g',
			  '|',
			  'pr:t',
			  #'rc:t',
			  #'F:t',
			  #qw(apr:t arc:t aF:t),
			  'ar:t',
			 ],
   default=>'summarizeDefault',


   ##-- Filler(s)
   ':'    => { path=>[], eval=>'":"', title=>':', },
   '::'   => { path=>[], eval=>'"::"', title=>'::', },
   '='    => { path=>[], eval=>'"="', title=>'=', },
   '/'    => { path=>[], eval=>'"/"', title=>'/', },
   '|'    => { path=>[], eval=>'"|"', title=>'|', },
   '&'    => { path=>[], eval=>'"&"', title=>'&', },
   '\\'   => '\\\\',
   '\\\\' => { path=>[], eval=>'"\\\\\\\\"', title=>'\\\\', },

   ##-- MetaProfile: label
   lrlabel => { path=>[qw(xvars lrlabel)], n=>0, fmt=>'auto', title=>'lrlab',
		alt=>[qw(xvars->lrwhich xvars->tcd xvars->tcm xvars->tccd xvars->tccm),
		      qw(lrwhich tcd tcm tccd tccm),
		     ],
		hr=>undef, condense=>0,
	      },
   lrlab   => 'lrlabel',

   ##-- Corpus
   corpus => { path=>[qw(xvars icbase)], n=>0, fmt=>'auto', title=>'corpus',
	       alt=>[
		     qw(xvars->icorpus xvars->icbase xvars->tcorpus),
		     qw(icorpus icbase tcorpus)
		    ],
	       hr=>'micro', condense=>1,
	     },
   lang   => { path=>[qw(xvars icbase)], n=>0, fmt=>'auto', title=>'lang',
	       eval=>'$_ =~ /^[uz]/ ? "de" : "en"',
	       hr=>'micro', condense=>1,
	     },

   ##-- MetaProfile: numeric indices
   'stage' => { path=>[qw(xvars stage)], n=>1, fmt=>'%3d', title=>'stg',
		alt=>[qw(stage xvars->stage)],
		hr=>'major',
		condense=>1,
		part=>'stage',
	      },
   'emi'   => { path=>[qw(xvars emi)],   n=>1, fmt=>'%3d', title=>'emi',
		alt=>[qw(emi xvars->emi)],
		hr=>'minor',
		condense=>1,
	      },
   'stage.emi' => {
		   path=>[qw(xvars)],
		   n=>0,
		   fmt=>'%5.2f',
		   eval=>'sprintf("%2d.%02d", $_->{stage}, $_->{emi})',
		   title=>'stg.emi',
		   alt=>[qw(xvars->stage xvars->emi stg emi),
			 qw(stage emi),
			],
		   hr=>'minor',
		   condense=>1,
		  },

   ##-------------------------------------------------
   ## Eval: Meta-(precision,recall,F,ambig-rate)

   ##-------------------------------------
   ## Eval: Meta-*: Global
   'pr:g'  => { path=>[qw(eval_global precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' pr:g'},
   'rc:g'  => { path=>[qw(eval_global recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' rc:g'},
   'F:g'   => { path=>[qw(eval_global F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' F:g' },
   'ar:g'  => { path=>[qw(eval_global arate1)],    n=>1, fmt=>'%.3f', eval=>'0+$_',  title=>' ar:g',
		condense=>0,
	      },

   ##-------------------------------------
   ## Eval: Meta-*: Targets
   'pr:t'  => { path=>[qw(eval_targets precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' pr:t'},
   'rc:t'  => { path=>[qw(eval_targets recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' rc:t'},
   'F:t'   => { path=>[qw(eval_targets F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' F:t' },
   'ar:t'  => { path=>[qw(eval_targets arate1)],    n=>1, fmt=>'%.3f', eval=>'0+$_', title=>' ar:t',
		condense=>0,
	      },

   ##-------------------------------------------------
   ## Eval: Tagwise-average (precision,recall,F,ambig-rate)

   ##-------------------------------------
   ## Eval: Tagwise-average-*: Global
   'apr:g'  => { path=>[qw(eval_global avg_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' apr:g'},
   'arc:g'  => { path=>[qw(eval_global avg_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' arc:g'},
   'aF:g'   => { path=>[qw(eval_global avg_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' aF:g' },

   ##-------------------------------------
   ## Eval: Tagwise-average-*: Targets
   'apr:t'  => { path=>[qw(eval_targets avg_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' apr:t'},
   'arc:t'  => { path=>[qw(eval_targets avg_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' arc:t'},
   'aF:t'   => { path=>[qw(eval_targets avg_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' aF:t' },

   ##-------------------------------------------------
   ## Eval: Single-tag (precision,recall,F,ambig-rate)

   ##-------------------------------------
   ## Eval: Single-tag-*: Global
   'tagpr:g' => { path=>[qw(eval_global tag2info)], n=>1, fmt=>'%.2f',
		  evaltitle=>'"$field->{tag}:pr:g"',
		  eval=>'100*$_->{$field->{tag}}{pr}',
		},
   'tagrc:g' => { path=>[qw(eval_global tag2info)], n=>1, fmt=>'%.2f',
		  evaltitle=>'"$field->{tag}:rc:g"',
		  eval=>'100*$_->{$field->{tag}}{rc}',
		},
   'tagF:g'  => { path=>[qw(eval_global tag2info)], n=>1, fmt=>'%.2f',
		  evaltitle=>'"$field->{tag}:rc:g"',
		  eval=>'100*$_->{$field->{tag}}{F}',
		},

   ##-------------------------------------
   ## Eval: Single-tag-*: Targets
   'tagpr:t' => { path=>[qw(eval_targets tag2info)], n=>1, fmt=>'%.2f',
		  evaltitle=>'"$field->{tag}:pr:t"',
		  eval=>'100*$_->{$field->{tag}}{pr}',
		},
   'tagrc:t' => { path=>[qw(eval_targets tag2info)], n=>1, fmt=>'%.2f',
		  evaltitle=>'"$field->{tag}:rc:t"',
		  eval=>'100*$_->{$field->{tag}}{rc}',
		},
   'tagF:t'  => { path=>[qw(eval_targets tag2info)], n=>1, fmt=>'%.2f',
		  evaltitle=>'"$field->{tag}:F:t"',
		  eval=>'100*$_->{$field->{tag}}{F}',
		},

   ##-------------------------------------------------
   ## Eval: All single-tags (precision,recall,F,ambig-rate)

   ##-------------------------------------
   ## Eval: All single-tag-*: Global
   'tags:pr:g' => { xcode=>\&_alltags_expand, _tag_field=>'tagpr:g', _tag_var=>'tag', },
   'tags:rc:g' => { xcode=>\&_alltags_expand, _tag_field=>'tagrc:g', _tag_var=>'tag', },
   'tags:F:g'  => { xcode=>\&_alltags_expand, _tag_field=>'tagF:g', _tag_var=>'tag', },

   ##-------------------------------------
   ## Eval: All single-tag-*: Targets
   'tags:pr:t' => { xcode=>\&_alltags_expand, _tag_field=>'tagpr:t', _tag_var=>'tag', },
   'tags:rc:t' => { xcode=>\&_alltags_expand, _tag_field=>'tagrc:t', _tag_var=>'tag', },
   'tags:F:t'  => { xcode=>\&_alltags_expand, _tag_field=>'tagF:t', _tag_var=>'tag', },

   ##-------------------------------------------------
   ## Eval: max-value search
   '*'=>'max',
   'best'=>'max',
   'max' => { vcode  =>\&_field_max,
	      of     =>'pr:g',        ##-- target field: to be set by user or alias
	      over   =>'(stage,emi)', ##-- field list
	      title  =>'_max',
	      hidden =>0,
	    },
  );

##-- Fields: Utility: best-value: --TODO--
sub _field_max {
  my ($mak,$pfield,%args) = @_;
  my $configs = $args{configs};
  return { %$pfield, code=>undef, eval=>'\"$pfield->{over}\"', };
}


##-- Fields: Utility: _alltags_expand
## @fields = _alltags_expand($make,$pseudofield,%args)
sub _alltags_expand {
  my ($mak,$pfield,%args) = @_;
  my $configs = $args{configs};
  my $tagfield = $pfield->{_tag_field};
  my $tagvar   = $pfield->{_tag_var};
  my $srcfield = $args{alias}{$tagfield};

  my ($cfg,$info);
  my %tags2 = qw();
  ##-- gather known tag2 values
  foreach $cfg (@$configs) {
    $info = $mak->pathValue($cfg,$srcfield->{path});
    @tags2{keys(%$info)} = undef;
  }

  ##-- map tag2-keys to expanded fields
  return map { {%$srcfield, $tagvar=>$_} } sort(keys(%tags2));
}

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
   'fields'=>[qw(%FIELDS)],
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
			   userfile=>'user.mak',
			   colmd5=>'', ##-- MD5 digest for collection file

			   ##-- Selection (subcollection)
			   selected=>undef,

			   ##-- Sorting (field-spec)
			   sortby => 'sortDefault',

			   ##-- Partitioning / best-value search (field-spec)
			   collect => 'collectDefault',

			   ##-- Summarization (field-spec)
			   summarize => 'summarizeDefault',

			   ##-- don't auto-display these potential fields (titles)
			   summarize_no_auto=>{
					       map { $_=>undef }
					       (
						#'xvars->stage',
						#'xvars->emi',
						#'xvars->lrwhich',
						#'xvars->tcd',
						#'xvars->tcm',
						#'xvars->tccd',
						#'xvars->tccm',
						#'xvars->lrlabel',
						#'xvars->icorpus',
						#'xvars->tbase',
						#'xvars->tcorpus',
					       ),
					      },

			   ##-- Status
			   changed=>0,  ##-- true if changes are KNOWN to have been made to collection
			   loaded=>0,   ##-- true if collection is loaded
			   paranoid=>0, ##-- set to true to backup old collection files

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
  print STDERR @msg if ($mak->{verbose} >= $level);
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
    carp(ref($mak),"::perform($actName,$actArg): skipping unknown action");
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
  my ($syn,$act);
  foreach $syn (sort(keys(%syn2act))) {
    $act = $syn2act{$syn};
    print STDERR
      sprintf("  %-18s : %s\n", $syn, ($act->{help} ? $act->{help} : 'no help'));
  }
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
   syntax=>'save FILE',
   help  =>'save collection to a (new) file FILE',
   code  => \&actSave,
  };
sub actSave {
  my ($mak,$file) = @_;
  return $mak->ensureLoaded && $mak->{col}->saveFile($file);
}

##---------------------------------------------------------------
## Actions: select

$ACTIONS{xselect} = 
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

$ACTIONS{select} = $ACTIONS{uselect} = 
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

##---------------------------------------------------------------
## Actions: expand

$ACTIONS{expandAll} =
  {
   syntax=>'expandAll',
   help  =>'(re-)expand selected configurations',
   code  => \&actExpandAll,
  };
sub actExpandAll {
  $_[0]->ensureLoaded() && $_[0]->selected()->expandAll();
}

$ACTIONS{expand} = $ACTIONS{expandMissing} =
  {
   syntax=>'expandMissing',
   help  =>'expand unexpanded selected configurations',
   code  => \&actExpandMissing,
  };
sub actExpandMissing {
  $_[0]->ensureLoaded() && $_[0]->selected()->expandMissing();
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
  $mak->{sortby} = [
		    $mak->fields([grep { defined($_) && $_ ne '' } @ufields],
				 #configs=>[values(%{$mak->selected->{uconfigs}})],
				)
		   ];
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
  return 1;
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

  my $part    = $mak->partition();
  my @configs = map { values(%$_) } values(%$part);
  if (!@configs) {
    print "  (no configurations selected)\n";
    return 1;
  }
  $fields = 'listDefault' if (!$fields);
  my @fields = $mak->fields($fields,
			    configs=>\@configs,
			    #noauto=>$mak->{summarize_no_auto},
			   );

  my ($pkey);
  foreach $pkey (sort keys(%$part)) {
    print
      (
       "\n",
       "Subcollection ($pkey):\n",
       (
	map { ("  ", $mak->listKey($_, (map { $_->{title} } @fields)), "\n") }
	map { $mak->configFieldStringHash($_,@fields) }
	$mak->sortConfigs([values(%{$part->{$pkey}})],
			  sortby=>$mak->{sortby}
			  #sortby=>\@fields,
			 )
       ),
      );
  }

  return 1;
}


## $listKey = $mak->listKey($field2val)
## $listKey = $mak->listKey($field2val, @titles)
sub listKey {
  my ($mak,$field2val,@titles) = @_;
  @titles = grep { $_ ne '_' } sort(keys(%$field2val)) if (!@titles);
  return join(' ', map { ($_
			  .'='
			  .(defined($field2val->{$_}) ? $field2val->{$_} :'-undef-')
			 )
		       } @titles);
}

##---------------------------------------------------------------
## Actions: Summarize (ASCII table format)

$ACTIONS{summarize} = $ACTIONS{summary} = $ACTIONS{table} = $ACTIONS{tab} =
  {
   syntax=>'summarize [FIELD,...]',
   help=>'summarize selected configurations (table-format)',
   code=>\&actSummary,
  };

sub actSummary {
  my ($mak,$fields) = @_;
  return 0 if (!$mak->ensureLoaded);

  my @configs = values(%{$mak->selected->{uconfigs}});
  if (!@configs) {
    $mak->vmsg('warn', ref($mak),"::actSummary(): no configurations selected\n");
    return 1;
  }

  $fields = 'summarizeDefault' if (!$fields);
  my $tdata = $mak->configTableFull(\@configs,$fields);

  ##-- Summarize: indent
  my $indent = ' ';
  my $format = $indent.$tdata->{format};
  my $linewd = $tdata->{linewd};
  my %hr = (
	    begin=>($indent.('-' x $linewd)."\n"),
	    end  =>($indent.('-' x $linewd)."\n"),
	    head =>($indent.('=' x $linewd)."\n"),

	    major=>($indent.('=' x $linewd)."\n"),
	    minor=>($indent.('-' x $linewd)."\n"),
	    micro=>($indent.('·' x $linewd)."\n"),
	    none=>'',
	   );

  ##-- Summarize: print headers
  print
    (
     ##-- hrule
     $hr{begin},

     ##-- header
     sprintf($format, map { $_->{title} } @{$tdata->{visible_fields}}),

     ##-- hrule
     $hr{head},
    );

  ##-- Summarize: step 5: print table
  my $crows = $tdata->{crows};
  my ($cf,$i);
  foreach $i (0..$#$crows) {
    $cf = $crows->[$i];
    print
      (
       (defined($cf->{__hr__})
	? $hr{$cf->{__hr__}}    ##-- separator?
	: sprintf($format,      ##-- data row
		  @$cf{map {$_->{title}} @{$tdata->{visible_fields}}}))
      );
  }

  print $hr{end};

  return 1;
}



##---------------------------------------------------------------
## Actions: make

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
    $rc &&= $cfg->make( dir=>$mak->{dir}, makefiles=>$mak->{makefiles}, dummy=>$mak->{dummy} );
    if (!$mak->{dummy}) {
      $mak->syncCollection() if ($mak->{paranoid});
    }
  }
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

## $key2ConfigSet = $mak->partition()
##  + uses $mak->{partition} if present, else id(selection)
sub partition {
  my $mak = shift;
  return $mak->{partition} ? $mak->{partition} : {''=>{map {$_=>$_} values(%{$mak->selected->{uconfigs}})}};
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
## Utilities: fields (sort, display)


## @fieldsExpanded = $mak->fields(\@field_hashes_or_strings, %args)
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

  ##-- Expand fields
  my ($field,$ufield,@parsed,$pathstr,$optstr,$opt,$optkey,$optval);

  my @fields = qw();
  my %pseudo = qw(); ##-- $fields_index => $name_of_pseudo_field
  my @ufields = ref($ufields) && ref($ufields) eq 'ARRAY' ? @$ufields : ($ufields);
  while (defined($ufield=shift(@ufields))) {
    ##-- expand aliases
    $ufield = $alias->{$ufield} while (defined($alias->{$ufield}));

    if ($ufield eq 'auto' || $ufield eq 'all') {
      ##-- check for pseudo-fields: expand later
      push(@fields, $ufield);
      $pseudo{$#fields} = $ufield;
      next;
    }
    elsif (ref($ufield) && ref($ufield) eq 'ARRAY') {
      ##-- aliased to a list of fields: expand & continue parsing
      unshift(@ufields, @$ufield);
      next;
    }
    elsif (ref($ufield) && defined($ufield->{xcode})) {
      ##-- dynamic alias expansion
      unshift(@ufields, $ufield->{code}->($mak,$ufield,%args,configs=>$configs,alias=>$alias));
      next;
    }
    elsif (!ref($ufield)) {
      ##-- parse user-fields: attempt to auto-detect perl code: "(...)" or "{...}"
      if ($ufield =~ /^[\(\{].*[\)\}]$/) {
	##-- perl-coded field spec
	@parsed = eval "{ no strict 'vars'; $ufield }";
	$mak->vmsg('error', "Error parsing perl-code field specification '$ufield': $@") if ($@);
	unshift(@ufields,@parsed);
      }
      else {
	##-- probably a simple field spec-list KEY1(FLAGS1,...), KEY2(FLAGS2,..), ...
	@parsed = qw();
	while ($ufield =~ s/^[\s\,]*([^\(\,\s]+)//) {
	  $pathstr = $1;
	  $optstr  = extract_bracketed($ufield, "\{(\"\')\}");
	  if (defined($optstr)) {
	    ##-- unbracket options-string
	    $optstr =~ s/^\(//;
	    $optstr =~ s/\)$//;
	  }

	  ##-- expand path aliases
	  $pathstr = $alias->{$pathstr} while (defined($alias->{$pathstr}));

	  ##-- test for pseudo-fields
	  if ($pathstr eq 'all' || $pathstr eq 'auto') {
	    push(@parsed, $pathstr);
	  }
	  ##-- test for list-aliases
	  elsif (ref($pathstr) && ref($pathstr) eq 'ARRAY') {
	    push(@parsed, @$pathstr);
	  }
	  ##-- add/change options to known fields
	  elsif (ref($pathstr) && ref($pathstr) eq 'HASH') {
	    push(@parsed, { %$pathstr, opts=>$optstr } );
	  }
	  ##-- full user-specified field
	  else {
	    push(@parsed, {path=>$1,opts=>$2});
	  }
	}
	unshift(@ufields, @parsed);
      }

      next;      ##-- continue parsing
    }

    ##-- set field
    $field=$ufield;

    ##-- parse field key-path
    if (!ref($pathstr = $field->{path})) {
      $field->{path} = [defined($pathstr) ? split(/\s*\-\>\s*/, $pathstr) : qw()];
    }

    ##-- parse field string-options
    if (defined($optstr = $field->{opts})) {
      ##-- auto-detect perl-code options
      if ($optstr =~ /^\{/) {
	my $opthash = eval "{ no strict 'vars'; $optstr }";
	$mak->vmsg('error', "Error parsing perl-code option specification '$optstr': $@") if ($@);
	@$field{keys(%$opthash)} = values(%$opthash);
      }
      else {
	##-- looks like simple comma-separated option string
	foreach $opt (split(/[\s\,]+/, $optstr)) {
	  ($optkey,$optval) = split(/\s*\=\s*/, $opt, 2);
	  $field->{$optkey} = defined($optval) ? $optval : 1;
	}
      }
    }
    delete($field->{opts});

    push(@fields,$field);
  }

  ##-- expand pseudo-fields
  if (%pseudo) {
    ##-- pseudo: get literal fields, indexing by title
    my @fields_literal = grep { ref($_) } @fields;
    my %lit2field      = map { $mak->fieldTitle($_)=>$_ } @fields_literal;

    ##-- auto: get active variables
    my @autovars = $mak->activeVariables($configs);
    my @allvars  = $mak->userVariables($configs);

    ##-- index fields by title
    my @fields_auto    = $mak->fields([map {"xvars->$_"} @autovars], %args);
    my %auto2field     = map { $mak->fieldTitle($_)=>$_ } @fields_auto;

    my @fields_all     = $mak->fields([map {"xvars->$_"}   @allvars], %args);
    my %all2field      = map { $mak->fieldTitle($_)=>$_ } @fields_all;

    ##-- auto: avoid auto-display of some fields
    delete(@auto2field{ keys(%{$args{noauto}}) }) if ($args{noauto});
    delete(@auto2field{ keys(%lit2field) });
    delete(@auto2field{ map { $_->{alt} ? @{$_->{alt}} : qw() } @fields_literal });
    @fields_auto = @auto2field{sort(keys(%auto2field))};

    delete(@all2field{ keys(%lit2field) });
    delete(@all2field{ map { $_->{alt} ? @{$_->{alt}} : qw() } @fields_literal });
    @fields_all = @all2field{sort(keys(%all2field))};

    ##-- expand pseudo-fields
    my ($i);
    foreach $i (sort { $b <=> $a } keys(%pseudo)) {
      $ufield = $pseudo{$i};
      splice(@fields,$i,1, $ufield eq 'auto' ? @fields_auto : @fields_all);
    }
  }

  return @fields;
}

##---------------------------------------------------------------
## Utilities: fields: values

## $pathValue = $mak->pathValue($cfg, \@path)
##  + just follows path; no defaults, eval etc.
sub pathValue {
  my ($mak,$cfg,$path) = @_;
  my $val = $cfg;
  ##-- array-expansion: follow list of keys
  my ($key);
  foreach $key (@$path) {
    return undef if (!defined($val=$val->{$key}));
  }
  return $val;
}

## $cfgFieldValue = $mak->fieldValue($cfg, $field)
sub fieldValue {
  my ($mak,$cfg,$field) = @_;

  ##-- Step 1: get path-value
  my $val = $mak->pathValue($cfg,$field->{path});

  ##-- Step 2: instantiate dynamic value ?!?!?! ---TODO---

  if (!defined($val)) {
    ##-- defaults for undefined values
    if ($field->{n}) { $val = 0; }
    else { $val = ''; }
  }

  if ($field->{eval}) {
    ##-- maybe eval some code
    my $tmp=$_;
    $_=$val; ##-- HACK
    eval qq(no warnings 'void'; \$val=$field->{eval};);
    $_=$tmp; ##-- HACK
    carp(ref($mak)."::fieldValue(): error in eval($field->{eval}): $@") if ($@);
  }
  return $val;
}

## $fieldValueString = $mak->fieldValueString($field,$value)
sub fieldValueString {
  my ($mak,$field,$val) = @_;
  return (defined($field->{fmt}) && $field->{fmt} ne 'auto'
	  ? sprintf($field->{fmt}, $val)
	  : $val);
}

## $fieldTitle = $mak->fieldTitle($field)
sub fieldTitle {
  my ($mak,$field) = @_;
  return $field->{title} if (defined($field->{title}));
  if (defined($field->{evaltitle})) {
    $field->{title} = eval qq({ no warnings 'void'; $field->{evaltitle} });
    carp(ref($mak),"::fieldTitle(): error evaluating field title '$field->{evalTitle}': $@")
      if ($@);
    return $field->{title} if (defined($field->{title}));
  }
  return
    $field->{title} =
    (
     ($#{$field->{path}}==1 && $field->{path}[0] =~ /^[ux]vars/
      ? $field->{path}[$#{$field->{path}}]                 ##-- use final path key element
      : join('->', @{$field->{path}})                      ##-- use whole path key
     ));
}

##---------------------------------------------------------------
## Utilities: pseudo-configurations

## \%fieldTitle2Value = $mak->configFieldHash($cfg, @fieldSpecs)
##   + adds special key ('_' => $cfg)
sub configFieldHash {
  my ($mak,$cfg,@fields) = @_;
  my $cfields = {};
  my ($field);
  foreach $field (@fields) {
    $cfields->{$mak->fieldTitle($field)} = $mak->fieldValue($cfg,$field);
  }
  $cfields->{_} = $cfg;
  return $cfields;
}

## \%fieldTitle2FormattedValue = $mak->configFieldStringHash($cfg, @fieldSpecs)
##   + adds special key ('_' => $cfg)
sub configFieldStringHash {
  my ($mak,$cfg,@fields) = @_;
  my $cfields = {};
  my ($field);
  foreach $field (@fields) {
    $cfields->{$mak->fieldTitle($field)} = $mak->fieldValueString($field,$mak->fieldValue($cfg,$field));
  }
  $cfields->{_} = $cfg;
  return $cfields;
}

## @fieldHashes = $mak->configFieldHashes(\@cfgs, @fieldSpecs)
sub configFieldHashes {
  my ($mak,$cfgs,@fields) = @_;
  return map { $mak->configFieldHash($_,@fields) } @$cfgs;
}

## @fieldStringHashes = $mak->configFieldStringHashes(\@cfgs, @fieldSpecs)
sub configFieldStringHashes {
  my ($mak,$cfgs,@fields) = @_;
  return map { $mak->configFieldStringHash($_,@fields) } @$cfgs;
}


##---------------------------------------------------------------
## Utilities: table generation

## \%configTableFull = $mak->configTableFull(\@configs,$fields,%args)
sub configTableFull {
  my ($mak,$configs,$fields,%args) = @_;
  return $mak->configTableFormat($mak->configTableBasic($configs,$fields,%args),%args);
}

## \%configTableData = $mak->configTableData(\@configs,$fields,%args)
##   + %configTableData keys:
##      fields  =>\@expanded_fields,   ##-- fields
##      rows    =>\@sortedFieldHashes, ##-- just the data
sub configTableBasic {
  my ($mak,$configs,$fields,%args) = @_;

  my $tdata = {};

  $fields = 'summarizeDefault' if (!$fields);
  $fields = $tdata->{fields} = [ $mak->fields($fields, %args, configs=>$configs) ];

  ##-- get (sorted) list of field-value configs
  $tdata->{rows}   = [ $mak->sortFieldHashes([$mak->configFieldHashes($configs,@$fields)]) ];

  return $tdata;
}

## \%configTable = $mak->configTableFormat(\%configTable,%args)
##   + adds %configTable keys:
##       visible_fields => \@expanded_visible_fields,
##       linewd         => $line_width,
##       title2len      => \%fieldTitle2maxLen,
##       format         => $sprintf_format,
##       frows          => \@field_hashes_with_formatted_values,
##       crows          => \@condensed_table_with_separators, ##-- seps: { __hr__=>$how }
sub configTableFormat {
  my ($mak,$tdata,%args) = @_;

  ##-- Format: step 1: get visible fields
  my $fields = $tdata->{fields};
  my $visible_fields = $tdata->{visible_fields} = [grep {!$_->{hidden}} @$fields];

  ##-- Format: step 2: get field lengths (all fields)
  my $title2len = $tdata->{title2len} = { map { $_->{title}=>length($_->{title}) } @$fields };
  my $frows = $tdata->{frows} = [];
  my ($cfin,$cf,$field,$ftitle);
  foreach $cfin (@{$tdata->{rows}}) {
    push(@$frows, $cf={%$cfin});
    foreach $field (@$fields) {
      $ftitle = $field->{title};
      $cf->{$ftitle} = $mak->fieldValueString($field,$cf->{$ftitle});
      $title2len->{$ftitle} = length($cf->{$ftitle}) if (length($cf->{$ftitle}) > $title2len->{$ftitle});
    }
  }

  ##-- Format: step 3: get sprintf() format (visible only)
  $tdata->{format} = join(' ', map { '%'.$title2len->{$_->{title}}.'s' } @$visible_fields)."\n";
  $tdata->{linewd} = -1;
  $tdata->{linewd} += $title2len->{$_->{title}}+1 foreach (@$visible_fields);

  ##-- Format: step 4: insert 'hr' separators
  my %hr2prec = ( major=>30, minor=>20, micro=>10, none=>0 );
  my $crows = $tdata->{crows} = [ { %{$tdata->{frows}[0]} }, ];

  my ($i,$cfprev,$hrhow);
  foreach $i (1..$#$frows) {
    ($cf,$cfprev) = @$frows[$i,$i-1];
    $hrhow        = undef;
    ##-- separate?
    foreach $field (grep {defined($_->{hr})} @$fields) {
      $ftitle = $field->{title};
      if ($cf->{$ftitle} ne $cfprev->{$ftitle}) {
	##-- check hr precedence
	$hrhow = $field->{hr} if (!defined($hrhow) || $hr2prec{$hrhow} < $hr2prec{$field->{hr}});
      }
    }
    push(@$crows, ($hrhow ? { __hr__=>$hrhow } : qw()), { %$cf }); ##-- copy rows
  }

  ##-- Format: step 5: condense field values
  foreach $i (grep { !defined($crows->[$_]{__hr__}) && !defined($crows->[$_-1]{__hr__}) } (1..$#$crows)) {
    ($cf,$cfprev) = @$crows[$i,$i-1];
    foreach $field (grep {$_->{condense}} @$fields) {
      $ftitle = $field->{title};
      if ($cf->{$ftitle} eq $cfprev->{$ftitle}) {
	$cf->{$ftitle} = '';
      }
    }
  }

  return $tdata;
}


##---------------------------------------------------------------
## Utilities: partitioning & collection

## \%differntiaFieldConfigKey_to_Config = $mak->collect(\@fieldsToCollect, %args)
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
  my ($cfg,$diffcf);
  my $key2cset = {};
  foreach $cfg (@$configs) {
    $diffcf = $mak->configFieldHash($cfg,@fields_diff);
    delete($diffcf->{_});
    $key2cset->{$mak->{col}->key($diffcf)}{$cfg} = $cfg;
  }

  return $key2cset;
}


##---------------------------------------------------------------
## Utilities: sort: high-level

## @sorted_selected_configs = $mak->sortSelection(%args)
##  + %args:
##     sortby=>$fieldSpec,
##  + sorts currently selected configs by the field-spec in $sortby (default=$mak->{sortby})
sub sortSelection {
  my $mak = shift;
  return qw() if (!$mak->ensureLoaded());
  return $mak->sortConfigs([values(%{$mak->selected->{uconfigs}})], @_);
}

## @sorted_configs = $mak->sortConfigs(\@configs, %args)
##  + %args:
##     sortby=>$fieldSpec,
##  + sorts @configs by the field-spec in $sortby (default=$mak->{sortby})
sub sortConfigs {
  my ($mak,$configs,%args) = @_;
  my $sortsub = $mak->configSortSub($configs,%args);
  return sort $sortsub @$configs;
}

## @sorted_fieldHashes = $mak->sortFieldHashes(\@fieldHashes, %args)
##  + %args:
##     sortby=>$fieldSpec,
##  + sorts @fieldHashes by the field-spec in $sortby (default=$mak->{sortby})
sub sortFieldHashes {
  my ($mak,$cfs,%args) = @_;
  my $sortsub = $mak->fieldHashSortSub($cfs,%args);
  return sort $sortsub @$cfs;
}

##---------------------------------------------------------------
## Utilities: field-based sorting: configs

## \&cmpcode = $mak->configSortSub($configs,%args)
##  + %args: sortby=>$fields,
sub configSortSub {
  my ($mak,$configs,%args) = @_;
  my $sortby = defined($args{sortby}) && $args{sortby} ne '' ? $args{sortby} : $mak->{sortby};
  my @sbfields = $mak->fields($sortby, configs=>$configs);
  my ($field,$aval,$bval,$cmp);
  return sub($$) {
    foreach $field (@sbfields) {
      $cmp = (defined($aval=$mak->fieldValue($_[0],$field))
	      ? (defined($bval=$mak->fieldValue($_[1],$field))
		 ? ($field->{n}
		    ? $aval <=> $bval
		    : (   $aval =~ /^[\+\-](?:\d*\.?)\d+(?:[Ee][\+\-]\d+)?$/
		       && $bval =~ /^[\+\-](?:\d*\.?)\d+(?:[Ee][\+\-]\d+)?$/
			  ? $aval <=> $bval
			  : $aval cmp $bval))
		 : -1)
	      : 0);
      $cmp = -$cmp if ($field->{r}); ##-- reverse sort
      return $cmp if ($cmp);
    }
    return 0; ##-- incomparable (effectively equal)
  };
}

##---------------------------------------------------------------
## Utilities: field-based sorting: pseudo-configs

## \&cmpcode = $mak->fieldHashSortSub($configs,%args)
##  + %args: sortby=>$fields,
sub fieldHashSortSub {
  my ($mak,$cfs,%args) = @_;
  my $sortby = defined($args{sortby}) && $args{sortby} ne '' ? $args{sortby} : $mak->{sortby};
  my @sbfields = $mak->fields($sortby, configs=>[map {$_->{_}} @$cfs]);
  $mak->fieldTitle($_) foreach (@sbfields); ##-- ensure field titles are defined
  my ($field,$aval,$bval,$cmp);
  return sub($$) {
    foreach $field (@sbfields) {
      $cmp = (defined($aval=$mak->fieldValue($_[0]{_},$field))
	      ? (defined($bval=$mak->fieldValue($_[1]{_},$field))
		 ? ($field->{n}
		    ? $aval <=> $bval
		    : (   $aval =~ /^[\+\-](?:\d*\.?)\d+(?:[Ee][\+\-]\d+)?$/
		       && $bval =~ /^[\+\-](?:\d*\.?)\d+(?:[Ee][\+\-]\d+)?$/
			  ? $aval <=> $bval
			  : $aval cmp $bval))
		 : -1)
	      : 0);
      $cmp = -$cmp if ($field->{r}); ##-- reverse sort
      return $cmp if ($cmp);
    }
    return 0; ##-- incomparable (effectively equal)
  };
}



##---------------------------------------------------------------
## Utilities: variant conditions

## @avars = $mak->activeVariables()
## @avars = $mak->activeVariables(\@configs)
*activeVars = \&activeVariables;
sub activeVariables {
  my ($mak,$configs) = @_;

  ##-- get selection
  if (!$configs) {
    return qw() if (!$mak->ensureLoaded());
    $configs = [values(%{$mak->selected->{uconfigs}})];
  }

  ##-- count number of (var,value) pairs
  my %var2val2n = qw();
  my ($cfg,$var);
  foreach $cfg (@$configs) {
    foreach $var (keys(%{$cfg->{uvars}})) {
      ++$var2val2n{$var}{$cfg->{uvars}{$var}} if (defined($cfg->{uvars}{$var}));
    }
  }

  ##-- get all actually varied ${var}s as those for which:
  ##   + only one defined value exists
  ##      AND
  ##   + every selected user-config declares that value
  my ($val2n);
  my @avars = qw();
  foreach $var (keys(%var2val2n)) {
    $val2n = $var2val2n{$var};
    push(@avars, $var) unless (
			       scalar(keys(%$val2n))==1
			       &&
			       (values(%$val2n))[0] == scalar(@$configs)
			      );
  }

  return @avars;
}


##---------------------------------------------------------------
## Utilities: all user variables

## @uvars = $mak->userVariables()
## @uvars = $mak->userVariables(\@configs)
*userVars = \&userVariables;
sub userVariables {
  my ($mak,$configs) = @_;

  ##-- get selection
  if (!$configs) {
    return qw() if (!$mak->ensureLoaded());
    $configs = [values(%{$mak->selected->{uconfigs}})];
  }

  ##-- get all user vars
  my %uvars = qw();
  my ($cfg,$var);
  foreach $cfg (@$configs) {
    @uvars{keys(%{$cfg->{uvars}})} = undef;
  }

  return keys(%uvars);
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
  $mak->vmsg('info', ref($mak),": loadCollection($mak->{colfile})... ");
  $mak->{col} = MUDL::Make::Collection->loadFile($mak->{colfile});
  if (!$mak->{col}) {
    confess(ref($mak), "::loadCollection($mak->{colfile}): load failed!\n");
    $mak->{loaded} = 0;
    $mak->{coldigest} = '';
    return undef;
  }
  $mak->{loaded} = 1;
  $mak->{coldigest} = $mak->collectionDigest();

  $mak->vmsg('info', "loaded.\n");
  return $mak->{col};
}

## $digest_str = $mak->collectionDigest()
##  + gets MD5 digest string (binary) from $mak->{collection}->savePerlString()
sub collectionDigest {
  my $mak = shift;
  return '' if (!$mak->{loaded});
  return Digest::MD5::md5($mak->{col}->savePerlString);
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

    my $rc = $mak->{col}->saveFile($file);
    $mak->{changed} = 0;
    $mak->{coldigest} = $digest;
    return $rc;
  }

  return 1;
}

1;

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
