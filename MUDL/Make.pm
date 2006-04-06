##-*- Mode: CPerl -*-

## File: MUDL::Make.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: makefile stuff
##======================================================================

package MUDL::Make;
use Cwd qw(abs_path);
use Digest::MD5 qw(md5);
use strict;
use Carp;
our @ISA = qw(MUDL::Object Exporter);

##======================================================================
## Globals
#our $MAKE  = 'env -i make'; ##-- PDL doesn't like empty environment at all (kira)
our $MAKE  = 'make';

our %EXPORT_TAGS =
  (
   progs=>[qw($MAKE)],
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

			   ##-- Selection & sorting
			   selected=>undef,
			   sortby=>[
				    'stage',
				    'emi',
				    'pr',
				    'rc',
				    'tgpr',
				    'tgrc',
				   ],

			   ##-- Summarization
			   display=>[
				     'stage.emi',
				     'lrlabel',
				     ':',
				     'auto',
				     ':',
				     'pr/g',
				     'pr/t',
				     #'ar/g',
				     #'ar/t',
				    ],

			   display_no_auto=>{
					     map { $_=>undef }
					     (
					      'xvars->stage',
					      'xvars->emi',
					      'xvars->lrwhich',
					      'xvars->tcd',
					      'xvars->tcm',
					      'xvars->tccd',
					      'xvars->tccm',
					      'xvars->lrlabel',
					      'xvars->icorpus',
					      'xvars->tbase',
					      'xvars->tcorpus',
					     ),
					    },

			   ##-- Status
			   #dirty=>0,    ##-- true if changes MAY have been made to collection
			   changed=>0,  ##-- true if changes are KNOWN to have been made to collection
			   loaded=>0,   ##-- true if collection is loaded
			   paranoid=>1, ##-- set to true to backup old collection files

			   ##-- Dummy?
			   dummy=>0,

			   ##-- User args
			   @_,
			  );
}

########################################################################
## ACTIONS: General
########################################################################

##======================================================================
## Actions: alias table
our %ACTIONS = qw();

$ACTIONS{null} = $ACTIONS{dummy} = $ACTIONS{','} =
  { syntax=>'null', help=>'no action', code=>sub { return 1; }, };


##======================================================================
## Actions: perform

## $bool = $mak->perform($action,\@ARGV);
sub perform {
  my ($mak,$act,$argv) = @_;
  my $acth = $act;
  $acth = $ACTIONS{$acth} while (defined($ACTIONS{$acth}));

  my $code = ref($acth) ? $acth->{code} : $mak->can($acth);
  if (!defined($code)) {
    carp(ref($mak),"::perform($act): skipping unknown action");
    return 0;
  }

  ##-- perform the action
  return $code->($mak,$argv);
}

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


########################################################################
## ACTIONS: Specific
########################################################################

##======================================================================
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
  #$mak->{dirty}  = 1;

  return 1;
}

##======================================================================
## Actions: ensure loaded

$ACTIONS{ensure} =
  {
   syntax=>'ensure',
   help=>'ensure collection is loaded',
   code=>\&ensureLoaded,
  };


##======================================================================
## Actions: save (explicit)


$ACTIONS{save} =
  {
   syntax=>'save FILE',
   help  =>'save collection to a (new) file FILE',
   code  => \&actSave,
  };
sub actSave {
  my ($mak,$argv) = @_;
  return $mak->ensureLoaded && $mak->{col}->saveFile(shift(@$argv));
}

##======================================================================
## Actions: select

## $subcol = $mak->selected()
##  + uses $mak->{selected} if defined, else $mak->{col}
sub selected {
  my $mak = shift;
  return $mak->{selected} ? $mak->{selected} : $mak->{col};
}

$ACTIONS{xselect} = 
  {
   syntax=>'xselect XCRITERIA',
   help  =>'select a subset of the collection configurations',
   code  => \&actSelectExtended,
  };
sub actSelectExtended {
  my ($mak,$argv) = @_;
  ($mak->ensureLoaded()
   && ($mak->{selected} = $mak->{col}->xcollect(shift(@$argv))));
}

$ACTIONS{select} = $ACTIONS{uselect} = 
  {
   syntax=>'uselect UCRITERIA',
   help  =>'select a subset of the collection configurations',
   code  => \&actSelectUser,
  };
sub actSelectUser {
  my ($mak,$argv) = @_;
  ($mak->ensureLoaded()
   && ($mak->{selected} = $mak->{col}->ucollect(shift(@$argv))));
}

##======================================================================
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


##======================================================================
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


##======================================================================
## Actions: eval CODE

$ACTIONS{eval} =
  { syntax=>'eval CODE',
    help  =>'eval() some perl CODE',
    code  => \&actEval,
  };
sub actEval {
  my ($mak,$argv) = @_;
  my $code = shift(@$argv);
  return 0 if (!$mak->ensureLoaded);

  my $col = $mak->{col};
  my $mcol = $mak->{col};
  #$mak->{dirty} = 1;

  eval "{ no strict; $code; }";
  if ($@) {
    carp(ref($mak)."::eval($code): error: $@");
    return 0;
  }
  return 1;
}


##======================================================================
## Actions: do FILE

$ACTIONS{do} = $ACTIONS{require} = $ACTIONS{read} = 
  { syntax=>'do FILE',
    help  =>'do() a perl FILE',
    code  =>\&actDoFile,
  };

sub actDoFile {
  my ($mak,$argv) = @_;
  my $file = shift(@$argv);
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

  #$mak->{dirty} = 1;
  return $rc;
}

##======================================================================
## Actions: list: sort priority

$ACTIONS{sortby} = $ACTIONS{priority} =
  {
   syntax=>'priority FIELD_1(FLAGS_1), ..., FIELD_N(FLAGS_N)',
   help=>'set sort-priority.  Flags: "n"=numeric, ...?',
   code=>\&actSortBy,
  };

sub actSortBy {
  my ($mak,$argv) = @_;
  unshift(@{$mak->{sortby}}, $mak->fields(shift(@$argv)));
  return 1;
}


##======================================================================
## Actions: list: long

$ACTIONS{listVerbose} = $ACTIONS{listLong} = $ACTIONS{ll} =
  {
   syntax=>'listLong',
   help=>'list selected configurations (long)',
   code=>\&actListLong,
  };

sub actListLong {
  my $mak = shift;
  return 0 if (!$mak->ensureLoaded);
  my @configs = $mak->sortSelection();
  if (@configs) {
    print map { ("  ", $mak->{col}->ukey($_), "\n") } @configs;
  } else {
    print "  (no configurations selected)\n";
  }
  return 1;
}

##======================================================================
## Actions: list: brief

$ACTIONS{listBrief} = $ACTIONS{list} = $ACTIONS{ls} =
  {
   syntax=>'list',
   help=>'list selected configurations (brief)',
   code=>\&actListBrief,
  };

sub actListBrief {
  my $mak = shift;
  return 0 if (!$mak->ensureLoaded);

  my @configs = values(%{$mak->selected->{uconfigs}});
  if (!@configs) {
    print "  (no configurations selected)\n";
    return 1;
  }
  my @avars = $mak->activeVariables(\@configs);

  ##-- get brief pseudo-configs
  my @bconfigs = map { $_->copyBrief(\@avars) } @configs;

  print
    (
     map { ("  ", $mak->{col}->ukey($_), "\n") }
    $mak->sortConfigs(@bconfigs)
    );

  return 1;
}

##======================================================================
## Actions: Summarize (ASCII table format)

$ACTIONS{summarize} = $ACTIONS{summary} = $ACTIONS{table} = $ACTIONS{tab} =
  {
   syntax=>'summary',
   help=>'summarize selected configurations (table-format)',
   code=>\&actSummary,
  };

sub actSummary {
  my $mak = shift;
  return 0 if (!$mak->ensureLoaded);

  my @configs = values(%{$mak->selected->{uconfigs}});
  if (!@configs) {
    print STDERR ref($mak),"::actSummary(): no configurations selected\n";
    return 1;
  }

  ##-- get active variables
  my @avars   = $mak->activeVariables(\@configs);

  ##-- get display fields
  my @dfields_literal = $mak->fields( grep {$_ ne 'auto'} @{$mak->{display}} );
  my %lit2field        = map { $mak->fieldTitle($_)=>$_ } @dfields_literal;

  my @dfields_auto = $mak->fields(map {"xvars->$_"} sort(@avars));
  my %auto2field   = map { $mak->fieldTitle($_)=>$_ } @dfields_auto;

  ##-- avoid auto-display of some fields
  delete(@auto2field{ keys(%{$mak->{display_no_auto}}), keys(%lit2field) });
  @dfields_auto = @auto2field{sort(keys(%auto2field))};

  ##-- get list of fields to display
  my @fields = @dfields_literal;
  foreach (0..$#{$mak->{display}}) {
    if ($mak->{display}[$_] eq 'auto') {
      splice(@fields,$_,0, @dfields_auto);
      last;
    }
  }

  ##-- get brief config list
  my @bconfigs = map { $_->copyBrief(\@avars) } @configs;

  ##-- Summarize: step 1: get field lengths
  my %title2len = map { $_->{flags}{title}=>length($_->{flags}{title}) } @fields;
  my ($cfg,$cfields,$field,$fval,$ftitle);
  foreach $cfg (@bconfigs) {
    $cfields = $cfg->{cfields} = {};
    foreach $field (@fields) {
      $ftitle = $field->{flags}{title};
      $fval   = $cfields->{$ftitle} = $mak->fieldValueString($cfg,$field);
      $title2len{$ftitle} = length($fval) if (length($fval) > $title2len{$ftitle});
    }
  }

  ##-- Summarize: step 2: get sprintf() format
  my $fmt    = ' '.join(' ', map { '%'.$title2len{$_->{flags}{title}}.'s' } @fields)."\n";
  my $linewd = 1;
  $linewd += $_+1 foreach (values(%title2len));

  ##-- Summarize: step 2: print headers
  print
    (
     ##-- hrule
     ' ', ('-' x $linewd), "\n",

     ##-- header
     sprintf($fmt, map { $_->{flags}{title} } @fields),

     ##-- hrule
     ' ', ('-' x $linewd), "\n",
    );

  ##-- Summarize: step 3: print configs
  foreach $cfg ($mak->sortConfigs(@bconfigs)) {
    print sprintf($fmt, map { $cfg->{cfields}{$_->{flags}{title}} } @fields);
  }


  print
    ##-- hrule
     ' ', ('-' x $linewd), "\n";

  return 1;
}


##======================================================================
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
  foreach (values(%{$mak->selected()->{uconfigs}})) {
    $rc &&= $_->make( dir=>$mak->{dir}, makefiles=>$mak->{makefiles}, dummy=>$mak->{dummy} );
    if (!$mak->{dummy}) {
      #$mak->{dirty} = 1;
      $mak->syncCollection() if ($mak->{paranoid});
    }
  }
  #$mak->{dirty} = 1 if (!$mak->{dummy});
  return $rc;
}



########################################################################
## UTILITIES
########################################################################

##======================================================================
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
    #$mak->{dirty} = 1;
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

##======================================================================
## Utilities: fields (sort, display)

## %FIELD_ALIAS: $pseudoKey=>{ key=>\@nestedKeys, flags=>$sortByFlags, ... }
our %FIELD_ALIAS =
  (
   ##-- Filler(s)
   ':'     => { key=>[], flags=>{eval=>'":"', title=>':'}, },

   ##-- MetaProfile: label
   lrlabel => { key=>[qw(xvars lrlabel)], flags=>{n=>0, fmt=>'auto', title=>'lrlab'} },
   lrlab   => { key=>[qw(xvars lrlabel)], flags=>{n=>0, fmt=>'auto', title=>'lrlab'} },

   ##-- MetaProfile: numeric indices
   'stage' => { key=>[qw(xvars stage)], flags=>{n=>1, fmt=>'%-2d', title=>'stg'} },
   'emi'   => { key=>[qw(xvars emi)],   flags=>{n=>1, fmt=>'%-2d', title=>'emi'} },
   'stage.emi' => {
		   key=>[qw(xvars)],
		   flags=>{
			   n=>0,
			   fmt=>'%5.2f',
			   eval=>'sprintf("%2d.%02d", $_->{stage}, $_->{emi})',
			   title=>'stg.emi',
			  },
		  },

   ##-- Eval: Global
   'pr/g'  => { key=>[qw(eval_global precision)], flags=>{n=>1, fmt=>'%6.2f', eval=>'100*$_', title=>'pr/g'} },
   'rc/g'  => { key=>[qw(eval_global recall)],    flags=>{n=>1, fmt=>'%6.2f', eval=>'100*$_', title=>'rc/g'} },
   'F/g'   => { key=>[qw(eval_global F)],         flags=>{n=>1, fmt=>'%6.2f', eval=>'100*$_', title=>'F/g' } },
   'ar/g'  => { key=>[qw(eval_global arate1)],    flags=>{n=>1, fmt=>'%7.4f', eval=>'0+$_',   title=>'ar/g'} },

   ##-- Eval: Targets
   'pr/t'  => { key=>[qw(eval_targets precision)], flags=>{n=>1, fmt=>'%6.2f', eval=>'100*$_', title=>'pr/t'} },
   'rc/t'  => { key=>[qw(eval_targets recall)],    flags=>{n=>1, fmt=>'%6.2f', eval=>'100*$_', title=>'rc/t'} },
   'F/t'   => { key=>[qw(eval_targets F)],         flags=>{n=>1, fmt=>'%6.2f', eval=>'100*$_', title=>'F/t' } },
   'ar/t'  => { key=>[qw(eval_targets arate1)],    flags=>{n=>1, fmt=>'%7.4f', eval=>'0+$_',   title=>'ar/t'} },
  );

## @fieldsExpanded = $mak->fields(@field_hashes_or_aliases_or_strings)
sub fields {
  my ($mak,@ufields) = @_;

  my ($keystr,$flagstr, $flag,$flagkey,$flagval, @fieldkeys,%fieldflags);
  my ($ufield,$field);
  my @fields = qw();

  foreach $ufield (@ufields) {
    ##-- expand aliases
    $ufield = $FIELD_ALIAS{$ufield} while (defined($FIELD_ALIAS{$ufield}));

    if (ref($ufield)) {
      $field = $ufield;
    } else {
      ##-- parse it
      while ($ufield =~ /^[\s\,]*
                          ([^\(\,\s]+)
                          (?: \( ([^\)]*) \) )?
                        /xg)
      {
	($keystr,$flagstr) = ($1,$2);
	%fieldflags = qw();
	if (defined($flagstr)) {
	  foreach $flag (split(/[\s\,]+/, $flagstr)) {
	    ($flagkey,$flagval) = split(/\s*\=\s*/, $flag, 2);
	    $fieldflags{$flagkey} = defined($flagval) ? $flagval : 1;
	  }
	}
	@fieldkeys = split(/\s*\-\>\s*/, $keystr);
	$field = { key=>[@fieldkeys], flags=>{%fieldflags} };
      }
    }

    ##-- now, expand the field
    #@{$field->{key}} = map { exists($FIELD_ALIAS{$_}) ? @{$FIELD_ALIAS{$_}{key}} : $_ } @{$field->{key}};
    push(@fields, $field);
  }

  return @fields;
}

## $cfgFieldValue = $mak->fieldValue($cfg, $field)
sub fieldValue {
  my ($mak,$cfg,$field) = @_;
  my $val = $cfg;
  foreach (@{$field->{key}}) {
    return undef if (!defined($val=$val->{$_}));
  }

  if (!defined($val)) {
    ##-- defaults for undefined values
    if ($field->{flags} && $field->{flags}{n}) { $val = 0; }
    else { $val = ''; }
  }

  if ($field->{flags} && $field->{flags}{eval}) {
    ##-- maybe eval some code
    $_=$val;
    eval qq(no warnings 'void'; \$val=$field->{flags}{eval};);
    carp(ref($mak)."::fieldValue(): error in eval($field->{flags}{eval}): $@") if ($@);
  }
  return $val;
}

## $cfgFieldString = $mak->fieldValueString($cfg,$field)
sub fieldValueString {
  my ($mak,$cfg,$field) = @_;
  my $val = $mak->fieldValue($cfg,$field);
  return ($field->{flags} && defined($field->{flags}{fmt}) && $field->{flags}{fmt} ne 'auto'
	  ? sprintf($field->{flags}{fmt}, $val)
	  : $val);
}

## $fieldTitle = $mak->fieldTitle($field)
sub fieldTitle {
  my ($mak,$field) = @_;
  return $field->{flags}{title} if ($field->{flags} && defined($field->{flags}{title}));
  return
    $field->{flags}{title} =
    (
     #$field->{key}[$#{$field->{key}}]                                    ##-- use final key element
     join('->', @{$field->{key}})                                         ##-- use whole key
    );
}



##======================================================================
## Utilities: sort: high-level

## @configs = $mak->sortSelection()
sub sortSelection {
  my $mak = shift;
  return qw() if (!$mak->ensureLoaded());
  return $mak->sortConfigs(values(%{$mak->selected->{uconfigs}}));
}

## @configs_sorted = $mak->sortConfigs(@configs)
sub sortConfigs {
  my ($mak,@configs) = @_;

  my ($field,$aval,$bval,$cmp);
  return
    sort {
      foreach $field ($mak->fields(@{$mak->{sortby}})) {
	$cmp = (defined($aval=$mak->fieldValue($a,$field))
		? (defined($bval=$mak->fieldValue($b,$field))
		   ? ($field->{flags} && $field->{flags}{n}
		      ? $aval <=> $bval
		      : $aval cmp $bval)
		   : -1)
		: 0);
	return $cmp if ($cmp);
      }
      return 0; ##-- incomparable (effectively equal)
    } @configs;
}




##======================================================================
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





########################################################################
## I/O
########################################################################

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
  $mak->{col} = MUDL::Make::Collection->loadFile($mak->{colfile});
  if (!$mak->{col}) {
    confess(ref($mak)."::loadCollectio($mak->{colfile}): load failed!\n");
    $mak->{loaded} = 0;
    #$mak->{dirty}  = 0;
    $mak->{coldigest} = '';
    return undef;
  }
  $mak->{loaded} = 1;
  #$mak->{dirty}  = 0;
  $mak->{coldigest} = $mak->collectionDigest();

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
   syntax=>'sync',
   help  =>'synchonize collection to the default file',
   code  => \&syncCollection,
  };

sub syncCollection {
  my ($mak,$file) = @_;
  return 0 if (!$mak->ensureLoaded());

  my ($changed,$digest) = $mak->changed();
  if ($changed) {
    ##-- be paranoid ?
    if ($mak->{paranoid}) {
      my $ctr = 0;
      $file = $mak->{colfile} if (!$file);
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
