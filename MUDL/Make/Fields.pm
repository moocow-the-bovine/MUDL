##-*- Mode: CPerl -*-

## File: MUDL::Make::Fields.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: make administration: config (pseudo-)fields
##======================================================================

package MUDL::Make::Fields;
use MUDL::Make::FieldData;
use Text::Balanced qw(extract_bracketed);
use strict;
use Carp;
our @ISA = qw(MUDL::Object Exporter);

##======================================================================
## Globals

##---------------------------------------------------------------
## Globals: Field Aliases (end = search for 'EOFIELDS')

## %FIELDS: ( $fieldName => \%fieldSpecHash, ..., $aliasName=>$fieldName, )
##  + %fieldSpecHash keys:
##     path => \@path,       ##-- nested MUDL::Make::Config key-path
##     n    => $bool,        ##-- true iff numeric
##     fmt  => $how,         ##-- sprintf template for tabular formatting (default='auto')
##     title => $title,      ##-- field title (for tab)
##     evaltitle=>$str,      ##-- field title, eval'd (with variable $field set to full field)
##     alt   => \@titles,    ##-- alternative titles
##     eval  => $eval,       ##-- eval() for value adjustment (vars $cfg, $field, $val are set)
##     hr    => $how,        ##-- summarize(): separator type for changed values qw(major minor micro)
##     condense=>$bool,      ##-- summarize(): condense consecutive duplicate values?
##     expand_code =>\&code, ##-- dynamic alias: calls (\@expanded=$code->($mf,$pfield,\@xfields)) to expand
##                           ##   + code should return expanded fields as ARRAY-ref
##     hidden=>$bool,        ##-- if true, field is not displayed
##
##     ##-- TODO--
##     vcode =>\&vcode,      ##-- dynamic value: calls $code->($mf,$cfg,$field) for val
##     ....
our %FIELDS =
  (
   ##-- Pseudo-fields
   'all'  => {
	      ##-- all user-defined fields
	      expand_code=>\&_expand_all, expand_priority=>100,
	     },
   'auto' => {
	      ##-- all user-defined fields in selection which actually vary
	      expand_code=>\&_expand_auto, expand_priority=>1000,
	     },

   ##-- Action-specific field aliases
   'listDefault' => [ qw(auto) ],
   'sortDefault' => [
		     qw(stage emi corpus lrlab auto),
		      #qw(pr:g rc:g pr:t rc:t)
		    ],
   'collectDefault' => [
			  qw(corpus stage),
			  #qw(emi),
			 ],

   'tabDefault'       => 'summarizeDefault',
   'tabId'            => 'summarizeId',
   'tabResults'       => 'summarizeResults',
   'summarizeDefault' => 'summarizeId,|,summarizeResults',
   'summarizeId' => [
		     'stage.emi',
		     'corpus',
		     #'lang',
		     'lrlabel',
		     #':',
		     'auto',
		    ],
   'summarizeResults'=>[
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
   'tags:pr:g' => { expand_code=>\&_expand_tags, _tag_field=>'tagpr:g', _tag_var=>'tag', },
   'tags:rc:g' => { expand_code=>\&_expand_tags, _tag_field=>'tagrc:g', _tag_var=>'tag', },
   'tags:F:g'  => { expand_code=>\&_expand_tags, _tag_field=>'tagF:g', _tag_var=>'tag', },

   ##-------------------------------------
   ## Eval: All single-tag-*: Targets
   'tags:pr:t' => { expand_code=>\&_expand_tags, _tag_field=>'tagpr:t', _tag_var=>'tag', },
   'tags:rc:t' => { expand_code=>\&_expand_tags, _tag_field=>'tagrc:t', _tag_var=>'tag', },
   'tags:F:t'  => { expand_code=>\&_expand_tags, _tag_field=>'tagF:t', _tag_var=>'tag', },

   ##-------------------------------------------------
   ## Eval: max-value search (TODO)
   '*'=>'max',
   'best'=>'max',
   'max' => { expand_code =>\&_expand_max,
	      of          =>'pr:g',        ##-- target field: to be set by user or alias
	      for         =>'stage,emi',   ##-- field list, max for every value-tuple of these fields
	      evaltitle   =>'"max(of=$field->{of},for=($field->{for}))"',
	      hidden      =>0,
	    },
  );
##-- EOFIELDS

##---------------------------------------------------------------
## Fields: expanders: 'max'

## \@expanded = _expand_max($mf,$max_field,\@xfields)
sub _expand_max {
  my ($mf,$max_field,$xfields) = @_;

  my $for_fields = $mf->expand($max_field->{for});
  my $of_field   = $mf->expand($max_field->{of})->[0];
  my ($cfg,%for_vars,$for_key,$of_val);
  my $for2max    = $max_field->{for2max} = {};
  foreach $cfg (@{$mf->{configs}}) {
    %for_vars = map { $_->{title}=>$mf->fieldValue($cfg,$_) } @$for_fields;
    $for_key = $max_field->{cfg2forkey}{$cfg} = vkey(\%for_vars);
    $of_val  = $mf->fieldValue($cfg,$of_field);
    $for2max->{$for_key} = $of_val
      if (defined($of_val) && (!defined($for2max->{$for_key}) || $of_val > $for2max->{$for_key}));
  }
  $max_field->{eval} = '$field->{for2max}{$field->{cfg2forkey}{$cfg}}';

  my @safe_src_keys = grep { $_ ne 'title' && !exists($max_field->{$_}) } keys(%$of_field);
  @$max_field{@safe_src_keys} = @$of_field{@safe_src_keys};

  return [$max_field];
}

## $varsKey = vkey(\%hash)
sub vkey {
  my $h = shift;
  return join(' ', map { "$_=$h->{$_}" } sort(keys(%$h)));
}

##---------------------------------------------------------------
## Fields: expanders: 'all'

## \@expanded = _expand_all($mf,$all_field,\@xfields)
sub _expand_all {
  my ($mf,$all_field,$xfields) = @_;

  ##-- get literal fields, indexing by title
  my @fields_literal = grep { ref($_) } @$xfields;
  my %lit2field      = map { $mf->fieldTitle($_)=>$_ } @fields_literal;

  ##-- get all user variables, indexing by title
  my @allvars     = userVariables($mf->{configs});
  my $fields_all  = $mf->expand([map {"xvars->$_"}  @allvars]);
  #my %all2field   = map { $mf->fieldTitle($_)=>$_ } @$fields_all;
  my %all2field   = map { $_->{title}=>$_ } @$fields_all;

  ##-- avoid inclusion of literal fields
  delete(@all2field{ keys(%lit2field) });
  delete(@all2field{ map { $_->{alt} ? @{$_->{alt}} : qw() } @fields_literal });

  ##-- avoid inclusion of user-specified 'except' fields
  if ($all_field->{except}) {
    my $except = $mf->expand($all_field->{except});
    delete(@all2field{ map { $_->{title} } @$except });
    delete(@all2field{ map { $_->{alt} ? @{$_->{alt}} : qw() } @$except });
  }

  #splice(@$xfields,$fi,1, @all2field{sort{keys(%all2field)}});
  return [ @all2field{sort keys %all2field} ];
}

## \@expanded = _expand_auto($mf,\@xfields,$findex)
sub _expand_auto {
  my ($mf,$auto_field,$xfields) = @_;

  ##-- get literal fields, indexing by title
  my @fields_literal = grep { ref($_) } @$xfields;
  my %lit2field      = map { $mf->fieldTitle($_)=>$_ } @fields_literal;

  ##-- get active user variables, indexing by title
  my @autovars    = activeVariables($mf->{configs});
  my $fields_auto = $mf->expand([map {"xvars->$_"}  @autovars]);
  #my %auto2field  = map { $mf->fieldTitle($_)=>$_ } @$fields_auto;
  my %auto2field  = map { $_->{title}=>$_ } @$fields_auto;

  ##-- avoid inclusion of literal fields
  delete(@auto2field{ keys(%lit2field) });
  delete(@auto2field{ map { $_->{alt} ? @{$_->{alt}} : qw() } @fields_literal });

  ##-- avoid inclusion of user-specified 'except' fields
  if ($auto_field->{except}) {
    my $except = $mf->expand($auto_field->{except});
    delete(@auto2field{ map { $_->{title} } @$except });
    delete(@auto2field{ map { $_->{alt} ? @{$_->{alt}} : qw() } @$except });
  }

  #splice(@$xfields, $i, 1, @auto2field{sort{keys(%auto2field)}});
  return [ @auto2field{sort keys %auto2field} ];
}


##---------------------------------------------------------------
## Fields: expanders: 'tags:...'

## \@expanded = _expand_tags($mf,$alltags_field,\@xfields)
sub _expand_tags {
  my ($mf,$pfield) = @_;
  my $configs  = $mf->{configs};
  my $tagfield = $pfield->{_tag_field};
  my $tagvar   = $pfield->{_tag_var};
  my $srcfield = $mf->{alias}{$tagfield};

  my ($cfg,$info);
  my %tags2 = qw();
  ##-- gather known tag2 values
  foreach $cfg (@$configs) {
    $info = pathValue($cfg,$srcfield->{path});
    @tags2{keys(%$info)} = undef;
  }

  ##-- map tag2-keys to expanded fields
  #splice(@$xfields, $fi,1, map { {%$srcfield, $tagvar=>$_} } sort(keys(%tags2)));
  return [map { {%$srcfield, $tagvar=>$_} } sort(keys(%tags2))];
}

##---------------------------------------------------------------
## Globals: Exporter (see end of perl code, near '__END__' token)
our %EXPORT_TAGS =
  (
   'alias'=>[qw(%FIELDS)],
  );


##======================================================================
## Constructor (inherited)
sub new {
  my $that = shift;
  my $mf = $that->SUPER::new(
			     ##-- Basic data
			     mak=>undef,    ##-- referencing MUDL::Make object (HACK)
			     configs=>[],   ##-- all configs for these fields
			     fields=>[''],  ##-- user-selected fields
			     xfields=>[],   ##-- expanded fields

			     ##-- field-value data
			     #cf=>[],        ##-- field-values
			     #config2cf=>{}, ##-- maps configs to field-values (obsolete?)

			     ##-- field aliases
			     alias=>\%FIELDS,

			     ##-- Boolean: expanded?
			     expanded=>0,

			     @_,
			    );

  #$mf->fields($mf->{fields}); ##-- parse & expand fields

  return $mf;
}

##---------------------------------------------------------------
## Manipulators: configs

## \@configs = $mf->configs()
## \@configs = $mf->configs(\@configs)
sub configs {
  my ($mf,$configs) = @_;
  if (defined($configs)) {
    $mf->{configs}  = $configs;
    $mf->{expanded} = 0;
  }
  return $mf->{configs};
}

## \@user_fields = $mf->fields()
## \@user_fields = $mf->fields(@field_hashes_or_strings)
sub fields {
  my $mf = shift;
  if (@_) {
    @{$mf->{fields}} = [grep { defined($_) } @_];
    $mf->{expanded} = 0;
  }
  return $mf->{fields};
}

## \@expanded_fields = $mf->xfields()
*ensureExpanded = \&xfields;
sub xfields {
  my $mf = shift;
  if (!$mf->{expanded}) {
    @{$mf->{fieldx}} = qw();
    $mf->expand($mf->{fields}, to=>$mf->{xfields});
    $mf->{expanded} = 1;
  }
  return $mf->{xfields};
}


##---------------------------------------------------------------
## Generators: field-data hashes (pseudo-configurations)

## @configs_fdata = $mf->fieldData(@configs)
sub fieldData {
  my $mf = shift;
  return map { MUDL::Make::FieldData->new(mfields=>$mf, config=>$_) } @_;
}


##---------------------------------------------------------------
## Utilities: fields: values

## $cfgFieldValue = $mf->fieldValue($cfg, $field)
sub fieldValue {
  my ($mf,$cfg,$field) = @_;

  ##-- Step 1: get path-value
  my $val = pathValue($cfg,$field->{path});

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
    carp(ref($mf)."::fieldValue(): error in eval($field->{eval}): $@") if ($@);
  }
  return $val;
}

## $fieldValueString = $mf->fieldValueString($field,$value)
sub fieldValueString {
  my ($mf,$field,$val) = @_;
  return (defined($field->{fmt}) && $field->{fmt} ne 'auto'
	  ? sprintf($field->{fmt}, $val)
	  : $val);
}

## $fieldTitle = $mf->fieldTitle($field)
##  + destructive alters $field (adds 'title' key if not present)
sub fieldTitle {
  #my ($mf,$field) = @_;
  #return $field->{title} if (defined($field->{title}));
  ##--
  return $_[1]{title} if (defined($_[1]{title}));
  my ($mf,$field) = @_;
  ##--
  if (defined($field->{evaltitle})) {
    $field->{title} = eval qq({ no warnings 'void'; $field->{evaltitle} });
    carp(ref($mf),"::fieldTitle(): error evaluating field title '$field->{evalTitle}': $@")
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
## Utilities: sort: configs

## @sorted_configs = $mf->sortConfigs(\@configs,%args)
##  + sorts \@configs by $args{sortby}
##  + %args:
##     sortby =>$sortby_fieldspec,  ##-- default: $mf->xfields()
sub sortConfigs {
  my ($mf,$configs,%args) = @_;
  $configs    = $mf->{configs} if (!$configs);
  my $sortsub = $mf->configSortSub(%args);
  return sort $sortsub @$configs;
}

## @sorted_fdata = $mf->sortFieldData(\@fieldData,%args)
##  + sorts \@fieldData by $args{sortby}
##  + %args:
##      sortby =>$sortby_fieldspec,  ##-- default: $mf->xfields()
sub sortFieldData {
  my ($mf,$fdata,%args) = @_;
  $fdata = [$mf->fieldData(@{$mf->{configs}})] if (!$fdata);
  my $sortsub = $mf->fieldDataSortSub(%args);
  return sort $sortsub @$fdata;
}

##---------------------------------------------------------------
## Utilities: sort: comparision: configs

## \&cmpcode = $mf->configSortSub(%args)
##  + %args:
##     #configs=>\@configs,          ##-- default: $mf->{configs}: for field-expansion
##     sortby =>$sortby_fieldspec,  ##-- default: $mf->xfields()
sub configSortSub {
  my ($mf,%args) = @_;
  my $sbfields = $args{sortby}  ? $mf->expand($args{sortby}) : $mf->xfields;

  my ($field,$aval,$bval,$cmp);
  return sub($$) {
    foreach $field (@$sbfields) {
      $cmp = (defined($aval=$mf->fieldValue($_[0],$field))
	      ? (defined($bval=$mf->fieldValue($_[1],$field))
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
## Utilities: sort: comparison: field data

## \&cmpcode = $mf->fieldDataSortSub()
sub fieldDataSortSub {
  my $mf = shift;
  my $cfgSortSub = $mf->configSortSub();
  return sub($$) { return $cfgSortSub->($_[0]{_}, $_[1]{_}); };
}



##---------------------------------------------------------------
## Guts: Field-expansion

## \@fields_expanded = $mf->expand(\@field_hashes_or_strings, %args)
##   + %args:
##      to=>\@expandedFieldArray,
sub expand {
  my ($mf,$ufields,%args) = @_;

  ##-- get configs
  my $configs = $mf->{configs};
  my $alias   = $mf->{alias} ? $mf->{alias} : \%FIELDS;

  ##-- Expand fields
  my ($field,$ufield,@parsed,$pathstr,$optstr,$opt,$optkey,$optval);

  my @ufields = ref($ufields) && ref($ufields) eq 'ARRAY' ? @$ufields : ($ufields);
  my $xfields = $args{to} ? $args{to} : [];

  while (defined($ufield=shift(@ufields))) {
    ##-- expand aliases
    $ufield = $alias->{$ufield} while (defined($alias->{$ufield}));

    if (ref($ufield) && ref($ufield) eq 'ARRAY') {
      ##-- aliased to a list of fields: expand & continue parsing
      unshift(@ufields, @$ufield);
      next;
    }
    #elsif (ref($ufield) && defined($ufield->{expand_code})) {
    #  ##-- dynamic alias expansion (list): LATER
    #  unshift(@ufields, $ufield->{code}->($mf, $ufield, %args));
    #  next;
    #}
    elsif (!ref($ufield)) {
      ##-- parse user-fields: attempt to auto-detect perl code: "(...)" or "{...}"
      if ($ufield =~ /^[\(\{].*[\)\}]\s*$/) {
	##-- perl-coded field spec
	@parsed = eval "{ no strict 'vars'; $ufield }";
	carp(ref($mf), ": Error parsing perl-code field specification '$ufield': $@") if ($@);
	unshift(@ufields,@parsed);
      }
      else {
	##-- probably a simple field spec-list KEY1(FLAGS1,...), KEY2(FLAGS2,..), ...
	@parsed = qw();
	while ($ufield =~ s/^[\s\,]*([^\(\{\,\s]+)//) {
	  $pathstr = $1;
	  $optstr  = extract_bracketed($ufield, "\{(\"\')\}");
	  if (defined($optstr)) {
	    ##-- unbracket options-string
	    $optstr =~ s/^\(//;
	    $optstr =~ s/\)$//;
	  }

	  ##-- expand path aliases
	  $pathstr = $alias->{$pathstr} while (defined($alias->{$pathstr}));

	  ##-- test for list-aliases
	  if (ref($pathstr) && ref($pathstr) eq 'ARRAY') {
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
    $field={ %$ufield };

    ##-- parse field key-path
    if (!ref($pathstr = $field->{path})) {
      $field->{path} = [defined($pathstr) ? split(/\s*\-\>\s*/, $pathstr) : qw()];
    }

    ##-- parse field string-options
    if (defined($optstr = $field->{opts})) {
      ##-- auto-detect perl-code options
      if ($optstr =~ /^\{/) {
	my $opthash = eval "{ no strict 'vars'; $optstr }";
	carp(ref($mf), ": Error parsing perl-code option specification '$optstr': $@") if ($@);
	@$field{keys(%$opthash)} = values(%$opthash);
      }
      else {
	while ($optstr =~ s/^[\s\,]*([^\=\,\s\(\{]+)(?:\=?)//) {
	  $optkey = $1;
	  $optval = extract_bracketed($optstr, "{(\"\')}");
	  if (!$optval && $optstr =~ s/^([^\s\,]*)//) {
	    $optval = $1;
	  }
	  if (defined($optval)) {
	    ##-- parse option-value
	    if ($optval =~ /^[\"\'\(]/) {
	      ##-- quoted or parenthesizd: literal
	      $optval =~ s/^[\"\'\(]//;
	      $optval =~ s/[\"\'\)]$//;
	      $field->{$optkey} = $optval;
	    }
	    elsif ($optval =~ /^\{.*\}/) {
	      ##-- perl code
	      my $eoptval = eval "{ no strict 'vars'; $optval }";
	      carp(ref($mf), ": Error parsing perl-code option value '$optval': $@") if ($@);
	      $field->{$optkey} = $optval;
	    }
	    else {
	      ##-- literal value
	      $field->{$optkey} = $optval;
	    }
	  }
	  else {
	    ##-- no value: assume boolean flag
	    $field->{$optkey} = 1;
	  }
	}
	if ($optstr !~ /^[\s\,]*$/) {
	  ##-- unparsed junk
	  carp(ref($mf), ": unparsed junk at end of option string '$optstr'");
	}
      }
    }
    delete($field->{opts});

    push(@$xfields,$field);
  }

  ##-- dynamic field expansion
  my ($expanded);
  my @expandable = sort _expand_field_cmp grep {$_->{expand_code}} @$xfields;
  foreach $field (@expandable) {
    $expanded = $field->{expand_code}->($mf,$field,$xfields);
    @$xfields = map { $_ eq $field ? @$expanded : $_ } @$xfields;
  }

  ##-- expand field titles
  $mf->fieldTitle($_) foreach (@$xfields);

  return $xfields;
}


##---------------------------------------------------------------
## Guts: Field-expansion: dynamically expanded fields: sorting

## $cmp = _expand_field_cmp($a,$b)
sub _expand_field_cmp {
  our ($a,$b);
  return (defined($a->{expand_priority})
	  ? (defined($b->{expand_priority})
	     ? $a->{expand_priority} <=> $b->{expand_priority}
	     : $a->{expand_priority} <=> 0)
	  : (defined($b->{expand_priority})
	     ? 0 <=> $b->{expand_priority}
	     : 0));
}



##======================================================================
## FUNCTIONS
##======================================================================

##---------------------------------------------------------------
## Functions: path values

## $pathValue = pathValue($cfg, \@path)
##  + just follows path; no defaults, eval etc.
push(@{$EXPORT_TAGS{utils}}, 'pathValue');
sub pathValue {
  my ($cfg,$path) = @_;
  my $val = $cfg;
  ##-- array-expansion: follow list of keys
  my ($key);
  foreach $key (@$path) {
    return undef if (!defined($val=$val->{$key}));
  }
  return $val;
}

##---------------------------------------------------------------
## Functions: variant conditions

## @avars = activeVariables(\@configs)
push(@{$EXPORT_TAGS{utils}}, 'activeVariables');
sub activeVariables {
  my $configs = shift;

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
## Functions: all user variables

## @uvars = userVariables(\@configs)
push(@{$EXPORT_TAGS{utils}}, 'userVariables');
sub userVariables {
  my $configs = shift;

  ##-- get all user vars
  my %uvars = qw();
  my ($cfg,$var);
  foreach $cfg (@$configs) {
    @uvars{keys(%{$cfg->{uvars}})} = undef;
  }

  return keys(%uvars);
}




##---------------------------------------------------------------
## Globals: Exporter

$EXPORT_TAGS{all} = [map {@$_} values(%EXPORT_TAGS)];
our @EXPORT_OK    = @{$EXPORT_TAGS{all}};

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
