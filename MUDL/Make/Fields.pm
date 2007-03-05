##-*- Mode: CPerl -*-

## File: MUDL::Make::Fields.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: make administration: config (pseudo-)fields
##======================================================================

package MUDL::Make::Fields;
BEGIN { print STDERR __PACKAGE__  , " loading...\n"; }
use MUDL::Make::FieldData;
use Text::Balanced qw(extract_bracketed);
use strict;
use Carp;
our @ISA = qw(MUDL::Object Exporter);

##======================================================================
## Globals

##-------------------
## @_eval_base_fields
##  + many generated fields are created by iterating over these
our @_eval_base_fields =
  (
   ##-- Total (token-wise)
   qw(tpr:g trc:g tF:g),
   qw(tpr:t trc:t tF:t),
   qw(tpr:tk trc:tk tF:tk),

   ##-- Meta (token-wise)
   qw(pr:g rc:g F:g ar:g),
   qw(pr:t rc:t F:t ar:t),
   qw(pr:tk rc:tk F:t ar:tk),

   ##-- Adjusted Meta (token-wise)
   qw(ampr:g amrc:g amF:g),
   qw(ampr:t amrc:t amF:t),
   qw(ampr:tk amrc:tk amF:tk),

   ##-- MI,H (token-wise)
   qw(mi:g mi:t mi:tk),
   qw(Hpr:g Hrc:g HF:g HI:g),
   qw(Hpr:t Hrc:t HF:t HI:t),
   qw(Hpr:tk Hrc:tk HF:tk HI:tk),

   ##-- Average tag2 (Schütze-style)
   qw(apr:g arc:g aF:g),
   qw(apr:t arc:t aF:t),
   qw(apr:tk arc:tk aF:tk),

   ##-- Weighted Average tag2 (Schütze-style, weighted)
   qw(wapr:g warc:g waF:g),
   qw(wapr:t warc:t waF:t),
   qw(wapr:tk warc:tk waF:tk),

   ##-- Pairwise (Schulte im Walde-style)
   qw(ppr:g prc:g pF:g),
   qw(ppr:t prc:t pF:t),
   qw(ppr:tk prc:tk pF:tk),

   ##-- Weighted Pairwise (Schulte im Walde-style)
   qw(wppr:g wprc:g wpF:g),
   qw(wppr:t wprc:t wpF:t),
   qw(wppr:tk wprc:tk wpF:tk),

   ##-- Rand Index
   qw(ARand:g ARand:t ARand:tk),
  );

##-------------------
## %_eval_base_families = ($family => \@field_bases)
##  + other generated fields are created by iterating over these
our %_eval_base_families =
  (
   meta  => [qw(pr rc F)],
   ameta => [qw(ampr amrc amF)],
   H     => [qw(Hpr Hrc HF)], ##-- HI
   HI    => [qw(Hpr Hrc HI)],
   avg   => [qw(apr arc aF)],
   wavg  => [qw(wapr warc waF)],
   pair  => [qw(ppr prc pF)],
   wpair => [qw(wppr wprc wpF)],
   #Rand  => [qw(Rand)],
   ARand => [qw(ARand)],
  );
our $_eval_default_family = 'meta';

##-------------------
## %aggregateFuncs = ($func=>\%fieldConfig)
##  + ???
our %aggregateFuncs =
  (
   min=>{ aggregateName=>'min', aggregate=>'min', },
   max=>{ aggregateName=>'max', aggregate=>'max', },
   sum=>{ aggregateName=>'sum', aggregate=>'sum', },
   prd=>{ aggregateName=>'prd', aggregate=>'prod', },

   avg=>{ aggregateName=>'avg', aggregate=>'avg', },
   var=>{ aggregateName=>'Var', aggregate=>'var', },
   dev=>{ aggregateName=>'dev', aggregate=>'stddev', },
   adev=>{ aggregateName=>'adev', aggregate=>'avgdev', },

   med=>{ aggregateName=>'med', aggregate=>'med', }, ##-- median
   mvar=>{ aggregateName=>'mVar', aggregate=>'mvar', },
   mdev=>{ aggregateName=>'mdev', aggregate=>'mdev', },
   amdev=>{ aggregateName=>'amdev', aggregate=>'amdev', },

   nnz =>{aggregateName=>'nnz',  aggregate=>'nnz', options=>{eval=>'($_||0)', n=>1, fmt=>'%d'}, },
   ndef=>{aggregateName=>'ndef', aggregate=>'nnz', options=>{eval=>'($_||0)', n=>1, fmt=>'%d'}, },
  );
##-- aggregate functions: aliases
our %_aggregateAliases =
  (
   'prod'=>'prd', 'product'=>'prod',
   'Var'=>'var',  'variance'=>'var',
   'stddev'=>'dev', 'sigma'=>'dev',
   'median'=>'med',
  );
##-- @aggregateAvgs : average-like aggregates (keys of %aggregateFuncs)
our @aggregateAvgs = qw(avg med min max); ##--ndef nnz
##-- @aggregateDevs : deviation-like aggregates (keys of %aggregateFuncs)
our @aggregateDevs = qw(dev adev mdev amdev);

##-- add some aggregate families to @_eval_base_fields, %_eval_base_families
#our @_aggr_apply_families = keys(%_eval_base_families);
#foreach my $family (@_aggr_apply_families) {
#  foreach my $aggr (qw(avg dev)) {
#    $_eval_base_families{"${aggr}:${family}"} = [map { "${aggr}(of=$_)" } @{$_eval_base_families{$family}}];
#  }
#}





##-------------------
## @_eval_rxcomps
##  + generated evaluator cross-comparison field aliases iterate over these
our %_rxcomp_base =
  (
   expand_code=>\&_expand_rxcomp,
   rfunc=>undef,
   rargs=>undef,
   #xc_prepend=> '|',
   xc_prepend=>undef,
   #xc_sortby => 'xvars->xlabel',
   xc_sortby => undef, ##-- default=xc_title
   xc_title  => '($cfg->{xvars}{lang}||"*")."/".($cfg->{xvars}{xlabel}||"*")',
   rxc_fmt   => '%.2g',
   n         => 1,
  );
our %_eval_rxcomps =
  (
   #'wilcox-test:p' => { %_rxcomp_base, rfunc => 'wilcox.test', rattr=>'p.value' },
   #'t-test:p'      => { %_rxcomp_base, rfunc => 't.test',      rattr=>'p.value' },
   ##--
   'wilcox-test' => { %_rxcomp_base, rfunc => 'wilcox.test', rattr=>'p.value',
		      rargs=>[qw('paired=FALSE')],
		    },
   't-test'      => { %_rxcomp_base, rfunc => 't.test',      rattr=>'p.value' },
  );

##-------------------
## %_eval_xcomps
##  + native evaluator cross-comparison field aliases
our %_xcomp_base =
  (
   expand_code=>\&_expand_xcomp,
   xc_prepend=>undef,
   xc_sortby => undef, ##-- default=xc_title
   xc_title  => '($cfg->{xvars}{lang}||"*")."/".($cfg->{xvars}{xlabel}||"*")',
   n         => 1,
   #on        => 'pr:g', ##-- optional target field; expanded into $xcfield->{_on} if present
   #fmt       => '%.2g', ##-- adopted from 'on' if present
  );
our %_eval_xcomps =
  (
   ##-- native x-comparisons
   'diff' => {
	      %_xcomp_base,
	      xc_eval=>'$mf->fieldValue($cfg1,$field->{_on})-$mf->fieldValue($cfg2,$field->{_on})',
	     },
   'errdiff' => {
		 %_xcomp_base,
		 errmax=>100,
		 xc_eval=>('__errdiff('
			   .' $mf->fieldValue($cfg1,$field->{_on}),'
			   .' $mf->fieldValue($cfg2,$field->{_on}),'
			   .' $field->{errmax}'
			   .')'),
		},
  );
our %_eval_xcomps_dev =
  (
   'devdiff' => {
		 %_xcomp_base,
		 expand_code=>\&_expand_devdiff,
		 dev=>'dev:pr:g',   ##-- deviation-like field
		 on =>'avg:pr:g',   ##-- target field (average-like)
		},
  );


##---------------------------------------------------------------
## Globals: Field Aliases (end = search for 'EOFIELDS')

## %FIELDS: ( $fieldName => \%fieldSpecHash, ..., $aliasName=>$fieldName, )
##  + %fieldSpecHash keys:
##     path => \@path,       ##-- nested MUDL::Make::Config key-path
##     aggregate => $func,   ##-- aggregate function (class basename, see MUDL::Make::Config::Group::pathValue)
##     n    => $bool,        ##-- true iff numeric
##     fmt  => $how,         ##-- sprintf template for tabular formatting (default='auto')
##     title => $title,      ##-- field title (for tab)
##     evaltitle=>$str,      ##-- field title, eval'd (with variable $field set to full field)
##     name  => $name,       ##-- field name (primary key for field; default: field title)
##     evalname=> $str,      ##-- field name, eval'd (with variable $field set to full field)
##     alt   => \@names,     ##-- alternative names
##     eval  => $eval,       ##-- eval() for value adjustment (vars $mf, $cfg, $field, $val are set)
##     hr    => $how,        ##-- summarize(): separator type for changed values qw(major minor micro)
##     condense=>$bool,      ##-- summarize(): condense consecutive duplicate values?
##     expand_code =>\&code, ##-- dynamic alias: calls (\@expanded=$code->($mf,$pfield,\@xfields)) to expand
##                           ##   + code should return expanded fields as ARRAY-ref
##     expand_priority=>$n,  ##-- expansion priority (lower priority --> expanded earlier; default=0)
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
   'none' => [], ##-- dummy field

   ##-- Action-specific field aliases
   'listDefault' => [ qw(auto) ],
   'sortDefault' => [
		     qw(nT stage emi ci xvars->fcorpus corpus xvars->tcclass lrlab nlbds nbds ntgs auto),
		      #qw(pr:g rc:g pr:t rc:t)
		    ],
   'collectDefault' => [ qw(corpus stage), ],
   'plotKeyDefault' => [qw(corpus lrlab auto)],

   'default'          => 'tabDefault',
   'tabid'            => 'tabId',
   'id'               => 'tabId',
   #'results'          => 'tabResults',
   'N'                => [ 'count(title=N,eval=($_||1))' ],
   'rtabDefault'      => [
			  'rtabId',
			  qw(values:meta values:pair values:wpair values:H HI:g HI:t values:ARand),
			 ],

   (map { ("tab$_"  => "summarize$_") } qw(Default Id Results)),
   (map { ("ltab$_"  => "latex$_") } qw(Default Id Results)),


   ##-- Summarize (ASCII)
   'summarizeDefault' => ['summarizeId', '|', 'summarizeResults'],
   'summarizeId' => [ 'emid' ],
   'summarizeResults'=>[ qw(results) ],

   ##-- table: Ids
   'mpId'  => ['mpid'],
   'mpid'  => [ qw(stgf ci lg lrlabel auto) ],

   'emId'  => ['emid'],
   'emid'  => [ qw(stgf emi ci lg lrlabel auto) ],

   'rtabId' => [ qw(cspliti stg nstages emi lg flabel dlabel xlabel auto N) ],

   ##-- Field-joining
   'join' => {
	      of=>'auto', ##-- field-spec
	      with=>'/',  ##-- join character
	      expand_code=>\&_expand_join,
	     },

   ##-- Enumeration
   ##   + enum(of=$FIELDSPEC) : maps each $FIELDSPEC to a natural number
   'enum' => {
	      expand_code=>\&_expand_enum,
	      of=>'auto',       ##-- field-spec
	      from=>1,          ##-- first enum value
	      eval=>undef,      ##-- $_ in {eval} gets replaced by enumeration value
	      sortby=>undef,    ##-- field-spec for sortby: default='$enum_field->{of}'
	     },

   ##-- Results: general
   (map {
     my $family         = $_;
     my $family_default = $_eval_base_families{$family}->[0];
     (
      ##--------------------------------------------------------
      ## Tables: by family
      "mptab:${family}" => [ "mpid", "|", "results:${family}" ],
      "emtab:${family}" => [ "emid", "|", "results:${family}" ],
      _expand_gt("mptab:${family}:__GT__" => [ "mpid", "|", "results:${family}:__GT__" ],
		 "emtab:${family}:__GT__" => [ "emid", "|", "results:${family}:__GT__" ],
		),

      ##--------------------------------------------------------
      ## Tables: by family: values only
      "mpvalues:${family}" => [ "mpid", "values:${family}" ],
      "emvalues:${family}" => [ "emid", "values:${family}" ],
      _expand_gt("mpvalues:${family}:__GT__" => [ "mpid", "values:${family}:__GT__" ],
		 "emvalues:${family}:__GT__" => [ "emid", "values:${family}:__GT__" ],
		),

      ##--------------------------------------------------------
      ## Results: by family
      ## + results:meta => [ qw(results:meta:g | results:meta:t) ]
      "results:${family}" => [ "results:${family}:g", '|', "results:${family}:tk" ],
      ##
      ## Results: by family: global, targets
      ## + results:meta:g => [ qw(*:pr:g pr:g +:rc:g rc:g ~:F:g F:g) ]
      _expand_gt("results:${family}:__GT__",
		 _expand_prF([map { ($_.":__GT__") } @{$_eval_base_families{$family}}])),

      ##--------------------------------------------------------
      ## Results: by family: values only
      ## + values:meta => [ qw(values:meta:g | values:meta:t) ]
      "values:${family}" => [ "values:${family}:g", "values:${family}:t", "values:${family}:tk" ],
      ##
      ## Results: by family: global, targets
      ## + values:meta:g => [ qw(*:pr:g pr:g +:rc:g rc:g ~:F:g F:g) ]
      _expand_gt("values:${family}:__GT__",
		 [map { ($_.":__GT__") } @{$_eval_base_families{$family}}]),

      ##--------------------------------------------------------
      ## Results: by family: aggregates
      map {
	my $aggr=$_;
	(
	 ##
	 ## Results: aggregates: single fields
	 ## + avg:pr:g => [ 'avg(of=pr:g)' ]
	 (map {
	   _expand_gt("${aggr}:${_}:__GT__", ["${aggr}(of=${_}:__GT__)"])
	 } @{$_eval_base_families{$family}}),
	 ##
	 ## Results: aggregates: by family: values only: global|targets
	 ## + values:avg:meta:g => [ qw(avg:pr:g avg:rc:g avg:F:g) ]
	 _expand_gt("values:${aggr}:${family}:__GT__",
		    [map { ("${aggr}:${_}:__GT__") } @{$_eval_base_families{$family}}]),
	 ##
	 ## Results: aggregates: by family: values only
	 ## + values:avg:meta => [ qw(values:avg:meta:g | values:avg:meta:t ]
	 "values:${aggr}:${family}" => [ "values:${aggr}:${family}:g", "values:${aggr}:${family}:t" ],
	 ##
	 ## Results: aggregates: by family: values+stars: global|targets
	 ## + results:avg:meta:g => [ qw(*:avg:pr:g avg:pr:g +:avg:rc:g avg:rc:g ~:avg:F:g avg:F:g) ]
	 _expand_gt("results:${aggr}:${family}:__GT__",
		    _expand_prF([map { ("${aggr}:${_}:__GT__") } @{$_eval_base_families{$family}}])),
	 ##
	 ## Results: aggregates: by family: values only
	 ## + results:avg:meta => [ qw(results:avg:meta:g | results:avg:meta:t ]
	 "results:${aggr}:${family}" => [ "results:${aggr}:${family}:g", '|', "results:${aggr}:${family}:tk" ],
	 ##
	 ##
	 (##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  ## TOO LONG LOADING: NEEDS A RETHINK!
	  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	  0 ? (## rethink:
	       ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	       ## Results: aggregates: by family: vs. max(emi): global, targets
	       ## + 'results:avg:meta:max:g' => [ 'results:avg:meta:max:pr:g ]
	       _expand_gt("results:${aggr}:${family}:max:__GT__", ["results:${aggr}:${family}:max:${family_default}:__GT__"]),
	       ##
	       ## Results: aggregates: by family: vs. max(aggr|emi): global, targets: by evaluator field
	       ## + 'results:avg:meta:max:pr:g' => [ qw(*:avg_pr:g avg:pr:g e-max:avg:pr:g:emi(title=e-max)) ]
	       (map {
		 my $field = $_;
		 _expand_gt("results:${aggr}:${family}:max:${field}:__GT__",
			    [ @{_expand_prF([ "${aggr}:${field}:__GT__" ])}, "e-max:${aggr}:${field}:__GT__:emi(title=e-max)" ])
	       } @{$_eval_base_families{$family}}),
	       ##
	       ##
	       ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	       ## Results: by family: vs. max(stage)
	       ## + 'results:avg:meta:Max' => [ qw(results:avg:meta:Max:g | results:avg:meta:Max:t) ]
	       "results:${aggr}:${family}:Max" => [ "results:${aggr}:${family}:Max:g", '|', "results:${aggr}:${family}:Max:t" ],
	       ##
	       ## Results: by family: vs. max(stage): global, targets
	       ## + 'results:avg:meta:Max:g' => [ 'results:avg:meta:Max:pr:g ]
	       _expand_gt("results:${family}:Max:__GT__", ["results:${family}:Max:${family_default}:__GT__"]),
	       ##
	       ## Results: by family: vs. max(stage): global, targets: by evaluator field
	       ## + 'results:avg:meta:Max:pr:g' => [ qw(*:avg:pr:g avg:pr:g e-max:avg:pr:g:stage(title=e-Max)) ]
	       (map {
		 my $field = $_;
		 _expand_gt("results:${aggr}:${family}:Max:${field}:__GT__",
			    [ @{_expand_prF([ "${aggr}:${field}:__GT__" ])}, "e-max:${aggr}:${field}:__GT__:stage(title=e-Max)" ])
	       } @{$_eval_base_families{$family}}),
	       ##
	       ##
	       ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	       ## Results: by family: vs. max(corpus)
	       ## + 'results:avg:meta:MAX' => [ qw(results:avg:meta:MAX:g | results:avg:meta:MAX:t) ]
	       "results:${aggr}:${family}:MAX" => [ "results:${aggr}:${family}:MAX:g", '|', "results:${aggr}:${family}:MAX:t" ],
	       ##
	       ## Results: by family: vs. max(corpus): global, targets
	       ## + 'results:avg:meta:MAX:g' => [ 'results:avg:meta:MAX:pr:g ]
	       _expand_gt("results:${aggr}:${family}:MAX:__GT__", ["results:${aggr}:${family}:MAX:${family_default}:__GT__"]),
	       ##
	       ## Results: by family: vs. max(corpus): global, targets: by evaluator field
	       ## + 'results:avg:meta:MAX:pr:g' => [ qw(*:avg:pr:g avg:pr:g e-max:avg:pr:g:corpus(title=e-MAX)) ]
	       (map {
		 my $field = $_;
		 _expand_gt("results:${aggr}:${family}:MAX:${field}:__GT__",
			    [ @{_expand_prF([ "${aggr}:${field}:__GT__" ])}, "e-max:${aggr}:${field}:__GT__:corpus(title=e-MAX)" ])
	       } @{$_eval_base_families{$family}}),
	       ##
	       ##
	       ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	       ## Results: by family: vs. max(*)
	       ## + 'results:avg:meta:e-' => [ qw(results:avg:meta:e-:g | results:avg:meta:e-:t) ]
	       "results:${aggr}:${family}:e-" => [ "results:${aggr}:${family}:e-:g", '|', "results:${aggr}:${family}:e-:t" ],
	       ##
	       ## Results: by family: vs. max(*): global, targets
	       ## + 'results:avg:meta:e-:g' => [ 'results:avg:meta:e-:pr:g ]
	       _expand_gt("results:${aggr}:${family}:e-:__GT__", ["results:${aggr}:${family}:e-:${family_default}:__GT__"]),
	       ##
	       ## Results: by family: vs. max(*): global, targets: by evaluator field
	       ## + 'results:avg:meta:e-:pr:g' => [ qw(*:pr:g pr:g),
	       ##                               qw(e-max:pr:g:emi(title=e-max)),
	       ##                               qw(e-Max:pr:g:stage(title=e-Max)),
	       ##                               qw(e-MAX:pr:g:corpus(title=e-MAX)),
	       ##                             ]
	       (map {
		 my $field = $_;
		 _expand_gt("results:${aggr}:${family}:e-:${field}:__GT__",
			    [
			     @{_expand_prF([ "${aggr}:${field}:__GT__" ])},
			     "e-max:${aggr}:${field}:__GT__:emi(title=e-max)",
			     "e-max:${aggr}:${field}:__GT__:stage(title=e-Max)",
			     "e-max:${aggr}:${field}:__GT__:corpus(title=e-MAX)"
			    ])
	       } @{$_eval_base_families{$family}}),
	      )  ##--/rethink: aggregates
	  : qw() ##--/retthink: else
	 )##-- /rethink: ?:
	)##-- map return list
      } keys(%aggregateFuncs),


      ##--------------------------------------------------------
      ## Results: by family: vs. prev(stage)
      ## + 'results:meta:e+pstg' => [ qw(results:meta:e+pstg:g | results:meta:e+pstg:t) ]
      "results:${family}:e+pstg" => [ "results:${family}:e+pstg:g", '|', "results:${family}:e+pstg:t" ],
      "values:${family}:e+pstg"  => [ "values:${family}:e+pstg:g", '|', "values:${family}:e+pstg:t" ],
      ##
      ## Results: MetaProfile: by family: vs. prev(stage): global, targets
      ## + 'results:meta:e+pstg:g' => [ 'results:meta:e+pstg:pr:g ]
      _expand_gt("results:${family}:e+pstg:__GT__", ["results:${family}:e+pstg:${family_default}:__GT__"],
		 "values:${family}:e+pstg:__GT__",  ["values:${family}:e+pstg:${family_default}:__GT__"],
		),
      ##
      ## Results: MetaProfile: by family: vs. prev(stage): global, targets: by evaluator field
      ## + 'results:meta:e+pstg:pr:g' => [ qw(*:pr:g pr:g e+pstage:pr:g(title=e+pstg)) ]
      (map {
	 my $field = $_;
	 _expand_gt("results:${family}:e+pstg:${field}:__GT__",
		    [ @{_expand_prF([ "${_}:__GT__" ])}, "e+pstage:${field}:__GT__(title=e+pstg)" ])
       } @{$_eval_base_families{$family}}),

      ##--------------------------------------------------------
      ## Results: by family: vs. prev(emi)
      ## + 'results:meta:e+pemi' => [ qw(results:meta:e+pemi:g | results:meta:e+pemi:t) ]
      "results:${family}:e+pemi" => [ "results:${family}:e+pemi:g", '|', "results:${family}:e+pemi:t" ],
      ##
      ## Results: MetaProfile: by family: vs. prev(emi): global, targets
      ## + 'results:meta:e+pemi:g' => [ 'results:meta:e+pemi:pr:g ]
      _expand_gt("results:${family}:e+pemi:__GT__", ["results:${family}:e+pemi:${family_default}:__GT__"]),
      ##
      ## Results: MetaProfile: by family: vs. prev(emi): global, targets: by evaluator field
      ## + 'results:meta:e+pemi:pr:g' => [ qw(*:pr:g pr:g e+pemi:pr:g(title=e+pemi)) ]
      (map {
	 my $field = $_;
	 _expand_gt("results:${family}:e+pemi:${field}:__GT__",
		    [ @{_expand_prF([ "${field}:__GT__" ])}, "e+pemi:${field}:__GT__(title=e+pemi)" ])
       } @{$_eval_base_families{$family}}),

      ##--------------------------------------------------------
      ## Results: by family: vs. prev(*)
      ## + 'results:meta:e+' => [ qw(results:meta:e+:g | results:meta:e+:t) ]
      "results:${family}:e+" => [ "results:${family}:e+:g", '|', "results:${family}:e+:t" ],
      ##
      ## Results: by family: vs. prev(*): global, targets
      ## + 'results:meta:e+:g' => [ 'results:meta:e+:pr:g ]
      _expand_gt("results:${family}:e+:__GT__", ["results:${family}:e+:${family_default}:__GT__"]),
      ##
      ## Results: by family: vs. prev(*): global, targets: by evaluator field
      ## + 'results:meta:e+:pr:g' => [ qw(*:pr:g pr:g),
      ##                               qw(e+pemi:pr:g(title=e+pemi)),
      ##                               qw(e+pstage:pr:g:stage(title=e+pstg)),
      ##                             ]
      (map {
	my $field = $_;
	_expand_gt("results:${family}:e+:${field}:__GT__",
		   [
		    @{_expand_prF([ "${field}:__GT__" ])},
		    "e+pemi:${field}:__GT__(title=e+pemi)",
		    "e+pstage:${field}:__GT__(title=e+pstg)",
		   ])
      } @{$_eval_base_families{$family}}),

      ##--------------------------------------------------------
      ## Results: by family: vs. cmax(emi)
      ## + 'results:meta:max' => [ qw(results:meta:max:g | results:meta:max:t) ]
      "results:${family}:max" => [ "results:${family}:max:g", '|', "results:${family}:max:t" ],
      ##
      ## Results: by family: vs. max(emi): global, targets
      ## + 'results:meta:max:g' => [ 'results:meta:max:pr:g ]
      _expand_gt("results:${family}:max:__GT__", ["results:${family}:max:${family_default}:__GT__"]),
      ##
      ## Results: by family: vs. max(emi): global, targets: by evaluator field
      ## + 'results:meta:max:pr:g' => [ qw(*:pr:g pr:g e-max:pr:g:emi(title=e-max)) ]
      (map {
	 my $field = $_;
	 _expand_gt("results:${family}:max:${field}:__GT__",
		    [ @{_expand_prF([ "${field}:__GT__" ])}, "e-max:${field}:__GT__:emi(title=e-max)" ])
       } @{$_eval_base_families{$family}}),

      ##--------------------------------------------------------
      ## Results: by family: vs. max(stage)
      ## + 'results:meta:Max' => [ qw(results:meta:Max:g | results:meta:Max:t) ]
      "results:${family}:Max" => [ "results:${family}:Max:g", '|', "results:${family}:Max:t" ],
      ##
      ## Results: by family: vs. max(stage): global, targets
      ## + 'results:meta:Max:g' => [ 'results:meta:Max:pr:g ]
      _expand_gt("results:${family}:Max:__GT__", ["results:${family}:Max:${family_default}:__GT__"]),
      ##
      ## Results: by family: vs. max(stage): global, targets: by evaluator field
      ## + 'results:meta:Max:pr:g' => [ qw(*:pr:g pr:g e-max:pr:g:stage(title=e-Max)) ]
      (map {
	 my $field = $_;
	 _expand_gt("results:${family}:Max:${field}:__GT__",
		    [ @{_expand_prF([ "${field}:__GT__" ])}, "e-max:${field}:__GT__:stage(title=e-Max)" ])
       } @{$_eval_base_families{$family}}),

      ##--------------------------------------------------------
      ## Results: by family: vs. max(corpus)
      ## + 'results:meta:MAX' => [ qw(results:meta:MAX:g | results:meta:MAX:t) ]
      "results:${family}:MAX" => [ "results:${family}:MAX:g", '|', "results:${family}:MAX:t" ],
      ##
      ## Results: by family: vs. max(corpus): global, targets
      ## + 'results:meta:MAX:g' => [ 'results:meta:MAX:pr:g ]
      _expand_gt("results:${family}:MAX:__GT__", ["results:${family}:MAX:${family_default}:__GT__"]),
      ##
      ## Results: by family: vs. max(corpus): global, targets: by evaluator field
      ## + 'results:meta:MAX:pr:g' => [ qw(*:pr:g pr:g e-max:pr:g:corpus(title=e-MAX)) ]
      (map {
	 my $field = $_;
	 _expand_gt("results:${family}:MAX:${field}:__GT__",
		    [ @{_expand_prF([ "${field}:__GT__" ])}, "e-max:${field}:__GT__:corpus(title=e-MAX)" ])
       } @{$_eval_base_families{$family}}),

      ##--------------------------------------------------------
      ## Results: by family: vs. max(*)
      ## + 'results:meta:e-' => [ qw(results:meta:e-:g | results:meta:e-:t) ]
      "results:${family}:e-" => [ "results:${family}:e-:g", '|', "results:${family}:e-:t" ],
      ##
      ## Results: by family: vs. max(*): global, targets
      ## + 'results:meta:e-:g' => [ 'results:meta:e-:pr:g ]
      _expand_gt("results:${family}:e-:__GT__", ["results:${family}:e-:${family_default}:__GT__"]),
      ##
      ## Results: by family: vs. max(*): global, targets: by evaluator field
      ## + 'results:meta:e-:pr:g' => [ qw(*:pr:g pr:g),
      ##                               qw(e-max:pr:g:emi(title=e-max)),
      ##                               qw(e-Max:pr:g:stage(title=e-Max)),
      ##                               qw(e-MAX:pr:g:corpus(title=e-MAX)),
      ##                             ]
      (map {
	my $field = $_;
	_expand_gt("results:${family}:e-:${field}:__GT__",
		   [
		    @{_expand_prF([ "${field}:__GT__" ])},
		    "e-max:${field}:__GT__:emi(title=e-max)",
		    "e-max:${field}:__GT__:stage(title=e-Max)",
		    "e-max:${field}:__GT__:corpus(title=e-MAX)"
		   ])
      } @{$_eval_base_families{$family}}),

     ) ##-- return list for outer evalutator map

   } keys(%_eval_base_families)), ##-- outer evaluator map

   ##--------------------------------------------------------
   ## Cross-comparison / R
   ## + 'rx:wilcox-test:mpr:g', ...
   (map {
     my $field = $_;
     (map {
       my $rxc_name = $_;
       my $rxc_src  = $_eval_rxcomps{$rxc_name};
       ##--
       ("rx:${rxc_name}:${field}" => {%$rxc_src, on=>$field})
     } keys(%_eval_rxcomps)),
   } @_eval_base_fields),

   ##--------------------------------------------------------
   ## Cross-comparison / native
   ## + 'xc:diff:mpr:g', ...
   (map {
     my $field = $_;
     (
      ##-- Literal cross-comparisons
      (map {
	my $xc_name = $_;
	my $xc_src  = $_eval_xcomps{$xc_name};
	(
	 ##-- Literal value comparison: "xc:diff:pr:g"
	 ("xc:${xc_name}:${field}" => {%$xc_src, on=>$field}),
	 ##
	 ##-- Average-comparison: "xc:diff:avg:pr:g"
	 (map {
	   my $avg=$_;
	   ##-- Literal average-comparison: "xc:diff:avg:pr:g"
	   ("xc:${xc_name}:${avg}:${field}" => {%$xc_src, on=>"${avg}:${field}"}),
	 } @aggregateAvgs),
	),
      } keys(%_eval_xcomps)),

      ##-- (average+deviation)-based cross-comparisons
      (map {
	my $xc_name = $_;
	my $xc_src  = $_eval_xcomps_dev{$xc_name};
	(map {
	  my $avg=$_;
	  (map {
	    my $dev=$_;
	    ##--average+deviation-comparison: "xc:devdiff:avg:dev:pr:g"
	    ("xc:${xc_name}:${avg}+${dev}:${field}" => {%$xc_src, on=>"${avg}:${field}", dev=>"${dev}:${field}"}),
	  } @aggregateDevs),
	} @aggregateAvgs),
      } keys(%_eval_xcomps_dev)),
     )
   } @_eval_base_fields),


   ##-- tables: default
   _expand_gt(
	      "results" => [ "results:${_eval_default_family}" ],
	      "mptab"   => [ "mptab:${_eval_default_family}" ],
	      "emtab"   => [ "emtab:${_eval_default_family}" ],

	      "mptab:__GT__" => [ "mptab:${_eval_default_family}:__GT__" ],
	      "emtab:__GT__" => [ "emtab:${_eval_default_family}:__GT__" ],
	     ),

   ##-- Summarize (LaTeX)
   'latexDefault' => ['|', 'latexId', '||', 'latexResults', '|'],
   'latexId' => [
		 'stage.emi',
		 'corpus',
		 'lrlabel',
		 'auto',
		],
   'latexResults'=>[
		    qw(*l:pr:g ar:g),
		    '|',
		    qw(*l:pr:t ar:t),
		   ],

   ##-- cross-comparisons (e.g. significance tests)
   xcomp => {
	     expand_code=>\&_expand_xcomp,
	     xc_title=>'$cfg',
	     #xc_eval=>'?',
	     #xc_sortby=>'auto', ##-- sort-by specification for xc
	     xc_sortby=>undef, 
	     ##
	     ##-- prepend or append a column to every generated column
	     #xc_prepend => ['|'],
	     #xc_append => ['|'],
	    },
   rxcomp => {
	      ##-- for R tests
	      expand_code=>\&_expand_rxcomp,
	      rfunc => 'wilcox.test',
	      rargs => undef,
	      on    => 'pr:g',
	     },


   ##-- Filler(s)
   ':'    => { eval=>'":"',  title=>':', },
   '::'   => { eval=>'"::"', title=>'::', },
   '='    => { eval=>'"="',  title=>'=', },
   '/'    => { eval=>'"/"',  title=>'/', },
   '&'    => { eval=>'"&"',  title=>'&',  latexSep=>1, latexAlignHead=>'' },
   '|'    => { eval=>'"|"',  title=>'|',  latexSep=>1, latexAlignHead=>'|' },
   '||'   => { eval=>'"||"', title=>'||', latexSep=>1, latexAlignHead=>'||' },

   ##-- MetaProfile: label
   lrlabel => { path=>[qw(xvars lrlabel)], n=>0, fmt=>'auto', title=>'lrlabel',
		alt=>[qw(xvars->lrwhich xvars->tcd xvars->tcm xvars->tccd xvars->tccm),
		      qw(lrwhich tcd tcm tccd tccm),
		     ],
		hr=>undef,
		condense=>0,
	      },
   lrlab   => 'lrlabel',

   ##-- MetaProfile: lrwhich
   lrwhich => { path=>[qw(xvars lrwhich)], title=>'lrw', alt => [qw(lrwhich xvars->lrwhich)], },
   tcd    => { path=>[qw(xvars tcd)], title=>'tcd', },
   tcm    => { path=>[qw(xvars tcm)], title=>'tcm', },
   tccd   => { path=>[qw(xvars tccd)], title=>'tccd', },
   tccm   => { path=>[qw(xvars tccm)], title=>'tccm', },

   ##-- MetaProfile: numeric keys
   tck          => { path=>[qw(xvars tck)], title=>'tck', n=>1 },
   'xvars->tck' => { path=>[qw(xvars tck)], title=>'tck', n=>1 },

   ##-- MetaProfile xvars aliases
   'xlabel'     => { path=>[qw(xvars xlabel)], title=>'method',
		     alt=>[qw(xvars->tcclass xvars->tcm xvars->tccm),
			   qw(tcclass tcm tccm),
			   qw(lrlabel xvars->lrlabel), ##-- hack
			  ],
		   },
   'method'=>'xlabel',
   'dlabel'     => { path=>[qw(xvars dlabel)], title=>'dist',
		     alt=>[qw(xvars->tcd xvars->tccd),
			   qw(tcd tccd),
			   qw(lrlabel xvars->lrlabel), ##-- hack
			  ],
		   },
   'dist'=>'dlabel',
   'flabel'     => { path=>[qw(xvars flabel)], title=>'feat',
		     alt=>[qw(lrwhich xvars->lrwhich),
			   qw(lrlabel xvars->lrlabel), ##-- hack
			  ],
		   },
   'feat'=>'flabel',

   ##-- Corpus
   corpus => {
	      #path=>[qw(xvars icbase)],
	      path=>[qw(xvars fcorpus)],
	      n=>0, fmt=>'auto', title=>'corpus',
	       alt=>[
		     qw(xvars->icorpus xvars->icbase xvars->tcorpus),
		     qw(icorpus icbase tcorpus),
		     qw(fcorpus fbase),
		     qw(xvars->fcorpus xvars->fbase xvars->lang xvars->lg),
		    ],
	       eval => '$_ =~ s/\.(?:(?:.?tiny)|(?:train)|(?:i-\d+))//g ? $_ : $_',
	       hr=>'micro',
	       condense=>1,
	     },
   lg=>'lang',
   lang   => {
	      #path=>[qw(xvars icbase)],
	      path=>[qw(xvars fcorpus)],
	      n=>0, fmt=>'auto', title=>'lg',
	      eval=>'$_ =~ /^(\w\w)\-/ ? $1 : ($_ =~ /^[uz]/ ? "de" : "en")',
	      alt=>[
		    qw(xvars->icorpus xvars->icbase xvars->tcorpus),
		    qw(icorpus icbase tcorpus),
		    qw(fcorpus fbase),
		    qw(xvars->fcorpus xvars->fbase xvars->lang xvars->lg),
		   ],
	      hr=>'micro',
	      condense=>1,
	     },

   ci=>'cspliti',
   cspliti => {
	       path=>[qw(xvars cspliti)], n=>1, fmt=>'auto', title=>'ci',
	       hr=>'micro',
	       eval=>'$_ ? $_ : 0',
	       alt=>[qw(xvars->cspliti cspliti)],
	       condense=>1,
	      },

   ##-- MetaProfile: numeric indices
   'stg' => 'stage',
   'stage' => { path=>[qw(xvars stage)], n=>1, fmt=>'%3d', title=>'stg',
		alt=>[qw(stage xvars->stage stagef nstages)],
		hr=>'major',
		condense=>1,
		part=>'stage',
	      },

   'stagef' => {
		path=>[qw(xvars)], fmt=>'%s', title=>'stgf',
		alt=>[qw(stage stg xvars->stage nstages xvars->nstages)],
		hr=>'major',
		condense=>1,
		part=>'stage',
		eval=>'sprintf("%2d/%d", $_->{stage}, $_->{nstages})'
	       },
   'stgf' => 'stagef',

   'emi'   => { path=>[qw(xvars emi)],   n=>1, fmt=>'%3d', title=>'emi',
		alt=>[qw(emi xvars->emi)],
		hr=>'minor',
		condense=>1,
	      },
   'stg.emi' => 'stage.emi',
   'stage.emi' => {
		   path=>[qw(xvars)],
		   n=>0,
		   #fmt=>'%5s',
		   fmt=>'%5s ',
		   eval=>'sprintf("%2d.%02d", $_->{stage}, $_->{emi})',
		   title=>'stg.emi',
		   alt=>[ qw(xvars->stage xvars->emi stg emi stage emi), ],
		   hr=>'minor',
		   condense=>1,
		  },

   ##-------------------------------------------------
   ## Constant fields
   'constant' => 'const',
   'const'    => {
		  value=>0,
		  eval=>'"$field->{value}"',
		  evaltitle=>'"const($field->{value})"',
		 },
   '-5' => [ 'const(value=-5,title=-5)' ],
   '+5' => [ 'const(value=+5,title=+5)' ],


   ##-------------------------------------------------
   ## MetaProfile Summary data
   'nT'   => { path=>[qw(mpsummary nTargets)],   n=>1,  title=>'nT',   condense=>1, alt=>[qw(stage)],
	       latexTitle=>'$\mathbf{|T_{\le k}|}$',
	     },
   'ntgs' => 'nT',

   'nT_k' => { path=>[qw(mpsummary nTargets_k)], n=>1,  title=>'nT_k', condense=>1, alt=>[qw(stage)],
	       latexTitle=>'$\mathbf{|T_k|}$',
	     },
   'ntgsk'  => 'nT_k',
   'ntgs_k' => 'nT_k',
   'ntgsk'  => 'nT_k',

   'nB'   => { path=>[qw(mpsummary nBounds)],    n=>1,  title=>'nB',   condense=>1, alt=>[qw(stage)],
	       latexTitle=>'$\mathbf{|B_k|}$',
	     },
   'nbds' => 'nB',

   'nC'   => { path=>[qw(mpsummary nClusters)],  n=>1,  title=>'nC',   condense=>1,
	       latexTitle=>'$\mathbf{|C|}$',
	     },
   'nclusters' => 'nC',

   'fT_k' => { path=>[qw(mpsummary ugk_avg)],    n=>1,  title=>'avg_f(T_k)', condense=>1, alt=>[qw(stage nT)],
	       eval=>'sprintf("%.2f",$_)',
	       latexTitle=>'$\mathbf{E(f(T_k))}$',
	     },
   'tgf'  => 'fT_k',

   'mbest'=> { path=>[qw(mpsummary d2p_n)],     n=>1,  title=>'m',    condense=>0,
	       latexTitle=>'$\mathbf{m}$',
	     },
   'd2pn' => 'mbest',

   'tck' => { path=>[qw(xvars tck)], n=>1, alt=>[qw(xvars->tck)], },

   ##----------------------------------------------------------
   ## Eval: low-level keys
   (map {
     my $gt = $_;
     my %gt2path = ('g'=>'eval_global','t'=>'eval_targets','tk'=>'eval_targets_k');
     my $p  = $gt2path{$gt};

     (
      ##-------------------------------------------------
      ## Eval: Ambiguity Rates: ar:*
      "ar:$gt" => { path=>[$p, qw(arate1)],  n=>1, fmt=>'%.3f', eval=>'0+$_',  title=>" ar:$gt" },

      ##-------------------------------------
      ## Eval: Meta-*: m*:*
      "mpr:$gt" => { path=>[$p, qw(precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"mpr:$gt"},
      "mrc:$gt" => { path=>[$p, qw(recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"mrc:$gt"},
      "F:$gt" => { path=>[$p, qw(F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"mF:$gt"},

      ##-------------------------------------
      ## Eval: Meta-*: *:*
      "pr:$gt" => { path=>[$p, qw(precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"mpr:$gt"},
      "rc:$gt" => { path=>[$p, qw(recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"mrc:$gt"},
      "F:$gt" => { path=>[$p, qw(F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"mF:$gt"},

      ##-------------------------------------
      ## Eval: Adjusted Meta-*: am*:*
      "ampr:$gt" => { path=>[$p,qw(ameta_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"ampr:$gt"},
      "amrc:$gt" => { path=>[$p,qw(ameta_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"amrc:$gt"},
      "amF:$gt"  => { path=>[$p,qw(ameta_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"amF:$gt" },

      ##-------------------------------------
      ## Eval: Total-*: t*:*
      "tpr:$gt"  => { path=>[$p,qw(total_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"tpr:$gt"},
      "trc:$gt"  => { path=>[$p,qw(total_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"trc:$gt"},
      "tF:$gt"   => { path=>[$p,qw(total_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"tF:$gt" },

      ##-------------------------------------
      ## Eval: MI, H: mi*:*, H*:*
      "mi:$gt"   => { path=>[$p,qw(mi)],        n=>1, fmt=>'%.3f', eval=>'0+$_', title=>" mi:$gt" },
      "Hpr:$gt"  => { path=>[$p,qw(H_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"Hpr:$gt"},
      "Hrc:$gt"  => { path=>[$p,qw(H_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"Hrc:$gt"},
      "HF:$gt"   => { path=>[$p,qw(H_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"HF:$gt" },
      "HI:$gt"   => { path=>[$p,qw(H_I)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"HI:$gt" },

      ##-------------------------------------
      ## Eval: Tagwise-average-*: Global
      "apr:$gt"  => { path=>[$p,qw(avg_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"apr:$gt"},
      "arc:$gt"  => { path=>[$p,qw(avg_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"arc:$gt"},
      "aF:$gt"   => { path=>[$p,qw(avg_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"aF:$gt" },

      ##-------------------------------------------------
      ## Eval: Weighted Tagwise-average (precision,recall,F)
      "wapr:$gt"  => { path=>[$p,qw(wavg_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"wapr:$gt"},
      "warc:$gt"  => { path=>[$p,qw(wavg_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"warc:$gt"},
      "waF:$gt"   => { path=>[$p,qw(wavg_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"waF:$gt"},

      ##-------------------------------------------------
      ## Eval: Pairwise (precision,recall,F)
      "ppr:$gt"  => { path=>[$p,qw(pair_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"ppr:$gt"},
      "prc:$gt"  => { path=>[$p,qw(pair_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"prc:$gt"},
      "pF:$gt"   => { path=>[$p,qw(pair_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>" pF:$gt"},

      ##-------------------------------------------------
      ## Eval: Weighted Pairwise (precision,recall,F)
      "wppr:$gt" => { path=>[$p,qw(wpair_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"wppr:$gt"},
      "wprc:$gt" => { path=>[$p,qw(wpair_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"wprc:$gt"},
      "wpF:$gt"  => { path=>[$p,qw(wpair_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"wpF:$gt"},

      ##-------------------------------------
      ## Eval: Rand Index
      #"Rand:$gt" => { path=>[$p,qw(Rand)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"Rand:$gt"},
      #"rand:$gt" => ["Rand:$gt"],

      ##-------------------------------------
      ## Eval: Adjusted Rand Index
      "ARand:$gt" => { path=>[$p,qw(RandA)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>"ARand:$gt"},
      #"RandA:$gt" => ["ARand:$gt"],

      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Eval: Single-tag: typewise-density (# word types / total_ntypes)
      "tag:dens:$gt" => { path=>[$p,qw(tag2i)], n=>1, fmt=>'%.2f',
			  evalname=>qq("\$field->{tag}:dens:$gt"),
			  evaltitle=>q("$field->{tag}"),
			  eval=>q(100*($_->{$field->{tag}}{wtype_density}||0)),
			},
      "itag:dens:$gt" => { path=>[$p,qw(tag1i)], n=>1, fmt=>'%.2f',
			   evalname=>qq("\$field->{tag}:dens:$gt"),
			   evaltitle=>q("$field->{tag}"),
			   eval=>q(100*($_->{$field->{tag}}{wtype_density}||0)),
			 },
      "tags:dens:$gt"  => { expand_code=>\&_expand_tags,  _tag_field=>"tag:dens:$gt",  _tag_var=>'tag', },
      "itags:dens:$gt" => { expand_code=>\&_expand_tags,  _tag_field=>"itag:dens:$gt", _tag_var=>'tag', },

      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Eval: Single-tag (precision,recall,F): Meta
      "tag:pr:$gt" => { path=>[$p,qw(tag2i)], n=>1, fmt=>'%.2f',
			evalname=>qq("\$field->{tag}:pr:$gt"),
			evaltitle=>q("$field->{tag}"),
			eval=>q(100*($_->{$field->{tag}}{meta_precision}||0)),
		      },
      "tag:rc:$gt" => { path=>[$p,qw(tag2i)], n=>1, fmt=>'%.2f',
			evalname=>qq("\$field->{tag}:rc:$gt"),
			evaltitle=>q("$field->{tag}"),
			eval=>q(100*($_->{$field->{tag}}{meta_recall}||0)),
		      },
      "tag:F:$gt"  => { path=>[$p,qw(tag2i)], n=>1, fmt=>'%.2f',
			evalname=>qq("\$field->{tag}:F:$gt"),
			evaltitle=>q("$field->{tag}"),
			eval=>q(100*($_->{$field->{tag}}{meta_F}||0)),
		      },

      ##-------------------------------------------------
      ## Eval: Single-tag (precision,recall,F): Average
      "tag:apr:$gt" => { path=>[$p,qw(tag2i)], n=>1, fmt=>'%.2f',
			 evalname=>qq("\$field->{tag}:apr:$gt"),
			 evaltitle=>q("$field->{tag}"),
			 eval=>q(100*$_->{$field->{tag}}{avg_precision}),
		       },
      "tag:arc:$gt" => { path=>[$p,qw(tag2i)], n=>1, fmt=>'%.2f',
			 evalname=>qq("\$field->{tag}:arc:$gt"),
			 evaltitle=>q("$field->{tag}"),
			 eval=>q(100*$_->{$field->{tag}}{avg_recall}),
		       },
      "tag:aF:$gt"  => { path=>[$p,qw(tag2i)], n=>1, fmt=>'%.2f',
			 evalname=>qq("\$field->{tag}:aF:$gt"),
			 evaltitle=>q("$field->{tag}"),
			 eval=>q(100*$_->{$field->{tag}}{avg_F}),
		       },

      ##-------------------------------------
      ## Eval: All single-tag-*: Meta
      "tags:pr:$gt" => { expand_code=>\&_expand_tags, _tag_field=>"tag:pr:$gt", _tag_var=>'tag', },
      "tags:rc:$gt" => { expand_code=>\&_expand_tags, _tag_field=>"tag:rc:$gt", _tag_var=>'tag', },
      "tags:F:$gt"  => { expand_code=>\&_expand_tags, _tag_field=>"tag:F:$gt", _tag_var=>'tag', },

      ##-------------------------------------
      ## Eval: All single-tag-*: Average
      "tags:apr:$gt" => { expand_code=>\&_expand_tags, _tag_field=>"tag:apr:$gt", _tag_var=>'tag', },
      "tags:arc:$gt" => { expand_code=>\&_expand_tags, _tag_field=>"tag:arc:$gt", _tag_var=>'tag', },
      "tags:aF:$gt"  => { expand_code=>\&_expand_tags, _tag_field=>"tag:aF:$gt", _tag_var=>'tag', },
     ),
   } ('g', 't', 'tk')),

   ##-------------------------------------------------
   ## Eval: aggregate functions
   ##  + avg { of=>$of_field, ... }
   ##  + dev { of=>$of_field, ... }
   ##  + ... etc.
   (map {
     ($_=>{
	   %{$aggregateFuncs{$_}},
	   expand_code=>\&_expand_aggregate,
	   of=>undef, ##-- field name
	  })
   } keys(%aggregateFuncs)),

   ##-------------------------------------------------
   ## Eval: log
   ##  + log { of=>$of_field ... }
   'log' => (our $_field_log=
	     {
	      evaltitle=>'"log($field->{of})"',
	      eval=>'log($mf->fieldValue($cfg,$mf->expand($field->{of})->[0]))',
	      n=>1,
	      fmt=>'%.2f',
	     }),

   ##-------------------------------------------------
   ## Eval: max-value
   ##  + max { of=>$of_field, for=>$for_fields, ... }
   'cmax' => (our $_max_field =
	      { expand_code =>\&_expand_max,
		of          =>'pr:g',               ##-- target field: to be set by user or alias
		for         =>'corpus,stage,emi',   ##-- field list: max for every value-tuple of these fields
		evalname    =>'"max(of=$field->{of},for=($field->{for}))"',
		hidden      =>0,
	      }),

   ##-------------------------------------------------
   ## Eval: max-value: aliases
   ##  + max:${of_field}:${innermost_for_field}
   (map { _max_fields($_) } @_eval_base_fields),
   ##
   ## max-value: aggregates
   ##  + max:${avg}:${of_field}:${innermost_for_field}
   (map {
     my $ebase = $_;
     (map { _max_fields("${_}:$ebase") } @aggregateAvgs)
   } @_eval_base_fields),

   ##-------------------------------------------------
   ## Eval: ifequal (numeric or string: uses 'n' field-flag)
   ##  ifeq { a=>$fieldA, b=>$fieldB, then=>$then_string, else=>$else_string, ... }
   'ifequal' => {
		 expand_code =>\&_expand_ifequal,
		 a           =>'pr:g',
		 b           =>'max:pr:g',
		 evalname    =>
		 '"ifequal($field->{a},$field->{b}) then=($field->{then}) else=($field->{else}))"',
		 then        =>'*',           ##-- value if ${ $field->{a} } == ${ $field->{b} }
		 else        =>'',            ##-- value if ${ $field->{b} } != ${ $field->{b} }
		 title       =>'',
		 hidden      =>0,
		 padright    =>'',
		},
   'ifeq' => 'ifequal',


   ##-------------------------------------------------
   ## Eval: conditional: max-value
   ##  + ifmax { of=>$of_field, for=>$for_fields, then=>$then_string, else=>$else_string, ... }
   ##  + uses max(), ifequal()
   'ifmax'  => (our $_ifmax_field =
		{
		 expand_code=>\&_expand_ifmax,
		 evalname    =>
		 '"ifmax(of=$field->{a},for=($field->{for}),then=($field->{then}),else=($field->{else}))"',
		 title       => '',
		 then        => '*',
		 else        => '',
		 padright    => '',
		}),

   ##-- max: search markers: useful aliases
   '*:corpus'      => (our $_ifmax_corpus = { %$_ifmax_field, for=>'corpus' }),
   '*:stage'       => (our $_ifmax_stage  = { %$_ifmax_field, for=>'corpus,stage' }),
   '*:stg'         => '*:stage',
   '*:emi'         => (our $_ifmax_emi    = { %$_ifmax_field, for=>'corpus,stage,emi' }),

   ##-------------------------------------
   ## Max: Eval: Mark: "${marker}:${of_field}:${innermost_for_field}"
   ##'*:pr:g:corpus' => { %$_ifmax_corpus, of=>'pr:g' },
   ##'*:pr:g:stage'  => { %$_ifmax_stage,  of=>'pr:g' },
   ##'*:pr:g:stg'    => '*:pr:g:stage',
   ##'*:pr:g:emi'    => { %$_ifmax_emi,    of=>'pr:g' },
   ##'**:pr:g'       => [ '*:pr:g:corpus', '*:pr:g:stage' ]
   ##'***:pr:g'      => [ '*:pr:g:corpus', '*:pr:g:stage', '*:pr:g:emi' ]
   ##'*:pr:g'        => '***:pr:g',
   (map { _markmax_fields($_) } @_eval_base_fields),
   (map {
     my $aggr=$_;
     map { _markmax_fields("${aggr}:$_") } @_eval_base_fields
   } @aggregateAvgs), ##-- keys(%aggregateFuncs)

   ##-- mark max: latex
   (map { _markmax_fields_latex($_) } @_eval_base_fields),
   (map {
     my $aggr=$_;
     map { _markmax_fields("${aggr}:$_") } @_eval_base_fields
   } @aggregateAvgs), ##--keys(%aggregateFuncs)

   ##-------------------------------------
   ## Error difference (vs. best): errdiff
   ##  errdiff { of=>$of_field, for=>$for_fields, vs=>$base_field, ... }
   'errdiff' => (our $_errdiff_field =
		{ expand_code =>\&_expand_errdiff,
		  of          =>'pr:g',        ##-- target field: to be set by user or alias
		  vs          =>'max:pr:g',
		  evalname    => '"errdiff(of=$field->{of},vs=$field->{vs})"',
		  evaltitle   =>'"e-($field->{of}|$field->{vs})"',
 		  errmax      => 100.0,        ##-- for error-rate acquisition
		  fmt         => '%.2f',
		  n           => 1,
		}),

   ##-------------------------------------
   ## errdiff(): aliases:
   ## + "e-max:${of}:${for}" --> errdiff(of="$of", vs="max:${of}:${for}")
   ## + "e-:${of}:${for}"    --> "e-max:${of}:${for}"
   (map { _errdiff_max_fields($_) } @_eval_base_fields),
   ##
   ## errdiff(): aliases: aggregates
   ## + "e-max:${aggr}:${of}:${for}" --> errdiff(of="$of", vs="max:${aggr}:${of}:${for}")
   ## + "e-:${aggr}:${of}:${for}"    --> "e-max:${aggr}:${of}:${for}"
   (map {
     my $ebase = $_;
     (map { _errdiff_max_fields("${_}:$ebase") } @aggregateAvgs)
   } @_eval_base_fields),

   ##-------------------------------------
   ## Related configurations: previous stage
   'pstage'  => { expand_code=>\&_expand_prev, 'index'=>'stage' },
   'pstg'    => 'pstage',
   'stage-1' => 'pstage',
   'stg-1'   => 'pstage',

   ##-------------------------------------
   ## Related configurations: previous EM-iteration
   'pemi'  => { expand_code=>\&_expand_prev_emi, 'index'=>'emi' },
   'emi-1' => 'pemi',

   ##-------------------------------------
   ## Related configurations: MetaProfile base (for EM-iteration)
   'mp'     => { expand_code=>\&_expand_mp_base },
   'mpbase' => 'mp',

   ##-------------------------------------
   ## Related configurations: field
   'relative:field' => (our $_relative_field =
			{
			 expand_code=>\&_expand_relative,
			 relative=>'pstage',                   ##-- expands to related-config field
			 field=>'pr:g',                        ##-- field to read off of related-config
			 eval=>'$mf->fieldValue($field->{_relative}{_map}{$cfg}, $field->{_field})',
			 evalname=>'"$field->{relative}:$field->{field}"',
			}),

   ##-------------------------------------
   ## Relative configurations: aliases: previous stage
   ## + "pstage:${field}"
   (map { _relative_fields('pstage',$_) } @_eval_base_fields),

   ## Relative configurations: aliases: previous stage: error-difference
   ## + "e+pstage:${field}"
   (map { _errdiff_relative_fields('pstage',$_) } @_eval_base_fields),

   ##-------------------------------------
   ## Relative configurations: aliases: previous EMI
   ## + "pemi:${field}"
   (map { _relative_fields('pemi',$_) } @_eval_base_fields),

   ## Relative configurations: aliases: previous EMI: error-difference
   ## + "e+pemi:${field}"
   (map { _errdiff_relative_fields('pemi',$_) } @_eval_base_fields),

   ##-------------------------------------
   ## Relative configurations: aliases: MetaProfile base
   ## + "mp:${field}"
   (map { _relative_fields('mp',$_) } @_eval_base_fields),

   ## Relative configurations: aliases: MetaProfile base: error-difference
   ## + "e+mp:${field}"
   (map { _errdiff_relative_fields('mp',$_) } @_eval_base_fields),
  );
##-- EOFIELDS

##---------------------------------------------------------------
## Fields: family expander: macro substitution

## $str = _expand_macros($str, %macros)
sub _expand_macros {
  my $str = shift;
  my ($macro,$val);
  while (@_) {
    ($macro,$val) = (shift,shift);
    $str =~ s/\Q$macro\E/$val/g;
  }
  return $str;
}

##---------------------------------------------------------------
## Fields: family expander: results

## %expanded = _expand_gt(%aliases)
##   + replace macro '__GT__' with 'g', 't', or 'tk'
sub _expand_gt {
  my %aliases = @_;
  my %expanded = qw();
  my ($aname,$avals,$gt);
  while (($aname,$avals)=each(%aliases)) {
    foreach $gt (qw(g t tk)) {
      $expanded{_expand_macros($aname,'__GT__'=>$gt)} = [map {_expand_macros($_,'__GT__'=>$gt)} @$avals];
    }
  }
  return %expanded;
}

## \@field_aliases = _expand_prF(\@prFnames)
##   + no eval()
sub _expand_prF {
  my ($prFnames) = @_;
  my $expanded = [];
  my ($field,$star);
  foreach $field (@$prFnames) {
    $star = '~';
    if ($field =~ /\b\w{0,2}pr\:/) { $star = '*'; }
    elsif ($field =~ /\b\w{0,2}rc\:/) { $star = '+'; }
    elsif ($field =~ /\b\w{0,2}[FI]\:/) { $star = '~'; }
    push(@$expanded, "${star}:${field}", $field);
  }
  return $expanded;
}


## %field_aliases = _expand_family($family, \@fields, $alias_name_template=>\@alias_value_template, $join_field)
##  + templates may use variables "$family" (source family), "$field" or "$_" (source field)
sub _expand_family {
  my ($family, $fields, $tname, $tfields, $join) = @_;
  my $aname  = eval($tname);
  my $avalue = [];
  my ($tfieldi, $tfield,$field);
  foreach $tfieldi (0..$#$tfields) {
    $tfield = $tfields->[$tfieldi];
    foreach (@$fields) {
      $field = $_;
      push(@$avalue, eval($tfield));
    }
    push(@$avalue, $join) if ($join && $tfieldi < $#$tfields);
  }
  return ($aname => $avalue);
}



##---------------------------------------------------------------
## Fields: alias expander: relative fields

## %field_aliases = _relative_fields($relative_field,$base_field)
sub _relative_fields {
  my ($rfield,$ffield) = @_;
  our ($_relative_field);
  return ( "${rfield}:${ffield}" => { %$_relative_field, relative=>$rfield, field=>$ffield } );
}

##---------------------------------------------------------------
## Fields: expander: relative fields: field-value

## \@expanded = _expand_relative($mf,$pstage_ref_field,\@xfields)
sub _expand_relative {
  my ($mf,$field,$xfields) = @_;
  my $relative = $field->{relative};
  $field->{_relative} = $mf->expand($field->{relative})->[0];
  $field->{_field}    = $mf->expand($field->{field})->[0];

  ##-- inherit some source keys
  my %bad_src_keys  = map { $_=>undef } qw(title evaltitle name evalname);
  my @safe_src_keys = grep { !exists($bad_src_keys{$_}) && !exists($field->{$_}) } keys(%{$field->{_field}});
  @$field{@safe_src_keys} = @{$field->{_field}}{@safe_src_keys};

  delete($field->{expand_code});
  return [$field];
}

##---------------------------------------------------------------
## Fields: expanders: relative fields: config: MetaProfile base (for EM-iteration)

## \@expanded = _expand_mp_base($mf,$mp_base_field,\@xfields, %args)
##  + %args:
##      configs => \@configs, ##-- default: $mf->{configs}
sub _expand_mp_base {
  my ($mf,$field,$xfields, %args) = @_;

  ##-- Step 2: get potential MetaProfile configs (by class && emi==-1)
  my $map       = $field->{_map} = {};
  my $configs   = $args{configs} ? $args{configs} : $mf->{configs};
  my @mpconfigs = grep {ref($_) =~ /::MetaProfile$/ && $_->{xvars}{emi}==-1} @$configs;

  ##-- Step 2: map $config=>$mpbase for EM configs
  my ($emcfg,$mpcfg,%em_mp_vars,@em_mpconfigs,$var);

  foreach $emcfg (grep {ref($_) =~ /MetaProfile::EM$/} @$configs)
    {
      ##-- extract MetaProfile-relevant variables (hack)
      %em_mp_vars = %{$emcfg->{uvars}};
      delete(@em_mp_vars{ grep { $_ =~ /^em/ } keys(%em_mp_vars) });

      ##-- find matching MetaProfile config(s)
      @em_mpconfigs = qw();
    MPCFG:
      foreach $mpcfg (@mpconfigs) {
	foreach $var (keys(%em_mp_vars)) {
	  next MPCFG if ($mpcfg->{xvars}{$var} ne $em_mp_vars{$var});
	}
	push(@em_mpconfigs,$mpcfg);
      }

      ##-- no matching MetaProfile config found: can't handle it
      next if (!@em_mpconfigs);

      if (@em_mpconfigs > 1) {
	carp(__PACKAGE__,
	     "::_expand_mp_base(): mutliple matches for ", vkey(\%em_mp_vars), ": using longest.\n");
	@em_mpconfigs = sort { scalar(keys(%{$b->{uvars}})) <=> scalar(keys(%{$a->{uvars}})) } @mpconfigs;
      }

      ##-- finally: map it
      $map->{$emcfg} = $em_mpconfigs[0];
    }

  ##-- set base=$cfg for real MetaProfile configs
  foreach $mpcfg (grep {ref($_) =~ /::MetaProfile$/} @$configs) {
    $map->{$mpcfg} = $mpcfg;
  }

  ##-- Step 3: set field eval()
  $field->{eval} =
    sub {
      return $map->{$_[1]};
    };

  delete($field->{expand_code}); ##-- avoid deep recursion (?)
  return [$field];
}


##---------------------------------------------------------------
## Fields: expanders: relative fields: config: previous EMI (hack)

## \@expanded = _expand_prev_emi($mf,$prev_emi_field,\@xfields)
sub _expand_prev_emi {
  my ($mf,$field,$xfields) = @_;

  ##-- Step 1: default expansion (works for everything but maybe emi==0)
  ##   + initializes $field->{_map}
  $mf->_expand_prev($field,$xfields);
  my $map = $field->{_map};

  ##-- Step 2: expand 'mpbase' field for emi==0 configs
  my $configs  = $mf->{configs};
  my @configs0 = grep {$_->{xvars}{emi}<=0} @$configs;
  my $mpbase   = $field->{_mpbase} = $mf->_expand_mp_base({}, $xfields, configs=>\@configs0)->[0];

  ##-- Step 3: merge '_mpbase' configs in as map-values of emi==0 EM-configs
  my ($emcfg);
  foreach $emcfg (grep {!defined($map->{$_})} @$configs) {
    $map->{$emcfg} = $mpbase->{_map}{$emcfg};
  }

  delete($field->{expand_code}); ##-- avoid deep recursion (?)
  return [$field];
}


##---------------------------------------------------------------
## Fields: expanders: relative fields: config: previous ${ $field->{'index'} }

## \@expanded = _expand_prev($mf,$prev_field,\@xfields)
sub _expand_prev {
  my ($mf,$field,$xfields) = @_;

  ##-- Step 1: map key=>$config
  my $index_field = $mf->expand($field->{'index'})->[0];
  my $key2cfg     = {}; ##-- $key   => $cfg, ...
  my $cfg2val     = {}; ##-- $cfg   => $indexVal, ...
  my ($cfg,$ckey,$cval);
  foreach $cfg (@{$mf->{configs}}) {
    $cval   = $mf->fieldValue($cfg,$index_field);
    $ckey   = vkey( {%{$cfg->{uvars}}, $field->{'index'}=>$cval} );
    $key2cfg->{$ckey} = $cfg;
    $cfg2val->{$cfg}  = $cval;
  }

  ##-- Step 2: map config=>$prev_config_or_undef
  my $map = $field->{_map} = {};
  my (%pvars,$pkey);
  foreach $cfg (@{$mf->{configs}}) {
    %pvars = ( %{$cfg->{uvars}}, $field->{'index'}=>($cfg2val->{$cfg}-1) );
    $map->{$cfg} = $key2cfg->{vkey(\%pvars)};
  }

  ##-- Step 3: set eval()
  $field->{eval} =
    sub {
      return $map->{$_[1]};
    };

  delete($field->{expand_code}); ##-- avoid deep recursion (?)
  return [$field];
}


##---------------------------------------------------------------
## Fields: alias expander: _errdiff_relative_fields (error difference of=X vs. relative:field of=X,relative=Y)
sub _errdiff_relative_fields {
  my ($rel,$of) = @_;

  our ($_errdiff_field);
  return (
	  "e+${rel}:${of}" => {
			       %$_errdiff_field,
			       of=>$of,
			       vs=>"${rel}:${of}",
			       title=>"e+${rel}:${of}",
			      },
	 );
}


##---------------------------------------------------------------
## Fields: alias expander: _errdiff_max_fields (error difference of=X vs. max of=X,for=Y)

## %field_aliases = _errdiff_max_fields($of_field)
sub _errdiff_max_fields {
  my ($of_field) = @_;

  our ($_errdiff_field);
  return (
	  "e-max:${of_field}:corpus" => {
					 %$_errdiff_field,
					 of=>$of_field,
					 vs=>"max:${of_field}:corpus",
					 title=>"e-max:S:${of_field}",
					},
	  "e-max:${of_field}:stage"  => {
					 %$_errdiff_field,
					 of=>$of_field,
					 vs=>"max:${of_field}:stage",
					 title=>"e-max:stg:${of_field}",
					},
	  "e-max:${of_field}:stg"    => "e-max:${of_field}:stage",
	  "e-max:${of_field}:emi"    => {
					 %$_errdiff_field,
					 of=>$of_field,
					 vs=>"max:${of_field}:emi",
					 title=>"e-max:emi:${of_field}",
					},
	  ##--
	  "e-:${of_field}:corpus"    => "e-max:${of_field}:corpus",
	  "e-:${of_field}:stage"     => "e-max:${of_field}:stage",
	  "e-:${of_field}:stg"       => "e-max:${of_field}:stage",
	  "e-:${of_field}:emi"       => "e-max:${of_field}:emi",
	  ##--
	  "e-:${of_field}"           => "e-max:${of_field}:corpus",
	 );
}

##---------------------------------------------------------------
## Fields: expanders: _expand_errdiff()

## \@expanded = _expand_errdiff($mf,$errdiff_field,\@xfields)
sub _expand_errdiff {
  my ($mf,$ediff_field,$xfields) = @_;

  ##-- get nested 'of' field
  my $of_field
    = $ediff_field->{_of}
      = $mf->expand($ediff_field->{of})->[0];
  my $vs_field
    = $ediff_field->{_vs}
      = $mf->expand($ediff_field->{vs})->[0];

  $ediff_field->{eval} = \&_error_difference;

  return [$ediff_field];
}

## $errdiff_value = _error_difference($mfields,$config,$errdiff_field,$ediff_val)
sub _error_difference {
  my ($mf,$cfg,$ediff_field) = @_;
  my $of = $mf->fieldValue($cfg,$ediff_field->{_of});
  my $vs = $mf->fieldValue($cfg,$ediff_field->{_vs});
  return __errdiff($of,$vs,$ediff_field->{errmax});
}

## $error_difference = __errdiff($of,$vs,[$max=1.0])
sub __errdiff {
  my ($of,$vs,$emax) = @_;
  $emax = 1.0 if (!defined($emax));
  return $emax * (($emax-$vs) - ($emax-$of)) / ($emax-$vs);
}

##---------------------------------------------------------------
## Fields: alias expander: _markmax_fields()

## %field_aliases = _markmax_fields($of_field)
sub _markmax_fields {
  my ($of_field) = @_;

  our ($_ifmax_corpus, $_ifmax_stage, $_ifmax_emi);
  return (
	  map {
	    (
	     "${_}:${of_field}:corpus" => { %$_ifmax_corpus, of=>$of_field, then=>$_ },
	     "${_}:${of_field}:stage"  => { %$_ifmax_stage,  of=>$of_field, then=>$_ },
	     "${_}:${of_field}:stg"    => "${_}:${of_field}:stage",
	     "${_}:${of_field}:emi"    => { %$_ifmax_emi,    of=>$of_field, then=>$_ },
	     ##--
	     "${_}${_}:${of_field}"      => [
					     "${_}:${of_field}:corpus",
					     "${_}:${of_field}:stage",
					    ],
	     "${_}${_}${_}:${of_field}"  => [
					     "${_}:${of_field}:corpus",
					     "${_}:${of_field}:stage",
					     "${_}:${of_field}:emi",
					    ],
	     "${_}:${of_field}"          => "${_}${_}${_}:${of_field}",
	    )
	  } ('*', '+', '-', '~')
	 );
}

sub _markmax_fields_latex {
  my ($of_field) = @_;

  my %argsL = (latexAttach=>'r');
  my %argsR = (latexAttach=>'l');
  our ($_ifmax_corpus, $_ifmax_stage, $_ifmax_emi);
  return
    (
     "*l:${of_field}:corpus:<" => { %$_ifmax_corpus, of=>$of_field, %argsL, then=>'$\star$' },
     "*l:${of_field}:corpus:>" => { %$_ifmax_corpus, of=>$of_field, %argsR, then=>'' },

     "*l:${of_field}:stage:<" => { %$_ifmax_stage, of=>$of_field, %argsL, then=>'\bfseries{', },
     "*l:${of_field}:stage:>" => { %$_ifmax_stage, of=>$of_field, %argsR, then=>'}', },

     "*l:${of_field}:stg:<"  => "*l:${of_field}:stage:<",
     "*l:${of_field}:stg:>"  => "*l:${of_field}:stage:>",

     "*l:${of_field}:emi:<"  => { %$_ifmax_emi,    of=>$of_field, %argsL, then=>'\slshape{', },
     "*l:${of_field}:emi:>"  => { %$_ifmax_emi,    of=>$of_field, %argsR, then=>'}', },

     ##--
     "**l:${of_field}"       => [
				 "*l:${of_field}:corpus:<",
				 "*l:${of_field}:stage:<",
				 $of_field,
				 "*l:${of_field}:stage:>",
				 "*l:${of_field}:corpus:>",
				],
     "***l:${of_field}"      => [
				 "*l:${of_field}:corpus:<",
				 "*l:${of_field}:stage:<",
				 "*l:${of_field}:emi:<",
				 $of_field,
				 "*l:${of_field}:emi:>",
				 "*l:${of_field}:stage:>",
				 "*l:${of_field}:corpus:>",
				],

     "*l:${of_field}"        => "***l:${of_field}",
    );
}


##---------------------------------------------------------------
## Fields: expanders: 'ifmax'

## \@expanded = _expand_ifmax($mf,$ifmax_field,\@xfields)
sub _expand_ifmax {
  my ($mf,$ifmax_field,$xfields) = @_;
  $ifmax_field->{a} = $ifmax_field->{of};
  $ifmax_field->{b} = $mf->_expand_max({%$ifmax_field, title=>undef,hidden=>0},$xfields)->[0];
  delete($ifmax_field->{expand_code});
  return $mf->_expand_ifequal($ifmax_field, $xfields);
}

## \@expanded = _expand_ifmax($mf,$ifmax_field,\@xfields)
sub _expand_ifmax_0 {
  my ($mf,$ifmax_field,$xfields) = @_;

  ##-- get nested 'max' field
  my $max_field
    = $ifmax_field->{_max}
      = $mf->_expand_max({%$ifmax_field, title=>undef,hidden=>0},$xfields)->[0];
  my $of_field
    = $ifmax_field->{_of}
      = $mf->expand($ifmax_field->{of})->[0];

  $ifmax_field->{eval} =
    ('$mf->fieldValue($cfg,$field->{_max}) == $mf->fieldValue($cfg,$field->{_of})'
     .' ? "$field->{then}"'
     .' : "$field->{else}"'
    );

  return [$ifmax_field];
}

##---------------------------------------------------------------
## Fields: expanders: 'ifequal'

## \@expanded = _expand_ifequal($mf,$ifequal_field,\@xfields)
sub _expand_ifequal {
  my ($mf,$ifeq_field,$xfields) = @_;

  ##-- get nested fields (a,b)
  my $af = $ifeq_field->{_a} = $mf->expand($ifeq_field->{a})->[0];
  my $bf = $ifeq_field->{_b} = $mf->expand($ifeq_field->{b})->[0];

  $ifeq_field->{eval} = sub {
    return ($af->{n} && $bf->{n}
	    ? ($mf->fieldValue($_[1],$af) == $mf->fieldValue($_[1],$bf)
	       ? $ifeq_field->{then}
	       : $ifeq_field->{else})
	    : ($mf->fieldValue($_[1],$af) eq $mf->fieldValue($_[1],$bf)
	       ? $ifeq_field->{then}
	       : $ifeq_field->{else}));
  };

  delete($ifeq_field->{expand_code});
  return [$ifeq_field];
}

##---------------------------------------------------------------
## Fields: aliases: 'max:*'

## %FIELD_ALIASES = _max_fields($of_field)
sub _max_fields {
  my $of = shift;
  our ($_max_field);
  return (
	  "max:${of}:corpus" => { %$_max_field, of=>$of, for=>'corpus',       title=>"max($of|S)"  },
	  "max:${of}:stage"  => { %$_max_field, of=>$of, for=>'corpus,stage', title=>"max($of|stg)" },
	  "max:${of}:stg"    => "max:${of}:stg",
	  "max:${of}:emi"    => { %$_max_field, of=>$of, for=>'corpus,stage,emi', title=>"max($of|emi)" },
	  ##--
	  "max:${of}"        => { %$_max_field, of=>$of, for=>'corpus', title=>"max($of)", },
	  ##--
	  "max:${of}:k:corpus" => { %$_max_field, of=>$of, for=>'tck,corpus',       title=>"max($of|S)"  },
	  "max:${of}:k:stage"  => { %$_max_field, of=>$of, for=>'tck,corpus,stage', title=>"max($of|stg)" },
	  "max:${of}:k:stg"    => "max:${of}:stg",
	  "max:${of}:k:emi"    => { %$_max_field, of=>$of, for=>'tck,corpus,stage,emi', title=>"max($of|emi)" },
	 );
}

##---------------------------------------------------------------
## Fields: expanders: aggregate functions

## \@expanded = _expand_aggregate($mf,$aggregate_field,\@xfields)
sub _expand_aggregate {
  my ($mf,$afield,$xfields) = @_;
  my $of_fields = $mf->expand($afield->{of});
  my $afunc = $afield->{aggregate};
  my $aname = $afield->{aggregateName} || $afunc;
  my $expanded = [];
  my ($src,$dst);
  foreach $src (@$of_fields) {
    ##-- sanity check
    if (!$src->{n}) {
      push(@$expanded, $src);
      next;
    }

    $dst = {
	    %$src,
	    #%$afield, ##-- NO!
	    aggregate=>$afunc,
	    aggregateName=>$aname,
	    (defined($afield->{options}) ? %{$afield->{options}} : qw()),
	   };

    ##-- path
    #$dst->{path} = [$afunc,@{$src->{path}}];

    ##-- name
    $dst->{name}     = $aname.':'.$dst->{name} if (defined($dst->{name}));
    $dst->{evalname} = "'${aname}:'.do { $dst->{evalname} }" if (defined($dst->{evalname}));

    ##-- title
    $dst->{title}     = $aname.':'.$dst->{title} if (defined($dst->{title}));
    $dst->{evaltitle} = "'${aname}:'.do { $dst->{evaltitle} }" if (defined($dst->{evaltitle}));

    ##-- expansion complete
    push(@$expanded, $dst);
  }

  return $expanded;
}

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

  my %bad_src_keys  = map { $_=>undef } qw(title evaltitle name evalname);
  my @safe_src_keys = grep { !exists($bad_src_keys{$_}) && !exists($max_field->{$_}) } keys(%$of_field);
  @$max_field{@safe_src_keys} = @$of_field{@safe_src_keys};

  delete($max_field->{expand_code});
  return [$max_field];
}

## $varsKey = vkey(\%hash)
sub vkey {
  my $h = shift;
  return join(' ', map { "$_=$h->{$_}" } sort(keys(%$h)));
}

##---------------------------------------------------------------
## Fields: expanders: 'auto', 'all'

## \@expanded = _expand_auto_all($mf,$auto_or_all_field,\@xfields,\@which_vars)
sub _expand_auto_all {
  my ($mf,$aa_field,$xfields,$aa_vars) = @_;

  ##-- get literal fields, indexing by title
  my @fields_literal = grep { ref($_) } @$xfields;
  my %lit2field      = map { $mf->fieldName($_)=>$_ } @fields_literal;

  ##-- get all user variables, indexing by title
  my $fields_aa = $mf->expand([map {"xvars->$_"}  @$aa_vars]);
  my %aa2field  = map { $_->{name}=>$_ } @$fields_aa;

  ##-- avoid inclusion of literal fields
  delete(@aa2field{ keys(%lit2field) });
  delete(@aa2field{ map { $_->{alt} ? @{$_->{alt}} : qw() } @fields_literal });

  ##-- avoid inclusion of user-specified 'except' fields
  if ($aa_field->{except}) {
    my $except = $mf->expand($aa_field->{except});
    delete(@aa2field{ map { $_->{name} } @$except });
    delete(@aa2field{ map { $_->{alt} ? @{$_->{alt}} : qw() } @$except });
  }

  return [ @aa2field{sort keys %aa2field} ];
}

##---------------------------------------------------------------
## Fields: expanders: 'all'

## \@expanded = _expand_all($mf,$all_field,\@xfields)
sub _expand_all {
  my ($mf,$all_field,$xfields) = @_;
  return $mf->_expand_auto_all($all_field,$xfields,[userVariables($mf->{configs})]);
}

##---------------------------------------------------------------
## Fields: expanders: 'auto'
## \@expanded = _expand_auto($mf,$auto_field,\@xfields)
sub _expand_auto {
  my ($mf,$auto_field,$xfields) = @_;
  return $mf->_expand_auto_all($auto_field,$xfields,[activeVariables($mf->{configs})]);
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
    $info = $cfg->pathValue($srcfield->{path});
    @tags2{keys(%$info)} = undef;
  }

  ##-- map tag2-keys to expanded fields
  #splice(@$xfields, $fi,1, map { {%$srcfield, $tagvar=>$_} } sort(keys(%tags2)));
  my ($ai,$bi);
  return [map { {%$srcfield, $tagvar=>$_} }
	  sort {
	    $ai = ($a =~ /(\d+)$/ ? $1 : 0);
	    $bi = ($b =~ /(\d+)$/ ? $1 : 0);
	    $ai <=> $bi || $a cmp $b
	  } keys(%tags2)
	 ];
}

##---------------------------------------------------------------
## Fields: expanders: 'xcomp'
##  + configuration cross-comparison (ordered pairs)

## \@expanded = _expand_xcomp($mf,$xcomp_field,\@xfields)
##  + %$xcomp_field keys:
##      xc_sortby=>$sortby_spec,      ##-- vars: none
##
##      xc_title=>$eval_str,          ##-- vars: $cfg  : REQUIRED
##      xc_evaltitle=>$eval_eval_str, ##-- vars: $cfg
##      xc_name=>$eval_str,           ##-- vars: $cfg
##      xc_evalname=>$eval_eval_str,  ##-- vars: $cfg
##
##      xc_eval =>$eval_str,          ##-- vars: $cfg1, $cfg2
##      ...
##  + $eval_str vars:
##      $mf   ##-- make fields object
##      $cfg1 ##-- row config
##      $cfg2 ##-- column config
sub _expand_xcomp {
  my ($mf,$xcfield,$xfields) = @_;
  my $configs  = $mf->{configs};

  ##-- Step 0: create '_on' subkey, if 'on' is available and no '_on' key is already defined
  if (defined($xcfield->{on}) && !defined($xcfield->{_on})) {
    $xcfield->{_on} = $mf->expand($xcfield->{on})->[0];
    ##-- adopt 'fmt' if available
    $xcfield->{fmt} = $xcfield->{_on}{fmt} if (!defined($xcfield->{fmt}));
  }

  ##-- Step 1: create 'xc_title' field
  my $cfg2xc      = $xcfield->{cfg2xc} = {};
  my $title_field = {
		     name=>"xc_title_${xcfield}",
		     title=>'',
		     cfg2xc=>$cfg2xc,
		     eval=>'$field->{cfg2xc}{$cfg}',
		    };
  my ($cfg);
  foreach $cfg (@$configs) {
    if    (defined($xcfield->{xc_title})) { $cfg2xc->{$cfg} = eval $xcfield->{xc_title}; }
    elsif (defined($xcfield->{xc_name}))  { $cfg2xc->{$cfg} = eval $xcfield->{xc_name};  }
  }

  ##-- Step 2: ensure 'xc_sortby', *prepend* it to $mf->{sortby}
  $xcfield->{xc_sortby} = $title_field if (!defined($xcfield->{xc_sortby}));
  if (defined($mf->{sortby})) {
    $mf->{sortby} = $mf->expand([$title_field,$mf->{sortby}]);
  } else {
    $mf->{sortby} = [$title_field];
  }

  ##-- Step 3: create one field (column) for each config
  my ($field,$cfg1,$cfg2);
  my @no_adopt_xc_keys = qw(title evaltitle name evalname expand_code);
  my @expanded = (
		  $xcfield->{xc_prepend} ? @{$mf->expand($xcfield->{xc_prepend})} : qw(),
		  $title_field,
		  $xcfield->{xc_append}  ? @{$mf->expand($xcfield->{xc_append})} : qw(),
		 );
  foreach $cfg ($mf->sortConfigs($configs,sortby=>$xcfield->{xc_sortby})) {
    $field = { %$xcfield };
    delete(@$field{@no_adopt_xc_keys});
    $field->{title} = eval $xcfield->{xc_title} if (exists($xcfield->{xc_title}));
    $field->{name}  = eval $xcfield->{xc_name}  if (exists($xcfield->{xc_name}));
    $field->{evaltitle} = eval $xcfield->{xc_evaltitle} if (exists($xcfield->{xc_evaltitle}));
    $field->{evalname}  = eval $xcfield->{xc_evalname}  if (exists($xcfield->{xc_evalname}));

    if (exists($xcfield->{xc_eval})) {
      $cfg2 = $cfg;
      $field->{xc_values} = {};
      $field->{eval}      = '$field->{xc_values}{$_}';
      foreach $cfg1 (@$configs) {
	$field->{xc_values}{$cfg1} = eval $xcfield->{xc_eval};
      }
    }

    push(@expanded,
	 $xcfield->{xc_prepend} ? @{$mf->expand($xcfield->{xc_prepend})} : qw(),
	 $field,
	 $xcfield->{xc_append}  ? @{$mf->expand($xcfield->{xc_append})} : qw(),
	);
  }

  return [@expanded];
}

##---------------------------------------------------------------
## Fields: expanders: 'rxcomp'
##  + configuration cross-comparison using R

## \@expanded = _expand_rxcomp($mf,$rxcomp_field,\@xfields)
##  + for cross-config R tests using MUDL::RSPerl
##  + new keys for %$rxcomp_field
##     rfunc => $literal_R_function,
##     rattr => $literal_R_attr,
##     rargs => \@additional_rfunc_args,
##     on    => $fieldToTest,
##     rxc_fmt => $fmtString,
##  + calls _expand_xcomp() on a generated xcomp field
##    - populates generated field's 'xc_eval' key
sub _expand_rxcomp {
  my ($mf,$rxcfield,$xfields) = @_;
  require MUDL::RSPerl;

  ##-- generate xcomp field
  my $on_field = $mf->expand($rxcfield->{on})->[0];
  my $xc_eval = ('MUDL::RSPerl->genericTest('
		 .join(',',
		       "'$rxcfield->{rfunc}'",
		       ($rxcfield->{rattr} ? "'$rxcfield->{rattr}'" : 'p.value'),
		       '[map {$mf->fieldValue($_,$field->{_on})} @{$cfg1->configArray}]',
		       '[map {$mf->fieldValue($_,$field->{_on})} @{$cfg2->configArray}]',
		       ($rxcfield->{rargs} ? @{$rxcfield->{rargs}} : qw()),
		      )
		 .')');
  $xc_eval = "sprintf(qq($rxcfield->{rxc_fmt}), $xc_eval)" if ($rxcfield->{rxc_fmt});

  my $xc_field = {
		  %$rxcfield,
		  expand_code=> \&_expand_xcomp,
		  _on        => $on_field,
		  xc_eval    => $xc_eval,
		 };

  return $mf->_expand_xcomp($xc_field,$xfields);
}

##---------------------------------------------------------------
## Fields: expanders: 'devdiff'
##  + configuration cross-comparison using average + dev
sub _expand_devdiff {
  my ($mf,$ddfield,$xfields) = @_;
  my $xfield = {%$ddfield};
  delete($xfield->{expand_code});

  ##-- Expand subfields: on,dev to '_on', '_dev'
  #my $on_field  = $xfield->{_on}  = $mf->expand($xfield->{on})->[0]; ##-- done by '_expand_xcomp'
  my $dev_field = $xfield->{_dev} = $mf->expand($xfield->{dev})->[0];

  $xfield->{xc_eval} = (''
			.'my $val1=$mf->fieldValue($cfg1,$field->{_on});'
			.'my $val2=$mf->fieldValue($cfg2,$field->{_on});'
			.'my $dev1=$mf->fieldValue($cfg1,$field->{_dev});'
			.'$dev1 ? (($val1-$val2)/$dev1) : 1e38'
		       );

  return $mf->_expand_xcomp($xfield,$xfields);
}


##---------------------------------------------------------------
## Fields: expanders: 'enum'
##  + configuration enumeration
sub _expand_enum {
  my ($mf,$efield,$xfields) = @_;
  my $xfield = {from=>0,cfg2i=>{},n=>1,%$efield};
  delete($xfield->{expand_code});
  if (!defined($efield->{eval})) {
    $xfield->{eval}='$field->{cfg2i}{$cfg}';
  } else {
    $xfield->{eval} = $efield->{eval};
    $xfield->{eval} =~ s/\$_/\$field->{cfg2i}{\$cfg}/g;
  }

  my $val2i    = {};
  my $i        = $xfield->{from}||0;
  my ($cfg,$val);
  $xfield->{sortby} = $xfield->{of} if (!defined($xfield->{sortby}));
  my $of_field = $mf->expand($xfield->{of})->[0];
  foreach $cfg ($mf->sortConfigs($mf->{configs}, sortby=>$xfield->{sortby})) {
    $val = $mf->fieldValue($cfg,$of_field);
    $val2i->{$val} = $i++ if (!defined($val2i->{$val}));
    $xfield->{cfg2i}{$cfg} = $val2i->{$val};
  }
  ##-- setup title, name
  $xfield->{name} = ("enum("
		     ."of=".$mf->fieldName($of_field)
		     .",from=$xfield->{from}"
		     .",sortby=$xfield->{sortby}"
		     .",eval=(".(defined($xfield->{eval}) ? $xfield->{eval} : '$_').")"
		     .")")
    if (!defined($xfield->{name}));
  $xfield->{title} = "enum(".$mf->fieldTitle($of_field).")"
    if (!defined($xfield->{title}));

  return [$xfield];
}

##---------------------------------------------------------------
## Fields: expanders: 'join'
##  + join multiple values
sub _expand_join {
  my ($mf,$jfield,$xfields) = @_;
  my $xfield = {%$jfield};
  delete($xfield->{expand_code});

  ##-- expand subfields
  my $of_fields = $xfield->{of} = $mf->expand($xfield->{of});

  ##-- properties
  $xfield->{title} = join($xfield->{with}, map {$mf->fieldTitle($_)} @$of_fields)
    if (!defined($xfield->{title}));

  $xfield->{name} = join($xfield->{with}, map {$mf->fieldName($_)} @$of_fields)
    if (!defined($xfield->{name}));

  $xfield->{eval} = 'join($field->{with}, map {$mf->fieldValue($cfg,$_)} @{$field->{of}})';

  return [$xfield];
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
## Field addition

## $mf = $mf->prepend(\@field_hashes_or_strings,...)
## $mf = $mf->prepend($mf2,...)
sub prependFields {
  my $mf = shift;
  $mf->{fields} = [$mf->{fields}] if (!ref($mf->{fields}));
  unshift(@{$mf->{fields}}, @{$mf->expand(@_)});
  return $mf;
}
## $mf = $mf->prepend(\@field_hashes_or_strings,...)
## $mf = $mf->prepend($mf2,...)
sub appendFields {
  my $mf = shift;
  $mf->{fields} = [$mf->{fields}] if (!ref($mf->{fields}));
  push(@{$mf->{fields}}, @{$mf->expand(@_)});
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

  ##-- Step 0: get path (impose aggregate function $field->{aggregate})
  my @path = defined($field->{path}) ? @{$field->{path}} : qw();
  unshift(@path, $field->{aggregate})
    if ($field->{aggregate} && UNIVERSAL::isa($cfg,'MUDL::Make::Config::Group'));

  ##-- Step 1: get path-value
  my $val = $cfg->pathValue(\@path);

  ##-- Step 2: instantiate dynamic value ?!?!?! ---TODO---

  if (!defined($val)) {
    ##-- defaults for undefined values
    if ($field->{n}) { $val = 0; }
    else { $val = ''; }
  }

  if ($field->{eval}) {
    ##-- maybe eval some code
    if (ref($field->{eval}) && ref($field->{eval}) eq 'CODE') {
      ##-- it's a CODE ref: call it with arguments ($mf,$cfg,$field,$val)
      $val = $field->{eval}->($mf,$cfg,$field,$val);
    }
    else {
      ##-- treat as a code string
      my $tmp=$_;
      $_=$val; ##-- HACK
      eval qq(no warnings 'void'; \$val=$field->{eval};);
      $_=$tmp; ##-- HACK
      carp(ref($mf)."::fieldValue(): error in eval($field->{eval}): $@") if ($@);
    }
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

## $fieldName = $mf->fieldName($field)
##  + destructively alters $field (adds 'name' key if not present)
##  + uses $mf->fieldTitle($field) if no 'name' or 'evalname' is present
sub fieldName {
  return $_[1]{name} if (defined($_[1]{name}));

  my ($mf,$field) = @_;
  if (defined($field->{evalname})) {
    $field->{name} = eval qq({ no warnings 'void'; $field->{evalname} });
    carp(ref($mf),"::fieldName(): error evaluating field title '$field->{evalname}': $@")
      if ($@);
    return $field->{name} if (defined($field->{name}));
  }
  return $field->{name} = $mf->fieldTitle($field);
}

## $fieldTitle = $mf->fieldTitle($field)
##  + destructively alters $field (adds 'title' key if not present)
##  + uses $field->{name} if defined and field has empty path
sub fieldTitle {
  return $_[1]{title} if (defined($_[1]{title}));
  my ($mf,$field) = @_;
  ##--
  if (defined($field->{evaltitle})) {
    $field->{title} = eval qq({ no warnings 'void'; $field->{evaltitle} });
    carp(ref($mf),"::fieldTitle(): error evaluating field title '$field->{evalTitle}': $@")
      if ($@);
    return $field->{title} if (defined($field->{title}));
  }
  elsif (defined($field->{name}) && (!$field->{path} || !@{$field->{path}})) {
    return $field->{title} = $field->{name};
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
##     sortby =>$sortby_fieldspec,   ##-- default: $mf->xfields()
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
    ##-- expand MUDL::Make::Field objects as themselves
    if (ref($ufield) && UNIVERSAL::isa($ufield,'MUDL::Make::Fields')) {
      unshift(@ufields, @{$ufield->{fields}});
      next;
    }

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
	    push(@parsed, {path=>$pathstr,opts=>$optstr});
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

  ##-- expand field names and titles
  foreach (@$xfields) {
    $mf->fieldTitle($_);
    $mf->fieldName($_);
  }

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
## Functions: variant conditions

## @avars = activeVariables(\@configs)
push(@{$EXPORT_TAGS{utils}}, 'activeVariables');
sub activeVariables {
  my $configs = shift;

  ##-- Get all user variables
  my @uvars = userVariables($configs);

  ##-- count number of (var,value) pairs
  my %var2val2n = qw();
  my ($cfg,$var,$val);
  foreach $cfg (@$configs) {
    foreach $var (@uvars) {
      if (defined($val=$cfg->{uvars}{$var})) {
	++$var2val2n{$var}{$val};
      }
      elsif (defined($val=$cfg->{xvars}{$var})) {
	++$var2val2n{$var}{$val};
      }
    }
  }

  ##-- get all actually varied ${var}s as those for which:
  ##   + only one defined value exists
  ##      AND
  ##   + every selected user-config declares that value in its {uvars} or, if undefined, in its {xvars}
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
