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

## @_eval_base_fields
##  + many generated fields are created by iterating over these
our @_eval_base_fields =
  (
   ##-- Total (token-wise)
   qw(tpr:g trc:g tF:g),
   qw(tpr:t trc:t tF:t),

   ##-- Meta (token-wise)
   qw(pr:g rc:g F:g ar:g),
   qw(pr:t rc:t F:t ar:t),

   ##-- MI,H (token-wise)
   qw(mi:g mi:t),
   qw(Hpr:g Hrc:g HF:g HI:g),
   qw(Hpr:t Hrc:t HF:t HI:t),

   ##-- Average tag2 (Schütze-style)
   qw(apr:g arc:g aF:g),
   qw(apr:t arc:t aF:t),

   ##-- Weighted Average tag2 (Schütze-style, weighted)
   qw(wapr:g warc:g waF:g),
   qw(wapr:t warc:t waF:t),

   ##-- Pairwise (Schulte im Walde-style)
   qw(ppr:g prc:g pF:g),
   qw(ppr:t prc:t pF:t),

   ##-- Weighted Pairwise (Schulte im Walde-style)
   qw(wppr:g wprc:g wpF:g),
   qw(wppr:t wprc:t wpF:t),
  );


##---------------------------------------------------------------
## Globals: Field Aliases (end = search for 'EOFIELDS')

## %FIELDS: ( $fieldName => \%fieldSpecHash, ..., $aliasName=>$fieldName, )
##  + %fieldSpecHash keys:
##     path => \@path,       ##-- nested MUDL::Make::Config key-path
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
		     qw(stage emi corpus lrlab auto),
		      #qw(pr:g rc:g pr:t rc:t)
		    ],
   'collectDefault' => [ qw(corpus stage), ],
   'plotKeyDefault' => [qw(corpus lrlab auto)],

   'default'          => 'tabDefault',
   'tabid'            => 'tabId',
   'id'               => 'tabId',
   'results'          => 'tabResults',

   (map { ("tab$_"  => "summarize$_") } qw(Default Id Results)),
   (map { ("ltab$_"  => "latex$_") } qw(Default Id Results)),


   ##-- Summarize (ASCII)
   'summarizeDefault' => ['summarizeId', '|', 'summarizeResults'],
   'summarizeId' => [
		     #'stage.emi',
		     'stage', 'emi',
		     'corpus',
		     #'lang',
		     'lrlabel',
		     #':',
		     'auto',
		    ],
   'summarizeResults'=>[
			qw(*:pr:g pr:g),
			qw(+:rc:g rc:g),
			qw(~:F:g  F:g),
			#qw(*:apr:g apr:g   *:arc:g arc:g  ~:aF:g aF:g),
			#qw(ar:g),
			'|',
			qw(*:pr:t pr:t),
			qw(+:rc:t rc:t),
			qw(~:F:t  F:t),
			#qw(ar:t),
		       ],

   ##-- table: variants: MetaProfile
   'mpId'  => 'mpid',
   'mpid'  => [ qw(stage corpus lrlabel auto) ],
   'mptab' => [ qw(mpid | mpresults) ],
   'mptab:H' => [ qw(mpid | mpresults:H) ],
   'mptab:a' => [ qw(mpid | mpresults:a) ],
   'mptab:wa' => [ qw(mpid | mpresults:wa) ],
   'mptab:p' => [ qw(mpid | mpresults:p) ],
   'mptab:wp' => [ qw(mpid | mpresults:wp) ],
   'mptab:pstg' => [ qw(mpid | mpresults:pstg) ],
   'mptab:e-'   => [ qw(mpid || mpresults:e-) ],

   ##-- Results: MetaProfile: meta-(pr,rc,F)
   'mpresults' => [
		   qw(*:pr:g pr:g +:rc:g rc:g ~:F:g F:g), '|',
		   qw(*:pr:t pr:t +:rc:t rc:t ~:F:t F:t),
		  ],

   ##-- Results: MetaProfile: H-(pr,rc,F)
   'mpresults:H' => [
		     #qw(*:Hpr:g Hpr:g *:Hrc:g Hrc:g *:HI:g HI:g ~:HF:g HF:g), '|',
		     #qw(*:Hpr:t Hpr:t *:Hrc:t Hrc:t *:HI:t HI:t ~:HF:t HF:t),
		     qw(*:Hpr:g Hpr:g +:Hrc:g Hrc:g ~:HF:g HF:g), '|',
		     qw(*:Hpr:t Hpr:t +:Hrc:t Hrc:t ~:HF:t HF:t),
		    ],

   ##-- Results: MetaProfile: avg-(pr,rc,F)
   'mpresults:a' => [
		     qw(*:apr:g apr:g +:arc:g arc:g ~:aF:g aF:g), '|',
		     qw(*:apr:t apr:t +:arc:t arc:t ~:aF:t aF:t),
		    ],

   ##-- Results: MetaProfile: weighted avg-(pr,rc,F)
   'mpresults:wa' => [
		      qw(*:wapr:g wapr:g +:warc:g warc:g ~:waF:g waF:g), '|',
		      qw(*:wapr:t wapr:t +:warc:t warc:t ~:waF:t waF:t),
		     ],

   ##-- Results: MetaProfile: weighted pair-(pr,rc,F)
   'mpresults:p' => [
		     qw(*:ppr:g ppr:g +:prc:g prc:g ~:pF:g pF:g), '|',
		     qw(*:ppr:t ppr:t +:prc:t prc:t ~:pF:t pF:t),
		    ],

   ##-- Results: MetaProfile: weighted weighted-pair-(pr,rc,F)
   'mpresults:wp' => [
		      qw(*:wppr:g wppr:g +:wprc:g wprc:g ~:wpF:g wpF:g), '|',
		      qw(*:wppr:t wppr:t +:wprc:t wprc:t ~:wpF:t wpF:t),
		     ],

   ##-- Results: MetaProfile: meta-(pr,rc,F): vs. previous stage
   'mpresults:pstg' => [
			qw(*:pr:g pr:g e+pstage:pr:g(title=e+pstg)), '|',
			qw(*:pr:t pr:t e+pstage:pr:t(title=e+pstg)),
		       ],

   ##-- Results: MetaProfile: meta-(pr,rc,F): vs. max(stage)
   'mpresults:e-' => [
		       qw(*:pr:g pr:g e-max:pr:g:stg(title=e-max)), '|',
		       qw(+:rc:g rc:g e-max:rc:g:stg(title=e-max)), '/',
		       qw(*:pr:t pr:t e-max:pr:t:stg(title=e-max)), '|',
		       qw(+:rc:t rc:t e-max:rc:t:stg(title=e-max)),
		      ],

   ##-- table: variants: EM
   'emid'  => [ qw(stg emi corpus lrlabel auto) ],
   'emtab'  => [ qw(emid | emresults), ],
   'emtab:max' => [ qw(emid | emresults:max), ],
   'emtab:H' => [ qw(emid | emresults:H), ],
   'emtab:H:max' => [ qw(emid | emresults:H:max), ],
   'emtab:a' => [qw(emid | emresults:a), ],
   'emtab:a:max' => [qw(emid | emresults:a:max), ],
   'emtab:wa' => [ 'emid', '|', 'emresults:wa', ],
   'emtab:wa:max' => [ 'emid', '|', 'emresults:wa:max', ],
   'emtab:p' => [ qw(emid | emresults:p), ],
   'emtab:wp' => [ 'emid', '|', 'emresults:wp', ],
   'emtab:emi' => [ 'emid', '|', 'emresults:emi', ],
   'emresults:mp'  => [qw(*:pr:g pr:g e+mp:pr:g(title=e+mp) | *:pr:t pr:t e+mp:pr:t(title=e+mp))],

   'emresults'     => [
		       qw(*:pr:g pr:g e+mp:pr:g(title=e+mp) e+pemi:pr:g(title=e+pemi)), '|',
		       qw(*:pr:t pr:t e+mp:pr:t(title=e+mp) e+pemi:pr:t(title=e+pemi))
		      ],
   'emresults:max'     => [
			   qw(*:pr:g pr:g e+pemi:pr:g(title=e+pemi) e-max:pr:g:emi(title=e-max)), '|',
			   qw(*:pr:t pr:t e+pemi:pr:t(title=e+pemi) e-max:pr:t:emi(title=e-max))
			  ],
   'emresults:Max'     => [
			  '*:pr:g',  'pr:g', 'e-max:pr:g:stg(title=e-Max)', '|',
			  '+:rc:g', 'rc:g', 'e-max:rc:g:stg(title=e-Max)', '|',
			  '~:F:g', 'F:g', 'e-max:F:g:stg(title=e-Max)',
			  ],
   'emresults:MAX'     => [
			  '*:pr:g',  'pr:g', 'e-max:pr:g:corpus(title=e-MAX)', '|',
			  '+:rc:g', 'rc:g', 'e-max:rc:g:corpus(title=e-MAX)', '|',
			  '~:F:g', 'F:g', 'e-max:F:g:corpus(title=e-MAX)',
			  ],

   'emresults:emi' => [qw(*:pr:g pr:g e+pemi:pr:g | *:pr:t pr:t e+pemi:pr:t)],

   'emresults:H'     => [
		       qw(*:Hpr:g Hpr:g e+mp:Hpr:g(title=e+mp) e+pemi:Hpr:g(title=e+pemi)), '|',
		       qw(*:Hpr:t Hpr:t e+mp:Hpr:t(title=e+mp) e+pemi:Hpr:t(title=e+pemi))
		      ],
   'emresults:H:max'     => [
			   qw(*:Hpr:g Hpr:g e+pemi:Hpr:g(title=e+pemi) e-max:Hpr:g:emi(title=e-max)), '|',
			   qw(*:Hpr:t Hpr:t e+pemi:Hpr:t(title=e+pemi) e-max:Hpr:t:emi(title=e-max))
			  ],

   'emresults:a'   => [
		       qw(*:apr:g apr:g +:arc:g arc:g ~:aF:g aF:g), '|',
		       qw(*:apr:t apr:t +:arc:t arc:t ~:aF:t aF:t),
		      ],
   'emresults:a:max' => [
			 qw(*:apr:g apr:g e+pemi:apr:g(title=e+pemi) e-max:apr:g:emi(title=e-max)), '|',
			 qw(*:apr:t apr:t e+pemi:apr:t(title=e+pemi) e-max:apr:t:emi(title=e-max)),
			],
   'emresults:wa'   => [
			qw(*:wapr:g wapr:g +:warc:g warc:g ~:waF:g waF:g), '|',
			qw(*:wapr:t wapr:t +:warc:t warc:t ~:waF:t waF:t),
		       ],
   'emresults:wa:max' => [
			 qw(*:wapr:g wapr:g e+pemi:wapr:g(title=e+pemi) e-max:wapr:g:emi(title=e-max)), '|',
			 qw(*:wapr:t wapr:t e+pemi:wapr:t(title=e+pemi) e-max:wapr:t:emi(title=e-max)),
			],
   'emresults:wa:Max' => [
			  '*:wapr:g',  'wapr:g', 'e-max:wapr:g:stg(title=e-Max)', '|',
			  '+:warc:g', 'warc:g', 'e-max:warc:g:stg(title=e-Max)', '|',
			  '~:waF:g', 'waF:g', 'e-max:waF:g:stg(title=e-Max)',
			],
   'emresults:wa:MAX' => [
			  '*:wapr:g',  'wapr:g', 'e-max:wapr:g:corpus(title=e-MAX)', '|',
			  '+:warc:g', 'warc:g', 'e-max:warc:g:corpus(title=e-MAX)', '|',
			  '~:waF:g', 'waF:g', 'e-max:waF:g:corpus(title=e-MAX)',
			],

   'emresults:p'   => [
		       qw(*:ppr:g ppr:g +:prc:g prc:g ~:pF:g pF:g), '|',
		       qw(*:ppr:t ppr:t +:prc:t prc:t ~:pF:t pF:t),
		      ],
   'emresults:p:max' => [
			 '*:ppr:g', 'wppr:g', 'e+pemi:ppr:g(title=e+pemi)', 'e-max:ppr:g:emi(title=e-max)', '|',
			 '*:ppr:t', 'wppr:t', 'e+pemi:ppr:t(title=e+pemi)', 'e-max:ppr:t:emi(title=e-max)',
			],
   'emresults:p:Max' => [
			  '*:ppr:g', 'wppr:g', 'e-max:ppr:g:stg(title=e-Max)', '|',
			  '+:prc:g', 'wprc:g', 'e-max:prc:g:stg(title=e-Max)', '|',
			  '~:pF:g',  'wpF:g',  'e-max:pF:g:stg(title=e-Max)',
			],
   'emresults:p:MAX' => [
			  '*:ppr:g', 'wppr:g', 'e-max:ppr:g:corpus(title=e-MAX)', '|',
			  '+:prc:g', 'wprc:g', 'e-max:prc:g:corpus(title=e-MAX)', '|',
			  '~:pF:g',  'wpF:g',  'e-max:pF:g:corpus(title=e-MAX)',
			],


   'emresults:wp'   => [
			'*:wppr:g', 'wppr:g', '+:wprc:g', 'wprc:g', '~:wpF:g', 'wpF:g', '|',
			'*:wppr:t', 'wppr:t', '+:wprc:t', 'wprc:t', '~:wpF:t', 'wpF:t',
		       ],
   'emresults:wp:max' => [
			  '*:wppr:g', 'wppr:g', 'e+pemi:wppr:g(title=e+pemi)', 'e-max:wppr:g:emi(title=e-max)', '|',
			  '*:wppr:t', 'wppr:t', 'e+pemi:wppr:t(title=e+pemi)', 'e-max:wppr:t:emi(title=e-max)',
			],
   'emresults:wp:Max' => [
			  '*:wppr:g', 'wppr:g', 'e-max:wppr:g:stg(title=e-Max)', '|',
			  '+:wprc:g', 'wprc:g', 'e-max:wprc:g:stg(title=e-Max)', '|',
			  '~:wpF:g',  'wpF:g',  'e-max:wpF:g:stg(title=e-Max)',
			],
   'emresults:wp:MAX' => [
			  '*:wppr:g', 'wppr:g', 'e-max:wppr:g:corpus(title=e-MAX)', '|',
			  '+:wprc:g', 'wprc:g', 'e-max:wprc:g:corpus(title=e-MAX)', '|',
			  '~:wpF:g',  'wpF:g',  'e-max:wpF:g:corpus(title=e-MAX)',
			],



   'res:prF:g' => [qw(*:pr:g pr:g e+mp:pr:g | +:rc:g rc:g e+mp:rc:g | ~:F:g F:g e+mp:F:g)],
   'res:prF:t' => [qw(*:pr:t pr:t | +:rc:t rc:t | ~:F:t F:t)],
   'res:aprF:g' => [qw(*:apr:g apr:g | +:arc:g arc:g | ~:aF:g aF:g)],
   'res:aprF:t' => [qw(*:apr:t apr:t | +:arc:t arc:t | ~:aF:t aF:t)],


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
   lrwhich => { path=>[qw(xvars lrwhich)], title=>'lrw', },
   tcd    => { path=>[qw(xvars tcd)], title=>'tcd', },
   tcm    => { path=>[qw(xvars tcm)], title=>'tcm', },
   tccd   => { path=>[qw(xvars tccd)], title=>'tccd', },
   tccm   => { path=>[qw(xvars tccm)], title=>'tccm', },

   ##-- Corpus
   corpus => { path=>[qw(xvars icbase)], n=>0, fmt=>'auto', title=>'corpus',
	       alt=>[
		     qw(xvars->icorpus xvars->icbase xvars->tcorpus),
		     qw(icorpus icbase tcorpus)
		    ],
	       hr=>'micro',
	       condense=>1,
	     },
   lg=>'lang',
   lang   => { path=>[qw(xvars icbase)], n=>0, fmt=>'auto', title=>'lg',
	       eval=>'$_ =~ /^[uz]/ ? "de" : "en"',
	       hr=>'micro',
	       condense=>1,
	     },

   ##-- MetaProfile: numeric indices
   'stg' => 'stage',
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
   'ntgsk' => 'nT_k',

   'nB'   => { path=>[qw(mpsummary nBounds)],    n=>1,  title=>'nB',   condense=>1, alt=>[qw(stage)],
	       latexTitle=>'$\mathbf{|B_k|}$',
	     },
   'nbds' => 'nB',

   'nC'   => { path=>[qw(mpsummary nClusters)],  n=>1,  title=>'nC',   condense=>1,
	       latexTitle=>'$\mathbf{|C|}$',
	     },
   'nclusters' => 'nC',

   'fT_k' => { path=>[qw(mpsummary ugk_avg)],    n=>1,  title=>'avg f(T_k)', condense=>1, alt=>[qw(stage nT)],
	       eval=>'sprintf("%.2f",$_)',
	       latexTitle=>'$\mathbf{E(f(T_k))}$',
	     },
   'tgf'  => 'fT_k',

   'mbest'=> { path=>[qw(mpsummary d2p_n)],     n=>1,  title=>'m',    condense=>0,
	       latexTitle=>'$\mathbf{m}$',
	     },
   'd2pn' => 'mbest',

   'tck' => { path=>[qw(xvars tck)], n=>1, alt=>[qw(xvars->tck)], },



   ##-------------------------------------------------
   ## Eval: Meta-(precision,recall,F,ambig-rate)

   ##-------------------------------------
   ## Eval: Meta-*: Global
   'pr:g'  => { path=>[qw(eval_global precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' pr:g'},
   'rc:g'  => { path=>[qw(eval_global recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' rc:g'},
   'F:g'   => { path=>[qw(eval_global F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' F:g' },
   'ar:g'  => { path=>[qw(eval_global arate1)],    n=>1, fmt=>'%.3f', eval=>'0+$_',  title=>' ar:g' },

   ##-------------------------------------
   ## Eval: Meta-*: Targets
   'pr:t'  => { path=>[qw(eval_targets precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' pr:t'},
   'rc:t'  => { path=>[qw(eval_targets recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' rc:t'},
   'F:t'   => { path=>[qw(eval_targets F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' F:t' },
   'ar:t'  => { path=>[qw(eval_targets arate1)],    n=>1, fmt=>'%.3f', eval=>'0+$_', title=>' ar:t' },

   ##-------------------------------------
   ## Eval: Total-*: Global
   'tpr:g'  => { path=>[qw(eval_global total_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' pr:g'},
   'trc:g'  => { path=>[qw(eval_global total_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' rc:g'},
   'tF:g'   => { path=>[qw(eval_global total_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' F:g' },

   ##-------------------------------------
   ## Eval: Total-*: Targets
   'tpr:t'  => { path=>[qw(eval_targets total_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' pr:t'},
   'trc:t'  => { path=>[qw(eval_targets total_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' rc:t'},
   'tF:t'   => { path=>[qw(eval_targets total_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' F:t' },


   ##-------------------------------------
   ## Eval: MI, H
   'mi:g'  => { path=>[qw(eval_global mi)],        n=>1, fmt=>'%.3f', eval=>'0+$_', title=>' mi:g' },
   'Hpr:g'  => { path=>[qw(eval_global H_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'Hpr:g'},
   'Hrc:g'  => { path=>[qw(eval_global H_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'Hrc:g'},
   'HF:g'   => { path=>[qw(eval_global H_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'HF:g' },
   'HI:g'   => { path=>[qw(eval_global H_I)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'HI:g' },

   'mi:t'  => { path=>[qw(eval_targets mi)],       n=>1, fmt=>'%.3f', eval=>'0+$_', title=>' mi:t' },
   'Hpr:t'  => { path=>[qw(eval_targets H_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'Hpr:t'},
   'Hrc:t'  => { path=>[qw(eval_targets H_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'Hrc:t'},
   'HF:t'   => { path=>[qw(eval_targets H_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'HF:t' },
   'HI:t'   => { path=>[qw(eval_targets H_I)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'HI:t' },


   ##-------------------------------------------------
   ## Eval: Tagwise-average (precision,recall,F,ambig-rate)

   ##-------------------------------------
   ## Eval: Tagwise-average-*: Global
   'apr:g'  => { path=>[qw(eval_global avg_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'apr:g'},
   'arc:g'  => { path=>[qw(eval_global avg_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'arc:g'},
   'aF:g'   => { path=>[qw(eval_global avg_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' aF:g' },

   ##-------------------------------------
   ## Eval: Tagwise-average-*: Targets
   'apr:t'  => { path=>[qw(eval_targets avg_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'apr:t'},
   'arc:t'  => { path=>[qw(eval_targets avg_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'arc:t'},
   'aF:t'   => { path=>[qw(eval_targets avg_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' aF:t' },

   ##-------------------------------------------------
   ## Eval: Weighted Tagwise-average (precision,recall,F)

   ##-------------------------------------
   ## Eval: Weighted Tagwise-average-*: Global
   'wapr:g'  => { path=>[qw(eval_global wavg_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'wapr:g'},
   'warc:g'  => { path=>[qw(eval_global wavg_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'warc:g'},
   'waF:g'   => { path=>[qw(eval_global wavg_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'waF:g'},

   ##-------------------------------------
   ## Eval: Weighted Tagwise-average-*: Targets
   'wapr:t'  => { path=>[qw(eval_targets wavg_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'wapr:t'},
   'warc:t'  => { path=>[qw(eval_targets wavg_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'warc:t'},
   'waF:t'   => { path=>[qw(eval_targets wavg_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'waF:t'},

   ##-------------------------------------------------
   ## Eval: Pairwise (precision,recall,F)

   ##-------------------------------------
   ## Eval: Pairwise: Global
   'ppr:g'  => { path=>[qw(eval_global pair_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'ppr:g'},
   'prc:g'  => { path=>[qw(eval_global pair_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'prc:g'},
   'pF:g'   => { path=>[qw(eval_global pair_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' pF:g'},

   ##-------------------------------------
   ## Eval: Pairwise: Targets
   'ppr:t'  => { path=>[qw(eval_targets pair_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'ppr:t'},
   'prc:t'  => { path=>[qw(eval_targets pair_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'prc:t'},
   'pF:t'   => { path=>[qw(eval_targets pair_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>' pF:t'},

   ##-------------------------------------------------
   ## Eval: Weighted Pairwise (precision,recall,F)

   ##-------------------------------------
   ## Eval: Weighted Pairwise: Global
   'wppr:g' => { path=>[qw(eval_global wpair_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'wppr:g'},
   'wprc:g' => { path=>[qw(eval_global wpair_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'wprc:g'},
   'wpF:g'  => { path=>[qw(eval_global wpair_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'wpF:g'},

   ##-------------------------------------
   ## Eval: Weighted Pairwise: Targets
   'wppr:t' => { path=>[qw(eval_targets wpair_precision)], n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'wppr:t'},
   'wprc:t' => { path=>[qw(eval_targets wpair_recall)],    n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'wprc:t'},
   'wpF:t'  => { path=>[qw(eval_targets wpair_F)],         n=>1, fmt=>'%.2f', eval=>'100*$_', title=>'wpF:t'},


   ##-------------------------------------------------
   ## Eval: Single-tag (precision,recall,F): Meta

   ##-------------------------------------
   ## Eval: Single-tag-*: Meta: Global
   'tag:pr:g' => { path=>[qw(eval_global tag2i)], n=>1, fmt=>'%.2f',
		   evalname=>'"$field->{tag}:pr:g"',
		   evaltitle=>'"$field->{tag}"',
		   eval=>'100*$_->{$field->{tag}}{meta_precision}',
		 },
   'tag:rc:g' => { path=>[qw(eval_global tag2i)], n=>1, fmt=>'%.2f',
		   evalname=>'"$field->{tag}:rc:g"',
		   evaltitle=>'"$field->{tag}"',
		   eval=>'100*$_->{$field->{tag}}{meta_recall}',
		  },
   'tag:F:g'  => { path=>[qw(eval_global tag2i)], n=>1, fmt=>'%.2f',
		   evalname=>'"$field->{tag}:F:g"',
		   evaltitle=>'"$field->{tag}"',
		   eval=>'100*$_->{$field->{tag}}{meta_F}',
		  },

   ##-------------------------------------
   ## Eval: Single-tag-*: Meta: Targets
   'tag:pr:t' => { path=>[qw(eval_targets tag2i)], n=>1, fmt=>'%.2f',
		    evalname=>'"$field->{tag}:pr:t"',
		    evaltitle=>'"$field->{tag}"',
		    eval=>'100*$_->{$field->{tag}}{meta_precision}',
		  },
   'tag:rc:t' => { path=>[qw(eval_targets tag2i)], n=>1, fmt=>'%.2f',
		    evalname=>'"$field->{tag}:rc:t"',
		    evaltitle=>'"$field->{tag}"',
		    eval=>'100*$_->{$field->{tag}}{meta_recall}',
		  },
   'tag:F:t'  => { path=>[qw(eval_targets tag2i)], n=>1, fmt=>'%.2f',
		    evalname=>'"$field->{tag}:F:t"',
		    evaltitle=>'"$field->{tag}"',
		    eval=>'100*$_->{$field->{tag}}{meta_F}',
		  },

   ##-------------------------------------------------
   ## Eval: Single-tag (precision,recall,F): Average

   ##-------------------------------------
   ## Eval: Single-tag-*: Average: Global
   'tag:apr:g' => { path=>[qw(eval_global tag2i)], n=>1, fmt=>'%.2f',
		    evalname=>'"$field->{tag}:apr:g"',
		    evaltitle=>'"$field->{tag}"',
		    eval=>'100*$_->{$field->{tag}}{avg_precision}',
		},
   'tag:arc:g' => { path=>[qw(eval_global tag2i)], n=>1, fmt=>'%.2f',
		    evalname=>'"$field->{tag}:arc:g"',
		    evaltitle=>'"$field->{tag}"',
		    eval=>'100*$_->{$field->{tag}}{avg_recall}',
		  },
   'tag:aF:g'  => { path=>[qw(eval_global tag2i)], n=>1, fmt=>'%.2f',
		    evalname=>'"$field->{tag}:aF:g"',
		    evaltitle=>'"$field->{tag}"',
		    eval=>'100*$_->{$field->{tag}}{avg_F}',
		  },

   ##-------------------------------------
   ## Eval: Single-tag-*: Average: Targets
   'tag:apr:t' => { path=>[qw(eval_targets tag2i)], n=>1, fmt=>'%.2f',
		    evalname=>'"$field->{tag}:apr:t"',
		    evaltitle=>'"$field->{tag}"',
		    eval=>'100*$_->{$field->{tag}}{avg_precision}',
		  },
   'tag:arc:t' => { path=>[qw(eval_targets tag2i)], n=>1, fmt=>'%.2f',
		    evalname=>'"$field->{tag}:arc:t"',
		    evaltitle=>'"$field->{tag}"',
		    eval=>'100*$_->{$field->{tag}}{avg_recall}',
		  },
   'tag:aF:t'  => { path=>[qw(eval_targets tag2i)], n=>1, fmt=>'%.2f',
		    evalname=>'"$field->{tag}:aF:t"',
		    evaltitle=>'"$field->{tag}"',
		    eval=>'100*$_->{$field->{tag}}{avg_F}',
		  },


   ##-------------------------------------------------
   ## Eval: All single-tags (precision,recall,F,ambig-rate)

   ##-------------------------------------
   ## Eval: All single-tag-*: Meta: Global
   'tags:pr:g' => { expand_code=>\&_expand_tags, _tag_field=>'tag:pr:g', _tag_var=>'tag', },
   'tags:rc:g' => { expand_code=>\&_expand_tags, _tag_field=>'tag:rc:g', _tag_var=>'tag', },
   'tags:F:g'  => { expand_code=>\&_expand_tags, _tag_field=>'tag:F:g', _tag_var=>'tag', },

   ##-------------------------------------
   ## Eval: All single-tag-*: Meta. Targets
   'tags:pr:t' => { expand_code=>\&_expand_tags, _tag_field=>'tag:pr:t', _tag_var=>'tag', },
   'tags:rc:t' => { expand_code=>\&_expand_tags, _tag_field=>'tag:rc:t', _tag_var=>'tag', },
   'tags:F:t'  => { expand_code=>\&_expand_tags, _tag_field=>'tag:F:t', _tag_var=>'tag', },


   ##-------------------------------------
   ## Eval: All single-tag-*: Average: Global
   'tags:apr:g' => { expand_code=>\&_expand_tags, _tag_field=>'tag:apr:g', _tag_var=>'tag', },
   'tags:arc:g' => { expand_code=>\&_expand_tags, _tag_field=>'tag:arc:g', _tag_var=>'tag', },
   'tags:aF:g'  => { expand_code=>\&_expand_tags, _tag_field=>'tag:aF:g', _tag_var=>'tag', },

   ##-------------------------------------
   ## Eval: All single-tag-*: Average: Targets
   'tags:apr:t' => { expand_code=>\&_expand_tags, _tag_field=>'tag:apr:t', _tag_var=>'tag', },
   'tags:arc:t' => { expand_code=>\&_expand_tags, _tag_field=>'tag:arc:t', _tag_var=>'tag', },
   'tags:aF:t'  => { expand_code=>\&_expand_tags, _tag_field=>'tag:aF:t', _tag_var=>'tag', },

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
   'max' => (our $_max_field =
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

   ##-- mark max: latex
   (map { _markmax_fields_latex($_) } @_eval_base_fields),

   ##-------------------------------------
   ## Error difference (vs. best): errdiff
   ##  errdiff { of=>$of_field, for=>$for_fields, vs=>$base_field, ... }
   'errdiff' => (our $_errdiff_field =
		{ expand_code =>\&_expand_errdiff,
		  of          =>'pr:g',        ##-- target field: to be set by user or alias
		  vs          =>'max:pr:g',
		  evalname    =>
		  '"errdiff(of=$field->{of},vs=$field->{vs})"',
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
  my $emax = $ediff_field->{errmax};

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
	 );
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
