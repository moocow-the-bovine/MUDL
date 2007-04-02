##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::PdlProfile::ITagEval.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile:
##    induced tagger evaluation via pdl
##======================================================================

package MUDL::Corpus::Profile::PdlProfile::ITagEval;
use MUDL::Corpus::Profile::PdlProfile;
use MUDL::PdlDist;
use MUDL::PdlDist::Sparse2d;
use MUDL::Object;
use PDL;
use PDL::CCS;
use PDL::Ngrams;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile::PdlProfile); #)

##======================================================================
## Constructor
##   + $obj = $class_or_obj->new(%args)
##
##   + %args for Profile::PdlProfile:
##     buffer => $corpus_buffer_pdltt,  ##-- buffer for induced tags (...,$tag1,...)
##     writer => $bufwriter,            ##-- buffer writer
##
##   + %args for Profile::PdlProfile:ITagEval:
##
##     ##-- I/O
##     gbuffer => $gbuffer,    ##-- Buffer::PdlTT containing gold-standard ($text,...,$tag2)
##
##     ##-- Selection & Behavior
##     tag1a => $tag1attr,     ##-- attribute index for 'tag1': default: 0
##     tag2a => $tag2attr,     ##-- attribute index for 'tag2': default: 1
##     targets => $enum,       ##-- enum only wrt these targets (default=none~all tokens)
##     targetsa => $attr,      ##-- attribute in $gbuffer corresponding to 'targets' (default=0 (text))
##     unknown1=>$unk1,        ##-- unknown tag1 string value (default='@UNKNOWN')
##
##     ##-- Summary formatting options
##     label1 => $label1,      ##-- for summary, native save
##     label2 => $label2,      ##-- for summary, native save
##
##     ##-- basic runtime data (new)
##     ntoks   => $ntokens,     ##-- total number of tokens processed
##     jpdist  => $sparse2d,    ##-- MUDL::PdlDist::Sparse2d: <tag1,tag2> => f(tag1 && tag2)
##     tag1txt => $sparse2d,    ##-- MUDL::PdlDist::Sparse2d: <tag1,txt>  => f(txt  && tag1)
##     tag2txt => $sparse2d,    ##-- MUDL::PdlDist::Sparse2d: <tag2,txt>  => f(txt  && tag2)
##
##     ##-- summary data: target restriction
##     token_coverage => $float, ##-- 0 <= $float <= 1: percentage of tokens covered by targets
##     type_coverage  => $float, ##-- 0 <= $float <= 1: percentage of types covered by targets
##
##     ##-- ambiguity data
##     ntypes => $n,           ##-- number of word types
##     nanals1 => $n1,         ##-- total number of (type,tag1) pairs
##     nanals2 => $n2,         ##-- total number of (type,tag2) pairs
##
##     ##-- summary data: basic
##     ntags1 => $n1,          ##-- number of distinct (tag1)s encountered
##     ntags2 => $n2,          ##-- number of distinct (tag2)s encountered
##
##     ##-- summary data: token-wise (meta-tagging)
##     meta_precision=>$prec,       ##-- p(best(tag2|tag1)|tag1)
##     meta_recall=>$recall,        ##-- p(best(tag1|tag2)|tag2)
##     meta_F=>$F,
##
##     ##-- summary data: adjusted (adjusted meta-tagging)
##     ameta_precision=>$pr,         ##-- adjusted meta-precision
##     ameta_recall=>$rc,            ##-- adjusted meta-recall
##     ameta_F=>$F,
##
##     ##-- summary data: average (Sch"utze-style)
##     avg_precision=>$pr,
##     avg_recall=>$rc,
##     avg_F=>$F,
##
##     ##-- summary data: weighted average (pseudo Sch"utze-style)
##     wavg_precision=>$pr,
##     wavg_recall=>$rc,
##     wavg_F=>$F,
##
##     ##-- summary data: pair-wise (Schulte-im-Walde style, following Hatzivassiloglou & McKeown (1993))
##     pair_precision=>$pr,
##     pair_recall=>$rc,
##     pair_F=>$F,
##
##     ##-- summary data: weighted pair-wise
##     wpair_precision=>$pr,
##     wpair_recall=>$rc,
##     wpair_F=>$F,
##
##     ##-- summary data: information-theoretic
##     ##    + where H_u(X) = entropy contribution of '@UNKNOWN' tag1 to H(X)
##     mi=>$mi_bits,       ##-- I(tag1;tag2) [HACKED modulo unknowns]
##     H_precision=>$pr,   ##-- (H(2) - ($H(2|1) + H(2  |1=u))) / H(2)
##     H_recall=>$rc,      ##-- (H(1) - ($H(1|2) + H(1=u|2  ))) / H(1)
##     H_I=>$I,            ##-- (I(1;2) - I(1=u;2))             / H(1,2)
##     H_F=>$F,            ##-- F(H_pr,H_rc)
##
##     ##-- Summary data: Rand
##     #Rand => $rand_index,        ##-- DISABLED
##     RandA => $adjust_rand_index, ##-- Adjusted Rand Index (~ F)
##
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new(
			       ##-- I/O
			       gbuffer=>MUDL::Corpus::Buffer::PdlTT->new(),

			       ##-- Selection & Behavior
			       tag1a=>0,
			       tag2a=>1,
			       targets=>undef,
			       targetsa=>0,
			       unknown1=>'@UNKNOWN',

			       ##-- Summary formatting options
			       label1=>'(tag1)',
			       label2=>'(tag2)',

			       ##-- basic runtime data
			       ntoks=>0,

			       ##-- user args
			       %args,
			      );

  return $self;
}

##======================================================================
## Errors

## undef = $eval->error(@msg);
##  + confesses()s and dies
sub error {
  my ($eval,@msg) = @_;
  confess(ref($eval), ": ", @msg, "\n> ");
}

##======================================================================
## Corpus::Profile::PdlProfile API

## undef = $profile->finishPdlProfile(%args)
##  + perform pdl-sensitive finishing actions
##  + called by default finish() method
##    - when this method is called, the buffer (if any) has been filled and pdl-ized
##    - after this completes, the buffer (if any) is deleted
sub finishPdlProfile {
  my $eval = shift;

  ##----------------------------------------------------
  ## rudimentary sanity checks
  my ($buf1,$buf2, $tag1pdl,$tag2pdl,$txtpdl);
  $eval->error("no induced-tag buffer") if (!defined($buf1=$eval->{buffer}));
  $eval->error("no induced-tag pdl")    if (!defined($tag1pdl=$buf1->{pdls}[$eval->{tag1a}]));
  $eval->error("no gold-standard text+tag buffer")  if (!defined($buf2=$eval->{gbuffer}));
  $eval->error("no gold-standard tag pdl")    if (!defined($tag2pdl=$buf2->{pdls}[$eval->{tag2a}]));
  $eval->error("no gold-standard text pdl")   if (!defined($txtpdl=$buf2->{pdls}[0]));
  $eval->error("corpus length mismatch") if ($tag1pdl->nelem != $tag2pdl->nelem);

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## enums
  my $tag1enum = $eval->{tag1enum} = $buf1->{enums}[$eval->{tag1a}];
  my $tag2enum = $eval->{tag2enum} = $buf2->{enums}[$eval->{tag2a}];
  my $txtenum  = $eval->{txtenum} = $buf2->{enums}[0];
  delete($eval->{txtenum});       ##-- don't save text enum

  my $Ntags1   = $tag1enum->size; ##-- $Ntags1:  number of (tag1) types (including unknowns)
  my $Ntags2   = $tag2enum->size; ##-- $Ntags2:  number of (tag2) types
  my $Ntags1k  = scalar(          ##-- $Ntags1k: number of (tag1) types (EXCLUDING unknowns)
			grep {
			  defined($_) && (!defined($eval->{unknown1}) || $_ ne $eval->{unknown1})
			} @{$tag1enum->{id2sym}}
		       );
  my $Ntxt     = $txtenum->size;  ##-- number of text types in the CORPUS
  my $Ntxtk    = $Ntxt;           ##-- number of KNOWN text types in the corpus

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## restrict to targets (maybe)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (defined($eval->{targets})) {
    my $tgattr  = $eval->{targetsa}||0;
    my $tgpdl2  = $buf2->{pdls}[$tgattr];
    $eval->error("no gold-standard pdl for restriction to targets") if (!defined($tgpdl2));
    my $tgenum2 = $buf2->{enums}[$tgattr];

    my $tgs_to_enum2 = $eval->{targets}->xlatePdlTo($tgenum2, badval=>-1);
    $tgs_to_enum2    = $tgs_to_enum2->where($tgs_to_enum2 >= 0);
    my $tgs_mask2    = zeroes(byte,$tgenum2->size);
    $tgs_mask2->index($tgs_to_enum2) .= 1;
    my $tg_token_which = $tgs_mask2->index($tgpdl2)->which;

    $tag1pdl = $tag1pdl->index($tg_token_which);
    $tag2pdl = $tag2pdl->index($tg_token_which);
    $txtpdl  = $txtpdl->index($tg_token_which);
    $Ntxtk   = $tgs_to_enum2->nelem;             ##-- number of known text types which were found

    ##-- target coverage info
    $eval->{token_coverage} = $tg_token_which->nelem   / $tgpdl2->nelem;
    $eval->{type_coverage}  = $tgs_mask2->which->nelem / $tgs_mask2->nelem;
  } else {
    ##-- target coverage info
    $eval->{token_coverage} = 1.0;
    $eval->{type_coverage}  = 1.0;
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## get basic data
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  $eval->{ntoks}  = $tag1pdl->nelem;

  ##-- basic data: jpdist: <tag1,tag2> -> f(tag1 && tag2)
  my ($vvecs,$vfreq,$vwhich);
  ($vfreq,$vwhich) = ng_cofreq($tag1pdl->cat($tag2pdl)->xchg(0,1), norotate=>1);

  my $jpdist = $eval->{jpdist} =
    MUDL::PdlDist::Sparse2d->new(dims=>[$Ntags1,$Ntags2],
				 enum=>MUDL::Enum::Nary->new(nfields=>2,enums=>[$tag1enum,$tag2enum]),
				);
  my @tag12_ccs = @$jpdist{qw(ptr rowids nzvals)} = ccsencode_i2d($vwhich->xchg(0,1)->dog, $vfreq, $Ntags1);

  ##-- dense joint matrix, handy here, but not stored
  my $jpdl                 = zeroes(long, $Ntags1,$Ntags2);
  $jpdl->indexND($vwhich) .= $vfreq;
  my $jpdld                = $jpdl->convert(double); ##-- as double

  ##-- basic data: txttag1: <tag1,text> -> f(tag1 && text) [sparse]
  $vvecs = zeroes($txtpdl->type, 2, $txtpdl->nelem);
  $vvecs->slice("(0),") .= $tag1pdl;
  $vvecs->slice("(1),") .= $txtpdl;
  ($vfreq,$vwhich) = ng_cofreq($vvecs, norotate=>1);
  my $tag1txt = $eval->{tag1txt} =
    MUDL::PdlDist::Sparse2d->new(dims=>[$Ntags1, $Ntxt],
				 enum=>MUDL::Enum::Nary->new(nfields=>2,enums=>[$tag1enum,undef]),
				 ##-- don't store the text enum!
				);
  my @tag1txt_ccs = @$tag1txt{qw(ptr rowids nzvals)} = ccsencode_i2d($vwhich->xchg(0,1)->dog, $vfreq, $Ntags1);

  ##-- basic data: txttag2: <tag2,text> -> f(tag2 && text) [sparse]
  $vvecs = zeroes($txtpdl->type, 2, $txtpdl->nelem);
  $vvecs->slice("(0),") .= $tag2pdl;
  $vvecs->slice("(1),") .= $txtpdl;
  ($vfreq,$vwhich) = ng_cofreq($vvecs, norotate=>1);
  my $tag2txt = $eval->{tag2txt} =
    MUDL::PdlDist::Sparse2d->new(dims=>[$Ntags2, $Ntxt],
				 enum=>MUDL::Enum::Nary->new(nfields=>2,enums=>[$tag2enum,undef]),
				 ##-- don't store the text enum!
				);
  my @tag2txt_ccs = @$tag2txt{qw(ptr rowids nzvals)} = ccsencode_i2d($vwhich->xchg(0,1)->dog, $vfreq, $Ntags2);


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Common variables
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my ($pr,$rc,$F);
  my $tag1syms = $tag1enum->{id2sym};
  my $tag2syms = $tag2enum->{id2sym};

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision, recall
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##-- allocate tag1-, tag2- info structures
  ##   %info_tag =
  ##   (
  ##    ##-- word-type information
  ##    nwtypes => $ntypes,                      ##-- number of word types occurring with this tag at least once
  ##    wtype_density => $ntypes/$total_types,   ##-- ...normalized to [0,1]
  ##    nwtypesi => $ntypesi,                    ##-- number of word types ocurring with this tag
  ##                                             ##   and some alternate tag mapped to it at least once
  ##    wtypei_density => $ntypesi/$total_types, ##-- ...normalized to [0,1]
  ##
  ##    ##-- best-map information
  ##    freq=>$f_tag,
  ##    fbest => max(f($tag,$othertag)),                 ##-- best match frequency
  ##    tbest => arg_{$othertag} max(f($tag,$othertag)), ##-- best match
  ##
  ##    ##-- Summary: structure
  ##    nbesti => $number_of_best_othertags_matching_this_tag, ##-- via inverse match
  ##
  ##    ##-- Summary: meta-*
  ##    meta_ncor12    => $ncorrect_tag1_to_tag2,
  ##    meta_ninc12    => $nincorrect_tag1_to_tag2,
  ##    meta_ncor21    => $ncorrect_tag2_to_tag1,
  ##    meta_ninc21    => $nincorrect_tag2_to_tag1,
  ##    nunknown       => $nunknowns,                    ##-- by tag2, distributed among best tag1s
  ##
  ##    meta_precision => $meta_precision,
  ##    meta_recall    => $meta_recall,
  ##    meta_F         => $meta_F,
  ##   )
  my $tag1i  = $eval->{tag1i} = {}; ##-- $tag1 => \%info_tag1
  my $tag1ia = [];
  my $tag2i  = $eval->{tag2i} = {}; ##-- $tag2 => \%info_tag2
  my $tag2ia = [];

  ##-- get unigram (tag1),(tag2) distributions
  my $tag1d   = MUDL::PdlDist->new(enum=>$tag1enum, pdl=>ccssumovert(@tag12_ccs));
  my $tag1dp  = $tag1d->{pdl};
  my $tag2d   = MUDL::PdlDist->new(enum=>$tag2enum, pdl=>ccssumover (@tag12_ccs, $Ntags2));
  my $tag2dp  = $tag2d->{pdl};
  my $ftotal  = $tag1dp->sumover;         ##-- total frequency, including unknowns
  my $ftotald = $ftotal->convert(double); ##-- ... as double

  ##-- get 'known' mask for tag1
  my $unkid     = $tag1enum->{sym2id}{$eval->{unknown1}};
  my $tag1kmask = ones(byte,$Ntags1);
  $tag1kmask->set($unkid, 0);

  ##-- get probabilities
  my $tag1dpd   = $tag1dp->convert(double); ##-- tag1 freq, as double
  my $tag2dpd   = $tag2dp->convert(double); ##-- tag2 freq, as double
  my $tag1prob  = $tag1dpd / $ftotald;      ##-- tag1 probabilities
  #my $tag1kprob = ($tag1dpd*$tag1kmask) / ($ftotal-$tag1dp->at($unkid));
  my $tag2prob  = $tag2dpd / $ftotald;      ##-- tag2 probabilities

  ##-- get best-match maps
  my $tag12best  = $jpdl->xchg(0,1)->maximum_ind;                     ##-- $tag1i => $best_tag2i
  my $tag21best  = ($jpdl*$tag1kmask)->maximum_ind;                   ##-- $tag2i => $best_tag1i
  my $tag12bestf = $jpdl->index2d(sequence(long,$Ntags1),$tag12best); ##-- $tag1i => f(best_tag2|tag1)
  my $tag21bestf = $jpdl->index2d($tag21best,sequence(long,$Ntags2)); ##-- $tag2i => f(best_tag1|tag2)

  ##-- get fallback values (for zero frequencies?); no unknowns
  my $fallback1  = ($tag1dp*$tag1kmask)->maximum_ind;
  my $fallback2  = $tag2dp->maximum_ind;
  $tag12best->where($tag12bestf==0) .= $fallback2;
  $tag21best->where($tag21bestf==0) .= $fallback1;

  ##-- get best-match masks
  my $isbest12   = zeroes(byte,$Ntags1,$Ntags2);               ##-- <tag1,tag2> =>  I($tag2==best(tag2|tag1))
  $isbest12->index2d(sequence(long,$Ntags1), $tag12best) .= 1;
  $isbest12     *= $tag1kmask;                                 ##-- no "best" match for 'unknown'
  my $nbest2i    = $isbest12->convert(long)->sumover;          ##-- $tag2 => |{tag1 | tag2==best(tag2|tag1)}|

  my $isbest21   = zeroes(ushort,$Ntags2,$Ntags1);             ##-- <tag2,tag1> =>  I($tag1==best(tag1|tag2))
  $isbest21->index2d(sequence(long,$Ntags2), $tag21best) .= 1;
  my $nbest1i    = $isbest21->convert(long)->sumover;          ##-- $tag1 => |{tag2 | tag1==best(tag1|tag2)}|

  ##-- get number of unknowns
  my $nunk2 = $jpdl->slice("($unkid),:");

  ##-- f(u,tag1) = \sum_{tag2} p(u,tag1,tag2)    * N                 ##-- ML, marginals
  ##             = \sum_{tag2} p(u|tag1,tag2)    * p(tag1,tag2) * N  ##-- assume u !<- tag1 | tag2
  ##             = \sum_{tag2} p(u|     tag2)    * f(tag1,tag2)      ##-- ML
  ##             = \sum_{tag2} f(u,tag2)/f(tag2) * f(tag1,tag2)      ##-- ML
  my $nunk1 = ($nunk2->convert(double)/$tag2dp * $jpdl->xchg(0,1))->sumover;
  #$nunk1->set($unkid,0);                                             ##-- chop out 'unknowns'
  #$nunk1  = ($nunk1/$nunk1->sumover * $nunk2->sumover);

  ##-- initialize: tag1i
  foreach (0..$#$tag1syms) {
    #next if ($_==$unkid);
    $tag1i->{$tag1syms->[$_]} = $tag1ia->[$_] = {};
  }
  pdls2info($tag1ia,
	    freq=>$tag1dp,
	    tbest=>[ @$tag2syms[$tag12best->list] ],
	    fbest=>$tag12bestf,
	    nbesti=>$nbest1i,
	    nunknown=>$nunk1,
	   );

  ##-- initialize: tag2i
  foreach (0..$#$tag2syms) {
    $tag2i->{$tag2syms->[$_]} = $tag2ia->[$_] = {};
  }
  pdls2info($tag2ia,
	    freq=>$tag2dp,
	    tbest=>[ @$tag1syms[$tag21best->list] ],
	    fbest=>$tag21bestf,
	    nbesti=>$nbest2i,
	    nunknown=>$nunk2,
	   );

  ##-- meta-evaluators: meta_(ncor|ninc)(12|21)_by(1|2)
  my $meta_ncor12_by1 = $tag12bestf * $tag1kmask;
  my $meta_ninc12_by1 = $tag1dp - $meta_ncor12_by1; # + $nunk1;
  #my $meta_ncor12_by2 = ($jpdl * $isbest12 * $tag1kmask)->sumover; ##-- don't need to mask if we tweak $isbest12
  my $meta_ncor12_by2 = ($jpdl * $isbest12 )->sumover;
  my $meta_ninc12_by2 = $tag2dp - $meta_ncor12_by2; # + $nunk2;

  my $meta_ncor21_by2 = $tag21bestf;
  my $meta_ninc21_by2 = $tag2dp - $meta_ncor21_by2;
  my $meta_ncor21_by1 = ($jpdl->xchg(0,1) * $isbest21)->sumover;
  my $meta_ninc21_by1 = $tag1dp - $meta_ncor21_by1; # + $nunk1;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision,recall: get breakdown precision, recall
  my $meta_pr_by1 = fracpdl($meta_ncor12_by1, $meta_ncor12_by1+$meta_ninc12_by1);
  my $meta_rc_by1 = fracpdl($meta_ncor21_by1, $meta_ncor21_by1+$meta_ninc21_by1);
  my $meta_F_by1  = pr2Fpdl($meta_pr_by1, $meta_rc_by1);

  my $meta_pr_by2 = fracpdl($meta_ncor12_by2, $meta_ncor12_by2+$meta_ninc12_by2);
  my $meta_rc_by2 = fracpdl($meta_ncor21_by2, $meta_ncor21_by2+$meta_ninc21_by2);
  my $meta_F_by2  = pr2Fpdl($meta_pr_by2, $meta_rc_by2);

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision,recall: update tag-info hashes
  pdls2info($tag1ia, meta_precision=>$meta_pr_by1, meta_recall=>$meta_rc_by1, meta_F=>$meta_F_by1);
  pdls2info($tag2ia, meta_precision=>$meta_pr_by2, meta_recall=>$meta_rc_by2, meta_F=>$meta_F_by2);

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision,recall: get totals
  $pr = $eval->{meta_precision} = fracpdl($meta_ncor12_by1->sumover, $ftotald)->sclr;
  $rc = $eval->{meta_recall}    = fracpdl($meta_ncor21_by1->sumover, $ftotald)->sclr;
  $F  = $eval->{meta_F}         = pr2F($pr,$rc);

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## adjusted meta-tagging precision, recall
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $Epr = frac(1,$Ntags2);
  my $Erc = frac(1,$Ntags1);
  my $ameta_pr = frac(($pr-$Epr), (1.0-$Epr));
  my $ameta_rc = frac(($rc-$Erc), (1.0-$Erc));
  @$eval{qw(ameta_precision ameta_recall ameta_F)} = ($ameta_pr, $ameta_rc, pr2F($ameta_pr,$ameta_rc));

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## tag-wise precision, recall (Sch"utze-style)
  ##  + adds \%tag2i values for $tag2:
  ##     avg_ncor     =>$avg_ncorrect,
  ##     avg_ncor     =>$avg_nincorrect,
  ##     avg_precision=>$avg_pr,
  ##     avg_recall   =>$avg_recall,
  ##     avg_F        =>$avg_F,
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $avg_tp_by2 = $meta_ncor12_by2;                               ## | U(best^{-1}_{C|G}(g)) \cap g |
  my $avg_fn_by2 = $meta_ninc12_by2;                               ## | g - U(best^{-1}_{C|G}(g)) |
  #my $avg_fn_by2 = $tag2dp - $avg_tp_by2;                         ## equivalent
  #my $avg_fn_by2 = ($jpdl * !$isbest12)->sumover;                 ## equivalent
  ##--
  my $avg_fp_by2 = (($tag1dp-$tag12bestf) * $isbest12)->sumover;   ## | U(best^{-1}_{C|G}(g)) - g |
  #my $avg_fp_by2 = (($tag1dp-($jpdl*$ibest12)->xchg(0,1)->sumover) ## equivalent
  #                  * $ibest12)->sumover;
  $avg_fp_by2   += $nunk2;                                         ##-- HACK: unknowns are always wrong...

  my $avg_pr_by2 = fracpdl($avg_tp_by2, $avg_tp_by2+$avg_fp_by2);
  my $avg_rc_by2 = fracpdl($avg_tp_by2, $avg_tp_by2+$avg_fn_by2);
  my $avg_F_by2  = pr2Fpdl($avg_pr_by2, $avg_rc_by2);

  ##-- set tag-wise & global values
  pdls2info($tag2ia, avg_precision=>$avg_pr_by2, avg_recall=>$avg_rc_by2, avg_F=>$avg_F_by2);
  my $avg_pr = $avg_pr_by2->avg;
  my $avg_rc = $avg_rc_by2->avg;
  @$eval{qw(avg_precision avg_recall avg_F)} = ($avg_pr,$avg_rc, pr2F($avg_pr,$avg_rc));

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## weighted tag-wise precision, recall (pseudo-Sch"utze-style)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $wavg_pr = sum($tag2prob * $avg_pr_by2);
  my $wavg_rc = sum($tag2prob * $avg_rc_by2);
  @$eval{qw(wavg_precision wavg_recall wavg_F)} = ($wavg_pr,$wavg_rc, pr2F($wavg_pr,$wavg_rc));

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## pair-wise precision, recall
  ##  + Schulte im Walde, following Hatzivassiloglou & McKeown (1993)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $pair_tp_2d = $jpdld * ($jpdld-1) * $tag1kmask; ## <c,g> => \npairs{|c \cap g - u|}
  my $pair_tp    = sum($pair_tp_2d);                 ## \sum_c \sum_g \npairs{|c \cap g - u|}
  ##
  ##-- using fp,fn [actually kind of inefficient]
  #my $pair_fp = sum($tag1dp * ($tag1dp-1)) - $pair_tp;  ## (\sum_c \npairs{|c|}) - tp_{pair}
  #my $pair_fn = sum($tag2dp * ($tag2dp-1)) - $pair_tp;  ## (\sum_g \npairs{|g|}) - tp_{pair}
  #$pr = $eval->{pair_precision} = frac($pair_tp, $pair_tp+$pair_fp);
  #$rc = $eval->{pair_recall}    = frac($pair_tp, $pair_tp+$pair_fn);
  #$F  = $eval->{pair_F}         = pr2F($pr, $rc);
  ##
  ##-- using \npairs{|c|}, \npairs{|g|},
  ## + where \npairs{N} = \nchoosek{N}{2}
  ##   : number of UNordered pairs in a set of size N
  my $npairs1v = $tag1dpd * ($tag1dpd-1);  ## c => \npairs{|c|}
  my $npairs1  = sum($npairs1v);           ## \sum_c \npairs{|c|}
  my $npairs2v = $tag2dpd * ($tag2dpd-1);  ## g => \npairs{|g|}
  my $npairs2  = sum($npairs2v);           ## \sum_g \npairs{|g|}
  $pr = $eval->{pair_precision} = frac($pair_tp, $npairs1);
  $rc = $eval->{pair_recall}    = frac($pair_tp, $npairs2);
  $F  = $eval->{pair_F}         = pr2F($pr, $rc);

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## weighted pair-wise precision, recall
  ##  + pseudo-Schulte im Walde, following Hatzivassiloglou & McKeown (1993)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##-- wpair_pr: \sum_c \sum_g (|c|/Ntoks) * (\npairs{|c \cap g - u|} / \npairs{|c|})
  $pr = sum(fracpdl(($tag1prob * $pair_tp_2d)->xchg(0,1)->sumover, $npairs1v));
  ##-- wpair_rc: \sum_c \sum_g (|g|/Ntoks) * (\npairs{|c \cap g - u|} / \npairs{|g|})
  $rc = sum(fracpdl(($tag2prob->slice("*1,") * $pair_tp_2d)->sumover, $npairs2v));

  @$eval{qw(wpair_precision wpair_recall wpair_F)} = ($pr, $rc, pr2F($pr,$rc));

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## mutual information (bits)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $log2     = pdl(double,2)->log;
  my $p12      = $jpdld / $ftotald;
  my $log_p12  = $p12->log->setnantobad->setbadtoval(0) / $log2;
  my $log_p1p2 = ( (log($tag1prob)+log($tag2prob->slice("*1,")))/$log2 )->setnantobad->setbadtoval(0);
  ##
  #my $mi_2d   = $p12 * ($log_p12 - log($tag1prob*$tag2prob->slice("*1,"))/$log2);
  #$mi_2d->inplace->setnantobad->inplace->setbadtoval(0);
  my $mi_2d    = $p12 * ($log_p12 - $log_p1p2); ##-- equivalent, more stable
  ##
  ##-- MI by definition, using 'unknown' as a "real" datum
  #my $mi_all = sum($mi_2d);
  ##
  ###-- MI breakdown
  my $mi_known = sum($mi_2d*$tag1kmask);
  my $mi_unk   = sum($mi_2d->slice("($unkid),:"));
  ##
  ##-- store 'em
  @$eval{qw(mi_known mi_unk mi_all)} = ($mi_known, $mi_unk, $mi_known+$mi_unk);
  $eval->{mi} = $mi_known - $mi_unk; ##-- hack: subtract MI contribution of unknowns

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## entropy
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##-- Entropy: joint & independent: vectors
  my $log_p1 = log($tag1prob)->inplace->setnantobad->inplace->setbadtoval(0)/$log2;
  my $log_p2 = log($tag2prob)->inplace->setnantobad->inplace->setbadtoval(0)/$log2;
  my $H12v = -$p12 * $log_p12;     ## <t1,t2> => H(t1,t2) [including unknowns]
  my $I12v = $mi_2d;               ## <t1,t2> => I(t1;t2) [including unknowns]
  my $H1v  = -$tag1prob * $log_p1; ## t1      => H(t1)    [including unknowns]
  my $H2v  = -$tag2prob * $log_p2; ## t2      => H(t2)    [including unknowns]

  ##-- Entropy: joint & independent: scalars
  my ($H12,$H1,$H2) = ($H12v->sum, $H1v->sum, $H2v->sum);

  ##-- Entropy: conditional entropy
  my $H1g2 = $H12 - $H2;           ## H(t1|t2) [+unknowns]
  my $H2g1 = $H12 - $H1;           ## H(t2|t1) [+unknowns]
  my $I12  = $H1 + $H2 - $H12;     ## I(1;2)   [+unknowns], == $I12v->sum() == ($mi_known+$mi_unk)

  ##-- Entropy: unknown contributions: joint & independent
  my $pu   = $tag1prob->slice("($unkid)");
  my $Hu1  = $H1v->slice("($unkid)");
  my $Hu2  = 0; ##-- unknown doesn't contribute anything to H(tag2)
  my $Hu12 = $H12v->slice("($unkid),:")->sum;
  my $Iu12 = $mi_unk;

  ##-- Entropy: unknown contributions: conditional
  my $Hu1g2 = $Hu12 - $Hu2;
  my $Hu2g1 = $Hu12 - $Hu1;
  #my $Iu12a = $Hu1 + $Hu2 - $Hu12; ##-- ~ I_u(1;2) ###-- NO!

  ##-- precision & recall (default)
  my $prH = frac($I12, $H2);
  my $rcH = frac($I12, $H1);
  my $IH  = frac($I12, $H12);
  my $IHuc = $IH;

  ##-- Entropy: get values (corresponds to old ($entropyMethod eq 'unknown-singletons'))
  if (1) {
    my $pu1      =  $ftotald**-1;                ##-- probability of a singleton     == 1.0/$ftotal
    my $log_pu1  = -log($ftotald)/$log2;         ##-- log-probability of a singleton == log($pu1)/log(2)
    my $H12_us_k =  sum($H12v * $tag1kmask);                  ##-- H(1,2): known
    my $H12_us_u = -sum($p12->slice("($unkid),:")*$log_pu1);  ##-- H(1,2): unknown_singletons
    my $H12_us   = $H12_us_k + $H12_us_u;                     ##-- H(1,2): known + unknown_singletons
    my $H1_us_k  =  sum($H1v * $tag1kmask);                   ##-- H(1)  : known
    my $H1_us_u  = -$pu * $log_pu1;                           ##-- H(1)  : unknown_singletons
    my $H1_us    = sclr($H1_us_k + $H1_us_u);                 ##-- H(1)  : known + unknown_singletons

    ##-- Entropy: "unknown-singletons": get adjusted MI and conditional entropies
    $H1g2 = $H12_us - $H2;
    $H2g1 = $H12    - $H1;
    my $I12_us = $H1_us + $H2 - $H12_us; ##-- unknowns-as-singletons       [high precision,  low recall]
    my $I12_uc = $H1    + $H2 - $H12;    ##-- unknowns-as-cluster          [low  precision, high recall]
    $I12       = $I12_uc;                ##-- default: unknowns-as-cluster

    ##-- Entropy: "unknown-singletons": get precision, recall, I
    $prH = frac($I12_uc, $H2);
    $rcH = frac($I12_us, $H1_us);
    $IH  = frac($I12_us + $I12_uc, $H12 + $H12_us);
  }

  ##-- Entropy: assign
  @$eval{qw(H_precision H_recall H_F H_I H_Iuc)} = ($prH,$rcH,pr2F($prH,$rcH), $IH, $IHuc);


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Rand Index (optional: quadratic in number of tokens!)
  ## --> IGNORED
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Adjusted Rand Index
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##-- ARand: "unknown-singletons" method (prevent pair association)
  #my $npairs1u = $npairs1v->slice($unkid);                ##--             npairs(n_{i=u.*})
  my $randSum1 = ($npairs1v*$tag1kmask)->sumover;         ##-- \sum_{i!=u} npairs(n_{i.*})
  my $randSum2 = $npairs2v->sumover;                      ##-- \sum_j      npairs(n_{*.j})
  my $npairsTotal = $ftotald * ($ftotald-1);              ##               npairs(n_{*.*})

  my $ARandMax    = 0.5 * ($randSum1 + $randSum2);        ##-- MaximumIndex
  my $ARandExpect = ($randSum1*$randSum2)/$npairsTotal;   ##-- ExpectedIndex
  my $ARandIndex  = $pair_tp;                             ##-- Index

  my $ARand = fracpdl($ARandIndex-$ARandExpect, $ARandMax-$ARandExpect)->sclr;
  $eval->{RandA} = $ARand;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Ambiguity rates & word-type info
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##-- ambiguity rates
  #my $ntypes  = $Ntxt; ##-- number of text types in the corpus
  my $ntypes  = $Ntxtk; ##-- number of known text types in the corpus
  my $nanals1 = $tag1txt->{nzvals}->nelem;
  my $nanals2 = $tag2txt->{nzvals}->nelem;
  @$eval{qw(ntypes nanals1 nanals2)} = ($ntypes,$nanals1,$nanals2);
  $eval->{arate1} = frac($nanals1, $ntypes);
  $eval->{arate2} = frac($nanals2, $ntypes);

  ##-- word-type info: pdls
  my $tag1txt_begin = $tag1txt->{ptr};
  my $tag1txt_end   = $tag1txt->{ptr}->rotate(-1)->slice("0:-2")->append($nanals1);
  my $tag1ntxt      = $tag1txt_end - $tag1txt_begin;

  my $tag2txt_begin = $tag2txt->{ptr};
  my $tag2txt_end   = $tag2txt->{ptr}->rotate(-1)->slice("0:-2")->append($nanals2);
  my $tag2ntxt      = $tag2txt_end - $tag2txt_begin;

  ##-- word-type info: inverse pdls
  my $tag1ntxti     = ($tag2ntxt * $isbest21)->sumover;
  my $tag2ntxti     = ($tag1ntxt * $isbest12)->sumover;

  ##-- word-type info: density
  my $wtype_density_tag1  = fracpdl($tag1ntxt,  $ntypes);
  my $wtypei_density_tag1 = fracpdl($tag1ntxti, $ntypes);
  my $wtype_density_tag2  = fracpdl($tag2ntxt,  $ntypes);
  my $wtypei_density_tag2 = fracpdl($tag2ntxti, $ntypes);

  ##-- word-type info: tag-wise info hashes
  pdls2info($tag1ia,
	    nwtypes =>$tag1ntxt,  wtype_density =>$wtype_density_tag1,
	    nwtypesi=>$tag1ntxti, wtypei_density=>$wtypei_density_tag1,
	   );
  pdls2info($tag2ia,
	    nwtypes =>$tag2ntxt,  wtype_density =>$wtype_density_tag2,
	    nwtypesi=>$tag2ntxti, wtypei_density=>$wtypei_density_tag2,
	   );

  ##-- number of *observed* tags
  $eval->{ntags1} = $tag1dp->nnz;
  $eval->{ntags2} = $tag2dp->nnz;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Cleanup
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##-- chuck out the pdls...
  #delete(@$eval{qw(jpdist tag1txt tag2txt)});

  ##-- finally: return
  return $eval;
}


##======================================================================
## Utilities: PDLs

## undef = pdls2info(\@info_array, $key1=>$pdl1, ...)
sub pdls2info {
  my ($ia,%pdls) = @_;
  @pdls{keys(%pdls)} = map {UNIVERSAL::isa($_,'PDL') ? [$_->list] : $_} values(%pdls);
  my ($i);
  foreach $i (0..$#$ia) {
    @{$ia->[$i]}{keys(%pdls)} = map {$_->[$i]} values(%pdls);
  }
}

## $Fpdl = pr2Fpdl($precision,$recall)
sub pr2Fpdl {
  return (
	  2.0/($_[0]**-1 + $_[1]**-1) ##-- Schütze
	  # (2*$pr*$rc)/($pr+$rc)   ##-- Schulte im Walde (equivalent to Schütze)
	 )->setnantobad->setbadtoval(0);
}


## $fracpdl = fracpdl($num,$div)
sub fracpdl { return ($_[0]->convert(double) / $_[1])->setnantobad->setbadtoval(0); }

##======================================================================
## Utilities: scalars

## $frac = frac($numerator,$denominator)
##  + compute fraction $numerator/$denominator
##  + returns 0 if $denominator==0
sub frac { return $_[1] ? ($_[0]/$_[1]) : 0; }

## $F = pr2F($precision,$recall)
##  + compute harmonic average of precision and recall
##  + returns 0 if either $precision or $recall is zero
sub pr2F {
  my ($pr,$rc) = @_;
  return
    ($pr && $rc
     ?  2/($pr**-1 + $rc**-1) ##-- Schütze
     #? (2*$pr*$rc)/($pr+$rc)  ##-- Schulte im Walde (equivalent to Schütze)
     ##--
     : 0);
}

## $npairs = npairs($n)
##  + computes binomial coefficient binom($n,2)
sub npairs {
  my $n = shift;
  return ($n/2) * ($n-1);
}

##======================================================================
## Corpus::Profile API
# (inherited from Corpus::Profile::PdlProfile)


##======================================================================
## Conversion: Summary

## $summary = $eval->summary(%args)
sub summary {
  my ($eval,%args) = @_;
  @$eval{keys %args} = values(%args);
  return MUDL::Corpus::Profile::PdlProfile::ITagEval::Summary->newFromEval($eval);
}


##======================================================================
## I/O: Native (short summary)

## $bool = $obj->saveNativeFh($fh,@args)
sub saveNativeFh {
  my ($eval,$fh,%args) = @_;
  return $eval->summary(%args)->saveNativeFh($fh);
}



##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Evaluate induced tagger output, PDL-ized\n)
     .qq(Options:\n)
     .qq(  gbuffer=BUF   [REQUIRED]\n)
     .qq(  label1=LABEL  [default=(tag1)]\n)
     .qq(  label2=LABEL  [default=(tag2)]\n)
     .qq(  tag1a=ATTRI   [default=0]\n)
     .qq(  tag2a=ATTRI   [default=1]\n)
     .qq(  targets=ENUM  [default=none (eval wrt all tokens)]\n)
     .qq(  targetsa=ATTR [default=0 (for "gbuffer")]\n)
     .qq(  unknown1=UNK  [default="\@UNKNOWN"]\n)
     .qq(  ... and more ...\n)
    );
}

########################################################################
##
## CLASS: MUDL::Corpus::Profile::ITagEval::Summary
##
## TODO:
##  - change eval & summary format to store *just* pdls
##    (eliminate parallel saving of $tag1i, $tag2i hash keys)
##  - requires: re-write of a lot of MUDL::Make::Fields
##    + MUDL::Make storage should be replaced by some kind of
##      sensible offline storage (SQL? Berkeley DB?) anyways,
##      but I think we're going to ignore that for now...
##  - as it is:
##     OLD_SUFFIX         : OLD_SIZE  /  NEW_SIZE : NEW_SUFFIX
##     .ett               :     854k  /    44k    : .itags.pdltt.bin
##     .eval.bin          :     660k  /   447k    : .eval.bin          (OPTIONAL)
##     .eval.summary.bin  :      17k  /    13k    : .eval.summary.bin  (with tag1 info)
##     .eval              :     2.2k  /   1.7k    : .eval
##     -------------------------------------------------------------------------
##     TOTAL              :     1.5M  /    59k    : TOTAL(REQUIRED) [ca. 96% saved space!]
########################################################################

package MUDL::Corpus::Profile::PdlProfile::ITagEval::Summary;
our @ISA = qw(MUDL::Corpus::Profile::PdlProfile::ITagEval);

## $obj = $obj->new()
sub new {
  my $that = shift;
  my $esum = bless({
		    label1=>'(tag1)',
		    label2=>'(tag2)',
		    @_
		   }, ref($that)||$that);
  return $esum;
}

## $esummary = $class_or_obj->newFromEval($itageval)
sub newFromEval {
  my ($that,$iteval) = @_;
  return $that->new()->fromEval($iteval);
}

## $esummary = $esummary->fromEval($itageval)
sub fromEval {
  my ($esum,$eval) = @_;

  ##-- duplicate some keys
  my @dup = (
	     qw(label1 label2),
	     qw(tag1i tag2i),
	     qw(ntoks ntypes nanals1 nanals2 arate1 arate2 nwtypes ntags1 ntags2),
	     (map { "meta_$_" } qw(precision recall F)),
	     (map { "ameta_$_" } qw(precision recall F)),
	     (map { "avg_$_" } qw(precision recall F)),
	     (map { "wavg_$_" } qw(precision recall F)),
	     (map { "pair_$_" } qw(precision recall F)),
	     (map { "wpair_$_" } qw(precision recall F)),
	     (map { "H_$_" } qw(precision recall I F Iuc)),
	     (map { "mi_$_" } qw(known unk all)),
	     qw(H1 H2 H12 H1g2 H2g1 Hu1 Hu2 Hu12 Hu1g2 Hu2g1),
	     #qw(Rand),  ##-- Rand Index
	     qw(RandA), ##-- Adjusted Rand Index
	    );
  @$esum{@dup} = @$eval{@dup};

  ##-- compatibility hacks
  @$esum{qw(precision recall F)}                   = @$esum{qw(meta_precision meta_recall meta_F)};
  @$esum{qw(total_precision total_recall total_F)} = @$esum{qw(meta_precision meta_recall meta_F)};

  return $esum;
}

##======================================================================
## I/O: Native

## $bool = $obj->saveNativeFh($fh,@args)
sub saveNativeFh {
  my ($esum,$fh,%args) = @_;
  @$esum{keys %args} = values(%args);

  my ($tag2);
  $fh->print
    (
     "##", ("-" x 78), "\n",
     "## ", ref($esum), " Summary\n",
     "## Identifiers:\n",
     "##   Got   : $esum->{label1}\n",
     "##   Wanted: $esum->{label2}\n",
     "## Num. Tokens              : ", sprintf("%6d", $esum->{ntoks}||0), "\n",
     "## Num. Got->Wanted         : ", sprintf("%6d", ($esum->{ntoks}||0)*($esum->{precision}||0)), "\n",
     "## Num. Wanted->Got         : ", sprintf("%6d", ($esum->{ntoks}||0)*($esum->{recall}||0)), "\n",
     "##\n",

     (defined($esum->{ntypes})
      ? ("## Num. Types               : ", sprintf("%6d\n",   $esum->{ntypes}))
      : qw()),
     (defined($esum->{ntags1})
      ? ("## Num. Tags / Got          : ", sprintf("%6d\n",   $esum->{ntags1}))
      : qw()),
     (defined($esum->{ntags2})
      ? ("## Num. Tags / Wanted       : ", sprintf("%6d\n",   $esum->{ntags2}))
      : qw()),
     (defined($esum->{nanals1}) && defined($esum->{arate1})
      ? ("## Ambiguity / Got          : ", sprintf("%6d  (%6.2f an/typ)\n", $esum->{nanals1}, $esum->{arate1}))
      : qw()),
     (defined($esum->{nanals2}) && defined($esum->{arate2})
      ? ("## Ambiguity / Wanted       : ", sprintf("%6d  (%6.2f an/typ)\n", $esum->{nanals2}, $esum->{arate2}))
      : qw()),

     "##\n",

     "## Meta-Precision           : ", sprintf("%6.2f %%", 100*($esum->{meta_precision}||0)), "\n",
     "## Meta-Recall              : ", sprintf("%6.2f %%", 100*($esum->{meta_recall}||0)), "\n",
     "## Meta F                   : ", sprintf("%6.2f %%", 100*($esum->{meta_F}||0)), "\n",
     "##\n",
     "## AMeta-Precision          : ", sprintf("%6.2f %%", 100*$esum->{ameta_precision}), "\n",
     "## AMeta-Recall             : ", sprintf("%6.2f %%", 100*$esum->{ameta_recall}), "\n",
     "## AMeta F                  : ", sprintf("%6.2f %%", 100*$esum->{ameta_F}), "\n",
     "##\n",
     "## Avg tag2-Precision       : ", sprintf("%6.2f %%", 100*$esum->{avg_precision}), "\n",
     "## Avg tag2-Recall          : ", sprintf("%6.2f %%", 100*$esum->{avg_recall}), "\n",
     "## Avg F                    : ", sprintf("%6.2f %%", 100*$esum->{avg_F}), "\n",
     "##\n",
     "## WAvg tag2-Precision      : ", sprintf("%6.2f %%", 100*$esum->{wavg_precision}), "\n",
     "## WAvg tag2-Recall         : ", sprintf("%6.2f %%", 100*$esum->{wavg_recall}), "\n",
     "## WAvg F                   : ", sprintf("%6.2f %%", 100*$esum->{wavg_F}), "\n",
     "##\n",
     "## Pair Precision           : ", sprintf("%6.2f %%", 100*$esum->{pair_precision}), "\n",
     "## Pair Recall              : ", sprintf("%6.2f %%", 100*$esum->{pair_recall}), "\n",
     "## Pair F                   : ", sprintf("%6.2f %%", 100*$esum->{pair_F}), "\n",
     "##\n",
     "## WPair Precision          : ", sprintf("%6.2f %%", 100*$esum->{wpair_precision}), "\n",
     "## WPair Recall             : ", sprintf("%6.2f %%", 100*$esum->{wpair_recall}), "\n",
     "## WPair F                  : ", sprintf("%6.2f %%", 100*$esum->{wpair_F}), "\n",
     "##\n",
     "## H Precision              : ", sprintf("%6.2f %%", 100*$esum->{H_precision}), "\n",
     "## H Recall                 : ", sprintf("%6.2f %%", 100*$esum->{H_recall}), "\n",
     "## H F                      : ", sprintf("%6.2f %%", 100*$esum->{H_F}), "\n",
     "## H I (uc+us)              : ", sprintf("%6.2f %%", 100*$esum->{H_I}), "\n",
     "## H I (uc)                 : ", sprintf("%6.2f %%", 100*$esum->{H_Iuc}), "\n",
     "##", ("-" x 78), "\n",
     "1;\n",
    );

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
