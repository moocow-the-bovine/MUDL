##-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::ITagEval.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: induced tagger evaluation
##======================================================================

package MUDL::Corpus::Profile::ITagEval;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use MUDL::Map;
use MUDL::Object;
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Profile);

##======================================================================
## Constructor
##   + $obj = $class_or_obj->new(%args)
##   + %args:
##     cr => $fileReader,      ##-- input reader: ($text,...,$tag1,...,$tag2,...)
##     label1 => $label1,      ##-- for summary, native save
##     label2 => $label2,      ##-- for summary, native save
##     tag1a => $tag1attr,     ##-- key(s) for 'tag1': used as $tok->attribute($key): default: 'tag'
##     tag2a => $tag2attr,     ##-- key(s) for 'tag2': used as $tok->attribute($key): default: '1'
##     do_ambig=>$bool,        ##-- whether to track ambiguity data
##     do_rand=>$bool,         ##-- whether to compute Rand Index (default=no)
##                             ##   + WARNING: quadratic in number of tokens!
##     unknown1=>$unk1,        ##-- unknown tag1 value (default='@UNKNOWN')
##
##     ##-- runtime data
##     enum  => $enum,         ##-- enum only wrt these targets (default=none~all tokens)
##     ntoks => $ntokens,
##     jdist => $dist,         ##-- MUDL::Dist::Nary, join tag1+tag2 counts
##     txts  => \%txt2undef,   ##-- word type pseudo-set
##     txttag1 => \%txttag1,   ##-- tok,tag1 pair pseudo-set
##     txttag2 => \%txttag2,   ##-- tok,tag2 pair pseudo-set
##
##     ##-- Rand Index (optional): runtime data
##     randidx => {
##                 objs=>[ $tok1_tag1,$tok1_tag2, ... ], ##-- list of all tokens encountered
##                },
##
##     ##-- on finish()
##     tag12m => $map12,       ##-- map $tag1=>$tag2, # s.t. $tag2 = arg_{$tag2} max p($tag2|$tag1)
##     tag21m => $map21,       ##-- map $tag2=>$tag1, # s.t. $tag1 = arg_{$tag1} max p($tag1|$tag2)
##     tag12b => $dist12,      ##-- $tag1=>$best_tag12_count,
##     tag21b => $dist21,      ##-- $tag2=>$best_tag21_count,
##
##     ##-- ambiguity data
##     ntypes => $n,           ##-- number of word types
##     nanals1 => $n1,         ##-- total number of (type,tag1) pairs
##     nanals2 => $n2,         ##-- total number of (type,tag2) pairs
##
##     ##-- summary data: token-wise
##     precision=>$prec,       ##-- p(best(tag2|tag1)|tag1)
##     recall=>$recall,        ##-- p(best(tag1|tag2)|tag2)
##     F=>$F,
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
##     H_precision=>$pr,   ##-- 1 - (H(tag2|tag1) + H_u(tag2|tag1))/H(tag2)
##     H_recall=>$rc,      ##-- 1 - (H(tag1|tag2) + H_u(tag1|tag2))/H(tag1)
##     H_I=>$I,            ##-- 1 - (I(X;Y) + I_u(X;Y))/H(X,Y)
##     H_F=>$F,            ##-- F(H_pr,H_rc)
##
##     ##-- Summary data: Rand
##     Rand  => $rand_index,        ##-- if $eval->{do_rand} is true
##     RandA => $adjust_rand_index
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new(cr=>'MUDL::CorpusIO',
			       label1=>'(tag1)',
			       label2=>'(tag2)',
			       tag1a=>'tag',
			       tag2a=>'1',
			       ntoks=>0,
			       jdist=>MUDL::Dist::Nary->new(nfields=>2,sep=>"\t"),
			       enum=>undef,
			       unknown1=>'@UNKNOWN',
			       do_ambig=>1,
			       do_rand=>0,
			       %args);

  if ($self->{do_rand}) {
    $self->{randidx}       = {} if (!$self->{randidx});
    $self->{randidx}{objs} = [] if (!$self->{randidx}{objs});
  }

  return $self;
}

##======================================================================
## Reset (file reader)

## $eval = $eval->reset()
##   + resets file reader
sub reset {
  my $eval = shift;
  $eval->{cr}->close() if (UNIVERSAL::can($eval->{cr},'close'));
  $eval->{cr} = 'MUDL::CorpusIO';
  return $eval;
}

##======================================================================
## Errors

## undef = $eval->error(@msg);
##  + croak()s and dies
sub error {
  my ($eval,@msg) = @_;
  croak(ref($eval), ": ", @msg, "\n",
	"> ",
	(defined($eval->{cr}) && defined($eval->{cr}{filename})
	 ? $eval->{cr}{filename}
	 : '(input file)'),
	" line ",
	(defined($eval->{cr}) && defined($eval->{cr}{fh})
	 ? $eval->{cr}{fh}->input_line_number
	 : '(unknown)'),
	"\n",
	"> ");
}

##======================================================================
## Profiling: addSentence

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($pr,$s) = @_;

  my ($tok, $txt, $tag1, $tag2);
  foreach $tok (@$s) {
    ##-- eval-by-target
    next if (defined($pr->{targets})
	     && defined($pr->{targeta})
	     && !defined($pr->{targets}->index($tok->attribute($pr->{targeta}))));

    $tag1 = $tok->attribute($pr->{tag1a});
    $tag2 = $tok->attribute($pr->{tag2a});

    error("undefined tag-1 (attr=$pr->{tag1a})") if (!defined($tag1));
    error("undefined tag-2 (attr=$pr->{tag2a})") if (!defined($tag2));

    ++$pr->{jdist}{nz}{"$tag1\t$tag2"};
    ++$pr->{ntoks};

    if ($pr->{do_ambig}) {
      $txt = $tok->text;
      ++$pr->{txt}{$txt};
      ++$pr->{txttag1}{$txt."\t".$tag1};
      ++$pr->{txttag2}{$txt."\t".$tag2};
    }

    ##-- Rand index
    if ($pr->{do_rand}) {
      push(@{$pr->{randidx}{objs}}, $tag1,$tag2);
    }
  }

  return $pr;
}

##======================================================================
## Profiling: finish()

## undef = $profile->finish(%args)
##  + get best-maps, precision, recall
sub finish {
  my $eval = shift;

  ##-- sanity check
  if (!$eval->{ntoks}) {
    carp(ref($eval), "::finish(): no tokens processed!");
    $eval->{ntoks} = -1;
  }

  ##-- common vars
  my $jdist = $eval->{jdist};
  my ($tag12,$f12, $tag1,$tag2,$tagi, $pr,$rc,$F);

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision, recall
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##-- allocate tag1-, tag2- info structures
  ##   %info_tag =
  ##   (
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
  my $tag1i = $eval->{tag1i} = {}; ##-- $tag1 => \%info_tag1
  my $tag2i = $eval->{tag2i} = {}; ##-- $tag2 => \%info_tag2

  ##-- allocate unigram (tag1,tag2) distributions
  my $tag1d  = MUDL::Dist::Partial->new();
  my $tag2d  = MUDL::Dist::Partial->new();
  my $ftotal = 0; ##-- total frequency, including unknowns

  ##-- get best-match maps
  my ($fbest);
  while (($tag12,$f12)=each(%{$jdist->{nz}})) {
    ($tag1,$tag2) = CORE::split(/\t+/,$tag12,2);

    $tag1d->{nz}{$tag1} += $f12;
    $tag2d->{nz}{$tag2} += $f12;
    $ftotal             += $f12;

    $tag1i->{$tag1}{freq} += $f12 if ($tag1 ne $eval->{unknown1});
    $tag2i->{$tag2}{freq} += $f12;

    ##-- record number unknowns for tag2, but don't consider them for best-match
    if ($tag1 eq $eval->{unknown1}) {
      $tag2i->{$tag2}{nunknown} += $f12;
      next;
    }

    $fbest = $tag1i->{$tag1}{fbest};
    if (!$fbest || $f12 > $fbest) {
      $tag1i->{$tag1}{fbest} = $f12;
      $tag1i->{$tag1}{tbest} = $tag2;
    }
    $fbest = $tag2i->{$tag2}{fbest};
    if (!$fbest || $f12 > $fbest) {
      $tag2i->{$tag2}{fbest} = $f12;
      $tag2i->{$tag2}{tbest} = $tag1;
    }
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision,recall: get number of matching classes
  my ($tbest);
  while (($tag1,$tagi)=each(%$tag1i)) {
    $tagi->{nbesti} = 0;
    $tagi->{nunknown} = 0;
    $tag2i->{$tagi->{tbest}}{nbesti}++;
  }
  while (($tag2,$tagi)=each(%$tag2i)) {
    $tagi->{nbesti} = 0 if (!$tagi->{nbesti});
    $tagi->{nunknown} = 0 if (!$tagi->{nunknown});
    $tag1i->{$tagi->{tbest}}{nbesti}++;
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision,recall: distribute unknowns among matching tag1s
  while (($tag1,$tagi)=each(%$tag1i)) {
    $tbest = $tagi->{tbest};
    $tagi->{nunknown} += $tag2i->{$tbest}{nunknown} / $tag2i->{$tbest}{nbesti};
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision,recall: get {meta_ncor*},{meta_ninc*}
  my ($ncor,$ninc);
  while (($tag1,$tagi)=each(%$tag1i)) {
    #next if ($tag1 eq $eval->{unknown1}); ##-- ignore unknowns (shouldn't be here anyway)
    $ncor = $tagi->{meta_ncor12} = $tagi->{fbest};
    $ninc = $tagi->{meta_ninc12} = ($tag1d->{nz}{$tag1} - $ncor) + $tagi->{nunknown};
    if (defined($tbest=$tagi->{tbest})) {
      $tag2i->{$tbest}{meta_ncor12} += $ncor;
      $tag2i->{$tbest}{meta_ninc12} += $ninc;
    }
  }
  while (($tag2,$tagi)=each(%$tag2i)) {
    $ncor = $tagi->{meta_ncor21} = $tagi->{fbest};
    $ninc = $tagi->{meta_ninc21} = $tag2d->{nz}{$tag2} - $ncor; ##-- unknowns are already handled here
    if (defined($tbest=$tagi->{tbest})) {
      $tag1i->{$tbest}{meta_ncor21} += $ncor;
      $tag1i->{$tbest}{meta_ninc21} += $ninc;
    }
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision,recall: get breakdown precision, recall
  while (($tag1,$tagi)=each(%$tag1i)) {
    ##-- ensure everything is defined
    foreach (qw(meta_ncor12 meta_ninc12 meta_ncor21 meta_ninc21)) {
      $tagi->{$_}=0 if (!defined($tagi->{$_}));
    }
    ##-- compute precision, recall
    $pr = $tagi->{meta_precision} = frac($tagi->{meta_ncor12}, $tagi->{meta_ncor12}+$tagi->{meta_ninc12});
    $rc = $tagi->{meta_recall}    = frac($tagi->{meta_ncor21}, $tagi->{meta_ncor21}+$tagi->{meta_ninc21});
    $F  = $tagi->{meta_F}         = pr2F($pr,$rc);
  }
  while (($tag2,$tagi)=each(%$tag2i)) {
    ##-- ensure everything is defined
    foreach (qw(meta_ncor12 meta_ninc12 meta_ncor21 meta_ninc21)) {
      $tagi->{$_}=0 if (!defined($tagi->{$_}));
    }
    ##-- compute precision, recall
    $pr = $tagi->{meta_precision} = frac($tagi->{meta_ncor12}, $tagi->{meta_ncor12}+$tagi->{meta_ninc12});
    $rc = $tagi->{meta_recall}    = frac($tagi->{meta_ncor21}, $tagi->{meta_ncor21}+$tagi->{meta_ninc21});
    $F  = $tagi->{meta_F}         = pr2F($pr,$rc);
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## meta-tagging precision,recall: get totals
  $ncor=0;
  while (($tag1,$tagi)=each(%$tag1i)) { $ncor += $tagi->{meta_ncor12}; }
  $pr = $eval->{meta_precision} = frac($ncor, $ftotal);

  $ncor=0;
  while (($tag2,$tagi)=each(%$tag2i)) { $ncor += $tagi->{meta_ncor21}; }
  $rc = $eval->{meta_recall} = frac($ncor, $ftotal);

  $eval->{meta_F} = pr2F($pr,$rc);


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## tag-wise precision, recall (Sch"utze-style)
  ##  + adds \%tag2i values for $tag2:
  ##    (
  ##     avg_ncor => $avg_ncorrect,
  ##     avg_ncor => $avg_nincorrect,
  ##
  ##     avg_precision=>$avg_pr,
  ##     avg_recall=>$avg_recall,
  ##     avg_F=>$avg_F,
  ##    )
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##-- get total number of tag2-types
  my @tags2 = keys(%$tag2i);

  ##-- get number of correct, incorrect assignments
  my ($besttag2);
  while (($tag12,$f12)=each(%{$jdist->{nz}})) {
    ($tag1,$tag2) = CORE::split(/\t+/,$tag12,2);

    ##-- ensure everything is defined
    $tagi = $tag2i->{$tag2};
    $tagi->{avg_ncor} = 0 if (!defined($tagi->{avg_ncor}));
    $tagi->{avg_ninc} = 0 if (!defined($tagi->{avg_ninc}));

    ##-- get best_{1->2}(tag1)
    $besttag2 = $tag1i->{$tag1}{tbest};

    if ($tag1 eq $eval->{unknown1}) {
      ##-- unknown tag1: distribute 'incorrect' among all tag2s
      $tag2i->{$_}{avg_ninc} += $f12/@tags2 foreach (@tags2);
      next;
    }
    elsif (!defined($besttag2)) {
      ##-- no best $tag2 (gold) for $tag1 (induced): complain
      carp(ref($eval),"::finish(): no best gold-tag for induced-tag '$tag1' -- using empty string");
      $besttag2 = ''; ##--> inconsistent (tag1==UNKNOWN)
    }

    if ($besttag2 eq $tag2) {
      $tag2i->{$tag2}{avg_ncor} += $f12;
    } else {
      $tag2i->{$tag2}{avg_ninc} += $f12; ##--> inconsistent results when ($besttag2 eq '') [?]
    }
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Average precision, recall: breakdown by tag2
  my ($ftag1,$ftag2);
  while (($tag2,$tagi)=each(%$tag2i)) {
    $ncor  = $tagi->{avg_ncor};
    $ninc  = $tagi->{avg_ninc};
    $ftag2 = $tag2d->{nz}{$tag2};

    $pr = $tagi->{avg_precision} = frac($ncor, $ncor+$ninc);
    $rc = $tagi->{avg_recall}    = frac($ncor, $ftag2);
    $F  = $tagi->{avg_F}         = pr2F($pr,$rc);
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Average precision, recall: total average
  my $avg_pr = 0;
  my $avg_rc = 0;
  while (($tag2,$tagi)=each(%$tag2i)) {
    $avg_pr += $tagi->{avg_precision};
    $avg_rc += $tagi->{avg_recall};
  }
  $avg_pr /= scalar(@tags2);
  $avg_rc /= scalar(@tags2);
  @$eval{qw(avg_precision avg_recall avg_F)} = ($avg_pr, $avg_rc, pr2F($avg_pr, $avg_rc));


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## weighted tag-wise precision, recall (pseudo-Sch"utze-style)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $wa_pr = 0;
  my $wa_rc = 0;
  while (($tag2,$ftag2)=each(%{$tag2d->{nz}})) {
    $wa_pr += ($ftag2/$ftotal) * $tag2i->{$tag2}{avg_precision};
    $wa_rc += ($ftag2/$ftotal) * $tag2i->{$tag2}{avg_recall};
  }
  @$eval{qw(wavg_precision wavg_recall wavg_F)} = ($wa_pr,$wa_rc, pr2F($wa_pr,$wa_rc));

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## pair-wise precision, recall
  ##  + Schulte im Walde, following Hatzivassiloglou & McKeown (1993)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $pair_tp1 = $eval->{pair_tp1} = {}; ##-- true positives by $tag1:  $tag1=>tp($tag1)
  my $pair_tp2 = $eval->{pair_tp2} = {}; ##-- true positives by $tag2:  $tag2=>tp($tag2)
  my ($npairs12);
  while (($tag12,$f12)=each(%{$eval->{jdist}{nz}})) {
    ($tag1,$tag2) = CORE::split(/\t+/,$tag12,2);
    if ($tag1 eq $eval->{unknown1}) {
      ##-- UNKNOWN tag1 is always bad
      $pair_tp1->{$tag1}  = 0;
      $pair_tp2->{$tag2} += 0;
      next;
    }
    $npairs12 = npairs($f12);
    $pair_tp1->{$tag1} += $npairs12;
    $pair_tp2->{$tag2} += $npairs12;
  }


  my ($pair_tp,$pair_fp,$pair_fn) = (0,0,0);
  my $pair_fp1 = $eval->{pair_fp} = {}; ##-- false positives by $tag1: $tag1=>fp($tag1)
  my $pair_fn2 = $eval->{pair_fn} = {}; ##-- false negatives by $tag2: $tag2=>fn($tag2)
  my $pair_pr1 = $eval->{pair_pr1} = {};
  my $pair_rc2 = $eval->{pair_rc2} = {};
  my ($ntp, $npairs1, $npairs2, $tp);
  while (($tag1,$tp)=each(%$pair_tp1)) {
    $npairs1 = npairs($tag1d->{nz}{$tag1});
    $pair_fp += $pair_fp1->{$tag1} = $npairs1-$tp;
    $pair_tp += $tp;
    $pair_pr1->{$tag1} = $npairs1 ? ($tp / $npairs1) : 0;
  }
  while (($tag2,$tp)=each(%$pair_tp2)) {
    $npairs2 = npairs($tag2d->{nz}{$tag2});
    $pair_fn += $pair_fn2->{$tag2} = $npairs2-$tp;
    $pair_rc2->{$tag2} = $npairs2 ? ($tp / $npairs2) : 0;
  }
  my $pair_pr = $eval->{pair_precision} = $pair_tp / ($pair_fp + $pair_tp);
  my $pair_rc = $eval->{pair_recall}    = $pair_tp / ($pair_fn + $pair_tp);
  my $pair_F  = $eval->{pair_F}         = pr2F($pair_pr,$pair_rc);

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## weighted pair-wise precision, recall
  ##  + pseudo-Schulte im Walde, following Hatzivassiloglou & McKeown (1993)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $wpair_pr = 0;
  my $wpair_rc = 0;
  my $npairs_total = npairs($ftotal);
  while (($tag1,$tp)=each(%$pair_tp1)) {
    ##-- weight by total number of pairs belonging to this tag1
    #$npairs1 = npairs($tag1d->{nz}{$tag1});
    #$wpair_pr += $npairs1/$npairs_total * $tp/$npairs1;
    ##    ^-- equiv ------v
    #$wpair_pr += $tp/$npairs_total;
    ##    ^-- NOT equiv --v
    ##-- weight by relative tag1 frequency
    $npairs1 = npairs($tag1d->{nz}{$tag1});
    $wpair_pr += $tag1d->{nz}{$tag1}/$ftotal * $tp/$npairs1 if ($npairs1);
  }
  while (($tag2,$tp)=each(%$pair_tp2)) {
    ##-- weight by total number of pairs belonging to this tag2
    #$npairs2 = npairs($tag2d->{nz}{$tag2});
    #$wpair_pr += $npairs2/$npairs_total * $tp/$npairs2;
    ##    ^-- equiv ------v
    #$wpair_rc += $tp/$npairs_total;
    ##    ^-- NOT equiv --v
    ##-- weight by relative tag2 frequency
    $npairs2 = npairs($tag2d->{nz}{$tag2});
    $wpair_rc += $tag2d->{nz}{$tag2}/$ftotal * $tp/$npairs2 if ($npairs2);
  }
  @$eval{qw(wpair_precision wpair_recall wpair_F)} = ($wpair_pr, $wpair_rc, pr2F($wpair_pr,$wpair_rc));

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## mutual information (bits)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my ($p1,$p2,$p12);
  my $mi=0;
  my $log2 = log(2.0);
  while (($tag12,$f12)=each(%{$eval->{jdist}{nz}})) {
    ($tag1,$tag2) = CORE::split(/\t+/,$tag12,2);
    #next if ($tag1 eq $eval->{unknown1}); ##-- UNKNOWN tag1 is always bad
    $p12 = $f12/$ftotal;
    $p1  = $tag1d->{nz}{$tag1} / $ftotal;
    $p2  = $tag2d->{nz}{$tag2} / $ftotal;
    $p1  = 2**-64 if ($p12 != 0 && $p1==0); ##-- avoid singularities (should never happen)
    $p2  = 2**-64 if ($p12 != 0 && $p2==0); ##-- avoid singularities (should never happen)
    if ($tag1 eq $eval->{unknown1}) {
      ##-- hack: subtract MI for unknown $tag1
      $mi -= $p12 * log($p12 / ($p1 *$p2)) / $log2;
    } else {
      $mi += $p12 * log($p12 / ($p1 *$p2)) / $log2;
    }
  }
  $eval->{mi} = $mi;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## entropy
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $fu      = $tag1d->{nz}{$eval->{unknown1}};
  my $pu  = $fu ? ($fu / $ftotal) : 0;
  my $Hu1 = $pu ? (-$pu * log($pu)/log(2)) : 0;
  my $Hu2 = 0;
  my $Hu12 = 0;
  my $Iu12 = 0;
  while (($tag12,$f12)=each(%{$jdist->{nz}})) {
    next if ($f12 <= 0);
    ($tag1,$tag2) = CORE::split(/\t+/,$tag12);
    if ($tag1 eq $eval->{unknown1}) {
      $p12   = $f12/$ftotal;
      $Hu12 -= $p12 * log($p12)/$log2;
      $Iu12 += $p12 * log( $p12 / (($fu/$ftotal)*($tag2d->{nz}{$tag2}/$ftotal)) )/$log2;
    }
  }

  ##-- get entropies
  my $H1  = $tag1d->entropy();    ##-- H(1)
  my $H2  = $tag2d->entropy();    ##-- H(2)
  my $H12 = $jdist->entropy();    ##-- H(1,2)

  ##-- get conditional entropies
  my $H1g2 = $H12 - $H2;          ##-- ~ H(1|2)
  my $H2g1 = $H12 - $H1;          ##-- ~ H(2|1)
  my $I12  = $H1 + $H2 - $H12;    ##-- ~ I(1;2)

  ##-- get unknown contributions to conditional entropies
  my $Hu1g2 = $Hu12 - $Hu2;        ##-- ~ H_u(1|2)
  my $Hu2g1 = $Hu12 - $Hu1;        ##-- ~ H_u(2|1)
  my $Iu12a  = $Hu1 + $Hu2 - $Hu12; ##-- ~ I_u(1;2) ###-- NO!

  ##-- pseudo-precision,recall
  my ($prH,$rcH,$IH);
  ##-- 1: (considering *all* unknown-contributions)
  #$prH = 1 - frac($H2g1 + $Hu2g1, $H2);
  #$rcH = 1 - frac($H1g2 + $Hu1g2, $H1);
  #$IH  =     frac($I12 - $Iu12,  $H12);
  ##
  ##-- 2: ...adding unknowns to 'false (neg|pos)'
  #$prH = frac(($H2 - $H2g1), ($H2 + $Hu2g1));
  #$rcH = frac(($H1 - $H1g2), ($H1 + $Hu1g2));
  #$IH  =     frac($I12 - $Iu12,  $H12);
  ##
  ##-- 3: ...subtracting unknowns from 'true pos' (as #1, above)
  $prH = frac(($H2 - ($H2g1+$Hu2g1)), $H2);
  $rcH = frac(($H1 - ($H1g2+$Hu1g2)), $H1);
  $IH  =     frac($I12 - $Iu12,  $H12);

  @$eval{qw(H_precision H_recall H_F H_I)} = ($prH,$rcH,pr2F($prH,$rcH), $IH);

  ##-- save intermediate information
  @$eval{qw(H1 H2 H12 H1g2 H2g1 Hu1 Hu2 Hu12 Hu1g2 Hu2g1)}
    = ($H1,$H2,$H12,$H1g2,$H2g1,$Hu1,$Hu2,$Hu12,$Hu1g2,$Hu2g1);

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Rand Index (optional: quadratic in number of tokens!)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ($eval->{do_rand}) {
    my $objs = $eval->{randidx}{objs};
    my $rand = 0;
    my ($pairi,$pairj, $tag1i,$tag2i, $tag1j,$tag2j);
    foreach $pairi (1..int($#$objs/2)) {
      ($tag1i,$tag2i) = @$objs[2*$pairi,2*$pairi+1];

      foreach $pairj (0..($pairi-1)) {
	($tag1j,$tag2j) = @$objs[2*$pairj,2*$pairj+1];
	++$rand if (($tag1i eq $tag1j && $tag2i eq $tag2j)
		    ||
		    ($tag1i ne $tag1j && $tag2i ne $tag2j));
      }
    }
    $rand /= npairs($ftotal);
    $eval->{Rand} = $rand;
  }

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Adjusted Rand Index
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  my $randSum1 = 0; ## == \sum_i npairs(n_{i.})
  my $randSum2 = 0; ## == \sum_j npairs(n_{.j})
  while (($tag1,$ftag1) = each(%{$tag1d->{nz}})) {
    $randSum1 += npairs($ftag1);
  }
  while (($tag2,$ftag2) = each(%{$tag2d->{nz}})) {
    $randSum2 += npairs($ftag2);
  }
  my $randMax = 0.5 * ($randSum1 + $randSum2);          ##-- MaximumIndex
  my $randExp = ($randSum1*$randSum2)/npairs($ftotal);  ##-- ExpectedIndex

  my $randPairs = 0; ##-- \sum_{i,j} npairs(n_{i.j})
  while (($tag12,$f12)=each(%{$jdist->{nz}})) {
    $randPairs += npairs($f12);
  }
  my $randAdj = ($randPairs-$randExp) / ($randMax-$randExp); ##-- AdjustedIndex
  $eval->{RandA} = $randAdj;

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Ambiguity rates
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ($eval->{do_ambig}) {
    my $ntypes  = scalar(keys(%{$eval->{txt}}));
    my $nanals1 = scalar(keys(%{$eval->{txttag1}}));
    my $nanals2 = scalar(keys(%{$eval->{txttag2}}));

    ##-- store ambiguity data
    @$eval{qw(ntypes nanals1 nanals2)} = ($ntypes,$nanals1,$nanals2);
    $eval->{arate1} = frac($nanals1, $ntypes);
    $eval->{arate2} = frac($nanals2, $ntypes);
  }

  ##-- reset file reader
  $eval->reset();

  return $eval;
}

##======================================================================
## Utilities

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
## Conversion: Summary

## $summary = $eval->summary(%args)
sub summary {
  my ($eval,%args) = @_;
  @$eval{keys %args} = values(%args);
  return MUDL::Corpus::Profile::ITagEval::Summary->newFromEval($eval);
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
    (qq(Evaluate token-text (unigram) maps [broken?]\n)
     .qq(Options:\n)
     .qq(  label1=LABEL [default=(tag1)]\n)
     .qq(  label2=LABEL [default=(tag2)]\n)
     .qq(  tag1=ATTR    [default='tag']\n)
     .qq(  tag2=ATTR    [default='1']\n)
     .qq(  targets=ENUM [default=none (eval wrt all tokens)]\n)
     .qq(  targeta=ATTR [default=text]\n)
    );
}

########################################################################
## CLASS: MUDL::Corpus::Profile::ITagEval::Summary
########################################################################

package MUDL::Corpus::Profile::ITagEval::Summary;
our @ISA = qw(MUDL::Corpus::Profile::ITagEval);

## $obj = $obj->new()
sub new {
  my $that = shift;
  my $esum = $that->SUPER::new(@_);
  delete(@$esum{qw(jdist enum do_ambig cr)});
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
	     qw(ntoks ntypes nanals1 nanals2 arate1 arate2),
	     (map { "meta_$_" } qw(precision recall F)),
	     (map { "avg_$_" } qw(precision recall F)),
	     (map { "wavg_$_" } qw(precision recall F)),
	     (map { "pair_$_" } qw(precision recall F)),
	     (map { "wpair_$_" } qw(precision recall F)),
	     ('mi', map { "H_$_" } qw(precision recall I F)),
	     qw(H1 H2 H12 H1g2 H2g1 Hu1 Hu2 Hu12 Hu1g2 Hu2g1),
	     qw(Rand RandA), ##-- Rand Index
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
    ("\$precision=$esum->{precision};\n",
     "\$recall=$esum->{recall};\n",
     "",
     "\$avg_precision=$esum->{avg_precision};\n",
     "\$avg_recall=$esum->{avg_recall};\n",
     "",
     "\$wavg_precision=$esum->{wavg_precision};\n",
     "\$wavg_recall=$esum->{wavg_recall};\n",
     "",
     "\$pair_precision=$esum->{pair_precision};\n",
     "\$pair_recall=$esum->{pair_recall};\n",
     "",
     "\$wpair_precision=$esum->{wpair_precision};\n",
     "\$wpair_recall=$esum->{wpair_recall};\n",
     "",
     "\$mi=$esum->{mi};\n",
     "\$H_precision=$esum->{H_precision};\n",
     "\$H_recall=$esum->{H_recall};\n",
     "\$H_I=$esum->{H_I};\n",

     (defined($esum->{ntypes})  ? "\$ntypes=$esum->{ntypes};\n" : qw()),
     (defined($esum->{nanals1}) ? "\$nanals1=$esum->{nanals1};\n" : qw()),
     (defined($esum->{nanals2}) ? "\$nanals2=$esum->{nanals2};\n" : qw()),

     "##", ("-" x 78), "\n",
     "## ", ref($esum), " Summary\n",
     "## Identifiers:\n",
     "##   Got   : $esum->{label1}\n",
     "##   Wanted: $esum->{label2}\n",
     "## Num. Tokens              : ", sprintf("%6d", $esum->{ntoks}), "\n",
     "## Num. Got->Wanted         : ", sprintf("%6d", $esum->{ntoks}*$esum->{precision}), "\n",
     "## Num. Wanted->Got         : ", sprintf("%6d", $esum->{ntoks}*$esum->{recall}), "\n",
     "##\n",

     (defined($esum->{ntypes})
      ? ("## Num. Types               : ", sprintf("%6d\n",   $esum->{ntypes}))
      : qw()),
     (defined($esum->{nanals1}) && defined($esum->{arate1})
      ? ("## Ambiguity / Got          : ", sprintf("%6d  (%6.2f an/typ)\n", $esum->{nanals1}, $esum->{arate1}))
      : qw()),
     (defined($esum->{nanals2}) && defined($esum->{arate2})
      ? ("## Ambiguity / Wanted       : ", sprintf("%6d  (%6.2f an/typ)\n", $esum->{nanals2}, $esum->{arate2}))
      : qw()),

     "##\n",

     "## Meta-Precision           : ", sprintf("%6.2f %%", 100*$esum->{meta_precision}), "\n",
     "## Meta-Recall              : ", sprintf("%6.2f %%", 100*$esum->{meta_recall}), "\n",
     "## Meta F                   : ", sprintf("%6.2f %%", 100*$esum->{meta_F}), "\n",
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
     "## H I                      : ", sprintf("%6.2f %%", 100*$esum->{H_I}), "\n",
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
