#-*- Mode: CPerl -*-

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
##
##     ##-- runtime data
##     enum  => $enum,         ##-- enum only wrt these targets (default=none~all tokens)
##     ntoks => $ntokens,
##     jdist => $dist,         ##-- MUDL::Dist::Nary, join tag1+tag2 counts
##     txts  => \%txt2undef,   ##-- word type pseudo-set
##     txttag1 => \%txttag1,   ##-- tok,tag1 pair pseudo-set
##     txttag2 => \%txttag2,   ##-- tok,tag2 pair pseudo-set
##
##     ##-- on finish()
##     tag12m => $map12,       ##-- map $tag1=>$tag2,
##     tag21m => $map21,       ##-- map $tag2=>$tag1,
##     tag12b => $dist12,      ##-- $tag1=>$best_tag12_count,
##     tag21b => $dist21,      ##-- $tag2=>$best_tag21_count,
##
##     ##-- ambiguity data
##     ntypes => $n,           ##-- number of word types
##     nanals1 => $n1,         ##-- total number of (type,tag1) pairs
##     nanals2 => $n2,         ##-- total number of (type,tag2) pairs
##
##     ##-- summary data
##     precision=>$prec,       ##-- p(best(tag2|tag1)|tag1)
##     recall=>$recall,        ##-- p(best(tag1|tag2)|tag2)
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
			       do_ambig=>1,
			       %args);
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

  ##-- get best maps
  my $tag12m = $eval->{tag12m} = MUDL::Map->new();
  my $tag21m = $eval->{tag21m} = MUDL::Map->new();
  my $tag12b = $eval->{tag12b} = MUDL::Dist->new();
  my $tag21b = $eval->{tag21b} = MUDL::Dist->new();

  my ($tag12,$f12, $tag1,$tag2, $tb);
  while (($tag12,$f12)=each(%{$eval->{jdist}{nz}})) {
    ($tag1,$tag2) = CORE::split(/\t+/,$tag12,2);

    next if ($tag1 eq '@UNKNOWN'); ##-- unknowns are always bad

    $tb = $tag12b->{$tag1};
    if (!$tb || $f12 > $tb) {
      $tag12b->{$tag1} = $f12;
      $tag12m->{$tag1} = $tag2;
    }

    $tb = $tag21b->{$tag2};
    if (!$tb || $f12 > $tb) {
      $tag21b->{$tag2} = $f12;
      $tag21m->{$tag2} = $tag1;
    }
  }

  my $precision = 0;
  $precision += $_ foreach (values(%$tag12b));
  $eval->{precision} = $precision / $eval->{ntoks};

  my $recall = 0;
  $recall += $_ foreach (values(%$tag21b));
  $eval->{recall} = $recall / $eval->{ntoks};


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## tag-wise precision, recall (Sch"utze-style)
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  my $g2i   = $eval->{g2i}   = {}; ##-- $tag2 => [ $tag1 : bestmatch($tag1) = $tag2 ] = bestmatch^{-1}($tag2)
  my $t2nc  = $eval->{t2nc}  = {}; ##-- $tag2 => | bestmatch^{-1}($tag2) |

  ##-- best-tag sets (best gold-tag(2) for each induced-tag(1), indexed by gold-tag)
  while (($tag1,$tag2)=each(%$tag12m)) {
    $g2i->{$tag2} = [] if (!defined($g2i->{$tag2}));
    push(@{$g2i->{$tag2}}, $tag1);

    $t2nc->{$tag2}++;
  }

  my $t2f   = $eval->{t2f}   = {}; ##-- $tag2 => f($tag2)
  my $t2cor = $eval->{t2cor} = {}; ##-- $tag2 => \sum_{$tag1 : bestmatch($tag1)==$tag2} f($tag1, $tag2)
  my $t2inc = $eval->{t2inc} = {}; ##-- $tag2 => \sum_{$tag1 : bestmatch($tag1)==$tag2} f($tag1,!$tag2)

  my ($besttag2);
  while (($tag12,$f12)=each(%{$eval->{jdist}{nz}})) {
    ($tag1,$tag2) = CORE::split(/\t+/,$tag12,2);
    $t2f->{$tag2} += $f12;

    ##-- ensure everything is defined
    $t2f->{$tag2}  = 0 if (!defined($t2f->{$tag2}));
    $t2nc->{$tag2} = 0 if (!defined($t2nc->{$tag2}));
    $t2cor->{$tag2} = 0 if (!defined($t2cor->{$tag2}));
    $t2inc->{$tag2} = 0 if (!defined($t2inc->{$tag2}));

    $besttag2 = $tag12m->{$tag1};
    $besttag2 = '' if (!defined($besttag2));
    if ($besttag2 eq $tag2) {
      $t2cor->{$tag2} += $f12;
    } else {
      $t2inc->{$besttag2} += $f12;
    }
  }

  ##-- precision, recall (by tag)
  my $t2pr  = $eval->{t2pr}  = {}; ##-- $tag2 => precision($tag2) = correct($tag2) / (correct($tag2)+incorrect($tag2))
  my $t2rc  = $eval->{t2rc}  = {}; ##-- $tag2 => recall($tag2)    = correct($tag2) / f($tag2)
  my $t2F   = $eval->{t2F}   = {}; ##-- $tag2 => F($tag2)         = 1/( .5*1/$p + .5*1/$r ) = 2/($p**-1 + $r**-1)
  my ($ftag2,$ncor,$ninc, $avg_pr,$avg_rc, $total_cor,$total_inc,$total_f);
  while (($tag2,$ftag2)=each(%$t2f)) {
    $ncor = $t2cor->{$tag2};
    $ninc = $t2inc->{$tag2};

    $t2pr->{$tag2} = ($ncor+$ninc ? ($ncor / ($ncor + $ninc)) : 0);
    $t2rc->{$tag2} = ($ftag2      ? ($ncor / $ftag2)          : 0);
    $t2F->{$tag2}  = 2/($t2pr->{$tag2}**-1 + $t2rc->{$tag2}**-1);

    $avg_pr += $t2pr->{$tag2};
    $avg_rc += $t2rc->{$tag2};

    $total_cor += $ncor;
    $total_inc += $ninc;
    $total_f   += $ftag2;
  }
  $avg_pr /= scalar(keys(%$t2f));
  $avg_rc /= scalar(keys(%$t2f));
  @$eval{qw(avg_precision avg_recall avg_F)} = ($avg_pr, $avg_rc, 2/($avg_pr**-1 + $avg_rc**-1));
  $eval->{total_precision} = $total_cor / ($total_cor + $total_inc);
  $eval->{total_recall}    = $total_cor / ($total_cor + $total_f);
  $eval->{total_F}         = 2/($eval->{total_precision}**-1 + $eval->{total_recall}**-1);


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Ambiguity rates
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ($eval->{do_ambig}) {
    my $ntypes  = scalar(keys(%{$eval->{txt}}));
    my $nanals1 = scalar(keys(%{$eval->{txttag1}}));
    my $nanals2 = scalar(keys(%{$eval->{txttag2}}));

    ##-- store ambiguity data
    @$eval{qw(ntypes nanals1 nanals2)} = ($ntypes,$nanals1,$nanals2);
  }

  ##-- reset file reader
  $eval->reset();

  return $eval;
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
	     qw(precision recall),
	     qw(avg_precision avg_recall avg_F),
	     qw(total_precision total_recall total_F),
	     qw(ntoks ntypes nanals1 nanals2),

	     'tag12m', ##-- $tag1=>$best_tag2_for_tag1,
	     'tag21m', ##-- $tag2=>$best_tag1_for_tag2,

	     #'g2i',   ##-- $tag2=> [ $tag1 : bestmatch($tag1) == $tag2 ]
	     #'t2f',   ##-- $tag2=>$freq{$tag2}
	     #'t2nc',  ##-- $tag2=>$nclasses{$tag2}
	     #'t2cor', ##-- $tag2=>$ncorrect{$tag2}
	     #'t2inc', ##-- $tag2=>$nincorrect{$tag2},
	     #'t2pr',  ##-- $tag2=>$precision{$tag2},
	     #'t2rc',  ##-- $tag2=>$recall{$tag2},
	     #'t2F',   ##-- $tag2=>$F{$tag2} == 1/( .5*1/$pr + .5*1/$rc ) == 2/($pr**-1 + $rc**-1)
	    );
  @$esum{@dup} = @$eval{@dup};

  ##-- generate new keys: ambiguity info
  my ($tag2);
  $esum->{tag2info} = {
		       map {
			 $_=>{
			      (
			       freq=>$eval->{t2f}{$_},
			       nclasses=>$eval->{t2nc}{$_},
			       correct=>$eval->{t2cor}{$_},
			       incorrect=>$eval->{t2inc}{$_},
			       pr=>$eval->{t2pr}{$_},
			       rc=>$eval->{t2rc}{$_},
			       F=>$eval->{t2F}{$_},
			      )
			     }
		       } keys(%{$eval->{t2f}})
		      };

  ##-- generate new keys: meta-F
  $esum->{F} = 2.0/($eval->{precision}**-1 + $eval->{recall}**-1);

  ##-- generate new keys: ambiguity rates
  $esum->{arate1} = $eval->{nanals1}/$eval->{ntypes} if ($eval->{ntypes});
  $esum->{arate2} = $eval->{nanals2}/$eval->{ntypes} if ($eval->{ntypes});

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
     "\$avg_precision=$esum->{avg_precision};\n",
     "\$avg_recall=$esum->{avg_recall};\n",
     "\$total_precision=$esum->{total_precision};\n",
     "\$total_recall=$esum->{total_recall};\n",

     (defined($esum->{ntypes})  ? "\$ntypes=$esum->{ntypes};\n" : qw()),
     (defined($esum->{nanals1}) ? "\$nanals1=$esum->{nanals1};\n" : qw()),
     (defined($esum->{nanals2}) ? "\$nanals2=$esum->{nanals2};\n" : qw()),

     "\$tag2info={\n",
     (map {
       (sprintf(" %8s=>{", "\'$_\'"),
	join(', ',
	     sprintf("freq=>%5d", $esum->{tag2info}{$_}{freq}),
	     sprintf("nclasses=>%3d", $esum->{tag2info}{$_}{nclasses}),
	     sprintf("correct=>%5d", $esum->{tag2info}{$_}{correct}),
	     sprintf("incorrect=>%5d", $esum->{tag2info}{$_}{incorrect}),
	     sprintf("pr=>%0.4f", $esum->{tag2info}{$_}{pr}),
	     sprintf("rc=>%0.4f", $esum->{tag2info}{$_}{rc}),
	     sprintf("F =>%0.4f", $esum->{tag2info}{$_}{F}),
	    ),
	"},\n")
     } sort(keys(%{$esum->{tag2info}}))),
     "  };\n",
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

     "## Meta-Precision           : ", sprintf("%6.2f %%", 100*$esum->{precision}), "\n",
     "## Meta-Recall              : ", sprintf("%6.2f %%", 100*$esum->{recall}), "\n",
     "## Meta F                   : ", sprintf("%6.2f %%", 100*$esum->{F}), "\n",
     "##\n",
     "## Avg tag2-Precision       : ", sprintf("%6.2f %%", 100*$esum->{avg_precision}), "\n",
     "## Avg tag2-Recall          : ", sprintf("%6.2f %%", 100*$esum->{avg_recall}), "\n",
     "## Avg F                    : ", sprintf("%6.2f %%", 100*$esum->{avg_F}), "\n",
     "##\n",
     "## Total Precision          : ", sprintf("%6.2f %%", 100*$esum->{total_precision}), "\n",
     "## Total Recall             : ", sprintf("%6.2f %%", 100*$esum->{total_recall}), "\n",
     "## Total F                  : ", sprintf("%6.2f %%", 100*$esum->{total_F}), "\n",
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
