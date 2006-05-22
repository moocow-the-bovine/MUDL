#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::MorphEval.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile:
##    induced segmentation (morphology) evaluation
##======================================================================

package MUDL::Corpus::Profile::MorphEval;
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
##     seg1a => $seg1attr,     ##-- key(s) for 'seg1': used as $tok->attribute($key): default: 'tag'
##     seg2a => $seg2attr,     ##-- key(s) for 'seg2': used as $tok->attribute($key): default: '1'
##     enum  => $enum,         ##-- enum only wrt these targets (default=none~all tokens)
##     sep   => $sep_char,     ##-- morph separator character (default='.')
##
##     ##-- runtime data: by token
##     ntoks => $ntokens,      ##-- number of tokens
##     nmatches => $nmatches,  ##-- number of correct induced segmentations (seg1==seg2) [true positives]
##     nsegs1   => $nsegs1,    ##-- number of induced segmentations (seg1)               [true pos + false pos]
##     nsegs2   => $nsegs2,    ##-- number of gold-std segmentations (seg2)              [true pos + false neg]
##     nunk1    => $nunknown1, ##-- number of un-(seg1)mented tokens [ignored]
##     nunk2    => $nunknown2, ##-- number of un-(seg2)mented tokens [ignored]
##
##     ##-- runtime data: by type
##     wtypes => \%wtypes,       ##-- pseudo-set of word types
##     ntypes => $ntypes,        ##-- number of word types
##     nmatches_bytype => $tp,   ##-- #(seg1==seg2): [true pos]             by word-type
##     nsegs1_bytype   => $tpfp, ##-- #(seg1)      : [true pos + false pos] by word-type
##     nsegs2_bytype   => $tpfn, ##-- #(seg2)      : [true pos + false neg] by word-type
##     nunk1_bytype    => $nunk1, ##-- number of un-(seg1)mented types [ignored]
##     nunk2_bytype    => $nunk2, ##-- number of un-(seg2)mented types [ignored]
##
##     ##-- summary data
##     coverage1=>$coverage1,  ##-- seg1 coverage rate
##     coverage2=>$coverage1,  ##-- seg2 coverage rate
##     precision=>$prec,       ##-- precision: tp/(tp+fp) ~= p(seg1==seg2|seg1)
##     recall=>$recall,        ##-- recall   : tp/(tp+fn) ~= p(seg1==seg2|seg2)
##     F=>$F,
##
##     coverage1_bytype=>$coverage1,  ##-- seg1 coverage rate
##     coverage2_bytype=>$coverage1,  ##-- seg2 coverage rate
##     precision_bytype=>$pr,  ##-- precision by word-type
##     recall_bytype=>$rc,     ##-- recall by word-type
##     F_bytype=>$F,
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new(cr=>'MUDL::CorpusIO',
			       label1=>'(seg1)',
			       label2=>'(seg2)',
			       seg1a=>'tag',
			       seg2a=>'1',
			       enum=>undef,
			       sep=>'.',

			       ntoks=>0,
			       nunk1=>0,
			       nunk2=>0,

			       wtypes=>{},
			       ntypes=>0,
			       nunk1_bytype=>0,
			       nunk2_bytype=>0,
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

  my ($tok, $txt, $seg1,$seg2, $known);
  my (@segs1,@segs2,%segs1,%matches);
  $pr->{ntoks} += @$s;
  foreach $tok (@$s) {
    ##-- eval-by-target
    next if (defined($pr->{targets})
	     && defined($pr->{targeta})
	     && !defined($pr->{targets}->index($tok->attribute($pr->{targeta}))));

    $txt  = $tok->text;
    $seg1 = $tok->attribute($pr->{seg1a});
    $seg2 = $tok->attribute($pr->{seg2a});
    $pr->{wtypes}{$txt} = undef if ( ! ($known=exists($pr->{wtypes}{$txt})) );

    if (!defined($seg1) || $seg1 eq '') {
      ++$pr->{nunk1};
      ++$pr->{nunk1_bytype} if (!$known);
      if (!defined($seg2) || $seg2 eq '') {
	++$pr->{nunk2};
	++$pr->{nunk2_bytype} if (!$known);
      }
    }
    elsif (!defined($seg2) || $seg2 eq '') {
      ++$pr->{nunk2};
      ++$pr->{nunk2_bytype} if (!$known);
    }
    else {
      @segs1 = @segs2 = %segs1 = %matches = qw();

      ##-- update: get split indices
      foreach (0..(length($seg1)-1)) {
	push(@segs1, $_-@segs1) if (substr($seg1,$_,1) eq $pr->{sep});
      }
      foreach (0..(length($seg2)-1)) {
	push(@segs2, $_-@segs2) if (substr($seg2,$_,1) eq $pr->{sep});
      }

      ##-- get matches
      @segs1{@segs1} = undef;
      @matches{grep {exists($segs1{$_})} @segs2} = undef;

      $pr->{nsegs1} += @segs1;
      $pr->{nsegs2} += @segs2;
      $pr->{nmatches} += scalar(keys(%matches));

      if (!$known) {
	$pr->{nsegs1_bytype} += @segs1;
	$pr->{nsegs2_bytype} += @segs2;
	$pr->{nmatches_bytype} += scalar(keys(%matches));
      }
    }
  }

  return $pr;
}

##======================================================================
## Profiling: finish()

## undef = $profile->finish(%args)
##  + get precision, recall
sub finish {
  my $eval = shift;

  ##-- sanity check
  if (!$eval->{ntoks}) {
    carp(ref($eval), "::finish(): no tokens processed!");
    $eval->{ntoks} = -1;
  }

  $eval->{coverage1} = ($eval->{ntoks}-$eval->{nunk1}) / $eval->{ntoks};
  $eval->{coverage2} = ($eval->{ntoks}-$eval->{nunk2}) / $eval->{ntoks};
  $eval->{precision} = $eval->{nmatches} / $eval->{nsegs1};
  $eval->{recall}    = $eval->{nmatches} / $eval->{nsegs2};
  $eval->{F}         = 2.0/($eval->{precision}**-1 + $eval->{recall}**-1);

  $eval->{ntypes} = scalar(keys(%{$eval->{wtypes}}));
  $eval->{coverage1_bytype} = ($eval->{ntypes}-$eval->{nunk1_bytype}) / $eval->{ntypes};
  $eval->{coverage2_bytype} = ($eval->{ntypes}-$eval->{nunk2_bytype}) / $eval->{ntypes};
  $eval->{precision_bytype} = $eval->{nmatches_bytype} / $eval->{nsegs1_bytype};
  $eval->{recall_bytype}    = $eval->{nmatches_bytype} / $eval->{nsegs2_bytype};
  $eval->{F_bytype}         = 2.0/($eval->{precision_bytype}**-1 + $eval->{recall_bytype}**-1);

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
  return MUDL::Corpus::Profile::MorphEval::Summary->newFromEval($eval);
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
    (qq(Compare text segmentations\n)
     .qq(Options:\n)
     .qq(  label1=LABEL [default=(seg1)]\n)
     .qq(  label2=LABEL [default=(seg2)]\n)
     .qq(  seg1=ATTR    [default='tag']\n)
     .qq(  seg2=ATTR    [default='1']\n)
     .qq(  targets=ENUM [default=none (eval wrt all tokens)]\n)
     .qq(  targeta=ATTR [default=text]\n)
    );
}

########################################################################
## CLASS: MUDL::Corpus::Profile::MorphEval::Summary
########################################################################

package MUDL::Corpus::Profile::MorphEval::Summary;
our @ISA = qw(MUDL::Corpus::Profile::MorphEval);

## $obj = $obj->new()
sub new {
  my $that = shift;
  my $esum = $that->SUPER::new(@_);
  delete(@$esum{qw(enum targets targeta wtypes cr)});
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
	     qw(ntoks ntypes),

	     qw(nsegs1 nsegs2 nmatches),
	     qw(coverage1 coverage2 precision recall F),

	     qw(nsegs1_bytype nsegs2_bytype nmatches_bytype),
	     qw(coverage1_bytype coverage2_bytype precision_bytype recall_bytype F_bytype),
	    );
  @$esum{@dup} = @$eval{@dup};

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
    ("\$coverage1=$esum->{coverage1};\n",
     "\$coverage2=$esum->{coverage2};\n",
     "\$precision=$esum->{precision};\n",
     "\$recall=$esum->{recall};\n",
     "\$F=$esum->{F};\n",

     "\$coverage1_bytype=$esum->{coverage1_bytype};\n",
     "\$coverage2_bytype=$esum->{coverage2_bytype};\n",
     "\$precision_bytype=$esum->{precision_bytype};\n",
     "\$recall_bytype=$esum->{recall_bytype};\n",
     "\$F_bytype=$esum->{F_bytype};\n",

     "##", ("-" x 78), "\n",
     "## ", ref($esum), " Summary\n",
     "## Identifiers:\n",
     "##   Got   : $esum->{label1}\n",
     "##   Wanted: $esum->{label2}\n",
     "##\n",
     "## Num. Tokens              : ", sprintf("%8d", $esum->{ntoks}), "\n",
     "## Num. Types               : ", sprintf("%8d", $esum->{ntypes}), "\n",
     "##\n",

     "## Token Coverage (#1)      : ", sprintf("%6.2f %%", 100*$esum->{coverage1}), "\n",
     "## Token Coverage (#2)      : ", sprintf("%6.2f %%", 100*$esum->{coverage2}), "\n",
     "## Token Precision          : ", sprintf("%6.2f %%", 100*$esum->{precision}), "\n",
     "## Token Recall             : ", sprintf("%6.2f %%", 100*$esum->{recall}), "\n",
     "## Token F                  : ", sprintf("%6.2f %%", 100*$esum->{F}), "\n",
     "##\n",

     "## Type Coverage (#1)       : ", sprintf("%6.2f %%", 100*$esum->{coverage1_bytype}), "\n",
     "## Type Coverage (#2)       : ", sprintf("%6.2f %%", 100*$esum->{coverage2_bytype}), "\n",
     "## Type Precision           : ", sprintf("%6.2f %%", 100*$esum->{precision_bytype}), "\n",
     "## Type Recall              : ", sprintf("%6.2f %%", 100*$esum->{recall_bytype}), "\n",
     "## Type F                   : ", sprintf("%6.2f %%", 100*$esum->{F_bytype}), "\n",
     "##\n",

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
