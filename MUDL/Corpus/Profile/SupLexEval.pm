#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::SupLexEval.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: supervised lexicon evaluation
##======================================================================

package MUDL::Corpus::Profile::SupLexEval;
use MUDL::Corpus::Profile;
use MUDL::Corpus::Profile::SupLex;
use MUDL::Map;
use MUDL::Object;
our @ISA = qw(MUDL::Corpus::Profile);

##======================================================================
## $obj = $class_or_obj->new(%args)
##  + %args:
##      lex=>$dist_nary (maps token text to tag)
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new(
			       lex=>MUDL::Corpus::Profile::SupLex->new(),
			       tok2tag=>{matched=>0,unknown=>0},
			       tag2tok=>{matched=>0,unknown=>0},
			       total=>0,
			       %args
			      );

  $self->{tok2tagMap} = $self->{lex}->toBestMap([0],[1]);
  $self->{tag2tokMap} = $self->{lex}->toBestMap([1],[0]);

  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($pr,$s) = @_;

  my ($toktext,$toktag,$mtag,$mtext);
  foreach $tok (@$s) {
    $toktext = $tok->text;
    $toktag  = $tok->tag;
    ++$pr->{total};

    if (defined($mtag=$pr->{tok2tagMap}{$toktext})) {
      ++$pr->{tok2tag}{matched} if ($mtag eq $toktag);
    } else {
      ++$pr->{tok2tag}{unknown};
    }

    if (defined($mtext=$pr->{tag2tokMap}{$toktag})) {
      ++$pr->{tag2tok}{matched} if ($mtext eq $toktext);
    } else {
      ++$pr->{tag2tok}{unknown};
    }
  }

  return $pr;
}

##======================================================================
## Evaluation: Unigram modelling

## ($precision,$recall) = $pr->evaluate()
##   + new
*finish = \&evaluate;
sub evaluate {
  my $pr = shift;
  $pr->{ptok2tag} = $pr->evaluateKnown('tok2tag');
  $pr->{ptag2tok} = $pr->evaluateKnown('tag2tok');
  return @$pr{qw(ptok2tag ptag2tok)};
}


## $accuracy = $pr->evaluateKnown($distNamePrefix)
sub evaluateKnown {
  my ($pr,$dname) = @_;
  my $info = $pr->{$dname}
    or confess(ref($pr),"::evaluateKnown(): no element named '$dname!");

  my $total = $pr->{total} - $info->{unknown};
  return 0 if (!$total);

  return $info->{matched} / $total;
}


##======================================================================
## I/O: native (save only)

## $bool = $obj->saveNativeFh($fh,%args)
sub saveNativeFh {
  my ($pr,$fh,%args) = @_;

  my $name = (defined($args{name})
	      ? $args{name}
	      : (defined($pr->{name})
		 ? $pr->{name}
		 : '--unnamed--'));

  $fh->print
    ("\n",
     "%%", ('-' x 70), "\n",
     "%% ", ref($pr), " Eval ($name):\n",
     "%%\n",
     "%%   p(tag|tok) [Precision] : ", sprintf("%6.2f%%", 100*$pr->{ptok2tag}), "\n",
     "%%   p(tok|tag) [Recall]    : ", sprintf("%6.2f%%", 100*$pr->{ptag2tok}), "\n",
     "%%", ('-' x 70), "\n",
    );
}


##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Evaluate supervised lexica\n)
     .qq(Options:\n)
     .qq(  lex=SupLex [default=empty])
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
