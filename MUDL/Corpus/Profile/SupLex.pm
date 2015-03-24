#-*- Mode: Perl -*-

## File: MUDL::Corpus::Profile::SupLex.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpus profile: supervised lexicon
##======================================================================

package MUDL::Corpus::Profile::SupLex;
use MUDL::Corpus::Profile;
use MUDL::Dist::Nary;
use Carp;
our @ISA = qw(MUDL::Dist::Nary MUDL::Corpus::Profile);

##======================================================================
## $obj = $class_or_obj->new(%args)
##  + %args:
##     from=>$field, ##-- one of 'text','tag': method name for MUDL::Token
##     to=>$field,   ##-- one of 'text','tag': method name for MUDL::Token
sub new {
  my ($that,%args) = @_;
  my $self = $that->SUPER::new(from=>'text',
			       to=>'tag',
			       nfields=>2,
			       %args);
  return $self;
}

##======================================================================
## Profiling

## undef = $profile->addSentence(\@sentence)
sub addSentence {
  my ($pr,$s) = @_;
  my ($fromsub,$tosub);
  foreach my $tok (@$s) {
    $fromsub = $tok->can($pr->{from});
    $tosub   = $tok->can($pr->{to});
    ++$pr->{nz}{join($pr->{sep}, $fromsub->($tok), $tosub->($tok))};
  }
  return $pr;
}

##======================================================================
## Evaluation: Unigram modelling

## undef = $pr->finish()
#sub finish {
#  my $pr = shift;
#  $pr->{ugmap} = $pr->toBestMap([0],[1]);
#  return $pr;
#}


##======================================================================
## I/O : info (save only)

## $bool = $obj->saveInfoFh($fh,%args)
sub saveInfoFh {
  my ($lex,$fh,%args) = @_;
  my $name = (defined($args{name})
	      ? $args{name}
	      : (defined($lex->{name})
		 ? $lex->{name}
		 : '--unnamed--'));

  my $total = $lex->total;
  my $lexH  = $lex->entropy($total);

  my $tokp = $lex->project1(0);
  my $tagp = $lex->project1(1);

  my $tokpH = $tokp->entropy($total);
  my $tagpH = $tagp->entropy($total);

  my $mi = $lex->mutualInformation([0],[1]);

  $fh->print
    ("\n",
     "%%", ('-' x 70), "\n",
     "%% ", ref($lex), " Info ($name):\n",
     "%% + P(tok,tag)\n",
     "%%    H / P = ", sprintf("%7.2f\t/\t%10.2f", $lexH, 2**$lexH), "\n",
     "%%    I     = ", sprintf("%7.2f", $mi), "\n",
     "%%\n",
     "%% + P(tag|tok)\n",
     "%%    H / P = ", sprintf("%7.2f\t/\t%10.2f", $lexH-$tokpH, 2**($lexH-$tokpH)), "\n",
     "%%\n",
     "%% + P(tok|tag)\n",
     "%%    H / P = ", sprintf("%7.2f\t/\t%10.2f", $lexH-$tagpH, 2**($lexH-$tagpH)), "\n",
     "%%", ('-' x 70), "\n");

  return 1;
}

__PACKAGE__->registerIOMode('info',{saveFh=>'saveInfoFh'});
__PACKAGE__->registerFileSuffix('.info', 'info');

##======================================================================
## Help

## $string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  return
    (qq(Extract supervised unigrams (token,tag) pairs.\n));
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
