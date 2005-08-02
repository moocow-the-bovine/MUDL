##-*- Mode: Perl -*-
##
## File: MUDL::CorpusIO::LOB.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpora: I/O: LOB format
##======================================================================

package MUDL::CorpusIO::LOB;
use MUDL::Object;
use MUDL::CorpusIO;
use MUDL::Token;
use MUDL::Sentence;
use IO::File;

use strict;
use Carp;


########################################################################
## I/O : LOB : Reader
########################################################################
package MUDL::CorpusReader::LOB;
use MUDL::CorpusIO;
use MUDL::Token;
use MUDL::Sentence;
use IO::File;

use strict;
use Carp;
our @ISA = qw(MUDL::CorpusReader::TT);

##-- new: inherited

##-- DESTROY: inherited

## $n = nSentences()
##  + inherited

## $n = $cr->nTokens()
##  + inherited

## reset()
##  + inherited

## $bool = $cr->eof
##  + inherited

## undef = $cr->fromString($str)
##  + inherited

## undef = $cr->fromFile($filename)
## undef = $cr->fromFh($fh)
##  + inherited

## \@sentence_or_undef = $cr->getSentence();
sub getSentence {
  my ($cr,%args) = @_;
  return undef if (!$cr->{fh} || $cr->{fh}->eof);
  $args{allow_empty_sentences}=0 if (!exists($args{allow_empty_sentences}));

  my ($line,$toks);
  my $sent = bless [], 'MUDL::Sentence';
  while (defined($line=$cr->{fh}->getline)) {
    chomp $line;
    next if ($line =~ /^\%\%/); ## ?? LOB comment format ??
    if ($line =~ /^\s*$/) {
      if (@$sent || $args{allow_empty_sentences}) {
	$cr->{nsents}++;
	return $sent;
      }
      next;
    }
    @$sent = map {
      (defined($_) && $_ ne '^'
       #? bless([split(/\_/, 2)], 'MUDL::Token::TT')
       ? bless([split(/\_/)], 'MUDL::Token::TT')
       : qw())
    } split(/\s+/, $line);
    $cr->{ntoks} += @$sent;
    last;
  }
  if (@$sent || $args{allow_empty_sentences}) {
    $cr->{nsents}++;
    return $sent;
  }
  return undef;
}

## \%token_or_undef = $cr->getToken();
##   + not implemented
*getToken = MUDL::Object::dummy('getToken');


########################################################################
## I/O : LOB : Writer
########################################################################
package MUDL::CorpusWriter::LOB;
use Carp;
use strict;
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusWriter::TT);


## $cw = class->new(%args)
##   + known %args:
##      layers => \@binmode_layer_flags
##   + inherited

## $obj->DESTROY
##   + inherited

## $bool = $cw->flush
##   + inherited

## undef = $cw->toString(\$str)
##   + inherited

## undef = $cw->toFile($filename_or_fh)
##   + inherited

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$sent) = @_;
  return undef if (!$cw->{fh});

  my $fh = $cw->{fh};
  $fh->print(join(' ',
		  map {
		    (ref($_)
		     ? $_->saveLobString
		     : $_)
		  } @$sent),
	     " ^\n");
}

## undef = $cw->putToken($text_or_hashref)
sub putToken {
  my ($cw,$tok) = @_;
  return undef if (!$cw->{fh});

  if (ref($tok)) {
    $cw->{fh}->print($tok->saveLobString);
  } elsif (defined($tok)) {
    $cw->{fh}->print($tok, ' ');
  } else {
    $cw->{fh}->print(" ");
  }
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
