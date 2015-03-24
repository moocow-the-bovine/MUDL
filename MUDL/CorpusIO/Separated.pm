##-*- Mode: CPerl -*-
##
## File: MUDL::CorpusIO::Separated.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpora: I/O: LOB|Brown format (1 sent per line)
##======================================================================

package MUDL::CorpusIO::Separated;
use MUDL::Object;
use MUDL::CorpusIO;
use MUDL::Token;
use MUDL::Sentence;
use IO::File;
use Encode qw(encode decode);

use strict;
use Carp;


########################################################################
## I/O : Separated : Reader
########################################################################
package MUDL::CorpusReader::Separated;
use MUDL::CorpusIO;
use MUDL::Token;
use MUDL::Sentence;
use IO::File;
use Encode qw(encode decode);

use strict;
use Carp;
our @ISA = qw(MUDL::CorpusReader::TT);

##-- new
sub new {
  my ($that,%args) = @_;
  return $that->SUPER::new(
			   tagsplit=>qr(/),
			   tagjoin=>'/',
			   commentre=>qr(^\%\%),
			   ignorewords=>{},
			   nfields=>0,           ##-- max # of fields to read
			   %args,
			  );
}

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

  my ($line,$toks,@fields,$njoin);
  my $sent = bless [], 'MUDL::Sentence';
  while (defined($line=$cr->{fh}->getline)) {
    chomp $line;
    next if (defined($cr->{commentre}) && $line =~ $cr->{commentre}); ## ?? comment format ??
    if ($line =~ /^\s*$/) {
      if (@$sent || $cr->{allow_empty_sentences} || $args{allow_empty_sentences}) {
	$cr->{nsents}++;
	return $sent;
      }
      next;
    }
    $line =~ s/^\s+//; ##-- trim leading whitespace

    @$sent = map {
      @fields = (defined($_) && !exists($cr->{ignorewords}{$_})
		 ? split($cr->{tagsplit},$_)
		 : qw());
      if ($cr->{nfields}) {
	$njoin  = $cr->{nfields} > scalar(@fields) ? scalar(@fields) : $cr->{nfields};
	$njoin  = $#fields-$njoin+1;
	@fields = (
		   grep { defined($_) && $_ ne '' }
		   join($cr->{tagjoin}, @fields[0..$njoin]),
		   @fields[($njoin+1)..$#fields]
		  );
      }
      @fields ? bless([@fields], 'MUDL::Token::TT') : qw()
    } split(/\s+/, decode($cr->{encoding},$line));
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
## I/O : Separated : Writer
########################################################################
package MUDL::CorpusWriter::Separated;
use Carp;
use Encode qw(encode decode);
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
		    encode($cw->{encoding}, (ref($_)
					     ? $_->saveSepString($cw->{tagjoin})
					     : $_))
		  } @$sent),
	     " ^\n");
}

## undef = $cw->putToken($text_or_hashref)
sub putToken {
  my ($cw,$tok) = @_;
  return undef if (!$cw->{fh});

  if (ref($tok)) {
    $cw->{fh}->print(encode($cw->{encoding},$tok->saveSepString($cw->{tagjoin})));
  } elsif (defined($tok)) {
    $cw->{fh}->print(encode($cw->{encoding},$tok), ' ');
  } else {
    $cw->{fh}->print(' ');
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

Bryan Jurish E<lt>moocow@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright (c) 2004, Bryan Jurish.  All rights reserved.

This package is free software.  You may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1)

=cut
