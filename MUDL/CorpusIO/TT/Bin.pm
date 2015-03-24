##-*- Mode: CPerl -*-
##
## File: MUDL::CorpusIO::TT::Bin.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpora: I/O:
##    binary files in pseudo-TT(TnT compat) format
##======================================================================

package MUDL::CorpusIO::TT::Bin;
use MUDL::Object;
use MUDL::CorpusIO;
use MUDL::CorpusIO::TT;
use MUDL::Token;
use MUDL::Sentence;
use IO::File;
#use Encode qw(encode decode);

use strict;
use Carp;


########################################################################
## I/O : TT::Bin : Reader
########################################################################
package MUDL::CorpusReader::TT::Bin;
use MUDL::CorpusIO;
use MUDL::Token;
use MUDL::Sentence;
use IO::File;
#use Encode qw(encode decode);
use strict;
use Carp;
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusReader::TT);

sub new {
  my ($that,%args) = @_;
  my $self = bless {
		    #allow_empty_sentences=>0,
		    nsents=>0,
		    ntoks=>0,
		    #encoding=>'UTF-8', ##-- binary always uses utf8
		    %args,
		   }, ref($that)||$that;
  return $self;
}

sub DESTROY {
  my $cr = shift;
  $cr->{fh}->close if (defined($cr->{fh}));
}

## $n = nSentences()
# (inherited)

## $n = $cr->nTokens()
# (inheritd)

## reset()
# (inherited)

## $bool = $cr->eof
# (inherited)

## undef = $cr->fromString($str)
# (inherited)

## undef = $cr->fromFile($filename)
## undef = $cr->fromFh($fh)
# (inherited)

## \@sentence_or_undef = $cr->getSentence();
sub getSentence {
  my $cr = shift;
  my ($buf,$slen,$s);
  return undef if (!$cr->{fh} || $cr->{fh}->eof || $cr->{fh}->read($buf,4) != 4);
  $slen = unpack('L',$buf);

  confess(ref($cr), "::getSentence(): inconsistency detected: cannot fill sentence buffer")
    if ($cr->{fh}->read($buf,$slen) != $slen);

  $s = bless [
	      map {
		bless [ grep {utf8::decode($_)||1} unpack('(Z*)*', $_) ], 'MUDL::Token::TT'
	      } unpack('(S/a*)*', $buf)
	     ], 'MUDL::Sentence';

  ++$cr->{nsents};
  $cr->{ntoks} += scalar(@$s);

  return $s;
}

## \%token_or_undef = $cr->getToken();
# (not implemented)
*getToken = dummy('getToken');

########################################################################
## I/O : TT : Bin: Writer
########################################################################
package MUDL::CorpusWriter::TT::Bin;
#use Encode qw(encode decode);
use Carp;
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusWriter::TT);


## $cw = class->new(%args)
##   + known %args:
##      #layers => \@binmode_layer_flags
sub new {
  my ($that,%args) = @_;
  #my $cw = bless { layers=>[qw(:utf8)], %args }, ref($that)||$that;
  my $cw = bless {
		  layers=>[],
		  #encoding=>'UTF-8',
		  %args
		 }, ref($that)||$that;
  return $cw;
}

## DESTROY
# (inherited)

## $bool = $cw->flush
# (inherited)

## undef = $cw->toString(\$str)
# (inherited: ?!)

## undef = $cw->toFile($filename_or_fh)
# (inherited)

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$sent) = @_;
  return undef if (!defined($cw->{fh}));

  my $fh = $cw->{fh};
  my ($tok);
  my $buf = pack('(S/a*)*',
		 map {
		   pack('(Z*)*',
			grep {utf8::upgrade($_)||1}
			(map {defined($_) ? $_ : ''}
			 (UNIVERSAL::isa($_,'MUDL::Token::TT')
			  ? @$_
			  : (ref($tok=$_)
			     ? ($tok->text,$tok->tag,map {$tok->attribute($_)} (sort $tok->attributeNames))
			     : $tok))))
		 } @$sent);
  $fh->print(pack('L', length($buf)), $buf);
}

## undef = $cw->putToken($text_or_hashref)
*putToken = dummy('putToken');


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
