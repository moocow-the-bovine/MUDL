##-*- Mode: CPerl -*-
##
## File: MUDL::CorpusIO::LOB.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpora: I/O: LOB format
##======================================================================

package MUDL::CorpusIO::LOB;
use strict;
use Carp;


########################################################################
## I/O : LOB : Reader
########################################################################
package MUDL::CorpusReader::LOB;
use MUDL::CorpusIO::Separated;

use strict;
use Carp;
our @ISA = qw(MUDL::CorpusReader::Separated);

##-- new
sub new {
  my ($that,%args) = @_;
  return $that->SUPER::new(
			   tagsplit=>qr(_),
			   tagjoin=>'_',
			   commentre=>qr(^%%),         ##-- LOB comment string?
			   ignorewords=>{'^'=>undef},  ##-- ignorable words
			   %args
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
##  + inherited

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
our @ISA = qw(MUDL::CorpusWriter::Separated);


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
##  + inherited

## undef = $cw->putToken($text_or_hashref)
##  + inherited


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
