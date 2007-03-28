#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Profile::PdlProfile.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus profiles: pdl-ized
##======================================================================

package MUDL::Corpus::Profile::PdlProfile;
use MUDL::Corpus::Profile;
use MUDL::CorpusIO;
use MUDL::Corpus::Buffer::PdlTT;
#use PDL;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile); #)

##======================================================================
## Constructors etc.

## $obj = $class_or_obj->new(%args)
##  + %args for Corpus::Profile::PdlProfile:
##     buffer => $corpus_buffer_pdltt,  ##-- buffer for generic Corpus::Profile API
##     writer => $bufwriter,            ##-- buffer writer


##======================================================================
## Corpus::Profile::PdlProfile : API

## $buf = $pdlprof->getBuffer()
##  + get or create a new MUDL::Corpus::Buffer::PdlTT in $obj->{buffer}
sub getBuffer {
  $_[0]{buffer} = MUDL::Corpus::Buffer::PdlTT->new() if (!defined($_[0]{buffer}));
  return $_[0]{buffer};
}

## $bufwriter = $pdlprof->getWriter();
##  + get or create a new writer $obj->{writer} for the (possibly new) buffer in $obj->{buffer}
sub getWriter {
  $_[0]{writer} = $_[0]->getBuffer()->writer() if (!defined($_[0]{writer}));
  return $_[0]{writer};
}

## undef = $pdlprof->clearBuffer()
##  + deletes underlying buffer & its writer; called by default finish() method
sub clearBuffer { delete(@{$_[0]}{qw(buffer writer)}); }

## undef = $profile->finishPdlProfile(%args)
##  + perform pdl-sensitive finishing actions
##  + called by default finish() method
##    - when this method is called, the buffer (if any) has been filled and pdl-ized
##    - after this completes, the buffer (if any) is deleted
sub finishPdlProfile { ; }

##======================================================================
## Corpus::Profile: API

## undef = $profile->addBuffer($buffer,@args)
##  + may adopt $buffer; otherwise calls addReader()
sub addBuffer {
  if (!defined($_[0]{buffer}) && $_[1]->isa('MUDL::Corpus::Buffer::PdlTT')) {
    $_[0]{buffer} = $_[1];
    return $_[0];
  }
  return $_[0]->addReader($_[1]->reader(), @_[2..$#_]);
}

## undef = $profile->addReader($CorpusReader,@args)
##  + may adopt $CorpusReader->{buffer}; otherwise calls addSentence($s) for every sentence
sub addReader {
  if (!defined($_[0]{buffer}) && $_[1]->isa('MUDL::CorpusReader::Buffer::PdlTT')) {
    $_[0]{buffer} = $_[1]{buffer};
    return $_[0];
  }
  my ($pr,$cr) = splice(@_,0,2);
  my ($s);
  $pr->addSentence($s,@_) while (defined($s=$cr->getSentence));
}

## undef = $profile->addSentence(\@sentence)
##  + default implementation just adds sentence to a (possibly new) PdlTT buffer
sub addSentence {
  $_[0]{writer} = $_[0]->getBuffer()->writer() if (!defined($_[0]{writer}));
  $_[0]{writer}->putSentence($_[1]);
}

## undef = $profile->finish(%args)
##  + perform any finishing actions
##  + default implementation pdl-izes buffer, calls finishPdlProfile(), then clears the buffer
sub finish {
  return undef if (!defined($_[0]{buffer})); ##-- sanity check
  $_[0]{buffer}->packPdls();                 ##-- pdl-ize the buffer
  my $rc = $_[0]->finishPdlProfile();        ##-- subclass hook
  $_[0]->clearBuffer();                      ##-- chuck the buffer
  return $rc;
}

## string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  if ((ref($that) && ref($that) eq __PACKAGE__) || $that eq __PACKAGE__) {
    return "Abstract corpus profiling plugin host class for PDL-ized profiles";
  }
  return (ref($that)||$that)."::helpString() not implemented.";
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
