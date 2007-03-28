#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Filter::PdlFilter.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus filters: pdl-ized
##======================================================================

package MUDL::Corpus::Filter::PdlFilter;
use MUDL::Corpus::Filter;
use MUDL::Object qw(dummy);
use MUDL::CorpusIO;
use MUDL::Corpus::Buffer::PdlTT;
use Carp;
our @ISA = qw(MUDL::Corpus::Profile); #)

##======================================================================
## Constructors etc.

## $pf = $class_or_obj->new(%args)
##  + %args from Corpus::Filter::PdlFilter
##     buffer  => $corpus_buffer_pdltt,  ##-- buffer for generic Corpus::Filter API (may be undef)
##     bwriter => $buf_writer,           ##-- writer to buffer
##
##  + %args from Corpus::Filter
##     reader=>$corpusReader,
##     writer=>$corpusWriter,
##     allow_empty_sentences=>$bool,  ##-- default=0
##     ...

##======================================================================
## Corpus::Filter::PdlFilter : API

## $buf = $pf->getBuffer()
##  + get or create a new MUDL::Corpus::Buffer::PdlTT in $obj->{buffer}
sub getBuffer {
  $_[0]{buffer} = MUDL::Corpus::Buffer::PdlTT->new() if (!defined($_[0]{buffer}));
  return $_[0]{buffer};
}

## $bufwriter = $pf->getBufWriter();
##  + get or create a new writer $obj->{writer} for the (possibly new) buffer in $obj->{buffer}
sub getBufWriter {
  $_[0]{bwriter} = $_[0]->getBuffer()->writer() if (!defined($_[0]{bwriter}));
  return $_[0]{bwriter};
}

## undef = $pf->clearBuffer()
##  + deletes underlying buffer & its writer; called by default finish() method
sub clearBuffer { delete(@{$_[0]}{qw(buffer bwriter)}); }

## $bool = $pf->processPdlBuffer(%args)
##  + perform processing on buffered data in $pf->buffer()
##  + called by default flush() method
##    - when this method is called, the buffer (if any) has been filled and pdl-ized
##    - after this completes, the buffer (if any) is deleted
sub processPdlBuffer { 1; }


##----------------------------------------------------------------------
## fileReader(), fileWriter()

## $reader = $obj->fileReader($filename,%args)
sub fileReader {
  my $filter = shift;
  return $filter->{reader} = $filter->{reader}->fileReader(@_);
}

## $writer = $obj->fileWriter($filename,%args)
sub fileWriter {
  my $filter = shift;
  return $filter->{writer} = $filter->{writer}->fileWriter(@_);
}

##----------------------------------------------------------------------
## Corpus::Filter API

## $s_or_undef = $cf->doSentence(\@sentence)
##  + dummy method: just pushes sentence to buffer
sub doSentence {
  $_[0]{writer} = $_[0]->getBuffer()->writer() if (!defined($_[0]{writer}));
  $_[0]{writer}->putSentence($_[1]);
  return $_[1];
}

## \@sentence = $cf->processSentence(%args)
##   + dummy method: just pushes sentence to buffer
sub processSentence {
  $_[0]{writer} = $_[0]->getBuffer()->writer() if (!defined($_[0]{writer}));
  $_[0]{writer}->putSentence($_[1]);
  return $_[1];
}

## $bool = $cf->process(%args)
sub process {
  my $f = shift;

  if (!defined($f->{buffer}) && $f->{reader}->isa('MUDL::CorpusReader::Buffer::PdlTT')) {
    ##-- fast way: gank the buffer
    $f->{buffer} = $f->{reader}{buffer};
  }
  else {
    ##-- generic reader: push to buffer
    my $bw = $f->getBufWriter();
    $bw->putReader($f->{reader});
  }

  return $f;
}

## $bool = $cf->flush(%args)
##  + finish processing, maybe flushing data
sub flush {
  my $cf = shift;
  ##
  ##-- preparatory stuff
  my $buf = $cf->getBuffer();
  $buf->packPdls();
  $cf->processPdlBuffer();    ##-- child classes should override this

  if ($cf->{writer}->isa('MUDL::CorpusWriter::Buffer::PdlTT')
      &&
      $cf->{writer}{buffer}->nSentences() == 0)
    {
      ##-- easy way out: gank the filter's buffer (by value, since buffer-writer toFile() uses a closure)
      %{$cf->{writer}{buffer}} = %{$cf->{buffer}};
    }
  else {
    ##-- do it the hard (i.e. "generic") way
    $cf->{writer}->putReader($buf->reader);
  }

  ##-- cleanup
  $cf->clearBuffer();
  return $cf->{writer}->flush(@_);
}


##----------------------------------------------------------------------
## Help String

## string = $class_or_obj->helpString()
sub helpString {
  my $that = shift;
  if ((ref($that) && ref($that) eq __PACKAGE__) || $that eq __PACKAGE__) {
    return "Abstract corpus filter class for batch filters on corpus Pdls.";
  }
  return (ref($that)||$that)."::helpString() not implemented.";
}

##----------------------------------------------------------------------
## AUTOLOAD: pass to reader, writer

##-- don't autoload DESTROY
sub DESTROY { ; }

our $AUTOLOAD;
sub AUTOLOAD {
  my $f = shift;
  return undef if (!defined($f));
  (my $name = $AUTOLOAD) =~ s/.*:://; ##-- strip qualification

  my ($sub);
  if ($sub=$f->{reader}->can($name)) {
    return $sub->($f->{reader},@_);
  }
  elsif ($sub=$f->{writer}->can($name)) {
    return $sub->($f->{writer},@_);
  }
  croak( ref($f) , "::$name() not defined in ", __PACKAGE__ , "::AUTOLOAD.\n");
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
