#-*- Mode: Perl -*-

## File: MUDL::Corpus.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpora
##======================================================================

package MUDL::Corpus;
use MUDL::XML;
use IO::File;
use Carp;

our $VERSION = 0.01;

#our @ISA = qw(MUDL::XML::Object);

##======================================================================
## Constructor
sub new {
  my $that = shift;
  return bless { sents=>[] }, ref($that)||$that;
}

##======================================================================
## accessors

## \@sentences = $corpus->sentences()
sub sentences { return $_[0]->{sents}; }

## $corpus = $corpus->clear()
sub clear {
  my $c = shift;
  @{$c->{sents}} = qw();
  return $c;
}

##======================================================================
## I/O: TT

## $corpus = $corpus->loadTTFile($filename_or_fh)
sub loadTTFile {
  my ($c,$file,%args) = @_;
  my $cr = MUDL::CorpusReader::TT->new(%args);
  $cr->fromFile($file);
  my $sents = $c->{sents};
  my $sent;
  while (defined($sent=$cr->getSentence(%args))) {
    push(@{$c->{sents}}, $sent);
  }
  return $c;
}
## $corpus = $corpus->saveTTFile($filename_or_fh)
sub saveTTFile {
  my ($c,$file,%args) = @_;
  my $cw = MUDL::CorpusWriter::TT->new(%args);
  $cw->toFile($file);
  my $sents = $c->{sents};
  foreach $sent (@{$c->{sents}}) {
    $cw->putSentence($sent);
  }
  return $c;
}

##======================================================================
## I/O: XML

## $corpus = $corpus->loadXMLFile($filename_or_fh)
sub loadXMLFile {
  my ($c,$file,%args) = @_;
  my $cr = MUDL::CorpusReader::XML->new(%args);
  $cr->fromFile($file);
  my $sents = $c->{sents};
  my $sent;
  while (defined($sent=$cr->getSentence(%args))) {
    push(@{$c->{sents}}, $sent);
  }
  return $c;
}
## $corpus = $corpus->saveXMLFile($filename_or_fh)
sub saveXMLFile {
  my ($c,$file,%args) = @_;
  my $cw = MUDL::CorpusWriter::XML->new(%args);
  $cw->toFile($file);
  my $sents = $c->{sents};
  foreach $sent (@{$c->{sents}}) {
    $cw->putSentence($sent);
  }
  return $c;
}


##======================================================================
## I/O : Abstract: CorpusIO
##======================================================================
package MUDL::CorpusIO;
use File::Basename;
use Carp;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(dummy);

sub new {
  my $that = shift;
  return bless {}, ref($that)||$that;
}
## \&dummy_sub = dummy($name)
sub dummy {
  my $name = shift;
  return sub { croak( __PACKAGE__ , "::${name}(): not implemented."); };
}

## $cr = $class_or_object->fileReader($filename,%args)
##  + new reader for $filename
##  + $filename may be prefixed with 'fmt:'
our %FORMATS =
  (xml => 'XML',
   ttt  => 'TT',
   tt  => 'TT',
   t => 'TT',
   tnt => 'TT',
   DEFAULT => 'XML',
  );
sub formatReader {
  my ($that,$file,@args) = @_;
  my $class = "MUDL::CorpusReader::";
  my $fmt = 'DEFAULT';
  if ($file =~ s/^(\S+)://) {
    $fmt = $1;
  } else {
    foreach (keys(%FORMATS)) {
      if ("\L$file\E" =~ /\.$_$/) {
	$fmt = $_;
	last;
      }
    }
  }
  $class .= $FORMATS{$fmt};
  my $obj;
  if (ref($that) && UNIVERSAL::isa($that,$class)) {
    $obj = $that;
  } else {
    $obj = $class->new(@args);
  }
  $obj->fromFile($file);
  return $obj;
}

## $cr = $class_or_object->fileWriter($filename,%args)
##  + new writer for $filename
##  + $filename may be prefixed with '${FORMAT}:'
sub formatWriter {
  my ($that,$file,@args) = @_;
  my $class = "MUDL::CorpusWriter::";
  my $fmt = 'DEFAULT';
  if ($file =~ s/^(\S+)://) {
    $fmt = $1;
  }
  else {
    foreach (keys(%FORMATS)) {
      if ("\L$file\E" =~ /\.$_$/) {
	$fmt = $_;
	last;
      }
    }
  }
  $class .= $FORMATS{$fmt};
  my $obj;
  if (ref($that) && UNIVERSAL::isa($that,$class)) {
    $obj = $that;
  } else {
    $obj = $class->new(@args);
  }
  $obj->toFile($file);
  return $obj;
}



##======================================================================
## I/O : Abstract: Corpus Reader
##======================================================================
package MUDL::CorpusReader;
use Carp;
our @ISA = qw(MUDL::CorpusIO);
MUDL::CorpusIO->import('dummy');

## $bool = $cr->eof
*eof = dummy('eof');

## \@sentence = $cr->getSentence();
*getSentence = dummy('getSentence');

## \%token_or_undef = $cr->getToken();
*getToken = dummy('getToken');

## undef = $cr->fromString($string)
*fromString = dummy('fromString');

## undef = $cr->fromFile($filename_or_fh);
*fromFile = dummy('fromFile');

##======================================================================
## I/O : Abstract: Corpus Writer
##======================================================================
package MUDL::CorpusWriter;
use Carp;
MUDL::CorpusIO->import('dummy');
our @ISA = qw(MUDL::CorpusIO);

## $bool = $cw->flush
*flush = dummy('flush');

## undef = $cw->putSentence(\@sent);
*putSentence = dummy('putSentence');

## undef = $cr->putToken($text_or_hashref);
*putToken = dummy('putToken');

## undef = $cr->toString(\$string)
*toString = dummy('toString');

## undef = $cr->toFile($filename_or_fh);
*toFile = dummy('toFile');

##======================================================================
## I/O : TT : Reader
##======================================================================
package MUDL::CorpusReader::TT;
use Carp;
MUDL::CorpusIO->import('dummy');
our @ISA = qw(MUDL::CorpusReader);

sub DESTROY {
  my $cr = shift;
  $cr->{fh}->close if (defined($cr->{fh}));
}

## $bool = $cr->eof
sub eof { return !$_[0]->{fh} || $_[0]->{fh}->eof; }

## undef = $cr->fromString($str)
sub fromString {
  my ($cr,$string) = @_;
  $cr->{fh}->close() if (defined($cr->{fh}));
  $cr->{fh} = IO::Scalar->new(\$string)
    or croak( __PACKAGE__ , "::fromString(): open failed: $!");
}

## undef = $cr->fromFile($filename_or_fh)
sub fromFile {
  my ($cr,$file) = @_;
  $cr->{fh}->close() if (defined($cr->{fh}));
  $cr->{fh} = ref($file) ? $file : IO::File->new("<$file");
  croak( __PACKAGE__ , "::fromFile(): open failed for '$file': $!") if (!$cr->{fh});
}

## \@sentence_or_undef = $cr->getSentence();
sub getSentence {
  my ($cr,%args) = @_;
  return undef if (!$cr->{fh} || $cr->{fh}->eof);
  %args = (allow_empty_sentences=>0,%args);

  my ($line,$text,@details);
  my @sent = qw();
  while (defined($line=$cr->{fh}->getline)) {
    chomp $line;
    next if ($line =~ /^\%\%/);
    if ($line =~ /^\s*$/) {
      if (@sent || $args{allow_empty_sentences}) {
	$cr->{nsents}++;
	return \@sent;
      }
      next;
    }

    ($text,@details) = split(/\s*\t+\s*/, $line);
    push(@sent, {text=>$text,details=>[@details]});
    $cr->{ntoks}++;
  }
  if (@sent || $args{allow_empty_sentences}) {
    $cr->{nsents}++;
    return \@sent;
  }
  return undef;
}

## \%token_or_undef = $cr->getToken();
sub getToken {
  my ($cr,%args) = @_;
  return undef if (!$cr->{fh} || $cr->{fh}->eof);

  my ($line,$text,@details);
  while (defined($line=$cr->{fh}->getline)) {
    chomp $line;
    next if ($line =~ /^\%\%/);
    if ($line eq '') {
      $cr->{nsents}++;
      return undef;
    }

    $cr->{ntoks}++;
    ($text,@details) = split(/\s*\t+\s*/, $line);
    return {text=>$text,details=>\@details};
  }
  return undef;
}



##======================================================================
## I/O : TT : Writer
##======================================================================
package MUDL::CorpusWriter::TT;
use Carp;
MUDL::CorpusIO->import('dummy');
our @ISA = qw(MUDL::CorpusWriter);

sub DESTROY {
  my $cw = shift;
  $cr->{fh}->close if (defined($cr->{fh}));
}

## $bool = $cw->flush
sub flush { return $_[0]->{fh} ? $_[0]->{fh}->flush : undef; }

## undef = $cw->toString(\$str)
sub toString {
  my ($cw,$sref) = @_;
  $cw->{fh}->close() if (defined($cw->{fh}));
  $cw->{fh} = IO::Scalar->new(\$string)
    or croak( __PACKAGE__ , "::toString(): open failed: $!");
}

## undef = $cw->toFile($filename_or_fh)
sub toFile {
  my ($cw,$file) = @_;
  $cw->{fh}->close() if (defined($cw->{fh}));
  $cw->{fh} = ref($file) ? $file : IO::File->new(">$file");
  croak( __PACKAGE__ , "::toFile(): open failed for '$file': $!") if (!$cw->{fh});
}

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$sent) = @_;
  return undef if (!$cw->{fh});

  my $fh = $cw->{fh};
  my ($tok);
  foreach $tok (@$sent) {
    if (ref($tok)) {
      $fh->print(join("\t", $tok->{text}, @{$tok->{details}}), "\n");
    } else {
      $fh->print($tok, "\n");
    }
  }
  $fh->print("\n");
}

## undef = $cw->putToken($text_or_hashref)
sub putToken {
  my ($cw,$token) = @_;
  return undef if (!$cr->{fh});

  if (ref($token)) {
    $cw->{fh}->print(join("\t", $tok->{text}, @{$tok->{details}}), "\n");
  } elsif (defined($token)) {
    $cw->{fh}->print($tok, "\n");
  } else {
    $cw->{fh}->print("\n");
  }
}


##======================================================================
## I/O : XML : Reader
##======================================================================
package MUDL::CorpusReader::XML;
use Carp;
MUDL::CorpusIO->import('dummy');
MUDL::XML->import(qw(:xpaths :styles));
our @ISA = qw(MUDL::CorpusReader);

## new(%args)
sub new {
  my ($that,%args) = @_;
  my $self = bless {
		    s_xpath => $s_xpath,
		    token_xpath => $token_xpath,
		    text_xpath => $text_xpath,
		    detail_xpath => $detail_xpath,
		    s_elt => 's',
		    token_elt => 'token',
		    text_elt => 'text',
		    detail_elt => 'detail',
		    xmlparser => MUDL::XML::Parser->new(),
		    snodes => [], ##-- remaining sentence nodes
		    sentbuf => undef, ##-- sentence buffer for getToken
		    nsents => 0,
		    ntoks => 0,
		    %args,
		   }, ref($that)||$that;

  if (!$self->{stylesheet}) {
    my $style = $self->{style} ? $self->{sytle} : stylesheet_xml2norm(%$self);
    my $xslt  = XML::LibXSLT->new();
    my $sdoc  = $self->{xmlparser}->parse_string($style)
      or croak( __PACKAGE__ , "::new(): could not parse stylesheet document: $!");
    $self->{stylesheet} = $xslt->parse_stylesheet($sdoc)
      or croak( __PACKAGE__ , "::new(): could not parse stylesheet: $!");
  }

  return $self;
}

## $bool = $cr->eof
sub eof { return !$_[0]->{snodes} && !$_[0]->{sentbuf}; }

## undef = $cr->fromString($string)
sub fromString {
  my ($cr,$str) = @_;
  $cr->{doc} = $cr->{xmlparser}->parse_string($string)
    or croak( __PACKAGE__ , "::fromString(): parse failed: $!");
  if ($cr->{stylesheet}) {
    $cr->{doc} = $cr->{stylesheet}->transform($cr->{doc})
      or croak( __PACKAGE__ , "::fromString(): transform failed: $!");
  }
  @{$cr->{snodes}} = $cr->{doc}->documentElement->getChildrenByTagName($cr->{s_elt});
  $cr->{nsents} = scalar(@{$cr->{snodes}});
}

## undef = $cr->fromFile($filename_or_fh);
sub fromFile {
  my ($cr,$file) = @_;
  if (ref($file)) {
    $cr->{doc} = $cr->{xmlparser}->parse_fh($file)
      or croak( __PACKAGE__ , "::fromFile(): parse failed for filehandle: $!");
  } else {
    $cr->{doc} = $cr->{xmlparser}->parse_file($file)
      or croak( __PACKAGE__ , "::fromFile(): parse failed for file '$file': $!");
  }
  if ($cr->{stylesheet}) {
    $cr->{doc} = $cr->{stylesheet}->transform($cr->{doc})
      or croak( __PACKAGE__ , "::fromString(): transform failed: $!");
  }
  @{$cr->{snodes}} = $cr->{doc}->documentElement->getChildrenByTagName($cr->{s_elt});
  $cr->{nsents} = scalar(@{$cr->{snodes}});
}


## \@sentence = $cr->getSentence();
#select(STDERR); $|=1; select(STDOUT);
sub getSentence {
  my $cr = shift;
  return undef if (!$cr->{doc} || !@{$cr->{snodes}});

  #print STDERR ".";

  if ($cr->{sentbuf}) {
    my $sent = $cr->{sentbuf};
    $cr->{sentbuf} = undef;
    return $sent;
  }

  my $snode = shift(@{$cr->{snodes}});
  my @sent = qw();
  foreach my $toknode ($snode->getChildrenByTagName($cr->{token_elt})) {
    $cr->{ntoks}++;
    push(@sent,
	 {
	  text=>join('', map { $_->textContent } $toknode->getChildrenByTagName($cr->{text_elt})),
	  details=>[ map { $_->textContent } $toknode->getChildrenByTagName($cr->{detail_elt}) ],
	 });
  }
  return \@sent;
}

## \%token_or_undef = $cr->getToken();
sub getToken {
  my $cr = shift;
  return undef if (!$cr->{doc} || !@{$cr->{snodes}});

  if ($cr->{sentbuf}) {
    if (@{$cr->{sentbuf}}) {
      $cr->{ntoks}++;
      return shift(@{$cr->{sentbuf}});
    }
  }
  $cr->{sentbuf} = $cr->getSentence;
  return undef;
}


##======================================================================
## I/O : XML : Writer
##======================================================================
package MUDL::CorpusWriter::XML;
use Carp;
MUDL::CorpusIO->import('dummy');
our @ISA = qw(MUDL::CorpusWriter);

## new(%args)
sub new {
  my ($that,%args) = @_;
  my $self = bless {
		    s_elt => 's',
		    token_elt => 'token',
		    text_elt => 'text',
		    detail_elt => 'detail',
		    root_elt => 'MUDL.Corpus',
		    flush => undef, ##-- flushing sub
		    xmlencoding => 'UTF-8',
		    xmlversion => '1.0',
		    %args,
		   }, ref($that)||$that;

  return $self;
}

## $bool = $cw->flush
sub flush {
  my $cw = shift;
  return $cw->{flush} ? &{$cw->{flush}}($cw) : undef;
}

## undef = $cw->toString(\$str)
sub toString {
  my ($cw,$sref) = @_;
  my $doc  = $cw->{doc} = XML::LibXML::Document->new($cw->{xmlversion}, $cw->{xmlencoding});
  $doc->setDocumentElement($cw->{root}=XML::LibXML::Element->new($cw->{root_elt}));
  $cw->{flush} =
    sub {
      my $cw = shift;
      if (defined($cw->{doc})) {
	$cw->{doc}->setCompression($cw->{compress}) if (defined($cw->{compress}));
	$$sref = $cw->{doc}->toString($cw->{format});
      }
    };
}

## undef = $cw->toFile($filename_or_fh)
sub toFile {
  my ($cw,$file) = @_;
  my $doc  = $cw->{doc} = XML::LibXML::Document->new($cw->{xmlversion}, $cw->{xmlencoding});
  $doc->setDocumentElement($cw->{root}=XML::LibXML::Element->new($cw->{root_elt}));
  $cw->{flush} =
    sub {
      my $cw = shift;
      if (defined($cw->{doc})) {
	$cw->{doc}->setCompression($cw->{compress}) if (defined($cw->{compress}));
	if (ref($file)) {
	  $cw->{doc}->toFH($file, $cw->{format});
	} else {
	  $cw->{doc}->toFile($file, $cw->{format});
	}
      }
    };
}

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$sent) = @_;
  return undef if (!$cw->{doc});

  my ($snode,$tok,$toknode,$det);
  $cw->{root}->appendChild($snode=XML::LibXML::Element->new($cw->{s_elt}));
  foreach $tok (@$sent) {
    $snode->appendChild($toknode = XML::LibXML::Element->new($cw->{token_elt}));
    if (ref($tok)) {
      $toknode->appendTextChild($cw->{text_elt}, $tok->{text});
      foreach $det (@{$tok->{details}}) {
	$toknode->appendTextChild($args{detail_elt}, $det);
      }
    } else {
      $toknode->appendTextChild($cw->{text_elt}, $tok);
    }
  }
}

## undef = $cw->putToken($text_or_hashref)
#sub putToken

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
