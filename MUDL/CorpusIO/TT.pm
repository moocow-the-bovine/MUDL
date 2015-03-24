##-*- Mode: CPerl -*-
##
## File: MUDL::CorpusIO::TT.pm
## Author: Bryan Jurish <moocow@cpan.org>
## Description:
##  + MUDL unsupervised dependency learner: corpora: I/O: TT (TnT compat)
##======================================================================

package MUDL::CorpusIO::TT;
use MUDL::Object;
use MUDL::CorpusIO;
use MUDL::Token::TT;
use MUDL::Sentence;
use IO::File;
use Encode qw(encode decode);

use strict;
use Carp;


########################################################################
## I/O : TT : Reader
########################################################################
package MUDL::CorpusReader::TT;
use MUDL::CorpusIO;
use MUDL::Token::TT;
use IO::File;
use Encode qw(encode decode);
use strict;
use Carp;
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusReader);

sub new {
  my ($that,%args) = @_;
  my $self = bless {
		    allow_empty_sentences=>0,
		    nsents=>0,
		    ntoks=>0,
		    encoding=>'UTF-8', ##-- default encoding
		    %args,
		   }, ref($that)||$that;
  return $self;
}

sub DESTROY {
  my $cr = shift;
  $cr->{fh}->close if (defined($cr->{fh}));
}

## $n = nSentences()
*nSents = *nsents = \&nSentences;
sub nSentences { return $_[0]->{nsents}; }

## $n = $cr->nTokens()
*nToks = *ntoks = \&nTokens;
sub nTokens { return $_[0]->{ntoks}; }

## reset()
sub reset {
  $_[0]{nsents} = $_[0]{ntoks} = 0;
  $_[0]{fh}->close if (defined($_[0]{fh}));
}

## $bool = $cr->eof
sub eof { return !$_[0]->{fh} || $_[0]->{fh}->eof; }

## undef = $cr->fromString($str)
sub fromString {
  my ($cr,$string) = @_;
  $cr->{fh}->close() if (defined($cr->{fh}));
  $cr->{fh} = IO::Scalar->new(\$string)
    or croak( __PACKAGE__ , "::fromString(): open failed: $!");
  #binmode($cr->{fh}, ':utf8');
}

## undef = $cr->fromFile($filename)
## undef = $cr->fromFh($fh)
*fromFh = \&fromFile;
sub fromFile {
  my ($cr,$file,%args) = @_;
  $cr->{fh}->close() if (defined($cr->{fh}));
  $cr->{fh} = ref($file) ? $file : IO::File->new("<$file");
  croak( __PACKAGE__ , "::fromFile(): open failed for '$file': $!") if (!$cr->{fh});
  $cr->{filename} = $file;
  @$cr{keys %args} = values(%args);
  #binmode($cr->{fh}, ':utf8');
}

## \@sentence_or_undef = $cr->getSentence();
sub getSentence {
  my ($cr,%args) = @_;
  return undef if (!$cr->{fh} || $cr->{fh}->eof);
  %args = (allow_empty_sentences=>0,%args);

  my ($line);
  my $sent = bless [], 'MUDL::Sentence::TT';
  while (defined($line=$cr->{fh}->getline)) {
    chomp $line;
    next if ($line =~ /^\%\%/);
    if ($line =~ /^\s*$/) {
      if (@$sent || $cr->{allow_empty_sentences} || $args{allow_empty_sentences}) {
	$cr->{nsents}++;
	return $sent;
      }
      next;
    }
    push(@$sent, bless [ split(/\s*\t+\s*/, decode($cr->{encoding},$line)) ], 'MUDL::Token::TT');
    $cr->{ntoks}++;
  }
  if (@$sent || $cr->{allow_empty_sentences} || $args{allow_empty_sentences}) {
    $cr->{nsents}++;
    return $sent;
  }
  return undef;
}

## \%token_or_undef = $cr->getToken();
sub getToken {
  my ($cr,%args) = @_;
  return undef if (!$cr->{fh} || $cr->{fh}->eof);

  my ($line);
  while (defined($line=$cr->{fh}->getline)) {
    chomp $line;
    next if ($line =~ /^\%\%/);
    if ($line eq '') {
      $cr->{nsents}++;
      return undef;
    }

    $cr->{ntoks}++;
    return bless [split(/\s*\t+\s*/, $cr->decode($cr->{encoding},$line))], 'MUDL::Token::TT';
  }
  return undef;
}



########################################################################
## I/O : TT : Writer
########################################################################
package MUDL::CorpusWriter::TT;
use Encode qw(encode decode);
use Carp;
MUDL::Object->import('dummy');
our @ISA = qw(MUDL::CorpusWriter);


## $cw = class->new(%args)
##   + known %args:
##      layers => \@binmode_layer_flags
sub new {
  my ($that,%args) = @_;
  #my $cw = bless { layers=>[qw(:utf8)], %args }, ref($that)||$that;
  my $cw = bless { layers=>[], encoding=>'UTF-8', %args }, ref($that)||$that;
  return $cw;
}

sub DESTROY {
  my $cw = shift;
  $cw->{fh}->close if (defined($cw->{fh}));
}

## $bool = $cw->flush
sub flush { return $_[0]->{fh} ? $_[0]->{fh}->flush : undef; }

## undef = $cw->toString(\$str)
sub toString {
  my ($cw,$sref) = @_;
  $cw->{fh}->close() if (defined($cw->{fh}));
  $cw->{fh} = IO::Scalar->new(\$sref)
    or croak( __PACKAGE__ , "::toString(): open failed: $!");
  binmode($cw->{fh}, $_) foreach (@{$cw->{layers}});
}

## undef = $cw->toFile($filename_or_fh)
*toFh = \&toFile;
sub toFile {
  my ($cw,$file,%args) = @_;
  $cw->{fh}->close() if (defined($cw->{fh}));
  $cw->{fh} = ref($file) ? $file : IO::File->new(">$file");
  croak( __PACKAGE__ , "::toFile(): open failed for '$file': $!") if (!$cw->{fh});
  binmode($cw->{fh}, $_) foreach (@{$cw->{layers}});
  @$cw{keys %args} = values(%args);
}

## undef = $cw->putSentence(\@sent);
sub putSentence {
  my ($cw,$sent) = @_;
  return undef if (!$cw->{fh});

  my $fh = $cw->{fh};
  my ($tok);
  foreach $tok (@$sent) {
    if (ref($tok) && ref($tok) eq 'MUDL::Token::TT') {
      ##-- fast write for MUDL::Token::TT
      $fh->print(encode($cw->{encoding}, join("\t", grep {defined($_)} @$tok)), "\n");
    }
    elsif (ref($tok)) {
      ##-- general case for MUDL::Token::Base API
      $fh->print(encode($cw->{encoding},
			#$tok->saveNativeString
			join("\t", grep {defined($_)} @{$tok->asArray}), "\n")
		);
    } else {
      ##-- highly deprecated backwards-compatibility: non-ref tokens
      $fh->print(encode($cw->{encoding},$tok), "\n");
    }
  }
  $fh->print("\n");
}

## undef = $cw->putToken($text_or_token)
sub putToken {
  my ($cw,$tok) = @_;
  my ($fh);
  return undef if (!defined($fh=$cw->{fh}));

  if (ref($tok) && ref($tok) eq 'MUDL::Token::TT') {
    ##-- fast write for MUDL::Token::TT
    $fh->print(encode($cw->{encoding}, join("\t", grep {defined($_)} @$tok)), "\n");
  }
  elsif (ref($tok)) {
    ##-- general case for MUDL::Token::Base API
    $fh->print(encode($cw->{encoding},
		      #$tok->saveNativeString
		      join("\t", grep {defined($_)} @{$tok->asArray}), "\n")
	      );
  } else {
    ##-- highly deprecated backwards-compatibility: non-ref tokens
    $fh->print(encode($cw->{encoding},$tok), "\n");
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
