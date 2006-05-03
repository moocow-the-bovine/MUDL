#-*- Mode: CPerl -*-

## File: MUDL::Corpus::Filter::ContextTagger.pm
## Author: Bryan Jurish <moocow@ling.uni-potsdam.de>
## Description:
##  + MUDL unsupervised dependency learner: corpus filters:
##    : Schütze (1995)-style context tagging
##======================================================================

package MUDL::Corpus::Filter::ContextTagger;
use MUDL::Corpus::Filter;
use MUDL::Cluster::Method;
use MUDL::Corpus::Profile::LR3;
use MUDL::Trigrams;
use PDL;
use PDL::Cluster;
use MUDL::CmdUtils qw(loadModule);
use Carp;

use strict;
our @ISA = qw(MUDL::Corpus::Filter);

our $DEFAULT_BS = 4096;

our %EXPORT_TAGS=
  (
   vlevels=>[qw($vl_none $vl_error $vl_warn $vl_info $vl_debug $vl_full $vl_default)],
  );
our @EXPORT_OK = (map { @$_ } values(%EXPORT_TAGS));

##-- verbosity levels
our $vl_none  = 0;
our $vl_error = 1;
our $vl_warn  = 2;
our $vl_info  = 3;
our $vl_debug = 10;
our $vl_full  = 255;

our $vl_default = $vl_info;


##----------------------------------------------------------------------
## Constructor: $ct = MUDL::Corpus::Filter::ContextTagger->new(%args)
##
## + %args:
##   prof    => $profile3, ##-- underlying trigram profile  (destructively altered!)
##   cm      => $cm,       ##-- MUDL::Cluster method object (destructively altered!)
##   targets => $targets3, ##-- MUDL::Enum::Nary::Flat (trigram enum)
##   cids    => $pdl,      ##-- best cluster-ids by trigram-id
##   bs      => $bsize,    ##-- blocksize for attachTrigrams()
##   bgs     => $bigrams,  ##-- bigrams to use for sub-profile construction in attachTrigrams()
##   #cdata   => $cdata,    ##-- pdl($d,$k) : centroid data: in {cm}
##   #cmask   => $cmask,    ##-- pdl($d,$k) : centroid mask: in {cm}
##   uid     => $id,       ##-- best cluster for 'unknown' (all zero) elements
##   utag    => $utag,      ##-- alternate unknown handling: just assign string "$utag"
##   cenum   => $class_enum, ##-- class-names
sub new {
  my ($that,%args)=@_;
  my $ct = $that->SUPER::new(
			     targets=>MUDL::Enum::Nary::Flat->new(nfields=>3),
			     bs=>$DEFAULT_BS,
			     cids=>undef,
			     verbose=>$vl_default,
			     %args,
			    );

  $ct->initialize() if (defined($ct->{prof}) && defined($ct->{cm}));

  return $ct;
}

########################################################################
## Utiltiies: Messages
########################################################################

## $ct->vmsg($level, @msg)
sub vmsg {
  my ($ct,$level,@msg) = @_;
  $ct->{verbose} = $vl_default if (!defined($ct->{verbose}));
  return if (!defined($ct->{verbose}) || $ct->{verbose} < $level);
  print STDERR ref($ct), ": ", @msg;
}

########################################################################
## Initialization
########################################################################

##----------------------------------------------------------------------
## Initialization

## $ct = $ct->initialize()
## $ct = $ct->initialize($cprof,$cm)
##  + grabs initial data from $ct->{prof} and $ct->{cm}
sub initialize {
  my ($ct,$prof,$cm) = @_;

  ##-- arg-parsing, clobbering
  $prof = $ct->{prof} if (!defined($prof));
  $cm   = $ct->{cm}   if (!defined($cm));

  ##-- ensure everything is loaded
  loadModule(ref($prof));
  loadModule(ref($prof->{uprof}));
  loadModule(ref($cm));

  ##-- setup: targets: this MUST be a copy
  $ct->{targets} = $prof->{targets}->copy();

  ##-- setup: profile(s)
  $prof->reset();

  ##-- setup: cm
  @{$ct->{cm}}{qw(cdata cmask)} = $cm->getcenters();
  $ct->{cids} = $cm->{clusterids};
  $ct->{cm} = $cm->shadow();
  $ct->{cm}{enum}->clear();

  ##-- setup: cenum
  $ct->{cenum} = $cm->clusterEnum;

  ##-- get best unknown id
  if (!defined($ct->{ustr})) {
    my $udata = zeroes(double, $ct->{cm}{cdata}->dim(0), 1);
    my $umask = ones(double, $udata->dims);
    my $uweight = ones(double, $udata->dim(0));
    my $useq   = zeroes(long,1);
    my $uids = zeroes(long,1);
    my $udists = zeroes(double,1);
    attachtonearest($udata,$umask,$uweight,
		    $useq,
		    @{$ct->{cm}}{qw(cdata cmask)},
		    $uids, $udists,
		    $ct->{cm}->cddist, $ct->{cm}->cdmethod);
    $ct->{uid} = $uids->at(0);
  }

  ##-- return
  return $ct;
}

##----------------------------------------------------------------------
## Training (attachment)

## $ct = $ct->attachTrigrams($trigrams,%args)
##   + attach $trigrams to best clusters
##   + %args: clobber %$ct
##   + interesting %args:
##      bs  => $blocksize,
##      bgs => $bigrams,
sub attachTrigrams {
  my ($ct,$trigs,%args) = @_;
  @$ct{keys(%args)} = values(%args);  ##-- clobber

  ##-- ensure everything is loaded
  loadModule(ref($ct->{prof}));
  loadModule(ref($ct->{prof}{uprof}));
  loadModule(ref($ct->{cm}));

  ##-- first, project bigrams
  my $bgs = $ct->{bgs};
  if (!defined($bgs)) {
    $ct->vmsg($vl_info, "attachTrigrams: project bigrams\n");
    $bgs = $ct->{bgs} = $trigs->projectN(0,1);
  }

  ##-- filter out any trigrams we already know about
  $ct->vmsg($vl_info, "attachTrigrams: pruneByEnum()\n");
  $trigs->{nz}->pruneByEnum(enum=>$ct->{targets}, keep=>0);

  ##-- attach new trigrams block-wise
  my $bs = $ct->{bs};
  my ($blocki, $tgsremaining);
  my $tgstotal = $trigs->{nz}->size;
  for ($blocki=1; $trigs->{nz}->size > 0; $blocki++) {
    $tgsremaining = $trigs->{nz}->size;
    $ct->vmsg($vl_info, sprintf("attachBlock[%3d]: %6d / %d ~ %5.1f%% done\n",
				$blocki,
				$tgstotal-$tgsremaining,
				$tgstotal,
				100.0*(1.0-$tgsremaining/$tgstotal)));
    $ct->attachBlock($bgs, $trigs, $blocki);
    if (defined($ct->{attachBlockHook})) {
      $ct->{attachBlockHook}->($ct);
    }
  }

  return $ct;
}

## $ct = $ct->attachBlock($bigrams,$trigrams,$blockid)
sub attachBlock {
  my ($ct,$bgs,$trigs,$blocki)=@_;

  ##-- get new targets: trigrams
  $ct->vmsg($vl_debug, "attachBlock[$blocki]: targets3\n");
  my $prof     = $ct->{prof};
  my $targets3 = $prof->{targets};
  $targets3->clear();

  my ($tg);
  my $counter = 0;
  foreach $tg (keys(%{$trigs->{nz}})) {
    $targets3->addSymbol($tg);
    last if (++$counter >= $ct->{bs});  ##-- respect block-size
  }

  ##-- profile new targets: dispatch to {prof}
  $ct->vmsg($vl_debug, "attachBlock[$blocki]: unigram profile (addBigrams)\n");
  $prof->setTargets($targets3);
  $prof->addBigrams($bgs);
  $prof->finish();
  #$prof->finish(verbose=>0);

  ##-- get block trigram profile
  $ct->vmsg($vl_debug, "attachBlock[$blocki]: trigram profile (toPDL)\n");
  my $data = $ct->{bdata} = $prof->toPDL($ct->{bdata});

  ##-- attach data
  $ct->vmsg($vl_debug, "attachBlock[$blocki]: attach (attachtonearest)\n");
  my $cm   = $ct->{cm};
  my $ntgs = $targets3->size;
  my ($cdata,$cmask) = @{$ct->{cm}}{qw(cdata cmask)};

  my ($tgcids,$tgdist) = $cm->attach(data=>$data,
				     rowids=>sequence(long,$ntgs),
				     cdata=>$cdata,
				     cmask=>$cmask);
  ##-- adopt into top-level
  $ct->vmsg($vl_debug, "attachBlock[$blocki]: adopt: trigram targets\n");
  my $alltargets3 = $ct->{targets};
  my $tgids = pdl(long, [map {$alltargets3->addSymbol($_)} $targets3->allSymbols]);

  $ct->vmsg($vl_debug, "attachBlock[$blocki]: adopt: cluster-ids\n");
  my $cids = $ct->{cids};
  $cids->reshape($alltargets3->size);
  $cids->index($tgids) .= $tgcids;
  $ct->{cids} = $cids;

  ##-- CAVEAT: don't min-normalize in LR3 children by default!

  ##-- cleanup
  $ct->vmsg($vl_debug, "attachBlock[$blocki]: cleanup\n");
  delete(@{$trigs->{nz}}{$targets3->allSymbols});
  $prof->reset();
  $targets3->clear;

  return $ct;
}

##----------------------------------------------------------------------
## Sentence Processing

## $s_or_undef = $ct->doSentence(\@sentence)
sub doSentence {
  my ($ct,$s) = @_;

  ##-- ensure that we have a cluster-enum
  my $cenum = $ct->{cenum};
  $cenum = $ct->{cenum} = $ct->{cm}->clusterEnum if (!defined($cenum));

  my @stxt = ($ct->{prof}{uprof}{bos},
	      (map { ref($_) ? $_->text : $_ } @$s),
	      $ct->{prof}{uprof}{eos});
  my @syms = (undef,splice(@stxt,0,2));

  my ($i,$tgid,$cid);
  foreach $i (0..$#$s) {
    ##-- update trigram buffer @syms
    @syms[0,1,2] = (@syms[1,2], shift(@stxt));

    ##-- get & assign tag
    if (defined($tgid = $ct->{targets}->lindex(@syms))) {
      $cid = $ct->{cids}->at($tgid);
      $s->[$i]->tag($cenum->{id2sym}[$cid]);
    }
    elsif (defined($ct->{uid})) {
      $cid = $ct->{uid};
      $s->[$i]->tag($cenum->{id2sym}[$cid]);
    }
    elsif (defined($ct->{ustr})) {
      $s->[$i]->tag($ct->{ustr});
    }
    else {
      $s->[$i]->tag("\@UNKNOWN");
    }
  }

  return $s;
}


##----------------------------------------------------------------------
## Help String

## $string = $class_or_obj->helpString()
sub helpString {
  return
    (''
     ."Map trigrams to best tags using Schütze's (1995) context distribution clustering.\n"
     ."Options:\n"
     ."  prof=PROFILE      [MUDL::Corpus::Profile::LR3]\n"
     ."  cm=CLUSTERER      [MUDL::Cluster::Method]\n"
     ."  targets=ENUM      [MUDL::Enum::Nary::Flat]\n"
     ."  bs=BLOCKSIZE      [=$DEFAULT_BS]\n"
     ."  uid=UNKNOWN_ID    [default: zero vector cluster]\n"
     ."  ustr=UNKNOWN_STR  [used in place of uid if given, default=\@UNKNOWN]\n"
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
