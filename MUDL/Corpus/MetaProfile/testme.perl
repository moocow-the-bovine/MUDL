#!/usr/bin/perl -wd

use lib qw(../../..);
use MUDL;
use MUDL::CmdUtils;
use PDL;
use MUDL::Corpus::MetaProfile;
use MUDL::Corpus::MetaProfile::Attach3;
use MUDL::Corpus::Profile::LRHB;
use MUDL::Cluster::Tree;

BEGIN {
  $, = ' ';
  select STDERR; $|=1; select STDOUT;
}

##----------------------------------------------------------------------
## test attach3 bootstrap
use vars qw($mp $trigrams $prof1);
sub ta3_1 {
  $prof1 = MUDL::Corpus::Profile::LRHB->new();
  $cm    = MUDL::Cluster::Tree->new(
				    nclusters=>100,
				    npass=>0,
				    dist=>'b',
				    method=>'m',
				    cddist=>'u',
				    cdmethod=>'v',
				    niters=>0,
				    svdr=>0,
				   );

  $mp = MUDL::Corpus::MetaProfile::Attach3->new(prof1=>$prof1,cm=>$cm);

  $trigrams = load('trigrams.bin');
  $tenum3   = load('targets3.enum.bin');
  $benum1   = load('bounds1.enum.bin');
  #$benum0   = MUDL::Enum->new();
  #$benum0->addSymbol('__$');

  ##--
  #$mp->initialize($trigrams);
  ##--
  $bigrams = load('bigrams.bin');
  $mp->initialize(f3=>$trigrams,f2=>$bigrams);

  $mp->bootstrap(tenum3=>$tenum3,benum1=>$benum1);
  $mp->rmtmps(force=>1);
  $mp->saveFile("mp3.stage1.bin");
}
#ta3_1;

sub ta3_2 {
  $mp = load("mp3.stage1.bin");
  $tenum3   = load('targets3-stage2.enum.bin');

  $mp->update(tenum3=>$tenum3);
  $mp->rmtmps(force=>1);
  $mp->saveFile("mp3.stage2.bin");
}
#ta3_2;

sub ta3_3 {
  $prof1 = MUDL::Corpus::Profile::LRHB->new();
  $cm    = MUDL::Cluster::Tree->new(
				    nclusters=>200,
				    npass=>0,
				    dist=>'b',
				    method=>'m',
				    cddist=>'u',
				    cdmethod=>'v',
				    niters=>0,
				    svdr=>0,
				   );

  $mp = MUDL::Corpus::MetaProfile::Attach3->new(prof1=>$prof1,cm=>$cm);

  $trigrams  = load('trigrams.bin');
  $tenum3_1  = load('tgs-3g-2000-r0.enum.bin');
  $benum1    = load('bounds1.enum.bin');
  $bigrams   = load('bigrams.bin');
  $mp->initialize(f3=>$trigrams,f2=>$bigrams);

  $mp->bootstrap(tenum3=>$tenum3_1,benum1=>$benum1);
  $mp->rmtmps(force=>1);
  $mp->saveFile("mp3.stage1.bin");

  $tenum3_2 = load('tgs-3g-4000-r0.enum.bin');
  $mp->update(tenum3=>$tenum3_2);
  $mp->saveFile("mp3.stage2.bin");
}
ta3_3;



##-- dummy
foreach $i (0..10 ) {
  print "-- dummy ($i) --\n";
}
