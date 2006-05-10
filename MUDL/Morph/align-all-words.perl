#!/usr/bin/perl -w

use lib qw(../..);
use MUDL::Enum;
use MUDL::CorpusIO;
use MUDL::Unigrams;
use PDL;
use PDL::EditDistance;
use Encode;

##-- progress globals
our $progress_mod = 16384;

##======================================================================
## Read in Unigram profile
$ugp = MUDL::Unigrams->new();
$ntoks = 0;
foreach $file (@ARGV) {
  print STDERR "$0: processing file '$file'...\n";
  $cr = MUDL::CorpusIO->fileReader($file);
  $ugp->addReader($cr);
  $ntoks += $cr->nTokens();
}
@words = sort(keys(%$ugp));
$ntypes = scalar(@words);


##======================================================================
## Get character enum
print STDERR "$0: generating character enum...\n";
$cenum = MUDL::Enum->new;
$cenum->addIndexedSymbol('<BOS>', 0);
%chars = qw();
foreach $w (keys(%$ugp)) {
  @wchars = split(//,$w);
  @chars{@wchars} = undef;
}
$cenum->addSymbol($_) foreach (sort(keys(%chars)));
$nctypes = $cenum->size-1;

##======================================================================
## Get word-pdls
print STDERR "$0: computing word PDLs...\n";
@wpdls = qw();
$c2id  = $cenum->{sym2id};
$nctoks = 0;
foreach $i (0..$#words) {
  $wpdls[$i] = pdl(byte, [0, @$c2id{split(//,$words[$i])}]);
  $nctoks    += length($words[$i]);
}

##======================================================================
## Preliminary summary
$dlen = length($nctoks);
$npairs = $ntypes*($ntypes+1)/2;
print STDERR
  ("$0: preliminaries completed:\n",
   sprintf("  + #/word tokens = %${dlen}d\n", $ntoks),
   sprintf("  + #/word types  = %${dlen}d (%d pairs)\n", $ntypes, $npairs),
   sprintf("  + #/char tokens = %${dlen}d\n", $nctoks),
   sprintf("  + #/char types  = %${dlen}d\n", $nctypes),
  );

##======================================================================
## Compute edit distance
print STDERR "$0: computing edit distances...\n";
$ncomps = 0;

our @costs = (our ($costMatch, $costInsert, $custSubst) = (0,1,2));
$dist  = zeroes(long,1);
$align = zeroes(byte,1);
foreach $i (1..$#words) {
  $ipdl = $wpdls[$i];

  foreach $j (0..($i-1)) {
    $jpdl = $wpdls[$j];

    ##-- report
    if ($ncomps++ % $progress_mod == 0) {
      print STDERR sprintf("$0: computing edit distance: %5.2f%% complete.\n", 100.0*$ncomps/$npairs);
    }

    ##-- align (Wagner-Fisher)
    $align->reshape($jpdl->nelem, $ipdl->nelem);
    _edit_align_static($jpdl,$ipdl, @costs, $dist, $align);

    ##-- output
    print
      (join("\t",
	    $words[$j],
	    $words[$i],
	    ("dist=(".$dist->at(0).")"),
	    ("dims=(".join(",", $align->dims).")"),
	    ("align=["
	     .join(",",
		   (map {
		     "[".join(',', $_->list)."]"
		   } $align->dog))
	     ."]")
	   ),
       "\n");
  }
}
