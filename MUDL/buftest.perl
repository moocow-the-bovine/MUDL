#!/usr/bin/perl -wd

use lib qw(..);
use Storable qw(store retrieve);
use MUDL;
use MUDL::CmdUtils;
use MUDL::CorpusIO;
use MUDL::Token;
#use MUDL::PToken;
use PDL;
use Benchmark qw(cmpthese timethese);

#BEGIN { $, = ' '; }

##======================================================================
## Buffer creation
##======================================================================
BEGIN {
  our $cbase = "buftest";
  our $cfile = "${cbase}.ttt";
  our @exts  = ('ttt',
		#'ttt.bin',
		'ttt.cbuf.bin',
		#'ttt.ebuf.bin',
		#'ttt.ebuftt.bin',
		#'ttt.pdlbuf.bin',
		#'ttt.packed.bin',
		'ttt.ptt.bin',
		'ttt.pdltt.bin',
		#qw(ttt.pbuf2.bin);
	       );
  our %extids = map { $exts[$_]=>$_ } (0..$#exts);
  our $ttw   = MUDL::CorpusIO->fileWriter("tt:-");
}


use MUDL::Corpus::Buffer;
sub create_corpus_buffer {
  our $cb = MUDL::Corpus::Buffer->fromFile($cfile);
  $cb->saveFile("$cfile.buf.bin");
  $cb->saveFile("$cfile.cbuf.bin");
}
#create_corpus_buffer;

use MUDL::Corpus::Buffer::PackedTT;
sub create_corpus_ptt {
  our $cb = load("$cfile.cbuf.bin");
  our $pttbw = MUDL::CorpusIO->fileWriter("$cfile.ptt.bin");
  $pttbw->putReader($cb->reader);
  $pttbw->flush;
}
#create_corpus_ptt;

use MUDL::Corpus::Buffer::PdlTT;
sub create_corpus_pdltt {
  our $cb = load("$cfile.cbuf.bin");
  our $pdlttw = MUDL::CorpusIO->fileWriter("$cfile.pdltt.bin");
  our $pdlb   = $pdlttw->{buffer};
  $pdlttw->putReader($cb->reader);
  $pdlttw->flush;

  $pdlb = load("$cfile.pdltt.bin");
  #$pdlb->unpackPdls();
  our $pdlbr = $pdlb->reader;
  our $s     = $pdlbr->getSentence;
  $ttw->putSentence($s);
}
#create_corpus_pdltt;

use MUDL::CorpusIO::TT::Bin;
sub create_corpus_binio {
  our $cb = load("$cfile.buf.bin");
  our $bw = MUDL::CorpusIO->fileWriter("$cfile.bin");
  $bw->putReader($cb->reader);
  $bw->flush;
}



##--------------------------
## create: all
sub create_all {
  create_corpus_buffer();
  create_corpus_ebuffer();
  create_corpus_buffer_pdl2();
  create_corpus_binio();
  create_corpus_ptt();
}
#create_all();

##----------------------------------------------------------------------
## Test: Buffer::PdlFull
##----------------------------------------------------------------------

sub test_pdlfull_fill {
  $cb = load("$cfile.cbuf.bin");
  $pb = MUDL::Corpus::Buffer::PdlFull->new(cb=>$cb->clone);
  $#{$pb->{cb}{sents}} = 2; ##-- reduce
  $pb->fillPdls();
  $pbr = $pb->reader;
  $pbs = $pbr->getSentence;
  $pbt = $pbs->[0];
  our $pbta = $pbt->asArray;
}
#test_pdlfull_fill;

sub test_pdlfull_reader {
  $pbr = MUDL::CorpusIO->fileReader("$cfile.pdlbuf.bin");
  $pb  = $pbr->{buffer};
  $s   = $pbr->getSentence;
  $ttw->putSentence($s);
}
#test_pdlfull_reader();

##----------------------------------------------------------------------
## test: pdltt
##----------------------------------------------------------------------

sub test_pdltt_ugs {
  $pb  = load("$cfile.pdltt.bin");
  $ug  = MUDL::Unigrams->new;
  $ug->addReader($pb->reader);
  $pb->packPdls();

  $e = $pb->{enums}[0];
  $ugp   = hist($pb->{pdls}[0],0,$pb->{enums}[0]->size,1);
  $ugpe  = MUDL::EDist->new(enum=>$e)->fromPDL($ugp);
  $ugped = $ugpe->toDist();

  my $ugpeds = join('', map {"$_\t$ugped->{$_}\n"} sort keys %$ugped);
  my $ugs    = join('', map {"$_\t$ug->{$_}\n"}    sort keys %$ug);

  print "Unigrams: ", ($ugs eq $ugpeds ? "ok" : "NOT ok"), "\n";
}
#test_pdltt_ugs;

use MUDL::PdlDist::Sparse2d;
use PDL::CCS;
sub test_pdltt_bgs {
  $,   = ' ';
  $pb  = load("$cfile.pdltt.bin");
  ##
  ##-- bigrams: insert BOS,EOS markers into full text pdl
  my $enum = $pb->{enums}[0];
  my $eos  = $enum->addSymbol('__$');
  my $fpdl = $pb->fullPdl->slice("(0),:");
  my $epdl = zeroes($fpdl->type, $fpdl->nelem + $pb->{begins}->nelem + 1)+$eos;
  my $beg  = $pb->{begins};
  my $f2e_delta = zeroes(long, $fpdl->nelem);
  $f2e_delta->index($beg) .= 1;
  my $f2e = $f2e_delta->cumusumover + sequence(long,$fpdl->nelem);
  $epdl->index($f2e) .= $fpdl;
  ##
  ##-- bigrams: get bigram indices
  my $w1i   = $epdl;
  my $w2i   = $w1i->rotate(-1);            ##-- requires that BOS,EOS have the same label
  my $w12i  = $w1i->cat($w2i)->xchg(0,1);  ##-- pdl(2,$nToks)  : raw bigram indices (with <BOS,EOS> at end)
  $w12i     = $w12i->slice(":,0:-2");      ##-- pdl(2,$nToks-1): raw bigram indices (without <BOS,EOS> at end)
  $w12i     = $w12i->convert(long);        ##-- need it as a long....
  ##
  ##-- bigrams: get flat indices
  my $nToks1    = $w12i->dim(1);
  my $w12i_flat = ($w12i->slice("(0),:")*$nToks1) + $w12i->slice("(1),:"); ##-- pdl($nToks-1): flat bigram indices
  #$w12i_flat_w1i = ($w12i_flat / $Nw); ##-- == $w12i->slice("(0),:")  ##-- test: recover 2d index from 1d: x
  #$w12i_flat_w2i = ($w12i_flat % $Nw); ##-- == $w12i->slice("(1),:")  ##-- test: recover 2d index from 1d: y
  ##
  ##-- bigrams: sort & count flat indices
  my $w12i_flat_sorted = $w12i_flat->qsort();    ##-- pdl($nToks-1): sorted flat indices (w1 > w2)
  my ($w12i_flat_sorted_f0,                      ##-- pdl($nToks-1): raw frequencies, with trailing zeroes
      $w12i_flat_sorted_i0)                      ##-- pdl($nToks-1): flat bigram indices, with empties
    = $w12i_flat_sorted->rle();                  ##-- run-length encode: see PDL::Slices
  my $Nnz = $w12i_flat_sorted_f0->nnz;           ##-- $Nnz: number of actually observed bigram types
  my $w12i_flat_sorted_f
    = $w12i_flat_sorted_f0->slice("0:".($Nnz-1)); ##-- pdl($Nnz): actual nonzero bigram freqs
  my $w12i_flat_sorted_i
    = $w12i_flat_sorted_i0->slice("0:".($Nnz-1)); ##-- pdl($Nnz): actual nonzero bigram indices (flat)
  ##
  ##-- bigrams: reconstruct 2d indices from $w12_flat_sorted_c
  my $w1i_nnz = $w12i_flat_sorted_i / $nToks1;   ##-- pdl($Nnz):  first component ($w1i) of nonzero bigram types
  my $w2i_nnz = $w12i_flat_sorted_i % $nToks1;   ##-- pdl($Nnz): second component ($w2i) of nonzero bigram types
  ##
  ##--BEGIN NOTE
  ## + it should now be the case that:
  ##      all( $w12i->uniqvec == ($w12i_nnz = $w1i_nnz->cat($w2i_nnz)->xchg(0,1)) )
  ##--END NOTE
  ##
  ##-- bigrams: ccs encode
  my ($ptr,$rowids,$nzvals) = ccsencode_i2d($w1i_nnz,$w2i_nnz,$w12i_flat_sorted_f);
  ##
  ##-- bigrams: to MUDL::PdlDist::Sparse2d
  my $bgenum  = MUDL::Enum::Nary->new(nfields=>2,enums=>[$enum,$enum]);
  my $bgpd    = MUDL::PdlDist::Sparse2d->new(
					     ptr=>$ptr,
					     rowids=>$rowids,
					     nzvals=>$nzvals,
					     dims=>[$nToks1,$nToks1],
					     enum=>$bgenum,
					    );

  ##.... the rest
  print "constructed.\n";

  ##-- get bigrams the old, text-based way
  $bg  = MUDL::Bigrams->new;
  $bg->addReader($pb->reader);
  our $nbgs = scalar(keys(%{$bg->{nz}})); ##-- 8033
  our @nbg_strs = map { "$_\t$bg->{nz}{$_}\n" } sort(keys(%{$bg->{nz}}));
  our $nbg_str  = join('',sort @nbg_strs);

  ##-- test: format
  my $id2sym  = $enum->{id2sym};
  my @w1i_nnz = $w1i_nnz->list;
  my @w2i_nnz = $w2i_nnz->list;
  my @f_nnz   = $w12i_flat_sorted_f->list;
  our @pbg_strs = map { join("\t", @$id2sym[$w1i_nnz[$_],$w2i_nnz[$_]], $f_nnz[$_])."\n" } (0..$#w1i_nnz);
  our $pbg_str  = join('',sort @pbg_strs);

  print "Bigrams: ", ($nbg_str eq $pbg_str ? "ok" : "NOT ok"), "\n";
}
#test_pdltt_bgs();

##----------------------------------------------------------------------
## Utils: store & retrieve timing results
##----------------------------------------------------------------------

BEGIN {
  our %files = map { ($_=>"${cbase}.$_") } @exts;
  our $benches = -r "bufdata.bin" ? retrieve("bufdata.bin") : {};
}
sub store_benches { store($benches,"bufdata.bin"); }

## cmpbench($label)
sub cmpbench {
  my $lab = shift;
  if (defined($benches->{$lab}) && defined($benches->{$lab}{ttr})) {
    print "\nBENCH: $lab\n";
    return cmpthese($benches->{$lab}{ttr});
  } else {
    print STDERR "BENCH: $lab: ERROR: no data for '$lab'!\n";
  }
}

sub store_timeresults {
  my ($ttr,$benchid) = @_;

  ##-- prepare base fields
  my ($b,$ts,$tssecs,@tfields);
  my %tfields = qw(); ##-- [iters, cpusecs, iterspersec, base_iterspersec]
  foreach (@exts) {
    if (defined($b=$ttr->{$_})) {
      $ts = $b->timestr('auto');
      $tssecs = ($ts =~ /=\s+([\d\.]+)\s+CPU\b/ ? $1 : 0);
      $tfields{$_} = {iters=>$ttr->{$_}->iters,
		      secs=>$tssecs,
		      ips=>($tssecs==0
			    ? ($ttr->{$_}->iters.".0/0")
			    : ((1.0*$ttr->{$_}->iters)/$tssecs))};
    } else {
      $tfields{$_} = {iters=>0,secs=>0,ips=>0};
    }
  }
  ##-- add reference value
  $tfields{$_}{base_ips} = $tfields{'ttt'}{ips} foreach (@exts);

##-- store data
#  open(OUT,">$file") or die("$0: open failed for $file: $!");
#  print OUT
#    ('#', join("\t", '1_extid', '2_name', '3_niters', '4_time_sec', '5_iters_per_sec', '6_base_ips'), "\n",
#     map {
#       join("\t", $extids{$_}, $_, @{$tfields{$_}}{qw(iters secs ips base_ips)})."\n";
#     } @exts);
#  close(OUT);

  $benches->{$benchid} = {fields=>\%tfields, ttr=>$ttr};
  store_benches();
}


##----------------------------------------------------------------------
## Utils: plot benchmark results
##----------------------------------------------------------------------

sub ADJUSTY_PLAIN {
  my ($ext,$lab,$fields)=@_;
  return undef if (!defined($fields));
  if ($lab eq 'size') {
    return 10.0*$fields->{size}/$fields->{ttt_size};
  }
  return $fields->{ips};
}
sub ADJUSTY_VS_TTT {
  my ($ext,$lab,$fields)=@_;
  return undef if (!defined($fields));
  if ($lab eq 'size') {
    return $fields->{size}/$fields->{ttt_size};
  }
  return $fields->{ips}/$fields->{base_ips};
}
BEGIN {
  #*ADJUSTY=\&ADJUSTY_PLAIN;
  *ADJUSTY=\&ADJUSTY_VS_TTT;
}

BEGIN { our @DEFAULT_PLOT_EXTS = @exts; }


## plotcmp($file,%OPTIONS)
##  + %OPTIONS:
##     labels   => \@labels,  ##-- default: all
##     exts     => \@exts,    ##-- default: all
##     adjusty  => \%lab2sub, ##-- y adjustment subs: default %ADJUSTY; sub called as &sub($ext,$lab,$fields)
##                            ##   + default: ADJUSTY()
##     adjustx  => $gpstr,    ##-- gnuplot function body: given ($lid,$xid)
##                            ##   + should return x-adjustment for $xid (default: '0.1*xid')
##     logscale => $xy,       ##-- 'x' or 'y' or 'xy': default none
##     with     => $how,      ##-- how to plot (default: 'i')
sub plotcmp {
  my ($file,%opts) = @_;
  our ($benches);

  ##-- get labels
  my ($labels); 
  $labels = [keys %$benches] if (!defined($labels=$opts{labels}));
  my %labids = map { ($labels->[$_]=>$_) } (0..$#$labels);

  ##-- get extensions (x vals)
  my ($exts);
  if (!defined($exts=$opts{exts})) {
    if (0) {
      ##-- AUTODETECT (UNUSED)
      my %xh = qw();
      foreach (@$benches{@$labels}) {
	@xh{keys %{$_->{fields}}} = undef if (defined($_) && defined($_->{fields}));
      }
      $exts = [sort keys %xh];
    }
    else {
      $exts = [@DEFAULT_PLOT_EXTS];
    }
  }
  my %xids = map { ($exts->[$_]=>$_) } (0..$#$exts);

  ##-- get adjustment subs
  my $adjy={};
  my $adjy_default = ref($opts{adjusty}) && ref($opts{adjusty}) eq 'CODE' ? $opts{adjusty} : \&ADJUSTY;
  %$adjy = %{$opts{adjusty}} if (ref($opts{adjusty}) && ref($opts{adjusty}) eq 'HASH');
  foreach (@$labels) {
    $adjy->{$_} = $adjy_default if (!defined($adjy->{$_}));
  }

  ##-- get y values: $ext => $lab => $yval
  my $ely = [];
  my ($ext,$xid,$lab,$lid,$fields,$y);
  foreach $ext (@$exts) {
    $xid = $xids{$ext};
    foreach $lab (@$labels) {
      $lid    = $labids{$lab};
      $fields = (defined($benches->{$lab}) && defined($benches->{$lab}{fields}{$ext})
		 ? $benches->{$lab}{fields}{$ext}
		 : undef);
      $y      = $adjy->{$lab}->($ext,$lab,$fields);
      $y      = '1/0' if (!defined($y));
      $ely->[$xid][$lid] = $y;
    }
  }

  ##-- get data commands
  my ($elx);
  my @data = qw();
  foreach $xid (0..$#$ely) {
    $elx = $ely->[$xid];
    foreach $lid (0..$#$elx) {
      #push(@data, join("\t", $xid, $lid, $ely->[$xid][$lid])."\n");
      push(@data, $ely->[$xid][$lid]."\n");
    }
    push(@data,"e\n");
  }
  ##-- save data
  #open(DAT, ">buftest-tmp.dat") or die("$0: open failed for 'buftest-tmp.dat': $!");
  #print DAT, @data;
  #close(DAT);

  ##-- commands: scale, style
  my @cmds = qw();
  if (defined($opts{logscale})) {
    push(@cmds, ($opts{logscale}
		 ? "set logscale $opts{logscale};\n"
		 : "unset logscale;\n"));
  }
  push(@cmds, "set style data " .  ($opts{with} ? $opts{with} : 'i') . ";\n");
  push(@cmds, "set xrange [-.1:".scalar(@$labels)."];\n");
  push(@cmds, ("adjustx(lid,xid) = lid+("
	       .(defined($opts{adjustx}) ? $opts{adjustx} : "0.1*xid")
	       .");\n"));

  ##-- commands: set x-tics: by label
  push(@cmds, 'set xtics ('.join(", ", (map {"\"$labels->[$_]\" $_"} 0..$#$labels)).");\n");

  ##-- commands: plot
  my $nx = scalar(@$exts);
  my $nl = scalar(@$labels);
  push(@cmds, "plot \\\n");
  foreach $xid (0..$#$exts) {
    $ext = $exts->[$xid];
    push(@cmds, "  '-' using (adjustx(\$0,$xid)):1 title '$ext'".($xid==$#$exts ? ";" : ", \\")."\n");
  }

  if (!defined($file)) { $file = '|gnuplot -persist'; }
  else                 { $file = ">$file"; }
  open(GP,$file) or die("$0: open failed for file $file: $!");
  print GP @cmds, "\n", @data, "\n";
  close(GP);
}
#plotcmp();

##----------------------------------------------------------------------
## Bench: get bigrams
##----------------------------------------------------------------------

sub getenum_bigrams {
  my $bg = shift;
  my $enum = MUDL::Enum->new;
  my $sym2id = $enum->{sym2id};
  my $id2sym = $enum->{id2sym};
  @$sym2id{map {split(/\t/,$_)} keys(%{$bg->{nz}})} = undef;
  my @bgkeys = keys %$sym2id;
  @$id2sym[0..$#bgkeys] = @bgkeys;
  @$sym2id{@bgkeys} = 0..$#bgkeys;
  return $enum;
}

BEGIN {
  our $GETBIGRAMS_STRINGS=1; ##-- set to true to get (only) string bigrams during benchmark
  our $GETBIGRAMS_PDLS=1;    ##-- set to true to get (only) PDL bigrams during benchmark
}

sub getbigrams_reader {
  my $cr  = shift;
  ##
  ##-- we always need string-bigrams here
  my $bg = MUDL::Bigrams->new();
  $bg->addReader($cr);
  ##
  my ($bgpd);
  if ($GETBIGRAMS_PDLS) {
    my $enum   = getenum_bigrams($bg);
    my $sym2id = $enum->{sym2id};
    my $bged = MUDL::EDist::Nary->new(nfields=>2,enums=>[$enum,$enum]);
    @{$bged->{nz}}{ map {join("\t", @$sym2id{split(/\t/,$_)})} keys(%{$bg->{nz}}) } = values(%{$bg->{nz}});
    $bgpd = $bged->toSparsePdlDist();
  }
  return ($bg,$bgpd);
}
#($bgs,$bgpd) = getbigrams_reader(load("$cfile.cbuf.bin")->reader);


## ($bgs,$bgpd) = getbigrams_from_pdls($txtpdl,$txtenum,$sbegins)
##   + FIX THIS: flat-index hack won't cut the butter for TRIGRAMS and above with nTypes=44k
sub getbigrams_from_pdls {
  my ($fpdl,$enum,$begins) = @_;
  ##
  ##-- add EOS to enum
  my $eos  = $enum->addSymbol('__$');
  #my $beg  = $begins;
  my $epdl = zeroes($fpdl->type, $fpdl->nelem + $begins->nelem + 1)+$eos;
  my $f2e_delta = zeroes(long, $fpdl->nelem);
  $f2e_delta->index($begins) .= 1;
  my $f2e = $f2e_delta->cumusumover + sequence(long,$fpdl->nelem);
  $epdl->index($f2e) .= $fpdl;
  ##
  ##-- bigrams: get 2d bigram indices
  my $w1i   = $epdl;
  my $w2i   = $w1i->rotate(-1);            ##-- requires that BOS,EOS have the same label
  my $w12i  = $w1i->cat($w2i)->xchg(0,1);  ##-- pdl(2,$nToks)  : raw bigram indices (with <BOS,EOS> at end)
  $w12i     = $w12i->slice(":,0:-2");      ##-- pdl(2,$nToks-1): raw bigram indices (without <BOS,EOS> at end)
  $w12i     = $w12i->convert(long);        ##-- need it as a long....
  ##
  ##-- bigrams: get flat bigram indices
  my $nToks1    = $w12i->dim(1);
  my $w12i_flat = ($w12i->slice("(0),:")*$nToks1) + $w12i->slice("(1),:"); ##-- pdl($nToks-1): flat bigram indices
  ##
  ##-- bigrams: sort & count flat indices
  my $w12i_flat_sorted = $w12i_flat->qsort();    ##-- pdl($nToks-1): sorted flat indices (w1 > w2)
  my ($w12i_flat_sorted_f0,                      ##-- pdl($nToks-1): raw frequencies, with trailing zeroes
      $w12i_flat_sorted_i0)                      ##-- pdl($nToks-1): flat bigram indices, with empties
    = $w12i_flat_sorted->rle();                  ##-- run-length encode: see PDL::Slices
  my $Nnz = $w12i_flat_sorted_f0->nnz;           ##-- $Nnz: number of actually observed bigram types
  my $w12i_flat_sorted_f
    = $w12i_flat_sorted_f0->slice("0:".($Nnz-1)); ##-- pdl($Nnz): actual nonzero bigram freqs
  my $w12i_flat_sorted_i
    = $w12i_flat_sorted_i0->slice("0:".($Nnz-1)); ##-- pdl($Nnz): actual nonzero bigram indices (flat)
  ##
  ##-- bigrams: reconstruct 2d indices from $w12_flat_sorted_c
  my $w1i_nnz = $w12i_flat_sorted_i / $nToks1;   ##-- pdl($Nnz):  first component ($w1i) of nonzero bigram types
  my $w2i_nnz = $w12i_flat_sorted_i % $nToks1;   ##-- pdl($Nnz): second component ($w2i) of nonzero bigram types
  ##
  ##--BEGIN NOTE
  ## + it should now be the case that:
  ##      all( $w12i->uniqvec == ($w12i_nnz = $w1i_nnz->cat($w2i_nnz)->xchg(0,1)) )
  ##--END NOTE
  ##
  ##-- bigrams: ccs encode
  my ($ptr,$rowids,$nzvals) = ccsencode_i2d($w1i_nnz,$w2i_nnz,$w12i_flat_sorted_f);
  ##
  ##-- bigrams: to MUDL::PdlDist::Sparse2d
  my $bgenum  = MUDL::Enum::Nary->new(nfields=>2,enums=>[$enum,$enum]);
  my $bgpd    = MUDL::PdlDist::Sparse2d->new(
					     ptr=>$ptr,
					     rowids=>$rowids,
					     nzvals=>$nzvals,
					     dims=>[$nToks1,$nToks1],
					     enum=>$bgenum,
					    );

  ##-- bigrams: strings: backtranslate (if requested)
  my ($bgs);
  if ($GETBIGRAMS_STRINGS) {
    $bgs = MUDL::Bigrams->new();
    my $id2sym  = $enum->{id2sym};
    my @w1i_nnz = $w1i_nnz->list;
    my @w2i_nnz = $w2i_nnz->list;
    my @f_nnz   = $w12i_flat_sorted_f->list;
    @{$bgs->{nz}}{ map { join("\t",@$id2sym[$w1i_nnz[$_],$w2i_nnz[$_]]) } (0..$#f_nnz) } = @f_nnz;
  }

  return ($bgs,$bgpd);
}

sub getbigrams_pdltt {
  my $pb   = shift;
  my $fpdl = $pb->fullPdl()->slice("(0),:");
  return getbigrams_from_pdls($fpdl, $pb->{enums}[0], $pb->{begins});
}
#($bgs,$bgpd) = getbigrams_pdltt(load("$cfile.pdltt.bin"));

sub getbigrams_ptt {
  my $pttb = shift;
  my ($enums,$fpdl) = bench_get_full_pdl_buf_ptt($pttb);
  my $slens         = pdl(long,map {scalar(@$_)} @{$pttb->{sents}});
  my $ends          = $slens->cumusumover;
  my $begins        = $ends - $slens;
  return getbigrams_from_pdls($fpdl->slice("(0),:"), $enums->[0], $begins);
}
#($bgs,$bgpd) = getbigrams_ptt(load("$cfile.ptt.bin"));

BEGIN {
  our %getbigrams_subs =
    (
     'ttt'           => sub { getbigrams_reader(MUDL::CorpusIO->fileReader("$cfile")); },
     'ttt.cbuf.bin'  => sub { getbigrams_reader(MUDL::CorpusIO->fileReader("$cfile.cbuf.bin")); },
     'ttt.ptt.bin'   => sub { getbigrams_ptt(load("$cfile.ptt.bin")); },
     'ttt.pdltt.bin' => sub { getbigrams_pdltt(load("$cfile.pdltt.bin")); },
    );
}

sub benchall_getbigrams_pdls {
  $GETBIGRAMS_PDLS    =1;
  $GETBIGRAMS_STRINGS =0;
  $ttr = timethese(-3,\%getbigrams_subs);
  cmpthese($ttr);
  store_timeresults($ttr,'bgPdl');
}
#benchall_getbigrams_pdls();
cmpbench('bgPdl');

sub benchall_getbigrams_strings {
  $GETBIGRAMS_PDLS    =0;
  $GETBIGRAMS_STRINGS =1;
  $ttr = timethese(-3,\%getbigrams_subs);
  cmpthese($ttr);
  store_timeresults($ttr,'bgStr');
}
#benchall_getbigrams_strings();
cmpbench('bgStr');

##----------------------------------------------------------------------
## Bench: File sizes
##----------------------------------------------------------------------

sub bench_filesize {
  my $extlen=0;
  foreach (@exts) { $extlen=length($_) if (length($_) > $extlen); }
  my %ext2size = map { ($_=>(-s $files{$_})) } keys(%files);

  my $refsize   = (-s $cfile);
  my %ext2delta = qw();
  my ($ext,$size);
  while (($ext,$size)=each(%ext2size)) {
    if ($refsize < $size) {
      $ext2delta{$ext}=sprintf("+%6.2f%% / -%6.2f%%",
			       100.0*($size-$refsize)/$refsize, 100.0*($size-$refsize)/$size);
    } else {
      $ext2delta{$ext}=sprintf("-%6.2f%% / +%6.2f%%",
			       -100.0*($size-$refsize)/$refsize, -100.0*($size-$refsize)/$size);
    }
  }

  print map {
    sprintf("%-${extlen}s  %7d  (%7s)\n", $_, $ext2size{$_}||0, $ext2delta{$_}||0)
  } sort { $ext2size{$a}<=>$ext2size{$b} } keys(%ext2size);

  ##-- save results
#  open(OUT,">bufdata-size.dat");
#  print OUT
#    ('#', join("\t", '1_extid', '2_name', '3_size', '4_ttt_size'), "\n",
#     map {
#       join("\t", $extids{$_}, $_, $ext2size{$_}||0, $ext2size{'ttt'})."\n"
#     }
#     @exts
#    );
#  close(OUT);

  ##-- store
  my %bench = qw();
  $bench{$_} = {size=>$ext2size{$_}||0, ttt_size=>$ext2size{'ttt'}||0 } foreach (@exts);
  $benches->{size} = {fields=>\%bench};
  store_benches();
}
bench_filesize;

#ttt.pdltt.bin     79969  (- 47.55% / + 90.67%)
#ttt.ptt.bin      100950  (+ 33.79% / - 51.04%)
#ttt              152479  (+  0.00% / -  0.00%)
#ttt.pdlbuf.bin   155978  (+  2.29% / -  2.24%)
#ttt.bin          173041  (+ 13.49% / - 11.88%)
#ttt.ebuftt.bin   202741  (+ 32.96% / - 24.79%)
#ttt.packed.bin   204808  (+ 34.32% / - 25.55%)
#ttt.cbuf.bin     260352  (+ 70.75% / - 41.43%)
#ttt.ebuf.bin     419900  (+175.38% / - 63.69%)

##----------------------------------------------------------------------
## Bench: Data: get reader
##----------------------------------------------------------------------

BEGIN {
  our %reader_subs =
    (
     'ttt'     => sub { return MUDL::CorpusIO->fileReader("$cfile"); },
     'ttt.bin' => sub { return MUDL::CorpusIO->fileReader("$cfile.bin"); },
     'ttt.cbuf.bin' => sub { return MUDL::Corpus::Buffer->loadFile("$cfile.cbuf.bin")->reader; },
     #'ttt.pbuf2.bin' => sub { return MUDL::Corpus::Buffer::Pdl2->loadFile("$cfile.pbuf2.bin")->reader; },
     #'ttt.ebuf.bin'  => sub { return MUDL::CorpusIO->fileReader("$cfile.ebuf.bin"); },
     #'ttt.ebuftt.bin'  => sub { return MUDL::CorpusIO->fileReader("$cfile.ebuftt.bin"); },
     #'ttt.pdlbuf.bin'  => sub { return MUDL::CorpusIO->fileReader("$cfile.pdlbuf.bin"); },
     #'ttt.packed.bin'  => sub { return MUDL::CorpusIO->fileReader("$cfile.packed.bin"); },
     'ttt.ptt.bin'  => sub { return MUDL::CorpusIO->fileReader("$cfile.ptt.bin"); },
     'ttt.pdltt.bin'  => sub { return MUDL::CorpusIO->fileReader("$cfile.pdltt.bin"); },
    );
}

##----------------------------------------------------------------------
## Bench: Subs: I/O: to TT writer
##----------------------------------------------------------------------

## bench_reader_to_tt($reader)
sub bench_reader_to_tt {
  my $cr = shift;
  my $cw = MUDL::CorpusIO->fileWriter("tt:/dev/null");
  $cw->putReader($cr);
  $cw->flush();
}
#bench_reader_to_tt(MUDL::CorpusIO->fileReader("$cfile.pdltt.bin"));

BEGIN {
  our %bench_to_tt_subs =
    (
     'ttt'     => sub { bench_reader_to_tt(MUDL::CorpusIO->fileReader("$cfile")); },
     'ttt.bin' => sub { bench_reader_to_tt(MUDL::CorpusIO->fileReader("$cfile.bin")); },
     'ttt.cbuf.bin' => sub { bench_reader_to_tt(MUDL::CorpusIO->fileReader("$cfile.cbuf.bin")); },
     #'ttt.pdlbuf.bin'=>sub { bench_reader_to_tt(MUDL::CorpusIO->fileReader("$cfile.tmp.pdlbuf.bin")); },#busted!
     'ttt.ptt.bin'  => sub { bench_reader_to_tt(MUDL::CorpusIO->fileReader("$cfile.ptt.bin")); },
     'ttt.pdltt.bin'  => sub { bench_reader_to_tt(MUDL::CorpusIO->fileReader("$cfile.pdltt.bin")); },
    );
}

sub benchall_reader_to_tt {
  $ttr = timethese(-3,\%bench_to_tt_subs);
  cmpthese($ttr);
  store_timeresults($ttr,"toTT");
}
#benchall_reader_to_tt();
cmpbench('toTT');


##----------------------------------------------------------------------
## Bench: Subs: I/O: from TT reader
##----------------------------------------------------------------------

## bench_putreader_ttbuf($writer)
sub bench_putreader_ttbuf {
  my $cw = shift;
  our ($_cbr);
  $_cbr->reset();
  $_cbr->{buffer}{sents} = Storable::dclone($_cbsents); ##-- hack for stealing methods
  $cw->putReader($_cbr);
  $cw->flush();
}
#bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ttt"));
#bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ttt.bin"));
#bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.cbuf.bin"));
#bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ebuftt.bin"));##-- buggy
#bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.pdlbuf.bin"));
#bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ptt.bin"));
#bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ptt.bin"));

BEGIN {
  our $_cbr     = MUDL::CorpusIO->fileReader("$cfile.cbuf.bin");
  our $_cbsents = Storable::dclone($_cbr->{buffer}{sents});
  our %bench_putreader_subs =
    (
     'ttt'     => sub { bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ttt")); },
     'ttt.bin' => sub { bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ttt.bin")); },
     'ttt.cbuf.bin' => sub { bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.cbuf.bin")); },
     #'ttt.ebuf.bin'  => sub { bench_putreader(MUDL::CorpusIO->fileWriter("$cfile.tmp.ebuf.bin")); },
     #'ttt.ebuftt.bin'  => sub { bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ebuftt.bin")); },
     #'ttt.pdlbuf.bin'  => sub { bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.pdlbuf.bin")); },
     'ttt.ptt.bin'  => sub { bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ptt.bin")); },
     'ttt.pdltt.bin'  => sub { bench_putreader_ttbuf(MUDL::CorpusIO->fileWriter("$cfile.tmp.ptt.bin")); },
#     'ttt.packed.bin'  =>
#     sub {
#       bench_putreader(MUDL::CorpusIO->fileWriter
#		       ("$cfile.tmp.packed.bin",
#			buffer=>MUDL::Corpus::Buffer::Packed->new(enums=>{map{$_=>MUDL::Enum->new}
#									  qw(text tag 0)}))
#		      );
#     },
    );
}

sub bench_writer_putreader_ttbuf {
  $ttr = timethese(-3,\%bench_putreader_subs);
  cmpthese($ttr);
  store_timeresults($ttr,"fromTT");
}
#bench_writer_putreader_ttbuf();
cmpbench('fromTT');

##----------------------------------------------------------------------
## Bench: Subs: Process: get corpus as a list of sentence PDLs (text only)
##----------------------------------------------------------------------

## $enum = bench_get_enum_ttsents(\@sents,$i)
sub bench_get_enum_ttsents {
  my ($sents,$attri) = @_;
  my $enum = MUDL::Enum->new();
  my $sym2id = $enum->{sym2id};
  my $id2sym = $enum->{id2sym};
  foreach (@$sents) {
    foreach (@$_) {
      if (!exists($sym2id->{$_->[$attri]})) {
	$sym2id->{$_->[$attri]} = scalar(@$id2sym);
	push(@$id2sym,$_->[$attri]);
      }
    }
  }
  return $enum;
}


## $nsents = bench_get_sent_pdls_ttreader($reader)
##  + benchmarking sub for readers returning MUDL::Token:TT objects
sub bench_get_sent_pdls_ttreader {
  my $cr = shift;
  my @sents = qw();
  my ($s);
  push(@sents,$s) while (defined($s=$cr->getSentence));
  my $enum = bench_get_enum_ttsents(\@sents,0);
  my $sym2id = $enum->{sym2id};
  my $psents = [
		map {
		  pdl(long, [ map { $sym2id->{$_->[0]} } @$_ ])
		} @sents
	       ];
  return ($enum,$psents);
}
#bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile"));
#bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile.bin"));
#bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile.cbuf.bin"));

sub bench_get_sent_pdls_buffer {
  my $buf = shift;
  my $enum = bench_get_enum_ttsents($buf->{sents},0);
  my $sym2id = $enum->{sym2id};
  my $psents = [
		map {
		  pdl(long, [ map { $sym2id->{$_->[0]} } @$_ ])
		} @{$buf->{sents}}
	       ];
  return ($enum,$psents);
}
#bench_get_sent_pdls_buffer(load("$cfile.buf.bin"));

sub bench_get_sent_pdls_pdlbuffer {
  my $buf = shift;
  my $psents = [];
  my $soff = $buf->{soffsets};
  my $txtpdl = $buf->{pdl}->slice("(0),:");
  my $si0=0;
  my ($si1);
  foreach $si1 (@$soff[1..$#$soff]) {
    push(@$psents, $txtpdl->slice("$si0:".($si1-1)));
    $si0=$si1;
  }
  return ($buf->{enums}{text},$psents);
}
#bench_get_sent_pdls_pdlbuffer(load("$cfile.pdlbuf.bin"));

sub bench_get_sent_pdls_pttreader {
  my $pttr = shift;
  my $buf  = $pttr->{buffer};
  $buf->packSentences();      ##-- ensure sentences are packed (they ought to be)
  my $packas = substr($buf->{packas},0,1);
  return ($buf->{enums}[0], [map {pdl(long, map {unpack($packas,$_)} @$_)} @{$buf->{sents}}]);
}
#bench_get_sent_pdls_pttreader(load("$cfile.ptt.bin")->reader);

sub bench_get_sent_pdls_pdltt_1 {
  my $pdlr = shift;
  my $buf  = $pdlr->{buffer};
  $buf->packPdls();            ##-- ensure sentences are packed (they ought to be)
  my @begins = $buf->{begins}->list;
  my @ends   = ($buf->{ends}-1)->list;
  my $txtpdl = $buf->{pdls}[0];
  return ($buf->{enums}[0],[
			    map {$txtpdl->slice("$begins[$_]:$ends[$_]")} (0..$#begins)
			   ]);
}
#($e1,$ps1)=bench_get_sent_pdls_pdltt_1(load("$cfile.pdltt.bin")->reader);

sub bench_get_sent_pdls_pdltt_2 {
  my $pdlr = shift;
  my $buf  = $pdlr->{buffer};
  my $fpdl = $buf->fullPdl();
  my $txtpdl = $fpdl->slice("(0),:");
  my @begins = $buf->{begins}->list;
  my @ends   = ($buf->{ends}-1)->list;
  return ($buf->{enums}[0],[
			    map {$txtpdl->slice("$begins[$_]:$ends[$_]")} (0..$#begins)
			   ]);
}
#($e2,$ps2)=bench_get_sent_pdls_pdltt_2(load("$cfile.pdltt.bin")->reader);


BEGIN {
  our %get_sent_pdls_subs =
    (
     'ttt'     => sub { bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile")); },
     #'ttt.bin' => sub { bench_get_sent_pdls_ttreader(MUDL::CorpusIO->fileReader("$cfile.bin")); },
     'ttt.cbuf.bin' => sub { bench_get_sent_pdls_ttreader(load("$cfile.cbuf.bin")->reader); },
     'ttt.ptt.bin'  => sub { bench_get_sent_pdls_pttreader(load("$cfile.ptt.bin")->reader); },
     'ttt.pdltt.bin'  => sub { bench_get_sent_pdls_pdltt_1(load("$cfile.pdltt.bin")->reader); },
     #'ttt.pdltt.bin_2'  => sub { bench_get_sent_pdls_pdltt_2(load("$cfile.pdltt.bin")->reader); }, #- slower!
     ##-- old
     #'ttt.prebuf.bin' => sub { $_cbr->reset; bench_get_sent_pdls_ttreader($_cbr); },
     #'ttt.ebuf.bin'  => sub { bench_get_sent_pdls_ebuffer(load("$cfile.ebuf.bin")); },
     #'ttt.ebuftt.bin'  => sub { bench_get_sent_pdls_ebuffer_tt(load("$cfile.ebuftt.bin")); },
     #'ttt.pdlbuf.bin' => sub { bench_get_sent_pdls_pdlbuffer(load("$cfile.pdlbuf.bin")); },
     #'ttt.packed.bin' => sub { bench_get_sent_pdls_packed(load("$cfile.packed.bin")); },
    );

}

sub bench_get_sent_pdls {
  $ttr = timethese(-3,\%get_sent_pdls_subs);
  cmpthese($ttr);
  store_timeresults($ttr,"getPdls");
}
#bench_get_sent_pdls();
cmpbench('getPdls');


##----------------------------------------------------------------------
## Bench: Subs: Process: get corpus as one monster pdl
##----------------------------------------------------------------------

## ($enums,$pdl) = bench_get_sent_pdls_ttreader($reader)
sub bench_get_full_pdl_rdr_tt {
  my $cr = shift;
  my $sents = [];
  my ($s);
  push(@$sents,$s) while (defined($s=$cr->getSentence));
  my @enums  = map { bench_get_enum_ttsents($sents,$_) } (0..$#{$sents->[0][0]});
  my @sym2id = map { $_->{sym2id} } @enums;
  my ($tok);
  my $pdl    = pdl(long,
		   map {
		     map {
		       $tok=$_;
		       [map { $sym2id[$_]{$tok->[$_]} } (0..$#$_)]
		     } @$_
		   } @$sents);
  return (\@enums,$pdl);
}
#($e,$p)=bench_get_full_pdl_rdr_tt(MUDL::CorpusIO->fileReader("$cfile"));
#($e,$p)=bench_get_full_pdl_rdr_tt(MUDL::CorpusIO->fileReader("$cfile.cbuf.bin"));

sub bench_get_full_pdl_buf_tt {
  my $buf    = shift;
  my $sents  = $buf->{sents};
  my @enums  = map { bench_get_enum_ttsents($sents,$_) } (0..$#{$sents->[0][0]});
  my @sym2id = map { $_->{sym2id} } @enums;
  my ($tok);
  my $pdl    = pdl(long,
		   map {
		     map {
		       $tok=$_;
		       [map { $sym2id[$_]{$tok->[$_]} } (0..$#$_)]
		     } @$_
		   } @$sents);
  return (\@enums,$pdl);
}
#($e,$p)=bench_get_full_pdl_buf_tt(load("$cfile.cbuf.bin"));

sub bench_get_full_pdl_buf_ptt {
  my $buf  = shift;
  $buf->packSentences();      ##-- ensure sentences are packed (they ought to be)
  my $packas = $buf->{packas};
  return ($buf->{enums},
	  pdl(long,
	      map {
		map { [unpack($packas,$_)] } @$_
	      } @{$buf->{sents}})
	 );
}
#($e,$p)=bench_get_full_pdl_buf_ptt(load("$cfile.ptt.bin"));

sub bench_get_full_pdl_buf_pdltt {
  my $buf = shift;
  return ($buf->{enums},$buf->fullPdl);
}
#($e,$p)=bench_get_full_pdl_buf_pdltt(load("$cfile.pdltt.bin"));


BEGIN {
  our %get_full_pdl_subs =
    (
     'ttt'          => sub { bench_get_full_pdl_rdr_tt   (MUDL::CorpusIO->fileReader("$cfile")); },
     'ttt.cbuf.bin' => sub { bench_get_full_pdl_buf_tt   (load("$cfile.cbuf.bin")); },
     'ttt.ptt.bin'  => sub { bench_get_full_pdl_buf_ptt  (load("$cfile.ptt.bin")); },
     'ttt.pdltt.bin'=> sub { bench_get_full_pdl_buf_pdltt(load("$cfile.pdltt.bin")); },
    );
}


sub bench_get_full_pdl {
  $ttr = timethese(-3,\%get_full_pdl_subs);
  cmpthese($ttr);
  store_timeresults($ttr,"get1Pdl");
}
#bench_get_full_pdl();
cmpbench('get1Pdl');

##----------------------------------------------------------------------
## Bench: Subs: I/O: churn
##   + TODO
##----------------------------------------------------------------------


##----------------------------------------------------------------------
## Dummy
##----------------------------------------------------------------------

foreach $i (0..3) {
  print "--dummy[$i]--\n";
}
