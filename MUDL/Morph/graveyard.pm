sub generateModelFromCuts_argh {
  my $morph = shift;
  my $cuts  = $morph->{cuts};

  ##-- generate model enums
  my $stems = $morph->{stems} = MUDL::Enum->new();
  my $suffs = $morph->{suffs} = MUDL::Enum->new();
  my $sigs  = $morph->{sigs}  = MUDL::Enum->new();
  my $t2f   = $morph->{t2f}   = {};
  my $t2sig = $morph->{t2sig} = {};

  ##-- ensure the NULL affix (id=0) is defined
  $suffs->addIndexedSymbol('',0);

  ##-- get all stems & their suffixes
  my ($vid,$wvec,$cutvec,@cuts, $stem,$stemid,$suff,$suffid,@suffids, $i);
  $vid = -1;
  foreach $wvec (@{$morph->{wenum}}) {
    ++$vid;
    $cutvec = $cuts->{$vid};
    if (!defined($cutvec) || $cutvec eq '') {
      ##-- no cuts for this word
      $stems->addSymbol($wvec);        ##-- ... add it as a stem
      $t2f->{$wvec} = pack('S*',0); ##-- ... with only the null suffix
      next;
    }
    @cuts = unpack('S*',$cutvec);
    $stem = substr($wvec,0,2*($cuts[0]+1));
    $stemid = $stems->addSymbol($stem);
    push(@cuts,length($wvec)/2-1); ##-- ???
    @suffids = qw();
    for ($i=1; $i < $#cuts; $i++) {
      $suff   = substr($wvec, 2*($cuts[$i]+1), 2*($cuts[$i+1]-$cuts[$i]));
      push(@suffids, $suffs->addSymbol($suff));
    }
    $t2f->{$stemid} = pack('S*', sort {$a<=>$b} @suffids);
  }
}
