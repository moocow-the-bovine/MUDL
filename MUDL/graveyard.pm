#########################################################################
# from: Bigrams.pm
#########################################################################

##======================================================================
## conditionalize

## $bg = $bg->conditionalize()
## $bg = $bg->conditionalize($unigrams)
## $bg = $bg->conditionalize($unigrams,$totalunigrams)
##   + converts $bg->{nz} to { "$w1$fs$w2" => P($w2|$w1), ... }
sub conditionalize {
  my ($bg,$ug,$total) = @_;
  $ug = $bg->unigrams if (!$ug);
  $total = $ug->total if (!$total);
  my ($k,$f1,$w1,$w2);
  foreach $k (keys(%{$bg->{nz}})) {
    ($w1,$w2) = split($bg->{fs}, $k, 2);
    $f1 = $ug->{$w1};
    if ($f1) {
      $bg->{nz}{$k} /= $f1;
    } else {
      carp ( __PACKAGE__ , "::conditionalize(): no unigram probability for '$w1': set to zero.\n");
      $bg->{nz}{$k} = 0;
    }
  }
  return $bg;
}

##======================================================================
## metrics, etc

## $H = $bg->conditionalEntropy($unigrams)
## $H = $bg->conditionalEntropy($unigrams,$ugtotal)
## $H = $bg->conditionalEntropy($unigrams,$ugtotal,$bgtotal)
sub conditionalEntropy {
  my ($bg,$ug,$ugtotal,$bgtotal) = @_;
  $ug = $bg->unigrams if (!$ug);
  my $Huni   = $ug->entropy($ugtotal);
  my $Hjoint = $bg->entropy($bgtotal);
  return $Hjoint - $Huni;
}
sub conditionalEntropy0 {
  my ($bg,$ug,$ugtotal,$bgtotal) = @_;
  $ug = $bg->unigrams if (!$ug);
  $ugtotal = $ug->total if (!$ugtotal);
  $bgtotal = $bg->total if (!$bgtotal);
  my ($k,$p1,$p12,$w1,$w2);
  my $H = 0;
  foreach $k (keys(%{$bg->{nz}})) {
    ($w1,$w2) = split($bg->{fs}, $k, 2);
    #$p1 = $ug->{$w1} / $ugtotal;
    $p2 = $ug->{$w2} / $ugtotal;
    $p12 = $bg->{nz}{$k} / $bgtotal;
    if ($p12 && $p2) {
      $H += $p12 * log($p2/$p12)/log(2);
    } else {
      carp ( __PACKAGE__ , "::conditionalEntropy(): missing data for '$w1,$w2': ignoring.\n");
    }
  }
  my $nz = $bg->nZero;
  $H += $bg->{zmass} * log($bgtotal/($bg->{zmass}/$nz))/log(2) if ($bg->{zmass} && $nz);
  return $H;
}

