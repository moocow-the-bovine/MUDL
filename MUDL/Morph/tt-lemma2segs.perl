#!/usr/bin/perl -w

$segsep='~';

while (<>) {
  chomp;
  next if ($_ =~ m/^\s*$/ || $_ =~ m/^\s*%/);
  ($text,$tag,@analyses) = grep { defined($_) && $_ ne '' } split(/\t+/,$_);
  %analyses = qw();

  while (@analyses) {
    $a = pop(@analyses);

    ##-- convert analysis to segments
    $a =~ s/[\(\)\\\#\~\*]+/$segsep/g;
    $a =~ s/([A-Z���])/$segsep$1/g;
    $a =~ tr/A-Z���/a-z���/;

    if ($a =~ m/^(.*)\|([^\|]*)$/)
    {
      push(@analyses, $1."${segsep}ge${segsep}".$2);
      $a =~ s/\|/$segsep/g;
    }
    $a =~ s/^\Q$segsep\E+//;
    $a =~ s/\Q$segsep\E+$//;
    $a =~ s/\Q$segsep\E+/$segsep/og;

    $analyses{$a}=undef;
  }
  $_ = join("\t", $text, $tag, sort(keys(%analyses)));
} continue {
  print $_, "\n";
}
