#!/usr/bin/perl -w

$nulltag='-';

while (<>) {
  next if ($_ =~ m/^\s*$/ || $_ =~ m/^\s*%/);
  chomp;
  @fields = grep { defined($_) && $_ ne '' } split(/\t+/,$_);
  splice(@fields,1,0,$nulltag);
  $_ = join("\t", @fields) . "\n";
} continue {
  print $_;
}
