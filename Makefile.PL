use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile
  (
   NAME         => 'MUDL',
   VERSION_FROM => 'MUDL.pm', # finds $VERSION
   AUTHOR       => 'Bryan Jurish <moocow@ling.uni-potsdam.de>',
   ABSTRACT     => 'MUDL Unsupervised Dependency Learner',
   PREREQ_PM    => {
		    XML::LibXML => undef,   # for XML I/O
		    XML::LibXSLT => undef,  # ... with implicit stylesheets
		    PDL => undef,           # for GT smoothing, etc.
		    Storable => undef,      # for binary storage
		    IO::Scalar => undef,    # for string I/O
		   },
  );

#---------------------------------------------------------------
# Extensions
#---------------------------------------------------------------

sub MY::postamble {
  # how to format the README file
  "README.txt : README.pod\n"
  ."\tpod2text README.pod README.txt\n\n";
}

sub MY::dist_core {
  package MY;
  my $inherited = shift->SUPER::dist_core(@_);
  $inherited =~
    # ... distribution requires current README file
    s/^dist\s*:/dist\ : README.txt/m;
  $inherited;
}
