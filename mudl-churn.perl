#!/usr/bin/perl -w

use lib qw(.);
use MUDL::CorpusIO;
use Getopt::Long qw(:config no_ignore_case);
use Pod::Usage;
use File::Basename qw(basename);

use Time::HiRes qw(gettimeofday tv_interval);

##----------------------------------------------------------------------
## Globals
##----------------------------------------------------------------------

our $VERSION = "0.01";

##-- program vars
our $progname     = basename($0);
our $outfile      = '-';
our $verbose      = 2;

##-- XML vars
our $XML_VERSION  = "1.0";
our $XML_ENCODING = 'UTF-8';

our $fmt_level = 0;
our $xmlmode = 'DEFAULT';

##----------------------------------------------------------------------
## Command-line processing
##----------------------------------------------------------------------
GetOptions(##-- general
	   'help|h' => \$help,
	   'man|M'  => \$man,
	   'version|V' => \$version,
	   'verbose|v=i' => \$verbose,

	   ##-- I/O
	   'output|o=s' => \$outfile,

	   'xmlmode|mode|m=s' => \$xmlmode,
	   'xmlencoding|xe|e=s' => \$XML_ENCODING,
	   'xmlversion|xv=s' => \$XML_VERSION,
	   'xmlformat|format|f=i' => \$fmt_level,
	  );

pod2usage({
	   -exitval=>0,
	   -verbose=>0
	  }) if ($help);
pod2usage({
	   -exitval=>0,
	   -verbose=>1
	  }) if ($man);

if ($version || $verbose >= 1) {
  print STDERR "$progname version $VERSION by Bryan Jurish\n";
  exit 0 if ($version);
}

##----------------------------------------------------------------------
## Subs: messages
##----------------------------------------------------------------------

# undef = vmsg($level,@msg)
#  + print @msg to STDERR if $verbose >= $level
sub vmsg {
  my $level = shift;
  print STDERR (@_) if ($verbose >= $level);
}

##----------------------------------------------------------------------
## MAIN
##----------------------------------------------------------------------
push(@ARGV, '-') if (!@ARGV);

$cr = 'MUDL::CorpusIO';
$cw = MUDL::CorpusWriter->fileWriter($outfile,
				     format=>$fmt_level,
				     xmlencoding=>$XML_ENCODING,
				     xmlversion=>$XML_VERSION)
  or die("$progname: could not create CorpusWriter for '$outfile': $!");

$ntoks = $nfiles = 0;
$time0 = [gettimeofday];

foreach $file (@ARGV) {
  vmsg(2, "$progname: processing ", basename($file), ": ");
  $cr->reset() if (ref($cr));
  $cr = $cr->fileReader($file, mode=>$xmlmode);

  $timeF0 = [gettimeofday];

  $cw->putSentence($s) while (defined($s=$cr->getSentence()));

  $ntoks += $ntoksF=$cr->nTokens;
  vmsg(2,
       "$ntoksF tok @ ",
       sprintf("%.2f", $ntoksF / tv_interval($timeF0, [gettimeofday])),
       " tok/sec\n");
  ++$nfiles;
}

$elapsed = tv_interval($time0, [gettimeofday]);
$elapsed = .001 if (!$elapsed);
vmsg(2, "$progname: $nfiles files, $ntoks toks in ",
     sprintf("%.2f secs @ %.2f tok/sec\n",
	     $elapsed, $ntoks / $elapsed));

$cw->flush;

__END__

###############################################################
## pods
###############################################################

=pod

=head1 NAME

program.perl - example program

=head1 SYNOPSIS

 program.perl OPTIONS [FILE(s)]

 General Options:
   -help
   -version
   -verbose LEVEL

 Other Options:
   -???

=cut

###############################################################
## OPTIONS
###############################################################
=pod

=head1 OPTIONS

=cut

###############################################################
# General Options
###############################################################
=pod

=head2 General Options

=over 4

=item -help

Display a brief help message and exit.

=item -version

Display version information and exit.

=item -verbose LEVEL

Set verbosity level to LEVEL.  Default=1.

=back

=cut


###############################################################
# Other Options
###############################################################
=pod

=head2 Other Options

=over 4

=item -someoptions ARG

Example option.

=back

=cut


###############################################################
# Bugs and Limitations
###############################################################
=pod

=head1 BUGS AND LIMITATIONS

Probably many.

=cut


###############################################################
# Footer
###############################################################
=pod

=head1 ACKNOWLEDGEMENTS

Perl by Larry Wall.

=head1 AUTHOR

Bryan Jurish E<lt>moocow@ling.uni-potsdam.deE<gt>

=head1 SEE ALSO

perl(1).

=cut

