#!/usr/bin/env perl

package PRT;

use strict;
use POSIX;

use FindBin;
use lib ( $FindBin::Bin );  # The absolute directory where this file
                            # lives.
chdir $FindBin::Bin;

use Carp qw ( verbose );
use Cwd;
use Data::Dumper;
use File::Basename;
use File::Copy qw ( copy );
use File::Find;
use File::Glob qw( :glob );
use File::Path qw(); # do not import any symbols
use File::Spec;
use FileHandle;
use Getopt::Long qw( :config no_ignore_case bundling );
use Sys::Hostname;

my @failures;

unlink "log"  if (-x "log") && !(-d "log");
mkdir "log"  if !(-x "log");
die "Can't create/verify log directory" unless -d "log";

die "NST_DIR not set" unless defined $ENV{NST_DIR};
die "NST_DIR not a directory" unless -d $ENV{NST_DIR};
my $nstdir = $ENV{NST_DIR};

die "CLOSER_MOP_DIR not set" unless defined $ENV{CLOSER_MOP_DIR};
die "CLOSER_MOP_DIR not a directory" unless -d $ENV{CLOSER_MOP_DIR};
my $closerdir = $ENV{CLOSER_MOP_DIR};

my $nstdirlisp = $FindBin::Bin . "/nstdir.lisp";
open DIRSETTER, ("> ".$FindBin::Bin."/".$nstdirlisp);
print DIRSETTER "(defconstant +NST-DIRECTORY+ #p\"", $nstdir, "\")\n";
print DIRSETTER "(defconstant +CLOSER-DIRECTORY+ #p\"", $closerdir, "\")\n";
close DIRSETTER;

my @lisps = ({
              tag => 'NST_ALLEGRO_MIXEDCASE',
              name => "Allegro mixed-case",
              leadArgs => [ "-qq", "-batch" ],
              fileArgsLead => [ "-L" ],
              trailArgs => [ "-kill" ]
             },
             {
              tag => 'NST_ALLEGRO_UPCASE',
              name => "Allegro uppercase",
              leadArgs => [ "-qq", "-batch" ],
              fileArgsLead => [ "-L" ],
              trailArgs => [ "-kill" ]
             },
             {
              tag => 'NST_ALLEGRO_MIXEDCASE8',
              name => "Allegro mixed-case 8-bit mode",
              leadArgs => [ "-qq", "-batch" ],
              fileArgsLead => [ "-L" ],
              trailArgs => [ "-kill" ]
             },
             {
              tag => 'NST_ALLEGRO_UPCASE8',
              name => "Allegro uppercase 8-bit mode",
              leadArgs => [ "-qq", "-batch" ],
              fileArgsLead => [ "-L" ],
              trailArgs => [ "-kill" ]
             },
             {
              tag => 'NST_CLISP_UPCASE',
              name => "CLISP default",
              leadArgs => [ "-on-error", "exit", "-x" ],
              fileArgsLead => [ "-i" ],
              trailArgs => [ "-i", "quit.lisp" ]
             },
             {
              tag => 'NST_CLISP_MIXEDCASE',
              name => "CLISP mixed-case",
              leadArgs => [ "-modern", "-on-error", "exit", "-x" ],
              fileArgsLead => [ "-i" ],
              trailArgs => [ "-i", "quit.lisp" ]
             },
             {
              tag => 'NST_CCL_UPCASE',
              name => "Clozure CL default",
              leadArgs => [ "--no-init", "--batch" ],
              fileArgsLead => [ "--load" ],
              trailArgs => [ "--load", "quit.lisp" ]
             },
             {
              tag => 'NST_SBCL_UPCASE',
              name => "SBCL uppercase",
              leadArgs => [ "--lose-on-corruption", "--no-sysinit", "--no-userinit" ],
              fileArgsLead => [ "--load" ],
              trailArgs => [ "--load", "quit.lisp", "--end-toplevel-options" ]
             },
##             {
##              tag => 'NST_LW_UPCASE',
##              name => "LispWorks uppercase",
##              leadArgs => [ "-siteinit", "-", "-init" ],
##              fileArgsLead => [  ],
##              trailArgs => [  ]
##             },
            );

sub runLisp {
  my $tag = shift;
  my $name = shift;
  my $action = shift;
  my @call = @_;
  print "Running $name\n";
  # print join(' ', @call), "\n";
  my $OLD_OUT = \*STDOUT;
  my $pid = fork;
  if ($pid == 0) {
    my $logfile = lc "log/$tag.log";
    open STDOUT, "> $logfile" or die "Can't reset STDOUT: $!";
    open STDERR, ">&STDOUT" or die "Can't dup STDERR to STDOUT: $!";
    select STDERR; $| = 1;      # make unbuffered
    select STDOUT; $| = 1;      # make unbuffered
    exec @call;
    exit 1;
  }
  waitpid $pid, 0;
  my $result = $?;
  if ($result != 0) {
    push @failures, "$name exits with failure $action";
    print "$name exits with failure $action\n";
  } else {
    print "Normal exit\n";
  }
  return $result;
}

foreach my $lispConfig (@lisps) {
  my $tag = $lispConfig->{tag};
  my $ignoreTag = "SKIP_".$tag;
  my $name = $lispConfig->{name};
  my $leadArgs = $lispConfig->{leadArgs};
  my $fileArgsLead = $lispConfig->{fileArgsLead};
  my $trailArgs = $lispConfig->{trailArgs};
  print "--------------------\n";

  if (!(defined $ENV{$ignoreTag})) {
    my $executable = $ENV{$tag};
    if (defined $executable) {
      if (-x $executable) {
        my @precall;
        push @precall, $executable;
        push @precall, @$leadArgs;
        push @precall, @$fileArgsLead;
        push @precall, $nstdirlisp;
        push @precall, @$fileArgsLead;
        push @precall, "require-asdf.lisp";
        push @precall, @$fileArgsLead;
        push @precall, "path-init.lisp";

        runLisp "$tag-full", $name, 'on full recompile',
          @precall, @$fileArgsLead, "test-nst-force.lisp", @$trailArgs;
        print " - - - - - - - - - -\n";
        runLisp "$tag-load", $name, 'loading without recompile',
          @precall, @$fileArgsLead, "test-nst-noforce.lisp", @$trailArgs;
      } else {
        push @failures, "No binary $executable for $name";
      }
    } else {
      push @failures, "$tag not set";
    }
  }
}
  print "====================\n";

if ($#failures >= 0) {
  foreach my $f (@failures) {
    print $f, "\n";
  }
  exit 1;
} else {
  print "No failures detected.\n";
  exit 0;
}

