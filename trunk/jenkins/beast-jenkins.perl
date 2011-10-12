#!/usr/bin/env perl
use strict;
use Carp;

print "+++ Workspace ", $ENV{WORKSPACE}, "\n";
print "+++ NST ", $ENV{NST_DIR}, "\n";
print "+++ Closer ", $ENV{CLOSER_MOP_DIR}, "\n";
print "*** $FindBin::Bin ***\n";
use lib ( $FindBin::Bin, "$FindBin::Bin/NSTjenkins" );  # The absolute directory where this file

use FullNstTest;
use NSTjenkins::CCL;
$NSTjenkins::CCL::PATH = '/home/jmaraist/Lib/Lisp/Clozure';
$NSTjenkins::Allegro::PATH = '/usr/local/acl/acl82';
$NSTjenkins::SBCL::PATH = '/home/jmaraist/Beast/sbcl-1.0.50-x86-64-linux/install/bin';
my $master = new FullNstTest();
$master->execute();

exit 0;
