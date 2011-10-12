#!/usr/bin/env perl
use strict;
use Carp;

use FindBin;
print "*** $FindBin::Bin ***\n";
use lib ( $FindBin::Bin, "$FindBin::Bin/NSTjenkins" );  # The absolute directory where this file

use FullNstTest;

use NSTjenkins::CCL;
$NSTjenkins::CCL::PATH = '/home/jmaraist/Lib/Lisp/Clozure/ccl';
my $master = new FullNstTest();
$master->execute();

exit 0;
