#!/usr/bin/env perl
use strict;
use Carp;

use FindBin;
use lib $FindBin::Bin;
use lib "$FindBin::Bin/NSTjenkins";

use FullNstTest;
use NSTjenkins::CCL;
$NSTjenkins::CCL::PATH = '/home/jmaraist/Lib/Lisp/Clozure';
$NSTjenkins::Allegro::PATH = '/usr/local/acl/acl82';
$NSTjenkins::SBCL::PATH = '/home/jmaraist/Beast/sbcl-1.0.50-x86-64-linux/install/bin';
my $master = new FullNstTest();
$master->execute();

exit 0;
