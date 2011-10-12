#!/usr/bin/env perl
use strict;
use Carp;

use FindBin;
use lib $FindBin::Bin;
use lib "$FindBin::Bin/NSTjenkins";

use FullNstTest;
use NSTjenkins::CCL;
$NSTjenkins::CCL::PATH = '/home/jmaraist/Lib/Lisp/Clozure/ccl';
$NSTjenkins::Allegro::PATH = '/usr/local/acl/acl82';
$NSTjenkins::SBCL::PATH = '/usr/local/bin/sbcl';
my $master = new FullNstTest();
$master->execute();

exit 0;
