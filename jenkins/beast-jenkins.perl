#!/usr/bin/env perl
use Carp;

use FindBin;
use lib "$FindBin::Bin";
use lib "$FindBin::Bin/NSTjenkins";

use FullNstTest;
use NSTjenkins::CCL;
use NSTjenkins::Allegro;
use NSTjenkins::SBCL;
$NSTjenkins::CCL::PATH = '/home/jmaraist/Lib/Lisp/Clozure/ccl';
$NSTjenkins::Allegro::PATH = '/usr/local/acl/acl82';
$NSTjenkins::SBCL::PATH = '/usr/local/bin/sbcl';
my $master = new FullNstTest();
$master->execute();

exit 0;
