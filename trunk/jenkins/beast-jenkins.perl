#!/usr/bin/env perl
use strict;
use Carp;

use FullNstTest;

use NSTjenkins::CCL;
$NSTjenkins::CCL::PATH = '/home/jmaraist/Lib/Lisp/Clozure/ccl';
my $master = new FullNstTest();
$master->execute();

exit 0;
