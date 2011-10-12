#!/usr/bin/env perl
use strict;
use Carp;

use FindBin;
use lib $FindBin::Bin;
use lib "$FindBin::Bin/NSTjenkins";

use FullNstTest;
use NSTjenkins::CCL;
my $master = new FullNstTest();
$master->execute();

exit 0;
