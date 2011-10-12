
package NSTtestrun;
use strict;
use Carp;
use NSTjenkins::TestRun;
our @ISA = ("NSTjenkins::TestRun");
our $gensym=0;

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->add_asdf_path('../');
  $self->add_asdf_path('../ext/defdoc/');
  $self->add_asdf_path('../ext/defcontract/');
  $self->add_asdf_path('../test/');
  $self->add_asdf_path('../test/asdf/');
  $self->add_asdf_path('../test/direct/');
  $self->add_asdf_path('../test/manual/');
  $self->add_asdf_path('../test/meta/');
  $self->add_asdf_path('../test/util/');
  $self->add_asdf_path('../../../../Lib/Lisp/SIFT/closer-mop/');
}

1;

package FullNstTest;
use strict;
use Carp;
use NSTjenkins::Master;
our @ISA = ("NSTjenkins::Master");

use FindBin;
use lib ( $FindBin::Bin );

use NSTjenkins::TestRun;
use NSTjenkins::System;
use NSTjenkins::Allegro;
use NSTjenkins::SBCL;
use NSTjenkins::CLISP;
use NSTjenkins::CCL;

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);

  my $forcecompile = new NSTtestrun("Fresh compile", 'force');
  $forcecompile->add_system
      (new NSTjenkins::System('nst-test-jenkins')->force(1));
  my $noforcecompile = new NSTtestrun("Without re-compile", 'noforce');
  $noforcecompile->add_system(new NSTjenkins::System('nst-test-jenkins'));

  $self->name('NST multiplatform tests');
  $self->tag('nst');
  $self->add_testrun($forcecompile, $noforcecompile);
  $self->add_platform(NSTjenkins::CCL::instance()->arch64(1));
  $self->add_platform(NSTjenkins::CCL::instance());
  $self->add_platform(NSTjenkins::CLISP::instance());
  $self->add_platform(NSTjenkins::CLISP::instance()->mixedcase(1));
  $self->add_platform(NSTjenkins::SBCL::instance());
  $self->add_platform(NSTjenkins::Allegro::instance()->mixedcase(0));
  $self->add_platform(NSTjenkins::Allegro::instance()->mixedcase(1));
}

1;
