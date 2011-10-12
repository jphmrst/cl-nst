# NSTjenkins::CLISP
# Abstract superclass of all CLISP platform descriptions.
##

package NSTjenkins::CLISP;
use strict;
use Carp;
use NSTjenkins::Platform;
use NSTjenkins::platforms::CLISP_2;
our $CLASS = "NSTjenkins::CLISP";
our @ISA = ("NSTjenkins::Platform");

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->declare_scalar_variable
      ('description',
       'Abstract superclass of all CLISP Lisp platform descriptions');
  $self->declare_scalar_variable('mixedcase', 0);
  $self->declare_scalar_variable('tag', 'clisp');
  $self->declare_scalar_variable('executable_name', 'clisp');
  $self->declare_scalar_variable('name', 'CLISP');
  $self->declare_scalar_variable('version', 'general');
}

sub instance {
  my $version = shift;

  if (defined $version) {
    return new NSTjenkins::platforms::CLISP_2(@_);
  } else {
    return new NSTjenkins::platforms::CLISP_2(@_);
  }
}

sub fullname {
  my $self = shift;
  my $name = $self->name();
  my $version = $self->version();
  my $tag = $self->tag();
  my $mixedcase = $self->mixedcase();
  return "$name $version "
      . ($self->mixedcase ? " modern (mixed-case) mode" : "")
      . " ($tag)";
}

sub setup_require_asdf {
  my $self = shift;
  my $testrun = shift;
  my $stream = shift;
  print {$stream} "(require '", ($self->mixedcase ? "ASDF" : "asdf"), ")\n";
}

sub tag {
  my $self = shift;
  my $mixedcase = $self->mixedcase();
  return $self->SUPER::tag() . ($mixedcase ? "m" : "");
}
