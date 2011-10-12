# NSTjenkins::SBCL
# Abstract superclass of all SBCL platform descriptions.
##

package NSTjenkins::SBCL;
use strict;
use Carp;
use NSTjenkins::Platform;
use NSTjenkins::platforms::SBCL_1_0;
our $CLASS = "NSTjenkins::SBCL";
our @ISA = ("NSTjenkins::Platform");
our $PATH;

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->declare_scalar_variable
      ('description',
       'Abstract superclass of all SBCL Lisp platform descriptions');
  $self->declare_scalar_variable('tag', 'sbcl');
  $self->declare_scalar_variable('executable_name', 'sbcl');
  $self->declare_scalar_variable('name', 'SBCL');
  $self->declare_scalar_variable('version', 'general');
}

sub get_global_path {
  return $PATH;
}

sub instance {
  my $version = shift;

  if (defined $version) {
    return new NSTjenkins::platforms::SBCL_1_0(@_);
  } else {
    return new NSTjenkins::platforms::SBCL_1_0(@_);
  }
}

1;
