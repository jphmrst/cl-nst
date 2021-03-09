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
  $self->declare_scalar
      ('description',
       'Abstract superclass of all SBCL Lisp platform descriptions');
  $self->declare_scalar('tag', 'sbcl');
  $self->declare_scalar('executable_name', 'sbcl');
  $self->declare_scalar('name', 'SBCL');
  $self->declare_scalar('version', 'general');
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
