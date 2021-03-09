# NSTjenkins::platforms::SBCL_1_0
# SBCL 1.0 platform descriptions.
##

package NSTjenkins::platforms::SBCL_1_0;
use strict;
use Carp;
use NSTjenkins::SBCL;
our $CLASS = "NSTjenkins::platforms::SBCL_1_0";
our @ISA = ("NSTjenkins::SBCL");

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->declare_scalar('version', '1.0');
}

sub get_command_arguments {
  my $self = shift;
  return [ "--lose-on-corruption", "--disable-debugger",
           "--no-sysinit", "--no-userinit" ];
}
