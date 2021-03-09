# NSTjenkins::platforms::CCL_1
# SBCL 1.0 platform descriptions.
##

package NSTjenkins::platforms::CCL_1;
use strict;
use Carp;
use NSTjenkins::SBCL;
our $CLASS = "NSTjenkins::platforms::CCL_1";
our @ISA = ("NSTjenkins::CCL");

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->declare_scalar('version', '1');
}

sub get_command_arguments {
  my $self = shift;
  return [ "--no-init", "--batch" ];
}
