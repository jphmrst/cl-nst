# NSTjenkins::platforms::Allegro_8_2
# Abstract superclass of all Allegro platform descriptions.
##

package NSTjenkins::platforms::Allegro_8_2;
use strict;
use Carp;
use NSTjenkins::Platform;
our $CLASS = "NSTjenkins::platforms::Allegro_8_2";
our @ISA = ("NSTjenkins::Allegro");

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->declare_scalar('description', 'Allegro 8.2 Lisp platform descriptions');
  $self->declare_scalar('version', '8.2');
}

sub get_command_arguments {
  my $self = shift;
  return [ "-batch", "-backtrace-on-error", "-qq" ];
}

