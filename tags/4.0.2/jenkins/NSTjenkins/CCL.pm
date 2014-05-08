# NSTjenkins::CCL
# Abstract superclass of all CCL platform descriptions.
##

package NSTjenkins::CCL;
use strict;
use Carp;
use NSTjenkins::Platform;
use NSTjenkins::platforms::CCL_1;
our $CLASS = "NSTjenkins::CCL";
our @ISA = ("NSTjenkins::Platform");
our $PATH;

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->declare_scalar_variable
      ('description',
       'Abstract superclass of all CCL Lisp platform descriptions');
  $self->declare_scalar_variable('tag', 'clozure');
  $self->declare_scalar_variable('arch64', 0);
  $self->declare_scalar_variable('executable_name', 'lx86cl');
  $self->declare_scalar_variable('name', 'CCL');
  $self->declare_scalar_variable('version', 'general');
}

sub instance {
  my $version = shift;

  if (defined $version) {
    return new NSTjenkins::platforms::CCL_1(@_);
  } else {
    return new NSTjenkins::platforms::CCL_1(@_);
  }
}

sub get_global_path {
  return $PATH;
}

sub fullname {
  my $self = shift;
  my $name = $self->name();
  my $version = $self->version();
  my $tag = $self->tag();
  my $arch64 = $self->arch64();
  return "$name $version "
      . ($self->arch64 ? "64-bit more" : "32-bit mode")
      . " ($tag)";
}

sub executable_name {
  my $self = shift;
  return $self->SUPER::executable_name() . ($self->arch64 ? "64" : "");

}

sub tag {
  my $self = shift;
  my $arch64 = $self->arch64();
  return $self->SUPER::tag() . ($arch64 ? "64" : "");
}

1;
