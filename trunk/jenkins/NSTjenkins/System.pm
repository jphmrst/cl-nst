# NSTjenkins::System
# Description of an invocation of an ASDF system.
##

package NSTjenkins::System;
use strict;
use Carp;
use NSTjenkins::Base;
our $CLASS = "NSTjenkins::System";
our @ISA = ("NSTjenkins::Base");

sub initialize {
  my $self = shift;
  my $name = shift;
  $self->{_asdf_directories} = [];
  $self->SUPER::initialize(@_);
  $self->declare_scalar_variable
      ('description', 'Abstract superclass of an ASDF system invocation.');
  $self->declare_scalar_variable('name', $name);
  $self->declare_scalar_variable('force', undef);
}

sub invocation {
  my $self = shift;
  my $result = "(asdf:oos 'asdf:test-op :" . $self->name;
  $result .= " :force t" if ($self->force);
  $result .= ")";
  return $result;
}

1;
