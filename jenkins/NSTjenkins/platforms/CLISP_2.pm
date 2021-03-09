# NSTjenkins::platforms::CLISP_2
# SBCL 1.0 platform descriptions.
##

package NSTjenkins::platforms::CLISP_2;
use strict;
use Carp;
use NSTjenkins::SBCL;
our $CLASS = "NSTjenkins::platforms::CLISP_2";
our @ISA = ("NSTjenkins::CLISP");

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->declare_scalar('version', '2');
}

sub get_command_arguments {
  my $self = shift;
  my @result = ( "-on-error", "exit", "-ansi" );
  push @result, "-modern" if $self->mixedcase;
  return \@result;
}
