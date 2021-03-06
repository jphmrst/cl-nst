# NSTjenkins::TestRun
# Description of a Lisp system run
##

package NSTjenkins::TestRun;
use strict;
use Carp;
use NSTjenkins::Base;
our $CLASS = "NSTjenkins::TestRun";
our @ISA = ("NSTjenkins::Base");
our $gensym=0;

sub initialize {
  my $self = shift;
  my $name = shift;
  my $tag = shift;
  $tag = sprintf("TestRun%05d", ++$gensym) unless defined $tag;
  $self->SUPER::initialize(@_);

  $self->declare_scalar('name', $name);
  $self->declare_scalar('tag', $tag);
  $self->declare_scalar
      ('description', 'Description of one run of a Lisp session');
  $self->declare_list('asdf_path');
  $self->declare_list('system');
}

sub execute {
  my $self = shift;
  my $stream = shift;
  my $systems = $self->systems();
  print " - ", $self->name, "...\n";
  foreach my $system (@$systems) {
    print {$stream} $system->invocation(), "\n";
  }
}

1;
