# NSTjenkins::Allegro
# Abstract superclass of all Allegro platform descriptions.
##

package NSTjenkins::Allegro;
use strict;
use Carp;
use NSTjenkins::Platform;
use NSTjenkins::platforms::Allegro_8_2;
our $CLASS = "NSTjenkins::Allegro";
our @ISA = ("NSTjenkins::Platform");
our $PATH;

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->declare_scalar
      ('description',
       'Abstract superclass of all Allegro Lisp platform descriptions');
  $self->declare_scalar('tag', 'allegro');
  $self->declare_scalar('mixedcase', 0);
  $self->declare_scalar('eightbitmode', 0);
  $self->declare_scalar('name', 'Allegro');
  $self->declare_scalar('version', 'general');
}

sub tag {
  my $self = shift;
  my $mixedcase = $self->mixedcase();
  my $eightbitmode = $self->eightbitmode();
  return $self->SUPER::tag()
      . ($mixedcase ? "m" : "c") . ($eightbitmode ? "8" : "");
}

sub instance {
  my $version = shift;

  if (defined $version) {
    return new NSTjenkins::platforms::Allegro_8_2(@_);
  } else {
    return new NSTjenkins::platforms::Allegro_8_2(@_);
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
  my $mixedcase = $self->mixedcase();
  my $eightbitmode = $self->eightbitmode();
  return "$name $version "
      . ($mixedcase ? "modern" : "classic")
      . ($eightbitmode ? " 8-bit mode" : "")
      . " ($tag)";
}

sub executable_name {
  my $self = shift;
  if ($self->mixedcase) {
    if ($self->eightbitmode) {
      return "mlisp8";
    } else {
      return "mlisp";
    }
  } else {
    if ($self->eightbitmode) {
      return "alisp8";
    } else {
      return "alisp";
    }
  }
}

sub shutdown {
  my $self = shift;
  my $testrun = shift;
  my $stream = shift;
  print {$stream} "(exit 0)\n";
}

1;
