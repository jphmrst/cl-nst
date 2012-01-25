# NSTjenkins::Base
# Base class providing core class functionality.
# (Usually) the common superclass
##

package NSTjenkins::Base;
our $AUTOLOAD;  # it's a package global
our $CLASS = "PRT::Base";
use strict;
use Carp;

# Generic constructor
sub new {
  my $class = shift;
  my $self = { __vars => {},
               __listacc => {} };
  bless $self, $class;
  $self->initialize(@_);
  return $self;
}

sub initialize {
  my $self = shift;
  # $self->SUPER::initialize(@_);
}

sub declare_scalar_variable {
  my $self = shift;
  my $field = shift;
  my $init = shift;
  $self->{__vars}{$field} = $init;
}

sub declare_list_accumulator {
  my $self = shift;
  my $field = shift;
  my $init = shift;
  $init = [] unless defined $init;
  my $plural = shift;
  $plural = $field . "s" unless defined $plural;
  $self->{__listaccs}{$field} = $init;
  $self->{__listaccslot}{$plural} = $field;
}

sub DESTROY {
  # could remove this object from the appropriate hashes
}

# Provide all accessors transparently based upon the "_permitted"
# fields, with defaults
sub AUTOLOAD {
  my $self = shift;
  return if $AUTOLOAD =~ /::DESTROY$/;
  my $type = ref($self);

  $AUTOLOAD =~ /.*::(\w+)$/;
  my $name = $1;
  if (defined $name) {
    if (exists $self->{__vars}{$name} ) {
      if ($#_ == 0) {
        my $value = shift;
        $self->{__vars}{$name} = $value;
        return $self;
      } else {
        return $self->{__vars}{$name};
      }

    } elsif (exists $self->{__listaccslot}{$name}
             && exists $self->{__listaccs}{$self->{__listaccslot}{$name}} ) {
      warn "$AUTOLOAD arguments ignored"  if $#_ >= 0;
      return $self->{__listaccs}{$self->{__listaccslot}{$name}};

    } elsif ($name =~ /^add_(.*)/ && exists $self->{__listaccs}{$1} ) {
      die "$AUTOLOAD requires at least one argument"  if $#_ < 0;
      push @{$self->{__listaccs}{$1}}, @_;
      return $self;

    }
  }

  die(ref($self) . qq(: no such method $name));
}

1;
