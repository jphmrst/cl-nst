# NSTjenkins::Platform
# Abstract superclass of all Lisp platform descriptions.
##

package NSTjenkins::Platform;
use strict;
use Carp;
use NSTjenkins::Base;
our $CLASS = "NSTjenkins::Platform";
our @ISA = ("NSTjenkins::Base");
our $gensym=0;

sub initialize {
  my $self = shift;
  $self->SUPER::initialize(@_);
  $self->declare_scalar_variable('tag', sprintf("platform%05d", ++$gensym));
  $self->declare_scalar_variable
      ('description', 'Abstract superclass of all Lisp platform descriptions');
  $self->declare_scalar_variable('path', undef);
  $self->declare_scalar_variable('executable_name', undef);
  $self->declare_list_accumulator('testrun');
  $self->declare_list_accumulator('version');
}

sub launch {
  die "Concrete class must give a definition for launch";
}

sub fullname {
  my $self = shift;
  my $name = $self->name();
  my $version = $self->version();
  my $tag = $self->tag();
  return "$name $version ($tag)";
}

sub get_full_executable {
  my $self = shift;
  my $result = $self->executable_name();
  my $path = $self->path();
  $path = get_global_path() unless defined $path;
  if (defined $path) {
    if ($path ne '') {
      $path .= '/' unless $path =~ m|/$|;
    }
    $result = $path . $result;
  }
  return $result;
}

sub get_global_path {
  return undef;
}

sub get_command_arguments {
  my $self = shift;
  return [];
}

sub get_command_line {
  my $self = shift;
  my @result = ( $self->get_full_executable() );
  foreach my $arg (@{$self->get_command_arguments()}) {
    push @result, $arg;
  }
  return \@result;
}

sub generate_testrun_input {
  my $self = shift;
  my $testrun = shift;
  my $stream = shift;

  $self->setup($testrun, $stream);
  $testrun->execute($stream);
  $self->shutdown($testrun, $stream);
}

sub execute_testrun {
  my $self = shift;
  my $testrun = shift;
  my $log_formatstring = shift;

  my $commandLine = $self->get_command_line();
  my $logdir = sprintf($log_formatstring, $self->tag, $testrun->tag);

  pipe(READ,WRITE);
  my $pid = fork();
  if ($pid==0) {
    close WRITE;
    close STDIN;
    open STDIN, "<&READ";
    close STDOUT;
    open STDOUT, ">$logdir";
    close STDERR;
    open STDERR, ">&STDOUT";
    exec @$commandLine;
  }

  close READ;
  $self->generate_testrun_input($testrun, \*WRITE, $logdir);
  close WRITE;
  waitpid($pid, 0);
  my $result = $?;
  print "   FAILED\n" unless $result==0;
  return $result;
}

sub execute {
  my $self = shift;
  my $global_testruns = shift;
  my $log_formatstring = shift;
  my $local_testruns = $self->testruns();
  print "\nRunning jobs on ", $self->fullname, "\n";
  my @result = ();

  foreach my $testrun (@$global_testruns) {
    if ($self->execute_testrun($testrun, $log_formatstring) != 0) {
      push @result, [$self, $testrun];
    }
  }
  foreach my $testrun (@$local_testruns) {
    if ($self->execute_testrun($testrun, $log_formatstring) != 0) {
      push @result, [$self, $testrun];
    }
  }

  return \@result;
}

sub setup {
  my $self = shift;
  my $testrun = shift;
  my $stream = shift;
  $self->setup_require_asdf($testrun, $stream);
  foreach my $dir (@{$testrun->asdf_paths()}) {
    print {$stream} sprintf("(push #p\"%s\" asdf:*central-registry*)", $dir);
  }
}

sub setup_require_asdf {
  my $self = shift;
  my $testrun = shift;
  my $stream = shift;
  print {$stream} "(require 'asdf)\n";
}

sub shutdown {
  my $self = shift;
  my $testrun = shift;
  my $stream = shift;
  print {$stream} "(quit)\n";
}

1;
