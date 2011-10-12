# NSTjenkins::Master
# Top-level control of an NST-Jenkins run.
##

package NSTjenkins::Master;
use strict;
use Carp;
use NSTjenkins::Base;
our $CLASS = "NSTjenkins::Master";
our @ISA = ("NSTjenkins::Base");
our $gensym=0;

sub initialize {
  my $self = shift;
  my $name = shift;
  my $tag = shift;
  $tag = sprintf("NSTjenkins%05d", ++$gensym) unless defined $tag;
  $self->SUPER::initialize(@_);
  $self->declare_scalar_variable('name', $name);
  $self->declare_scalar_variable('tag', $tag);
  $self->declare_scalar_variable('logsdir', 'log');
  $self->declare_scalar_variable('description',
                                 'Top-level control of an NSTjenkins run');
  $self->declare_list_accumulator('testrun');
  $self->declare_list_accumulator('platform');
}

sub execute {
  my $self = shift;
  my $testruns = $self->testruns();

  my $logsdir = $self->logsdir();
  die "$logsdir not a directory" if -e $logsdir && !(-d $logsdir);
  mkdir $logsdir unless -e $logsdir;

  my $name = $self->name;
  if (defined $name) {
    print "=== Running ", $self->name, " ===\n";
  } else {
    print "=== Running unnamed NST-jenkins job ===\n";
  }

  my @failures=();
  foreach my $platform (@{$self->platforms}) {
    my $logname = sprintf("%s/%s-%%s-%%s.log",$logsdir,$self->tag);
    foreach my $pair (@{$platform->execute($testruns,$logname)}) {
      push @failures, $pair;
    }
  }

  if (defined $name) {
    print "=== End of ", $self->name, " ===\n\n";
  } else {
    print "=== End of unnamed NST-jenkins job ===\n\n";
  }

  $self->on_failures(\@failures)  if ($#failures>=0);
}

sub on_failures {
  my $self = shift;
  my $failures = shift;

  print 1+$#{$failures}, " FAILURE", ($#{$failures}!=0 ? "S" : ""), ":\n";
  foreach my $list (@$failures) {
    my ($platform,$testrun) = @$list;
    printf(" - Test job %s failed on %s\n",
           $list->[1]->tag, $list->[0]->fullname);
  }

  exit 1;
}

1;
