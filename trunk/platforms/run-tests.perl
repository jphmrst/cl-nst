#!/usr/bin/env perl

###
###  Read in raw tests.
###
open(LOCAL, '<', "./tests.local")
  or die "Cannot open tests.local";
my @testlines = <LOCAL>;
close LOCAL;

my @tests;
my $test=-1;
my $errors=0;


my $mode=0;
my $line=1;
my $testspecline = shift @testlines;
while (defined $testspecline) {
  chomp $testspecline;
  $testspecline =~ s/^\s+//;
  $testspecline =~ s/\s+$//;

  if ($mode == 0) {
    if ($testspecline eq "") {
      $testspecline = shift @testlines; ++$line;
    } else {
      $mode = 1;
    }

  } elsif ($mode == 1) {
    if ($testspecline =~ /^\[([-a-zA-Z0-9]+)\]$/) {
      $tests[++$test]{tag} = $1;
      $testspecline = shift @testlines; ++$line;
      $mode = 2;
    } else {
      die "Expected [BLOCK] at line $line; got:\n  $testspecline";
    }

  } elsif ($mode == 2) {
    if ($testspecline eq "") {
      $mode = 0;
      $testspecline = shift @testlines; ++$line;
    } elsif ($testspecline =~ /^\[([-a-zA-Z0-9]+)\]$/) {
      $mode = 1;
    } elsif ($testspecline =~ /^\s*([-a-zA-Z0-9]+)\s*=\s*(.*)/) {
      $tests[$test]{lisp}{$1} = $2
        if validLispField($test, $1, $2);
      $testspecline = shift @testlines; ++$line;
    } elsif ($testspecline =~ /^\s*([0-9]+)\s*([-a-zA-Z0-9]+)\s*=\s*(.*)/) {
      $tests[$test]{task}[$1]{$2} = $3;
      $testspecline = shift @testlines; ++$line;
    } else {
      die "Can't make sense of line $line:  $testspecline";
    }

  } else {
    die "Internal error: bad scan mode $mode.";
  }
}


###
###  Check test fields for completeness, admissibility.
###
if ($#tests<0) {
  die "No tests specified";
}
for(my $i=0; $i<=$#tests; ++$i) {
  requireLispField(\@tests, $i, 'invoke');
  requireLispField(\@tests, $i, 'script');
  for(my $j=0; $j<=$#{$tests[$i]{task}}; ++$j) {
    checkTaskFields($tests[$i]{task}[$j], $tests[$i]{tag}, $j);
  }
}
if ($errors>0) {
  print "$errors error(s).\n";
  exit 1;
}
print "Test specs validated.\n";


###
###  Now run the tests.
###
for(my $i=0; $i<=$#tests; ++$i) {
  my $test = $tests[$i];
  my $testtag = $test->{tag};
  my $basecommand = "$test->{lisp}{invoke}";
  my $scriptUse = $test->{lisp}{script};
  my $inputPiped = ($scriptUse eq 'stdin');
  if ($scriptUse eq 'arg') {

  }

  for(my $j=0; $j<=$#{$tests[$i]{task}}; ++$j) {
    my $task = $tests[$i]{task}[$j];
    my $tasktype = $task->{type};
    my $taskname = $task->{name};

    if ($tasktype eq 'asdf') {
      print "Skipping task $taskname ($j, $tasktype) of $testtag\n";
    } elsif ($tasktype eq 'junit') {
      $taskname = $tasktype;
      print "Skipping task $taskname ($j, $tasktype) of $testtag\n";
    } else {
      print "Unknown task type $tasktype for $testtag task $j \"$taskname\"\n";
    }
  }
}



exit 0;

sub requireLispField {
  my $tests = shift;
  my $test = shift;
  my $field = shift;

  if (!(defined $tests->[$test]{lisp}{$field})) {
    print "Test $test: no field $field\n";
    $errors++;
  }
}

sub validLispField {
  my $test = shift;
  my $field = shift;
  my $value = shift;

  if ($field =~ /^(invoke|script)$/) {
    return 1;
  } else {
    $errors++;
    print "Test $test: unknown field $field assigned \"$value\".\n";
    return 0;
  }
}

sub checkTaskFields {
  my $task = shift;
  my $platform = shift;
  my $tasknum = shift;

  if (!(defined $task->{type})) {
    print "No type for task $tasknum of $platform.\n";
    $errors++;
    return;
  }

  my $type = $task->{type};
  if ($type =~ /^asdf$/) {
  } elsif ($type =~ /^junit$/) {
  } else {
    print "Unknown type \"$type\" for task $tasknum of $platform.\n";
    $errors++;
  }
}
