#!/usr/bin/perl -w
use strict;

my $maho = shift || '-';
my $cat = shift || '-';
open STDIN, '<', $maho or die "$0: $maho: $!\n" if $maho ne '-';
open STDOUT, '>', $cat or die "$0: $cat: $!\n" if $cat ne '-';

<STDIN>;
while (<STDIN>) {
 foreach my $sec ([0, 11], [11, 5], [20, 42], [62, 106], [168, -1]) {
  last if $sec->[0] >= length;
  (my $field = substr $_, $sec->[0], $sec->[1]) =~ s/^\s+|\s+$//g;
  print $sec->[0] == 0 ? '' : $sec->[0] == 20 || $sec->[0] == 62 ? "\t\t"
   : "\t", $field;
 }
} continue { print "\n" }
