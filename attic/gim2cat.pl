#!/usr/bin/perl -w
use strict;

my $gim = shift || '-';
my $cat = shift || '-';
open STDIN, '<', $gim or die "$0: $gim: $!\n" if $gim ne '-';
open STDOUT, '>', $cat or die "$0: $cat: $!\n" if $cat ne '-';

<STDIN>;
while (<STDIN>) {
 foreach my $sec ([1, 5], [7, 12], [20, 20], [41, 20], [62, 96], [169, -1]) {
  last if $sec->[0] >= length;
  (my $field = substr $_, $sec->[0], $sec->[1]) =~ s/^\s+|\s+$//g;
  $field =~ s/\s{2,}/ /g if $sec->[0] == 7;
  print $sec->[0] == 1 ? '' : $sec->[0] == 7 ? "\t\t" : "\t", $field;
 }
} continue { print "\n" }
