#!/usr/bin/perl -w
# Usage: mkindex.pl [-p path] infile *or* mkindex.pl -p path
use strict;
use Getopt::Std;

my %opts;
getopts('p:', \%opts) || exit 2;
my $path = $opts{p} || $ARGV[0]
 or die "You must specify a path for the C source to read from.\n";
my $infile = shift || '-';

open my $in, '<', $infile or die "$0: $infile: $!";
open my $out1, '>', 'stejudri.c' or die "$0: stejudri.c: $!";
select $out1;

my %rafsi = ();
my($prevtell, $qty, $maxlen) = (0, 0, 0);

print "#include \"stejudri.h\"\n";
print "const struct kvpair vlastecmi[] = {\n";
while (<$in>) {
 my($valsi, $selmaho, $raf) = split /\t/;
 print " {\"$valsi\", $prevtell},\n";
 $prevtell = tell;
 $rafsi{$_} = $. - 1 for split ' ', $raf;
 $qty++;
 $maxlen = length if length > $maxlen;
}
print "};\n\n";
print "const struct kvpair selrafsi[] = {\n";
print " {\"$_\", $rafsi{$_}},\n" for sort keys %rafsi;
print "};\n";
close $out1;

open my $out2, '>', 'stejudri.h' or die "$0: stejudri.h: $!";
print $out2 <<EOT;
#ifndef JBOBAF_STEJUDRI
#define JBOBAF_STEJUDRI

#define VLASTE  "$path"
#define VLASTECMI_QTY  $qty
#define CMIMA_LEN  $maxlen
#define SELRAFSI_QTY  @{[scalar keys %rafsi]}

struct kvpair {char* key; int val; };
extern const struct kvpair vlastecmi[];
extern const struct kvpair selrafsi[];
#endif
EOT
