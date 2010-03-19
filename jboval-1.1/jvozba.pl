#!/usr/bin/perl -wl
# $Id$
use strict;
use Getopt::Std;
use Lojban::Valsi;
use Lojban::Vlasisku qw< :DEFAULT :stodi >;
use Lojban::Vlatai qw< :fancu $c >;

my %opts;
getopts('acC', \%opts) || exit 2;

@ARGV = map { /$c$c/ ? $_ : split /(?<=.)(?=$c)/ } @ARGV;

if (@ARGV < 2) {
 print STDERR "mo'a valsi cu sumti la jvozba\n";
 exit 2;
}

my @rafsi;
my $i=0;
for (@ARGV) {
 tr/h/'/;
 my $valsi = getGismu($_, VLASISKU_LITERAL | VLASISKU_ANCHORED);
 if (!$valsi) {
  print STDERR "zo $_ na'e se rafsi\n";
  exit 2;
 }
 my @valraf = $valsi->rafsi();
 if ($i == $#ARGV) {
  if ($opts{C}) { @valraf = grep !/[aeiou]$/, @valraf }
  elsif (!$opts{c}) { @valraf = grep /[aeiou]$/, @valraf }
  push @valraf, $valsi->valsi() if $valsi->isGismu();
 } elsif ($valsi->isGismu()) {
  (my $vla = $valsi->valsi()) =~ s/[aeiou]$/y/;
  push @valraf, $vla;
 }
 if (!@valraf) {
  print STDERR "zo $_ se rafsi no mapti\n";
  exit 2;
 }
 push @rafsi, [ @valraf ];
 $i++;
}

my %jvoste;  # mapping from lujvo to score
my @rafyzva = (0) x @rafsi;
do {
 $i = 0;
 my $lujvo = rafyjongau map { $rafsi[$i++][$_] } @rafyzva;
 my $termre = jvomre $lujvo;
 $lujvo =~ s/[aeiou]$// if $opts{C};
 $jvoste{$lujvo} = $termre;
 for ($i = @rafsi - 1; $i >= 0; $i--) {
  if (++$rafyzva[$i] >= @{$rafsi[$i]}) { $rafyzva[$i] = 0 }
  else { last }
 }
} while ($i >= 0);

if ($opts{a}) {
 print "$_ - $jvoste{$_}" for sort { $jvoste{$a} <=> $jvoste{$b} } keys %jvoste
} else {
 print((sort { $jvoste{$a} <=> $jvoste{$b} } keys %jvoste)[0])
}

__END__

=pod

=head1 NAME

B<jvozba> - create Lojban I<lujvo> out of I<tanru>

=head1 SYNOPSIS

B<jvozba> [B<-a>] [B<-c> | B<-C>] I<pavyvalsi> I<relvalsi> ...

=head1 DESCRIPTION

B<jvozba> can be used to automatically create I<lujvo> from a I<tanru> and
calculate the best possible I<lujvo> according to the I<Complete Lojban
Language>'s scoring algorithm.  The I<gismu> and I<cmavo> that comprise the
I<tanru> are passed to the program on the command line, each as a separate word
and in the order that they appear in the I<tanru>.  B<jvozba> will then
determine all possible valid I<lujvo> that can be formed for the I<tanru> and,
by default, will print the "best" one to standard output.

Currently, B<jvozba> cannot handle I<cmavo> that lack I<rafsi>, nor can it
handle any I<cmene>, I<fu'ivla>, or I<lujvo> specified on the command line.

=head1 OPTIONS

=over

=item B<-a>

Print out all possible I<lujvo> along with their scores.

=item B<-c>

Calculate any I<cmene lujvo> (i.e., I<lujvo> that end in consonants) in
addition to the normal I<brivla lujvo>.

=item B<-C>

Calculate only I<cmene lujvo>.  I<cmene lujvo> that are formed by dropping the
final vowel from a I<lujvo> that ends with a five-letter I<rafsi> (e.g.,
I<lojbangirz.>) are also included, though the score listed (if B<-a> is also
given) will be for the I<brivla> form.

=back

=head1 DIAGNOSTICS

=over

=item C<mo'a valsi cu sumti la jvozba>

You did not specify at least two I<valsi> on the command line.

=item C<zo VALSI na'e se rafsi>

No I<rafsi> could be found for C<VALSI>.

=item C<zo VALSI se rafsi no mapti>

C<VALSI> came at the end of the I<tanru>, and no I<rafsi> could be found for it
that ended in a vowel (when the C<-c> and C<-C> options are not specified) or
that ended in a consonant (when the C<-C> option is specified).

=back

=head1 SEE ALSO

L<sisku(1)>, Lojban::Valsi, Lojban::Vlatai

I<The Complete Lojban Language> by John Woldemar Cowan

L<http://www.lojban.org/>

=head1 AUTHOR

John T. "kamymecraijun." Wodder II <minimiscience@gmail.com>

=head1 LICENSE

Feel free to do whatever the I<bais.> you want with this.

=cut
