#!/usr/bin/perl -wl
use strict;
use Getopt::Std;
use Lojban::Vlatai qw< $C jvozba JVOZBA_BRIVLA JVOZBA_CMENE >;

my %opts;
our $VERSION = 1.2;
getopts('acC', \%opts) || exit 2;

@ARGV = map { /$C$C/ ? $_ : split /(?<=.)(?=$C)/ } @ARGV;

if (@ARGV < 2) {print STDERR "mo'a valsi cu sumti la jvozba"; exit 2; }

my @lujvo = jvozba @ARGV, $opts{C} ? JVOZBA_CMENE : JVOZBA_BRIVLA |
 ($opts{c} ? JVOZBA_CMENE : 0);
if (!@lujvo) {print STDERR $Lojban::Vlatai::jbosrenoi; exit 2; }

if ($opts{a}) { print $_->valsi, ' - ', $_->termre foreach @lujvo }
else { print $lujvo[0]->valsi }

sub HELP_MESSAGE {
 select shift;
 print <<EOT;
Usage: jvozba [-a] [-c | -C] pavyvalsi relvalsi ...

Options:
 -a - List all possible {lujvo} with scores
 -c - Create both {brivla} and {cmene}
 -C - Create only {cmene}

Type "man jvozba" or "perldoc jvozba" for more information.
EOT
 exit 0;
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

Calculate any I<lujvo cmene> (i.e., I<lujvo> that end in consonants) in
addition to normal I<lujvo>.

=item B<-C>

Calculate only I<lujvo cmene>.  I<lujvo cmene> that are formed by dropping the
final vowel from a I<lujvo> that ends with a five-letter I<rafsi> (e.g.,
"I<lojbangirz.>") are also included, though the score listed (if B<-a> is also
given) will be for the I<brivla> form.

=back

=head1 DIAGNOSTICS

=over

=item C<mo'a valsi cu sumti la jvozba>

You did not specify at least two I<valsi> on the command line.

=item C<zo VALSI se rafsi noda>

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
