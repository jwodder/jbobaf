#!/usr/bin/perl -w
use strict;
use Getopt::Std;
use Lojban::Valsi;
use Lojban::Vlasisku qw< :DEFAULT :stodi >;

my %opts;
our $VERSION = 1.2;
getopts('cgGeiwFarRCqkdp', \%opts) || exit 2;

# Eliminate conflicting options:
$opts{g} = $opts{G} = 0 if $opts{c} || $opts{C};
$opts{g} = 0 if $opts{G};
$opts{c} = 0 if $opts{C};
$opts{r} = $opts{R} = 0 if $opts{e};
$opts{r} = 0 if $opts{R};
$opts{e} = 1 if $opts{k} || $opts{d};
$opts{k} = $opts{d} = 0 if $opts{k} && $opts{d};

if ($opts{p}) {
 print $Lojban::Vlasisku::gimste, "\n" unless $opts{c} || $opts{C};
 print $Lojban::Vlasisku::mahoste, "\n" unless $opts{g} || $opts{G};
 exit 0;
}

my($wordSearch, $keySearch, $defSearch) = (\&getValsi, \&getValsiByKeyword,
 \&getValsiByDefinition);

if ($opts{c} || $opts{C}) {
 $wordSearch = \&getCmavo;
 $keySearch = \&getCmavoByKeyword;
 $defSearch = \&getCmavoByDefinition;
} elsif ($opts{g} || $opts{G}) {
 $wordSearch = \&getGismu;
 $keySearch = \&getGismuByKeyword;
 $defSearch = \&getGismuByDefinition;
}

my $patopts;
$patopts = VLASISKU_INSENSITIVE if $opts{i};
if ($opts{w}) {
 if (defined $patopts) { $patopts |= VLASISKU_WHOLE_WORD }
 else { $patopts = VLASISKU_WHOLE_WORD }
}
if ($opts{F} && !defined $patopts) { $patopts = 0 }

if (@ARGV) {
 foreach (@ARGV) {
  my @valsi;
  my $op = defined $patopts && !/[][.*+?^\$\\|{}]/o ? $patopts | ($opts{F} ? 0
   : VLASISKU_ANCHORED) | VLASISKU_LITERAL : $patopts;
  if ($opts{e}) {
   @valsi = &$keySearch($_, $op) unless $opts{d};
   push @valsi, &$defSearch($_, defined $op ? ($op & ~VLASISKU_ANCHORED) : $op)
    unless $opts{k};
   @valsi = Lojban::Valsi::girxre @valsi unless $opts{k} || $opts{d};
  } else {
   tr/h/'/;
   @valsi = getValsiByRafsi($_, $op) if $opts{r} || $opts{R};
   push @valsi, &$wordSearch($_, $op) unless $opts{R};
   @valsi = Lojban::Valsi::girxre @valsi if $opts{r};
   @valsi = grep { $_->xucmavo } @valsi if ($opts{c} || $opts{C}) &&
    ($opts{r} || $opts{R});
  }
  @valsi = grep { $_->xugismu } @valsi if $opts{G};
  @valsi = grep { !$_->xulujmaho } @valsi if $opts{C};
  @valsi = ($valsi[0]) if !($opts{a} || $opts{q}) && @valsi;
  # Yes, getting the whole set and then discarding all but one is bad, but it's
  # the easiest way to implement the -G, -C, and -c with -[rR] options without
  # rewriting Vlasisku.pm.
  if ($opts{q}) { print scalar @valsi, "\n" }
  else { cusku $_ for @valsi }
 }
} else {
 if ($opts{c}) {
  my $cmavo = cunmaho;
  $cmavo->cusku;
 } elsif ($opts{C}) {
  my $cmavo = cunmaho;
  $cmavo = cunmaho while $cmavo->isCompoundCmavo();
  $cmavo->cusku;
 } elsif ($opts{G}) {
  my $gismu = cungihu;
  $gismu = cungihu until $gismu->isGismu();
  $gismu->cusku;
 } else {
  my $gimstecmi = cungihu;
  $gimstecmi->cusku;
 }
}

sub HELP_MESSAGE {
 select shift;
 print <<EOT;
Usage: sisku [-aCcFGgiqRrw] valsi ...
       sisku [-dek] [-aCcFGgiqw] definition ...
       sisku [-CcGgp]

Options:
 -a - Print all results
 -C - Search {cmavo} only
 -c - Search {cmavo} and compound {cmavo} only
 -d - Match against definitions
 -e - Match against definitions & keywords
 -F - Match substrings of entries
 -G - Search only {gismu}
 -g - Search only {gismu} and any {cmavo} with {rafsi}
 -i - Match case-insensitively
 -k - Match against keywords/glosses
 -p - Print the locations of the {gismu} & {cmavo} lists and exit
 -q - Print out the number of matches rather than the matches themselves
 -R - Search only {rafsi}
 -r - Search {rafsi} in addition to other criteria
 -w - Match only on full words

Type "man sisku" or "perldoc sisku" for more information.
EOT
 exit 0;
}

__END__

=pod

=head1 NAME

B<sisku> - search for I<valsi> in the official Lojban I<gismu> & I<cmavo> lists

=head1 SYNOPSIS

B<sisku> [B<-aCcFGgiqRrw>] I<valsi> ...

B<sisku> [B<-dek>] [B<-aCcFGgiqw>] I<definition> ...

B<sisku> [B<-CcGg>]

B<sisku> B<-p>

=head1 DESCRIPTION

B<sisku> searches through the official I<gismu> list and/or the official
I<cmavo> list and prints out the first entry that matches each operand.  It can
search based on I<valsi>, I<rafsi>, keyword, or definition using either plain
text comparison or L<perl(1)> regular expressions.  A separate search is
performed for each operand, and the results for each are printed out separately
(i.e., not sorted or merged together).  The exact way in which arguments are
matched against text is described below under L</"PATTERN MATCHING">.

When no strings to search for are given, B<sisku> prints out a random I<valsi>
(by default, an entry from the I<gismu> list, though this can be changed by
specifying one of the B<-CcG> switches) and then exits.

=head1 OPTIONS

=over

=item B<-a>

Print out all matching entries for each operand rather than just the first
match for each.  Note that each operand will still be searched for separately,
and the results will not be sorted or merged together.

=item B<-C>

Retrieve only entries from the I<cmavo> list, B<excluding> compound I<cmavo>.
I<rafsi> will not be output unless the B<-r> or B<-R> option is also given.

=item B<-c>

Retrieve only entries from the I<cmavo> list, B<including> compound I<cmavo>.
I<rafsi> will not be output unless the B<-r> or B<-R> option is also given.

=item B<-d>

Search English definitions for matches.

=item B<-e>

Search through both English keywords and definitions for matches.

=item B<-F>

Allow non-regex operands to match strings that contain them rather than just
strings that equal them, i.e., make them "free floating."  Note that this
option has no effect on matching of definitions, as those matches are always
"free floating" unless the user uses C<^> or C<$>.

=item B<-G>

Retrieve only I<gismu>.

=item B<-g>

Retrieve only entries from the I<gismu> list, i.e., both I<gismu> and any
I<cmavo> that have I<rafsi>.

=item B<-i>

Search case-insensitively.

=item B<-k>

Search English keywords for matches.

=item B<-p>

Print out the paths of the I<gismu> and I<cmavo> lists that B<sisku> searches.
If the B<-c> or B<-C> option is specified, only the path of the I<cmavo> list
will be printed, and likewise for B<-g> or B<-G> with the I<gismu> list.

=item B<-q>

Print out the quantity of matches for each operand rather than the matches
themselves.

=item B<-R>

Attempt to match B<only> I<rafsi> and display only those I<valsi> that have at
least one matching I<rafsi>.

=item B<-r>

Also attempt to match I<rafsi> and display any I<valsi> that has at least one
matching I<rafsi>.

=item B<-w>

Only display matches that match on a full word, i.e., act as if there is a
C<\b> at either end of each operand.

=back

=head2 Handling Mutually Exclusive Options

Due to Getopt::Std's parsing of all options at once, in contrast to the "one at
a time" method of L<getopt(3)>, mutually exclusive options cannot be resolved
by checking which one came last; instead, an arbitrary set of precedences is
used:

=over

=item

The B<-C> and B<-c> options override the B<-G> and B<-g> options.

=item

B<-C>, B<-G>, and B<-R> override B<-c>, B<-g>, and B<-r>, respectively.

=item

If both B<-d> and B<-k> are given, they are treated as if just B<-e> had been
supplied.

=item

If B<-e> and either B<-d> or B<-k> are given, the B<-e> is ignored.

=item

If B<-d>, B<-e>, or B<-k> is given, B<-r> and B<-R> are ignored.

=item

The B<-q> option overrides the B<-a> option.

=back

=head1 PATTERN MATCHING

Treatment of operands is handled using a rather straightforward rule: if an
operand contains any special regex characters, it is treated as a Perl regular
expression; otherwise, it is treated as a literal string that the text being
compared against must be equal to or (if the B<-F> option is given or
definitions are being searched) must contain.  As an exception, parentheses are
not recognized as special characters because, due to the lack of capturing in
B<sisku>, they serve no use unless other special characters are present.  For
reference, the special regex characters recognized are:

    . * + ? ^ $ | [ ] { } \

If you wish to include one of these characters in a search pattern without
giving it any special meaning, you must precede it with a backslash (possibly
two backslashes, depending on your shell & use of quotes).  For more
information on L<perl(1)>'s regular expressions, run C<perldoc perlrequick>,
C<perldoc perlretut>, and/or C<perldoc perlre>.  If you supply a bad regex to
B<sisku>, whatever happens is your own fault.

When not using any of the B<-dek> options, 'h's in the operands are converted
to apostrophes before searching.

Note that any periods at the beginning of I<cmavo> in the I<cmavo> list are
ignored both when using regexes & when using literal strings.  Thus, the
operand C<.e> will match I<be> but not I<.e>, and forcing literal
interpretation with C<\.e> will cause the query to return no results.  Periods
elsewhere in I<cmavo> are left intact, but note that I<na.a> is the only entry
in the official I<cmavo> list to have a non-leading period.

=head1 RESTRICTIONS

B<sisku> does not search the notes or hints sections of entries.

=head1 SEE ALSO

L<jvozba(1)>, Lojban::Valsi, Lojban::Vlasisku

I<The Complete Lojban Language> by John Woldemar Cowan

L<http://www.lojban.org/>

=head1 AUTHOR

John T. "kamymecraijun." Wodder II <minimiscience@gmail.com>

=head1 LICENSE

Feel free to do whatever the I<bais.> you want with this.

=cut
