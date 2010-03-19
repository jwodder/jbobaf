# $Id$
package Lojban::Vlasisku;

use constant {
 VLASISKU_ANCHORED => 1,
 VLASISKU_LITERAL => 2,
 VLASISKU_WHOLE_WORD => 4,
 VLASISKU_INSENSITIVE => 8
};

use Lojban::Valsi;
use Carp;
our @CARP_NOT = qw< Lojban::Vlasisku Lojban::Vlatai >;

our $gimste = "$ENV{HOME}/share/gismu.txt";
our $mahoste = "$ENV{HOME}/share/cmavo.txt";
our $dosyste = 0;  # Set to true if $gimste and $mahoste use CRLF terminators

require Exporter;
our @ISA = ("Exporter");
our $VERSION = 1.0;

our @EXPORT = qw< getGismu getCmavo getRafsiByValsi getValsiByRafsi
 getGismuByKeyword getCmavoByKeyword getGismuByDefinition getCmavoByDefinition
 cungihu cunmaho getValsi getValsiByKeyword getValsiByDefinition >;

our @EXPORT_OK = qw< $gimste $mahoste $dosyste VLASISKU_ANCHORED
 VLASISKU_LITERAL VLASISKU_WHOLE_WORD VLASISKU_INSENSITIVE >;

our %EXPORT_TAGS = (
 vlasisku => [qw< getValsi getValsiByKeyword getValsiByDefinition >],
 gimsisku => [qw< getGismu getGismuByKeyword getGismuByDefinition cungihu >],
 mahorsisku => [qw< getCmavo getCmavoByKeyword getCmavoByDefinition cunmaho >],
 rafsisku => [qw< getValsiByRafsi getRafsiByValsi >],
 cunso => [qw< cungihu cunmaho >],
 stodi => [qw< VLASISKU_ANCHORED VLASISKU_LITERAL VLASISKU_WHOLE_WORD
  VLASISKU_INSENSITIVE >]
);

# Do not export: gimcfa, mahocfa, prepareRegex, trim, $gimvor, $mahorvor

my($gimvor, $mahorvor);

sub gimcfa() {
 if (!defined $gimvor) {
  open $gimvor, '<', $gimste or croak "Lojban::Vlasisku: $gimste: $!\n";
  select((select($gimvor), $/ = "\cM\cJ")[0]) if $dosyste;
 } else { seek $gimvor, 0, 0 }
 <$gimvor>;
 $. = 1;
}

sub mahocfa() {
 if (!defined $mahorvor) {
  open $mahorvor, '<', $mahoste or croak "Lojban::Vlasisku: $mahoste: $!\n";
  select((select($mahorvor), $/ = "\cM\cJ")[0]) if $dosyste;
 } else { seek $mahorvor, 0, 0 }
 <$mahorvor>;
 $. = 1;
}

sub prepareRegex {
 my($pattern, $opts) = @_;
 $opts = $pattern =~ /[][.*+?^\$\\|{}]/ ? 0 : VLASISKU_ANCHORED |
  VLASISKU_LITERAL if !defined $opts;
 $pattern = ($opts & VLASISKU_LITERAL) ? qr/\Q$pattern\E/ : qr/$pattern/;
 if ($opts & VLASISKU_ANCHORED) { $pattern = qr/^$pattern$/ }
 elsif ($opts & VLASISKU_WHOLE_WORD) { $pattern = qr/\b$pattern\b/ }
 if ($opts & VLASISKU_INSENSITIVE) { $pattern = qr/$pattern/i }
 return $pattern;
}

sub trim($) {my $str = shift; $str =~ s/^\s+|\s+$//g; return $str; }


sub getGismu($;$) {  # Technically, get an entry from the gismu list
 gimcfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 for (grep { trim(substr $_, 1, 5) =~ $pattern } <$gimvor>) {
  return cminiho Lojban::Valsi $_ unless wantarray;
  push @vlaste, cminiho Lojban::Valsi $_;
 }
 return @vlaste;
}

sub getCmavo($;$) {
 mahocfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 for (grep { trim(substr $_, 1, 10) =~ $pattern } <$mahorvor>) {
  return cminiho Lojban::Valsi $_ unless wantarray;
  push @vlaste, cminiho Lojban::Valsi $_;
 }
 return @vlaste;
}

sub getRafsiByValsi($;$) {
# Unlike the others, this function does not search the entire gismu list when
# called in list context.
 gimcfa;
 my $pattern = prepareRegex(@_);
 while (<$gimvor>) {
  return split ' ', substr($_, 7, 12) if trim(substr($_, 1, 5)) =~ $pattern
 }
 return ();
}

sub getValsiByRafsi($;$) {
 gimcfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 for (grep { grep /$pattern/, split ' ', substr $_, 7, 12 } <$gimvor>) {
  return cminiho Lojban::Valsi $_ unless wantarray;
  push @vlaste, cminiho Lojban::Valsi $_;
 }
 return @vlaste;
}

sub getGismuByKeyword($;$) { # get gimstecmi, actually
 gimcfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 for (grep { trim(substr $_, 20, 20) =~ $pattern } <$gimvor>) {
  return cminiho Lojban::Valsi $_ unless wantarray;
  push @vlaste, cminiho Lojban::Valsi $_;
 }
 return @vlaste;
}

sub getCmavoByKeyword($;$) { # or by "gloss"
 mahocfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 for (grep { trim(substr $_, 20, 42) =~ $pattern } <$mahorvor>) {
  return cminiho Lojban::Valsi $_ unless wantarray;
  push @vlaste, cminiho Lojban::Valsi $_;
 }
 return @vlaste;
}

sub getGismuByDefinition($;$) { # get gimstecmi, actually
 gimcfa;
 $_[1] = $_[0] =~ /[][.*+?^\$\\|{}]/ ? 0 : VLASISKU_LITERAL if !defined $_[1];
 # No anchoring by default
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 for (grep { trim(substr $_, 62, 96) =~ $pattern } <$gimvor>) {
  return cminiho Lojban::Valsi $_ unless wantarray;
  push @vlaste, cminiho Lojban::Valsi $_;
 }
 return @vlaste;
}

sub getCmavoByDefinition($;$) {
 mahocfa;
 $_[1] = $_[0] =~ /[][.*+?^\$\\|{}]/ ? 0 : VLASISKU_LITERAL if !defined $_[1];
 # No anchoring by default
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 for (grep { trim(substr $_, 62, 96) =~ $pattern } <$mahorvor>) {
  return cminiho Lojban::Valsi $_ unless wantarray;
  push @vlaste, cminiho Lojban::Valsi $_;
 }
 return @vlaste;
}

sub cungihu() {  # cunso gimstecmi, actually
 gimcfa;
 my $gismu;
 while (<$gimvor>) { $gismu = $_ if (int rand $.) == 0 }
 return cminiho Lojban::Valsi $gismu;
}

sub cunmaho() {
 mahocfa;
 my $cmavo;
 while (<$mahorvor>) { $cmavo = $_ if (int rand $.) == 0 }
 return cminiho Lojban::Valsi $cmavo;
}

# These functions search both the gismu & cmavo lists and return the combined
# results from both.

sub getValsi($;$) {
 if (wantarray) { Lojban::Valsi::girxre &getGismu, &getCmavo }
 else { (Lojban::Valsi::girxre scalar &getGismu, scalar &getCmavo)[0] }
}

sub getValsiByKeyword($;$) {
 if (wantarray) {
  Lojban::Valsi::girxre &getGismuByKeyword, &getCmavoByKeyword
 } else {
  (Lojban::Valsi::girxre scalar &getGismuByKeyword, scalar
   &getCmavoByKeyword)[0]
 }
}

sub getValsiByDefinition($;$) {
 if (wantarray) {
  Lojban::Valsi::girxre &getGismuByDefinition, &getCmavoByDefinition
 } else {
  (Lojban::Valsi::girxre scalar &getGismuByDefinition, scalar
   &getCmavoByDefinition)[0]
 }
}

1;

__END__

=pod

=head1 NAME

Lojban::Vlasisku - Lojban word list searching

=head1 SYNOPSIS

    use Lojban::Vlasisku;

    $valsi = getValsi($pattern);
    $valsi->printEntry();

    use Lojban::Vlasisku qw(:stodi);

    @gismu = getGismuByDefinition($pattern, VLASISKU_LITERAL |
      VLASISKU_INSENSITIVE);
    $_->printEntry() for @gismu;

=head1 DESCRIPTION

Lojban::Vlasisku provides a number of Perl routines for fetching entries from
the Lojban I<gismu> and/or I<cmavo> lists.  One can search by I<valsi>,
I<rafsi>, keyword, or definition, and the exact way in which searches are
performed can be set by means of a bitfield of flags.

The default paths to the lists are C<$ENV{HOME}/share/gismu.txt> and
C<$ENV{HOME}/share/cmavo.txt>, respectively, though these may have been changed
when the module was installed.  If you wish to change the paths at runtime,
simply modify one or both of the variables C<$Lojban::Vlasisku::gimste> and
C<$Lojban::Vlasisku::mahoste> before calling any of the module's functions.
The files to which the new paths point must be formatted in the same way as the
official word lists, and if they use CRLF as the line terminator, the variable
C<$Lojban::Vlasisku::dosyste> must also be set to 1.  These variables may also
be imported by explicitly requesting them in the C<use> statement.

=head1 FUNCTIONS

The following functions are all exported by default.  Except for C<cungihu> and
C<cunmaho>, each function takes one required argument, a string containing a
regular expression, and one optional argument, a bitfield of flags.  For more
information on the handling of arguments, see L</"PATTERN MATCHING"> below.

Except where noted below, the return value of each function depends on the
context in which it was called.  In scalar context, the first match found is
returned, or a false value if there are no matches.  In list context, a list of
all matches is returned, or the empty list if there are none.  In both cases,
the individual matches are returned as objects of the C<Lojban::Valsi> class.

=over

=item cungihu

Returns a random entry from the I<gismu> list as a single object of class
C<Lojban::Valsi>.

=item cunmaho

Returns a random entry from the I<gismu> list as a single object of class
C<Lojban::Valsi>.

=item getCmavo PATTERN[, FLAGS]

Searches the I<cmavo> list for entries whose I<valsi> matches the specified
pattern.

=item getCmavoByDefinition PATTERN[, FLAGS]

Searches the I<cmavo> list for entries whose definition matches the specified
pattern.

=item getCmavoByKeyword PATTERN[, FLAGS]

Searches the I<cmavo> list for entries whose keyword or gloss matches the
specified pattern.

=item getGismu PATTERN[, FLAGS]

Searches the I<gismu> list for entries whose I<valsi> matches the specified
pattern.

=item getGismuByDefinition PATTERN[, FLAGS]

Searches the I<gismu> list for entries whose definition matches the specified
pattern.

=item getGismuByKeyword PATTERN[, FLAGS]

Searches the I<gismu> list for entries whose keyword or gloss matches the
specified pattern.

=item getRafsiByValsi PATTERN[, FLAGS]

Searches the I<gismu> list for the first entry whose I<valsi> matches the
specified pattern and returns that entry's three-letter I<rafsi> as an array of
strings.

=item getValsi PATTERN[, FLAGS]

Searches both the I<gismu> and I<cmavo> lists for entries whose I<valsi>
matches the specified pattern and uses C<Lojban::Valsi::girxre> to merge the
results.

=item getValsiByDefinition PATTERN[, FLAGS]

Searches both the I<gismu> and I<cmavo> lists for entries whose definition
matches the specified pattern and uses C<Lojban::Valsi::girxre> to merge the
results.

=item getValsiByKeyword PATTERN[, FLAGS]

Searches both the I<gismu> and I<cmavo> lists for entries whose keyword or
gloss matches the specified pattern and uses C<Lojban::Valsi::girxre> to merge
the results.

=item getValsiByRafsi PATTERN[, FLAGS]

Searches the I<gismu> list for entries that have one or more three-letter
I<rafsi> that match the specified pattern.

=back

=head1 CONSTANTS

The following constants can be used to contruct bitfields specifying how to
perform pattern matches.  They are not exported by default, but they may be
requested via the C<:stodi> tag.

=over

=item VLASISKU_ANCHORED

"Anchor" the pattern, requiring it to match the whole of a string rather than
just a substring.

=item VLASISKU_INSENSITIVE

Match case-insensitively.

=item VLASISKU_LITERAL

Treat the pattern as a literal string to look for rather than as a regular
expression to match against.

=item VLASISKU_WHOLE_WORD

Require the matched substring to be located at a word boundary, i.e., enclose
the pattern in a C<\b> on either side.

=back

=head1 EXPORT TAGS

The following tags may be used when importing symbols from Lojban::Vlasisku in
order to import groups of functions:

=over

=item :DEFAULT

All functions (excluding constants)

=item :cunso

cungihu, cunmaho

=item :gimsisku

cungihu, getGismu, getGismuByDefinition, getGismuByKeyword

=item :mahorsisku

cunmaho, getCmavo, getCmavoByDefinition, getCmavoByKeyword

=item :rafsisku

getRafsiByValsi, getValsiByRafsi

=item :stodi

VLASISKU_ANCHORED, VLASISKU_INSENSITIVE, VLASISKU_LITERAL, VLASISKU_WHOLE_WORD

=item :vlasisku

getValsi, getValsiByDefinition, getValsiByKeyword

=back

=head1 PATTERN MATCHING

The first argument to each function is a string giving a regular expression to
compare against the field being searched (I<valsi>, I<rafsi>, keyword, or
definition).  If it is present, the second argument is a bitfield of flags
specifying how to modify the regular expression before use (see L</"CONSTANTS">
above).  If the second argument is not present or is undefined, it is set
according to the following rule: if the first argument contains any special
regex characters, the second argument is set to 0; otherwise, it is set to
C<VLASISKU_ANCHORED | VLASISKU_LITERAL>.  As an exception, the
C<get*ByDefinition> functions will not set the C<VLASISKU_ANCHORED> flag if the
second argument is not defined.

Note that parentheses are not recognized as special characters for this purpose
because, due to the lack of capturing in these functions, they serve no use
unless other special characters are present.  For reference, the special regex
characters recognized are:

    . * + ? ^ $ | [ ] { } \

If you wish to pass a regex that has already been compiled with C<qr//> as the
first argument to one of these functions, you are advised to use a second
argument of 0 rather than leaving it undefined.  If you supply a bad regex,
whatever happens is your own fault.

Note that any periods at the beginning of I<cmavo> in the I<cmavo> list are
ignored when performing matches.  Thus, the string C<'.e'> will match I<be> but
not I<.e>, and forcing literal interpretation with C<'\.e'> will cause no
results to be returned.  Periods elsewhere in I<cmavo> are left intact, but
note that I<na.a> is the only entry in the official I<cmavo> list to have a
non-leading period.

=head1 SEE ALSO

L<sisku(1)>, Lojban::Valsi, Lojban::Vlatai

I<The Complete Lojban Language> by John Woldemar Cowan

L<http://www.lojban.org/>

=head1 AUTHOR

John T. "kamymecraijun." Wodder II <minimiscience@gmail.com>

=head1 LICENSE

Feel free to do whatever the I<bais.> you want with this.

=cut
