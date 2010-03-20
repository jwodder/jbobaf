package Lojban::Vlasisku;

require Exporter;
our @ISA = ('Exporter');
our $VERSION = v2.0;
our @EXPORT = qw< cunvla getValsi getRafsiByValsi getValsiByRafsi
 getValsiByKeyword getValsiByHint getValsiByDefinition getValsiByNotes
 getCmavoBySelmaho >;
our @EXPORT_OK = qw< $vlaste VLASISKU_ANCHORED VLASISKU_LITERAL
 VLASISKU_WHOLE_WORD VLASISKU_INSENSITIVE >;
our %EXPORT_TAGS = (stodi => [qw< VLASISKU_ANCHORED VLASISKU_LITERAL
 VLASISKU_WHOLE_WORD VLASISKU_INSENSITIVE >]);

use constant {
 VLASISKU_ANCHORED => 1,
 VLASISKU_LITERAL => 2,
 VLASISKU_WHOLE_WORD => 4,
 VLASISKU_INSENSITIVE => 8
};

use Carp;
use Lojban::Vlatid;
our @CARP_NOT = ('Lojban::Valsi', 'Lojban::Vlatai');

our $vlaste = "$ENV{HOME}/share/vlaste.tsv";
my $vlavor;

sub vlacfa() { # not exported
 if (!defined $vlavor) { $vlavor = cnino Lojban::Vlatid $vlaste }
 else { $vlavor->rapcfa }
}

sub prepareRegex { # not exported
 my($pattern, $opts) = @_;
 $opts = $pattern =~ /[][.*+?^\$\\|{}]/ ? 0 : VLASISKU_ANCHORED |
  VLASISKU_LITERAL if !defined $opts;
 $pattern = ($opts & VLASISKU_LITERAL) ? qr/\Q$pattern\E/ : qr/$pattern/;
 if ($opts & VLASISKU_ANCHORED) { $pattern = qr/^$pattern$/ }
 elsif ($opts & VLASISKU_WHOLE_WORD) { $pattern = qr/\b$pattern\b/ }
 if ($opts & VLASISKU_INSENSITIVE) { $pattern = qr/$pattern/i }
 return $pattern;
}

sub getValsi(@) {
 my $vorme = ref $_[0] eq 'Lojban::Vlatid' ? shift : vlacfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 while (my $vla = <$vorme>) {
  if ($vla->fadni =~ $pattern) {
   return $vla unless wantarray;
   push @vlaste, $vla;
  }
 }
 return @vlaste;
}

sub getRafsiByValsi(@) {
# Unlike the others, this function does not search the entire word list when
# called in list context.
 my $vorme = ref $_[0] eq 'Lojban::Vlatid' ? shift : vlacfa;
 my $pattern = prepareRegex(@_);
 while (my $vla = <$vorme>) { return $vla->rafsi if $vla->valsi =~ $pattern }
 return ();
}

sub getValsiByRafsi(@) {
 my $vorme = ref $_[0] eq 'Lojban::Vlatid' ? shift : vlacfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 while (my $vla = <$vorme>) {
  if (grep /$pattern/, $vla->rafsi) {
   return $vla unless wantarray;
   push @vlaste, $vla;
  }
 }
 return @vlaste;
}

sub getValsiByKeyword(@) {
 my $vorme = ref $_[0] eq 'Lojban::Vlatid' ? shift : vlacfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 while (my $vla = <$vorme>) {
  if (defined $vla->ralvla && $vla->ralvla =~ $pattern) {
   return $vla unless wantarray;
   push @vlaste, $vla;
  }
 }
 return @vlaste;
}

sub getValsiByDefinition(@) {
 my $vorme = ref $_[0] eq 'Lojban::Vlatid' ? shift : vlacfa;
 $_[1] = $_[0] =~ /[][.*+?^\$\\|{}]/ ? 0 : VLASISKU_LITERAL if !defined $_[1];
  # No anchoring by default
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 while (my $vla = <$vorme>) {
  if (defined $vla->selvla && $vla->selvla =~ $pattern) {
   return $vla unless wantarray;
   push @vlaste, $vla;
  }
 }
 return @valsi;
}

sub cunvla(;$) {
 my $vorme = ref $_[0] eq 'Lojban::Vlatid' ? shift : vlacfa;
 my $valsi;
 while (my $vla = <$vorme>) { $valsi = $vla if (int rand $vorme->selzva) == 0 }
 return $valsi;
}

sub getValsiByNotes(@) {
 my $vorme = ref $_[0] eq 'Lojban::Vlatid' ? shift : vlacfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 while (my $vla = <$vorme>) {
  if (defined $vla->notci && $vla->notci =~ $pattern) {
   return $vla unless wantarray;
   push @vlaste, $vla;
  }
 }
 return @vlaste;
}

sub getValsiByHint(@) {
 my $vorme = ref $_[0] eq 'Lojban::Vlatid' ? shift : vlacfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 while (my $vla = <$vorme>) {
  if (defined $vla->djuvla && $vla->djuvla =~ $pattern) {
   return $vla unless wantarray;
   push @vlaste, $vla;
  }
 }
 return @vlaste;
}

sub getCmavoBySelmaho(@) {
 my $vorme = ref $_[0] eq 'Lojban::Vlatid' ? shift : vlacfa;
 my $pattern = prepareRegex(@_);
 my @vlaste = ();
 while (my $vla = <$vorme>) {
  if (defined $vla->selmaho && $vla->selmaho =~ $pattern) {
   return $vla unless wantarray;
   push @vlaste, $vla;
  }
 }
 return @vlaste;
}

1;

__END__

=pod

=head1 NAME

Lojban::Vlasisku - Lojban word list searching

=head1 SYNOPSIS

    use Lojban::Vlasisku;

    $valsi = getValsi($pattern);
    $valsi->cusku;

    use Lojban::Vlasisku qw(:stodi);

    @gismu = getGismuByDefinition($pattern, VLASISKU_LITERAL |
      VLASISKU_INSENSITIVE);
    $_->cusku for @gismu;

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
official word lists.  These variables may also be imported by explicitly
requesting them in the C<use Lojban::Vlasisku> statement.

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

=item :stodi

VLASISKU_ANCHORED, VLASISKU_INSENSITIVE, VLASISKU_LITERAL, VLASISKU_WHOLE_WORD

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

As C<getValsi> matches against the normalized forms of words, note that any
periods at the beginning of I<cmavo> are ignored.  Thus, the string C<'.e'>
will match I<be> but not I<.e>, and forcing literal interpretation with
C<'\.e'> will cause no results to be returned.

=head1 SEE ALSO

L<sisku(1)>, Lojban::Valsi, Lojban::Vlatai

I<The Complete Lojban Language> by John Woldemar Cowan

L<http://www.lojban.org/>

=head1 AUTHOR

John T. "kamymecraijun." Wodder II <minimiscience@gmail.com>

=head1 LICENSE

Feel free to do whatever the I<bais.> you want with this.

=cut
