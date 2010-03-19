# $Id$
package Lojban::Vlatai;
# Lojban morphology routines

require Exporter;
our @ISA = ('Exporter');
our $VERSION = 1.0;
our @EXPORT = qw< rafyjongau jvomre jvokatna vlalei terjvo >;
our @EXPORT_OK = qw < $c $v $lidne $CCV $CVV $CVC $gism $voiced $unvoiced
 $verboten $nodup >;
our %EXPORT_TAGS = (
 fancu => [qw< rafyjongau jvomre jvokatna vlalei terjvo >],
 lerpoi => [qw< $c $v $lidne $CCV $CVV $CVC $gism $voiced $unvoiced $verboten
  $nodup >]
);

use Carp;
use Lojban::Valsi;
use Lojban::Vlasisku qw< getGismu getValsiByRafsi :stodi >;

our @CARP_NOT = qw< Lojban::Vlasisku Lojban::Vlatai >;

our $c = qr/[bcdfgjklmnprstvxz]/;
our $v = qr/[aeiou]/;
our $lidne = qr/bl|br|cf|ck|cl|cm|cn|cp|cr|ct|dj|dr|dz|fl|fr|gl|gr|jb|jd|jg|jm|
 jv|kl|kr|ml|mr|pl|pr|sf|sk|sl|sm|sn|sp|sr|st|tc|tr|ts|vl|vr|xl|xr|zb|zd|zg|zm|
 zv/x;
# our $lidne = qr/[pbcsmfvkgx][lr]|[td]r|[cs][pftkmn]|[jz][bvdgm]|t[cs]|d[jz]/;

our $CCV = qr/$lidne$v/;
our $CVV = qr/$c(?:ai|au|ei|oi|$v'$v)/;
our $CVC = qr/$c$v$c/;
our $gism = qr/(?:$lidne$v|$c$v$c)$c/;

our $voiced = qr/[bdgjvz]/;
our $unvoiced = qr/[cfkpstx]/;
our $verboten = qr/cx|kx|xc|xk|mz|n(?:d[jz]|t[cs])/;
our $nodup = qr/[cjsz]/;

sub rafyjongau(@) {  # join rafsi into a lujvo
 # I couldn't make the prototype ($$@) because then I couldn't pass an array.
 if (@_ < 2) {
  carp "mo'a rafsi cu sumti la'o py. Lojban::Vlatai::rafyjongau .py.";
  return undef;
 }
 my @rafste = @_;
 my($lujvo, $lihenraf) = ('');
 my $i=0;
 for my $rafsi (@rafste) {
  $rafsi .= 'y' if $rafsi =~ /^$gism$/o && $i != $#rafste;
  if (defined $lihenraf) {
   my($pa, $re) = (substr($lihenraf, -1), substr($rafsi, 0, 1));
   $rafsi = 'y' . $rafsi if $pa eq $re || $pa =~ $voiced && $re =~ $unvoiced ||
    $pa =~ $unvoiced && $re =~ $voiced || "$pa$re" =~ /^$verboten$/o ||
    $pa =~ $nodup && $re =~ $nodup || $pa eq 'n'
    && $rafsi =~ /^(?:d[jz]|t[cs])/;
  } else {
   $rafsi .= $rafste[1] =~ /^r/ ? 'n' : 'r' if $rafsi =~ /^$CVV$/o &&
    (@rafste > 2 || $rafste[1] !~ /^$CCV$/o)
  }
  $lujvo .= ($lihenraf = $rafsi);
  $i++;
 }
 # tosmabru test
 if ($lujvo =~ /^($CVC+)(?:($c)$v$lidne$v|($c)$v${c}y.+)$/o) {
  # If there aren't at least two CVCs before a 'y', no hyphen is needed.
  my($prefix, $nextC, $needsY) = ($1, $2 || $3, 1);
  while ($prefix =~ /($c$c)/g) {
   if ($1 !~ $lidne) {$needsY = 0; last; }
  }
  if ($needsY && (substr($prefix, -1) . $nextC) =~ $lidne) {
   $lujvo =~ s/($c)($c)/$1y$2/o
  }
 }
 return $lujvo;
}

sub jvomre($) {  # lujvo scorer
 # Remember, the goal is to get as low a score as possible.
 my $lujvo = shift;
 my $l = length $lujvo;
 my $a = $lujvo =~ tr/'//;
 my $h = $lujvo =~ tr/y//;
 $h++ if $lujvo =~ /^$CVV[rn]$c/o;
 my $r = 0;
 foreach (jvokatna($lujvo)) {
  /^$c$v$c$c$v$/o && do {$r += 1; next; };
  /^$c$v$c$c$/o && do {$r += 2; next; };
  /^$c$c$v$c$v$/o && do {$r += 3; next; };
  /^$c$c$v$c$/o && do {$r += 4; next; };
  /^$CVC$/o && do {$r += 5; next; };
  /^$c$v'$v$/o && do {$r += 6; next; };
  /^$CCV$/o && do {$r += 7; next; };
  /^$c$v$v$/o && do {$r += 8; next; };
 }
 my $V = $lujvo =~ tr/aeiou//;
 return (1000 * $l) - (500 * $a) + (100 * $h) - (10 * $r) - $V;
}

sub jvokatna($) {
 # Returns an array of the rafsi that comprise a lujvo.  Four-letter rafsi do
 # not have a 'y' attached.  If a non-lujvo is supplied, the results are
 # undefined.
 my @rafsi = split /y/, shift;
 splice @rafsi, 0, 1, grep { $_ } split(/^($CVV)[rn]/o, $rafsi[0]);
 grep { $_ } map { split /($gism$v?$ | $CVV | $CCV | $CVC)/x } @rafsi;
}

sub vlalei($) {
 (my $valsi = shift) =~ s/^[\s.]+|[\s.]+$//g;
 return 'cmene' if $valsi =~ /$c$/o;
 return 'cmavo' if $valsi !~ /$c$c/o;
 return 'gismu' if $valsi =~ /^$gism$v$/o;
 return 'lujvo' if $valsi =~ /^ $CVV$CCV $ |
  ^($CVV[rn]|$CCV|${CVC}y?|${gism}y)
   ($CVV|$CCV|${CVC}y?|${gism}y)*
   ($CVV|$CCV|$gism$v)$
  /xo;
 return '';
}

sub terjvo($;$) { # Split a {lujvo} into its root {valsi}
 my @valsi;
 for (jvokatna shift) {
  my $vla = /^$gism$v?$/o ? getGismu($_ . (/$v$/ ? '' : '.'), VLASISKU_ANCHORED)
   : getValsiByRafsi($_, VLASISKU_ANCHORED | VLASISKU_LITERAL);
  push @valsi, $vla || ($_[0] ? $_ : undef);
 }
 return @valsi;
}

1;

__END__

=pod

=head1 NAME

Lojban::Vlatai - Lojban morphology routines

=head1 SYNOPSIS

    use Lojban::Vlatai;

    $lujvo = rafyjongau(@rafsi);
    $score = jvomre($lujvo);
    @rafsi2 = jvokatna($lujvo);
    @valsi = terjvo($lujvo);

    $klesi = vlalei($valsi);

    use Lojban::Vlatai qw/$gism $v/;

    print "gismu\n" if $valsi =~ /^$gism$v$/;

=head1 DESCRIPTION

Lojban::Vlatai provides a few simple yet useful functions and regular
expressions for handling Lojban words at the morphological level.

The following functions are exported by default and may also be requested with
the C<:fancu> export tag.  Unless otherwise stated, all values are passed &
returned as strings or lists of strings and must be in lowercase.

=over

=item jvokatna LUJVO

Splits a I<lujvo> into its component I<rafsi>, minus hyphens and any 'y's at
the end of four-letter I<rafsi>, and returns the I<rafsi> as a list.  Note that
five-letter I<rafsi> (i.e., complete I<gismu> at the end of I<lujvo>) are left
intact and do not have the final vowel stripped.  If a non-I<lujvo> is
supplied, the results are undefined.

=item jvomre LUJVO

Calculates the score for the given I<lujvo> using the algorithm described in
section 4.12 of I<The Complete Lojban Language> by John Woldemar Cowan.  Recall
that the goal is to get as low a score as possible.  The score is returned as
an integer, but since this is Perl, you can use it as a string without any
problems.  If a non-I<lujvo> is passed to C<jvomre>, the results are undefined.

=item rafyjongau LIST

Joins a list of I<rafsi> into a valid I<lujvo>.  The 'y' at the end of any
four-letter I<rafsi> is optional, and the last I<rafsi> may end in a consonant
(for forming I<cmene>).  If fewer than two arguments are supplied, a warning is
printed, and C<undef> is returned.  If invalid I<rafsi> are passed, the results
are undefined.

=item terjvo LUJVO[, RETURN_BAD]

Splits a I<lujvo> into its component I<valsi> and returns them as a list of
C<Lojban::Valsi> objects.  If a I<rafsi> cannot be found (Lojban::Vlasisku is
used for the searching), the corresponding location in the returned list will
contain C<undef>, unless a true value is supplied as a second argument to
C<terjvo>, in which case the unidentified I<rafsi> will be returned verbatim.
If a non-I<lujvo> is supplied, the results are undefined.

=item vlalei VALSI

Determines the type of the given I<valsi> (I<cmene>, I<cmavo>, I<gismu>, or
I<lujvo>) and returns the type as a string.  If the type is none of the above,
the empty string is returned, which may indicate either a I<fu'ivla> or an
invalid Lojban word.

This function isn't entirely perfect, but it should handle most reasonable
situations.  All periods & whitespace are stripped from the ends of the
I<valsi> before processing, and no checks for valid Lojban words (e.g., illegal
consonant clusters) are performed at any point.  Anything that ends in a
consonant will be deemed a I<cmene>, and anything else that lacks a consonant
pair will be deemed a I<cmavo>.  I<lujvo> lacking hyphens or with superfluous
hyphens are accepted as normal I<lujvo>.

=back

A number of regular expressions for matching letterals or letteral strings can
also be exported with the C<:lerpoi> tag or just requested individually.  Even
if they are not imported into the current package, they may still be accessed
via the Lojban::Vlatai namespace (e.g., C<$Lojban::Vlatai::CVV>).  Note that
these regexes will only match lowercase letterals.

=over

=item C<$CCV>

Matches a "CCV" letteral sequence, i.e., an initial consonant pair followed by
a vowel.

=item C<$CVC>

Matches a "CVC" letteral sequence.

=item C<$CVV>

Matches a "CVV" letteral sequence, i.e., a consonant followed by either two
vowels separated by an apostrophe or one of the diphthongs "ai", "au", "ei",
and "oi".

=item C<$c>

Matches a Lojban consonant.

=item C<$gism>

Matches the first four letters of a valid I<gismu>.  The pattern C</$gism$v/>
can be used to match a complete I<gismu>.

=item C<$lidne>

Matches a valid initial consonant pair.

=item C<$nodup>

Matches one of the four consonants "c", "j", "s", & "z" which may not appear
next to each other in a word.

=item C<$unvoiced>

Matches an unvoiced consonant.

=item C<$v>

Matches one of the five main Lojban vowels.

=item C<$verboten>

Matches one of the explicitly forbidden consonant sequences "cx", "kx", "xc",
"xk", "mz", "ndj", "ndz", "ntc", and "nts".

=item C<$voiced>

Matches a voiced consonant.  Note that this is as defined for the purposes of
Lojban morphology, and so "l", "m", "n", and "r" are excluded.

=back

=head1 SEE ALSO

Lojban::Valsi, Lojban::Vlasisku

I<The Complete Lojban Language> by John Woldemar Cowan

L<http://www.lojban.org/>

=head1 AUTHOR

John T. "kamymecraijun." Wodder II <minimiscience@gmail.com>

=head1 LICENSE

Feel free to do whatever the I<bais.> you want with this.

=cut
