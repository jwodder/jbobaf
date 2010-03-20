package Lojban::Vlatai;
require Exporter;
our @ISA = ('Exporter');
our $VERSION = v2.0;
our @EXPORT = qw< rafyjongau jvomre jvokatna vlalei veljvo xugismu xucmavo
 xucmene xulujvo xufuhivla jvozba xulujmaho fadgau >;
our @EXPORT_OK = qw< $C $V $CC $C_C $CCV $CVV $CVC $gism $CyC $CxC @klesi
 JVOZBA_BRIVLA JVOZBA_CMENE >;
our %EXPORT_TAGS = (
 lerpoi => [qw< $C $V $CC $C_C $CCV $CVV $CVC $gism $CyC $CxC >],
 lujvo => [qw< rafyjongau jvomre jvokatna veljvo jvozba JVOZBA_BRIVLA
  JVOZBA_CMENE >],
 klesi => [qw< vlalei xugismu xucmavo xucmene xulujvo xufuhivla xulujmaho
  @klesi >]
);

use constant {JVOZBA_BRIVLA => 1, JVOZBA_CMENE => 2};
use utf8;
use Carp;
use Lojban::Valsi;
use Lojban::Vlasisku qw< getGismu getValsiByRafsi :stodi >;

our @CARP_NOT = ('Lojban::Vlasisku', 'Lojban::Valsi');

our($glisrenoi, $jbosrenoi);

our $C = qr/[bcdfgjklmnprstvxz]/i;
our $V = qr/[aeiou]/i;
our $Vy = qr/[aeiouy]/i;
our $CC = qr/[bcfgkmpsvx][lr]|[td]r|[cs][pftkmn]|[jz][bvdgm]|t[cs]|d[jz]/i;

our $C_C = qr/
 [bdgjvzcfkpstx][lrmn]
 | [lrn][bdgjvzcfkpstx]
 | b[dgjvz] | d[bgjvz] | g[bdjvz] | j[bdgv] | v[bdgjz] | z[bdgv]
 | c[fkpt] | f[ckpstx] | k[cfpst] | p[cfkstx] | s[fkptx] | t[cfkpsx] | x[fpst]
 | l[rmn] | r[lmn] | m[lrnbdgjvcfkpstx] | n[lrm]
/xi;

our $CxC = qr/
 [lmnr][bcdfgjkpstvx] | l[mnrz] | mn | n[lmrz] | r[lmnz]
 | b[dgjmnvz] | d[bglmnv] | g[bdjmnvz] | [jz][lnr] | v[bdgjmnz]
 | f[ckmnpstx] | k[cfmnpst] | p[cfkmnstx] | sx | t[fklmnpx] | x[fmnpst]
/xi;

our $CyC = qr/($C)\1 | [bdgjvz][cfkpstx] | [cfkpstx][bdgjvz] | [cjsz]{2}
 | [ck]x | x[ck] | mz/xi;

our $CCV = qr/$CC$V/;
our $CVV = qr/$C(?:ai|au|ei|oi|$V'$V)/i;
our $CVC = qr/$C$V$C/;
our $gism = qr/$CC$V$C|$C$V$C_C/;
our @klesi = qw< gismu lujvo fu'ivla cmavo lujma'o cmene >;
our $dotside = 0;

sub rafyjongau {  # join rafsi into a lujvo
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
   $rafsi = 'y' . $rafsi
    if "$pa$re" =~ $CyC || $pa eq 'n' && $rafsi =~ /^(?:d[jz]|t[cs])/i;
  } else {
   $rafsi .= $rafste[1] =~ /^r/i ? 'n' : 'r' if $rafsi =~ /^$CVV$/o
    && (@rafste > 2 || $rafste[1] !~ /^$CCV$/o)
  }
  $lujvo .= ($lihenraf = $rafsi);
  $i++;
 }
 # tosmabru test
 if ($lujvo =~ /^($CVC+)(?:($C)$V$CC$V|($C)$V${C}y.+)$/oi) {
  # If there aren't at least two CVCs before a 'y', no hyphen is needed.
  my($pre, $nextC, $needsY) = ($1, $2 || $3, 1);
  while ($pre =~ /($C$C)/g) {
   if ($1 !~ $CC) {$needsY = 0; last; }
  }
  $lujvo =~ s/($C)($C)/$1y$2/o if $needsY && (substr($pre, -1) . $nextC) =~ $CC;
 }
 return $lujvo;
}

sub jvomre($;@) {  # lujvo scorer
 # Remember, the goal is to get as low a score as possible.
 my $lujvo = shift;
 my $l = length $lujvo;
 my $a = $lujvo =~ tr/'//;
 my $h = $lujvo =~ tr/yY//;
 $h++ if $lujvo =~ /^$CVV[rn]$C/oi;
 my $r = 0;
 foreach (@_ ? @_ : jvokatna($lujvo)) {
  /^$C$V$C$C$V$/o && do {$r += 1; next; };
  /^$C$V$C$C$/o && do {$r += 2; next; };
  /^$CC$V$C$V$/o && do {$r += 3; next; };
  /^$CC$V$C$/o && do {$r += 4; next; };
  /^$CVC$/o && do {$r += 5; next; };
  /^$C$V'$V$/o && do {$r += 6; next; };
  /^$CCV$/o && do {$r += 7; next; };
  /^$C(?:[aeo]i|au)$/oi && do {$r += 8; next; };
 }
 my $vowels = $lujvo =~ tr/aeiouAEIOU//;
 return (1000 * $l) - (500 * $a) + (100 * $h) - (10 * $r) - $vowels;
}

sub jvokatna($) {
 # Returns an array of the rafsi that comprise a lujvo.  Four-letter rafsi do
 # not have a 'y' attached.  If a non-lujvo is supplied, the results are
 # undefined.
 my @rafsi = split /y/i, shift;
 splice @rafsi, 0, 1, grep { $_ } split(/^($CVV)[rn]/oi, $rafsi[0]);
 grep { $_ } map { split /($gism$V?$ | $CVV | $CCV | $CVC)/x } @rafsi;
}

sub vlalei($) {
 (my $valsi = shift) =~ s/^[\s.]+|[\s.]+$//g;
 return 'cmene' if $valsi =~ /$C$/o;
 return 'cmavo' if $valsi !~ /$C$C/o;
 return 'gismu' if $valsi =~ /^$gism$V$/o;
 return 'lujvo' if $valsi =~ /^ $CVV$CCV $ |
  ^(?:$CVV(?:r(?!r)|n(?=r))|$CCV|${CVC}y?|${gism}y)
   (?:$CVV|$CCV|${CVC}y?|${gism}y)*
   (?:$CVV|$CCV|$gism$V)$
  /xoi;
 return undef;
}

sub veljvo($;$) { # Split a {lujvo} into its root {valsi}
 my @valsi;
 for (jvokatna shift) {
  my $vla = /^$gism$V?$/o ? getGismu($_ . (/$V$/ ? '' : '.'), VLASISKU_ANCHORED)
   : getValsiByRafsi($_, VLASISKU_ANCHORED | VLASISKU_LITERAL);
  push @valsi, $vla || ($_[0] ? $_ : undef);
 }
 return @valsi;
}

sub xugismu($) { fadgau($_[0]) =~ /^$gism$V$/o }

sub xucmavo($) {
 # fadgau($_[0]) =~ /^$C?$Vy(?:'?$Vy)*$/o
 fadgau($_[0]) =~ /^$CVV$ |^(?:(?:$C|[iu])?$Vy)(?:'(?:$Vy|ai|au|ei|oi))*$/xoi
}

sub xulujmaho($) {
 for (split /[\s.]+|(?=$C)/, $_[0]) { return 0 if !xucmavo $_ }
 return 1;
}

sub xucmene($) {
 my $valsi = fadgau($_[0]);
 return $valsi =~ /$C$/o && $valsi !~ tr/ //
  && ($dotside || $valsi !~ /(?<!$C)(?:la|lai|la'i|doi)(?!$Vy|')/io);
}

sub xulujvo($) {
 fadgau($_[0]) =~ /^$CVV$CCV$ |
  ^(?:$CVV(?:r(?!r)|n(?=r))|$CCV|${CVC}y?|${gism}y)
   (?:$CVV|$CCV|${CVC}y?|${gism}y)*
   (?:$CVV|$CCV|$gism$V)$
  /xoi
}

sub xufuhivla($) {
 my $valsi = fadgau($_[0]);
 $valsi =~ /$V$/o && $valsi !~ tr/yY // && $valsi =~ /$V(?:$C+|[',])$V/o
  && !xugismu($valsi) && !xulujvo($valsi) && !($valsi =~ /^$C/
  && xulujvo("to$valsi")) && $valsi =~ /($C$C+)/o or return 0;
 my($cluster, $clustBegin, $clustEnd) = ($1, $-[1], $+[1]);
 my $head = substr $valsi, 0, $clustBegin;
 # If the consonant cluster is not initial or the part after the cluster is
 # monosyllabic, then the cluster must be preceded by /^$C?$V('?$V)*$/, which
 # may contain no more than three non-apostrophes.
 if ($cluster =~ $CxC || substr($valsi, $clustEnd) =~ /^$V+$/o) {
  return $head =~ /^$C?$V(?:'?$V)*$/o && length($head) - ($head =~ tr/'//) <= 3
 } else {
  # If the cluster is initial and the part after it is not monosyllabic, then
  # either (a) the part after the cluster fails the {slinku'i} test, in which
  # case the part before the cluster must be a valid beginning not of form CV,
  # or (b) the cluster is at the beginning of $valsi.
  return xulujvo('to' . substr($valsi, $clustEnd))
   ? $head =~ /^$C?$V(?:'?$V)*$/o && $head !~ /^$C$V$/o
   : $clustBegin == 0
 }
}

sub jvozba {
# In addition to the human-readable messages, should errors also be reported in
# a way that can be easily analyzed by the calling program?
 my $klesi = $_[$#_] =~ /^\d+$/ ? int pop : JVOZBA_BRIVLA;
 my @valsi = @_;
 if (@valsi < 2) {
  $glisrenoi = "At least two valsi are needed to make a lujvo.";
  $jbosrenoi = "nitcu su'o re valsi lonu zbasu lo lujvo";
  return ();
 }
 my @rafsi;
 my $i=0;
 for (@valsi) {
  my $valsi = ref $_ ? $_ : getGismu($_, VLASISKU_LITERAL | VLASISKU_ANCHORED);
  if (!$valsi) {
   $glisrenoi = "\"$_\" does not have any rafsi.";
   $jbosrenoi = "zo $_ se rafsi noda";
   return ();
  }
  my @valraf = $valsi->rafsi;
  if ($i == $#valsi) {
   if ($klesi == JVOZBA_CMENE) { @valraf = grep !/$V$/o, @valraf }
   elsif ($klesi == JVOZBA_BRIVLA) { @valraf = grep /$V$/o, @valraf }
   push @valraf, $valsi->valsi if $valsi->xugismu;
  } elsif ($valsi->xugismu) {
   (my $vla = $valsi->valsi) =~ s/$V$/y/o;
   push @valraf, $vla;
  }
  if (!@valraf) {
   $glisrenoi = "\"$_\" does not have any suitable rafsi.";
   $jbosrenoi = "zo $_ se rafsi no mapti";
   return ();
  }
  push @rafsi, [ @valraf ];
  $i++;
 }
 my @jvoste = ();
 my @rafyzva = (0) x @rafsi;
 do {
  $i = 0;
  my @raf = map { $rafsi[$i++][$_] } @rafyzva;
  my $lujvo = rafyjongau @raf;
  my $termre = jvomre $lujvo, @raf;
  $lujvo =~ s/$V$//o if $klesi == JVOZBA_CMENE;
  $lujvo = cnino Lojban::Valsi $lujvo, klesi => $lujvo =~ /$V$/o ? 'lujvo' :
   'cmene', krarafsi => [ @raf ], veljvo => [ @valsi ], termre => $termre;
  push @jvoste, $lujvo;
  for ($i = @rafsi - 1; $i >= 0; $i--) {
   if (++$rafyzva[$i] >= @{$rafsi[$i]}) { $rafyzva[$i] = 0 }
   else { last }
  }
 } while ($i >= 0);
 @jvoste = sort { $a->termre <=> $b->termre } @jvoste;
 return wantarray ? @jvoste : $jvoste[0];
}

my @namcu = qw< no pa re ci vo mu xa ze bi so >;

sub fadgau($) {
 (my $valsi = shift) =~ s/^[\s.,]+|[\s.,]+$//g;
 $valsi =~ s/(\d)/$namcu[$1]/g;
 $valsi =~ tr/áéíóúHh/AEIOU''/;
 $valsi =~ s/,*[\s.][\s.,]*/ /g;
 $valsi =~ tr/,//s;
 # Delete commas not between vowels; is this correct?
 $valsi =~ s/(?<!$Vy),|,(?!$Vy)//g;
 return $valsi !~ tr/A-GI-PR-VX-Za-gi-pr-vx-z',. //c && $valsi !~ $CyC
  && $valsi !~ /(?<!$Vy)'(?!$Vy)/ && $valsi;
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
    @valsi = veljvo($lujvo);

    $klesi = vlalei($valsi);

    use Lojban::Vlatai qw/$gism $V/;
    print "gismu\n" if $valsi =~ /^$gism$V$/;

=head1 DESCRIPTION

Lojban::Vlatai provides a few simple yet useful functions and regular
expressions for handling Lojban words at the morphological level.

The following functions are exported by default.  Unless otherwise stated, all
values are passed & returned as strings or lists of strings and must be in
lowercase.

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

=item veljvo LUJVO[, RETURN_BAD]

Splits a I<lujvo> into its component I<valsi> and returns them as a list of
C<Lojban::Valsi> objects.  If a I<rafsi> cannot be found (Lojban::Vlasisku is
used for the searching), the corresponding location in the returned list will
contain C<undef>, unless a true value is supplied as a second argument to
C<veljvo>, in which case the unidentified I<rafsi> will be returned verbatim.
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

=item C<$C>

Matches a Lojban consonant.

=item C<$gism>

Matches the first four letters of a valid I<gismu>.  The pattern C</$gism$v/>
can be used to match a complete I<gismu>.

=item C<$CC>

Matches a valid initial consonant pair.

=item C<$V>

Matches one of the five main Lojban vowels.

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
