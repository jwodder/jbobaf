use utf8;
use Lojban::Vlatai;
use Lojban::Valsi;

sub lerfendi($) {
 # Deal with (common) accents and Hs:
 (my $selsku = shift) =~ tr/áéíóúHh/AEIOU''/;
 # Split on whitespace & periods:
 my @valsi = split /[\s.]+/, $selsku;
  
 # Identify {cmene}:
 @valsi = map {
  if (/$C$/) {
   unless ($dotside) {
    my($pula, $bala) = (0, 0);
    ($pula, $bala) = ($-[0], pos) while /(?<!$C)(la|lai|la'i|doi)(?!$Vy|')/goi;
    return (substr($_, 0, $pula), substr($_, $pula, $bala - $pula),
     cnino Lojban::Valsi substr($_, $bala), klesi => 'cmene');
   } else { return cnino Lojban::Valsi $_, klesi => 'cmene' }
  } else { return $_ }
 } @valsi;

 # Split after emphasis: 
 @valsi = map {
  return $_ if ref;
  my @brivla = ();
  push @brivla, substr $_, 0, pos, ''
   while /[AEIOU]$V*(?:(?:$C|[yY,])+|')$V+(?=$C|$ )/xo;
  return(@brivla, $_);
 } @valsi;

 # Locate the beginnings of & identify {brivla}:
 @valsi = map {
  return $_ if ref || !/$C[yY]?$C/o;
  my $cfari = $-[0];
  my @lidne = ();  # Preceding {cmavo} (if any)
  push @lidne, substr $_, 0, $cfari, ''
   if substr($_, $cfari) =~ /^(?:$C[yY]){2,}/o
   || substr($_, $cfari) =~ /^$C[yY]/o && substr($_, 0, $cfari) !~ /$C$V$/o;
  push @lidne, substr $_, 0, 2, '' while /^$C[yY]/o;
  if (/($C(?:[yY]?$C)+)/o) {
   # We definitely have a {brivla}; now we just need to know where it starts.
   my($cluster, $brivla) = ($1, substr $_, $-[0]);
   push @lidne, substr $_, 0, $-[0];
   # The {brivla} begins at the start of the first consonant cluster *unless*:
   # - the cluster is not a valid initial cluster,
   # - the {brivla} formed this way would be monosyllabic,
   # - or the {brivla} is not a {lujvo}, and it fails the {slinku'i} test.
   if ($cluster =~ tr/yY// || $cluster =~ $CxC || $brivla =~ /^$C+$V+$/o
    || !xugismu($brivla) && !xulujvo($brivla) && xulujvo("to$brivla")) {
    # This ^^ part is a minor optimization.
   # If the {brivla} does not begin at the consonant cluster, prepend to it the
   # last portion of the text before it.  If there is no such syllable, or if
   # this results in an invalid word, the text is invalid; however, the user
   # will not be made aware of this until vlalei() or the like is called on the
   # {brivla}.
    @lidne and $lidne[$#lidne] =~ s/($C?$V(?:'?$V)*)$//o
     and $brivla = $1 . $brivla
   }
   return @lidne, cnino Lojban::Valsi $brivla
  } else { return @lidne, $_ }
 } @valsi;

 # Split {cmavo} clusters:
 @valsi = map {
  ref $_ ? $_ : map { cnino Lojban::Valsi $_, klesi => 'cmavo' }
   grep { $_ ne '' } split /(?=$C)/o
 } @valsi;

 return @valsi;
}

__END__

TO DO
- Test &lerfendi to make sure it works!!!!!
- Make &lerfendi remove Cy {cmavo} from the ends of potential {brivla}
- Rewrite &lerfendi to handle ZOI constructs; this may require changing it from
  processing the string all at once to processing it in pieces from start to
  finish.
- Make &lerfendi able to handle commas
