package Lojban::Valsi;
use warnings;
use strict;
use overload '<=>' => 'karbi', 'cmp' => 'karbi', '""' => 'valsi';
use Carp;
use Lojban::Vlatai (':lerpoi', '@klesi');
use Lojban::Vlasisku ('getGismu', 'getValsiByRafsi', ':stodi');

our $VERSION = v2.0;
our @CARP_NOT = ('Lojban::Vlasisku', 'Lojban::Vlatai', 'Lojban::Vlatid');

my @scalarAttr = ('valsi', 'selmaho', 'ralvla', 'djuvla', 'selvla', 'notci',
 'klesi', 'termre', 'xuvladra');
my @arrayAttr = ('rafsi', 'krarafsi', 'veljvo');

sub cnino {
 my $class = shift;
 my %selkai = (valsi => shift);
 my %userStuff = @_;
 foreach (@scalarAttr) {
  $selkai{$_} = $userStuff{$_} if defined $userStuff{$_} && $_ ne 'xuvladra'
   && ($_ ne 'klesi' || grep { $_ eq $userStuff{klesi} } @klesi)
 }
 foreach (@arrayAttr) {
  $selkai{$_} = [ @{$userStuff{$_}} ]
   if defined $userStuff{$_} && @{$userStuff{$_}}
 }
 bless { %selkai }, $class;
}

sub cminiho {
 my($class, $entry) = @_;
 my %data = ();
 # What's the best way to determine if an entry is from gimste or ma'oste?
 if (substr($entry, 10, 2) !~ /^ [[:upper:]]$/) {
  $data{klesi} = $entry =~ /^ $gism$V / ? 'gismu' : 'cmavo';
  foreach my $sec (['valsi', 1, 5], ['rafsi', 7, 12], ['ralvla', 20, 20],
   ['djuvla', 41, 20], ['selvla', 62, 96], ['notci', 169, -1]) {
   last if $sec->[1] >= length $entry;
   ($data{$sec->[0]} = substr $entry, $sec->[1], $sec->[2]) =~ s/^\s+|\s+$//g;
   $data{$sec->[0]} = [ split /\s+/, $data{$sec->[0]} ] if $sec->[0] eq 'rafsi';
  }
 } else {
  $data{klesi} = 'cmavo';
  foreach my $sec (['valsi', 0, 11], ['selmaho', 11, 5], ['ralvla', 20, 42],
   ['selvla', 62, 106], ['notci', 168, -1]) {
   last if $sec->[1] >= length $entry;
   ($data{$sec->[0]} = substr $entry, $sec->[1], $sec->[2]) =~ s/^\s+|\s+$//g;
  }
 }
 bless { %data }, $class;
}

my @tabFields = qw< valsi selmaho rafsi ralvla djuvla selvla notci klesi
 krarafsi veljvo termre >;

sub catniho {
 my($class, $entry) = @_;
 my %data = ();
 my $i = 0;
 foreach (split /\t/, $entry) {
  next if $_ eq '';
  last if $i >= @tabFields;
  ($data{$tabFields[$i]} = $_) =~ s/^\s+|\s+$//g;
 } continue { $i++ }
 if (!exists $data{valsi}) {
  carp "lo cartu cmima be secau lo valsi cu sumti la'o py. Lojban::Valsi::catniho .py.";
  return undef;
 }
 foreach (@arrayAttr) {
  $data{$_} = [ split ' ', $data{$_} ] if exists $data{$_}
 }
 return bless { %data }, $class;
}

sub valsi { $_[0]->{valsi} }
sub selmaho { $_[0]->{selmaho} }

sub rafsi {
 exists $self->{rafsi} && defined $self->{rafsi} ? @{$self->{rafsi}} : ()
}

sub ralvla { $_[0]->{ralvla} }
sub djuvla { $_[0]->{djuvla} }
sub selvla { $_[0]->{selvla} }
sub notci { $_[0]->{notci} }

sub klesi {
 my $self = shift;
 $self->{klesi} = Lojban::Vlatai::vlalei $self->valsi if !exists $self->{klesi};
 return $self->{klesi};
}

sub krarafsi {
 my $self = shift;
 $self->{krarafsi} = $self->klesi eq 'lujvo' ?
  [ Lojban::Vlatai::jvokatna $self->valsi ] : [] if !exists $self->{krarafsi};
 return @{$self->{krarafsi}};
}

sub veljvo {
 my $self = shift;
 $self->{veljvo} = [ map {
   scalar(/^$gism$V?$/o ? getGismu($_ . (/$V$/ ? '' : '.'), VLASISKU_ANCHORED)
   : getValsiByRafsi($_, VLASISKU_ANCHORED | VLASISKU_LITERAL))
   # If the user wants to know what {rafsi} doesn't have a {valsi}, ey can just
   # check the corresponding index in the krarafsi.
  } $self->krarafsi ] if !exists $self->{veljvo};
 return @{$self->{veljvo}};
}

sub termre {
 my $self = shift;
 if (!exists $self->{termre}) {
  my @rafsi = $self->krarafsi;
  if (@rafsi) { $self->{termre} = Lojban::Vlatai::jvomre $self->valsi, @rafsi }
  else { $self->{termre} = undef }
 }
 return $self->{termre};
}

sub xuvladra {
 my $self = shift;
 if (!exists $self->{xuvladra}) {
  my $vlalei = Lojban::Vlatai::vlalei $self->valsi;
  $self->{xuvladra} = defined($vlalei) && (exists $self->{klesi}
   ? $self->{klesi} eq $vlalei : ($self->{klesi} = $vlalei) && 1);
 }
 return $self->{xuvladra};
}

sub cmuselmaho {
 my $selmaho = $_[0]->selmaho;
 $selmaho =~ s/\*?\d*[[:lower:]]*$// if defined $selmaho;
 return $selmaho;
}

sub fadni { Lojban::Vlatai::fadgau $_[0]->valsi }

sub xugismu { defined $_[0]->klesi && $_[0]->klesi eq 'gismu' }
sub xucmavo { defined $_[0]->klesi && $_[0]->klesi eq 'cmavo' }
sub xulujmaho { defined $_[0]->klesi && $_[0]->klesi eq "lujma'o" }
sub xucmene { defined $_[0]->klesi && $_[0]->klesi eq 'cmene' }
sub xulujvo { defined $_[0]->klesi && $_[0]->klesi eq 'lujvo' }
sub xufuhivla { defined $_[0]->klesi && $_[0]->klesi eq "fu'ivla" }

sub xubrivla { $_[0]->xugismu || $_[0]->xulujvo || $_[0]->xufuhivla }
sub xusuzmaho { $_[0]->xucmavo || $_[0]->xulujmaho }

sub fukpi {
 my $self = shift;
 my %selkai = ();
 foreach (@scalarAttr) { $selkai{$_} = $self->{$_} if exists $self->{$_} }
 foreach (@arrayAttr) { $selkai{$_} = [ @{$self->{$_}} ] if exists $self->{$_} }
 bless { %selkai }, ref $self;
}

sub karbi {
 shift unless ref $_[0];  # in case of ``karbi Lojban::Valsi $pa, $re''
 my($pa, $re) = ($_[0]->fadni, (ref $_[1] ? $_[1]->fadni :
  Lojban::Vlatai::fadgau $_[1]));
 return $_[2] ? $re cmp $pa : $pa cmp $re;
}

sub cusku {
 my $self = shift;
 my $out = shift || select;
 select((select($out), local $~ = 'VLACISKA', local $: .= '/')[0]);
 foreach (qw< valsi klesi selmaho rafsi krarafsi veljvo termre ralvla djuvla
  selvla notci >) {  # Should `xuvladra' also be included?
  next if !exists $self->{$_};
  my $value = $self->{$_};
  next if !defined $value || $value eq '' || ref $value eq 'ARRAY' && !@$value;
  (my $label = uc) =~ tr/H/h/;
  my $text = ref $value eq 'ARRAY' ? join ' ', @$value : $value;
  write $out;
  format VLACISKA =
@<<<<<<<< ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
"$label:", $text
~~        ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$text
.
 }
 print $out "\n";
}

sub xrejmina {
 shift unless ref $_[0];  # in case of ``xrejmina Lojban::Valsi $pa, $re''
 my($pa, $re) = @_;
 if ($pa->valsi ne $re->valsi) {
  carp "lo naldunli valsi po'u zo $pa .e zo $re cu sumti la'o py. Lojban::Valsi::xrejmina .py.";
  return undef;
 }
 my %mixre = ();
 foreach (qw<valsi selmaho ralvla djuvla selvla notci klesi termre xuvladra>) {
  my($a, $b) = map { !defined || $_ eq '' ? undef : $_ } ($pa->$_, $re->$_);
  next if !defined $a && !defined $b;
  if (!defined $a) { $mixre{$_} = $b }
  elsif (!defined $b) { $mixre{$_} = $a }
  elsif ($a eq $b) { $mixre{$_} = $a }
  elsif ($_ ne 'klesi' && $_ ne 'termre' && $_ ne 'xuvladra')
  # There's probably a better way to do this:
   { $mixre{$_} = "$a --- $b" }
 }
 my $prev = undef;
 $mixre{rafsi} = [
  grep { (!defined $prev or $prev ne $_) and ($prev = $_ or 1) }
   sort(@{$pa->rafsi}, @{$re->rafsi})
 ];
 for (qw< krarafsi veljvo >) {
  my @a = @{$pa->$_};
  my @b = @{$re->$_};
  # Must... resist... urge... to... use... Perl 5.10-specific... operator...
  $mixre{$_} = [ @a ] if @a == @b && !map { $a[$_] eq $b[$_] ? () : 1 } 0..$#a;
 }
 $pa->SUPER::new(%mixre);
}

sub girxre {
 shift unless ref $_[0];
 my @mixre;
 my $prev;
 for (sort @_) {
  if (!defined $prev) { $prev = $_ }
  elsif ($prev->valsi eq $_->valsi) { $prev = xrejmina($prev, $_) }
  else {push @mixre, $prev; $prev = $_; }
 }
 push @mixre, $prev if defined $prev;
 return @mixre;
}

1;

__END__

Member variables of `Valsi':
 - valsi - the word itself
 - selmaho - {selma'o} ({ma'ostecmi} only)
 - rafsi - array of {rafsi} ({gimstecmi} only)
 - ralvla - keyword
 - djuvla - hint ({gimstecmi} only)
 - selvla - definition
 - notci - notes
 - klesi - type of word ({gismu}, {cmavo}, etc.)
 - krarafsi - {rafsi} used to form a {lujvo} or {lujvo cmene}
 - veljvo - array of the components of the source {tanru} (generated by the
   class as Lojban::Valsi objects, but there's nothing stopping the user from
   supplying strings instead)
 - termre - {lujvo} score (also applies to {lujvo cmene})
 - xuvladra - Boolean representing whether the word is morphologically correct

Various design notes to work into the new documentation:
 - The (non-entry) constructor takes a hash that specifies values for arbitrary
   attributes (except those that should only be calculated, e.g., normalized
   form and base {selma'o}).
 - Comparisons are done using normalized forms.
 - {cmene} are only recognized as {lujvo cmene} if a set of {krarafsi} was
   defined for them at creation time.

=pod

=head1 NAME

Lojban::Valsi - class for standard Lojban words

=head1 SYNOPSIS

    use Lojban::Valsi;

    $valsi = cminiho Lojban::Valsi $entry;
    print "$valsi:\n";
    $valsi->printEntry();

    @porsi = sort @liste;

    use Lojban::Vlasisku;
    @danfu = Lojban::Valsi::girxre getGismu($x), getCmavo($x);

=head1 DESCRIPTION

Lojban::Valsi is a class used to manipulate entries from the official Lojban
I<gismu> & I<cmavo> lists.  As its objects can only be constructed from lines
taken from said lists, you are most likely to encounter them as the return
values of Lojban::Vlasisku functions.  Also as a result of this, the class can
currently only be used to represent I<gismu>, I<cmavo>, and compound I<cmavo>;
I<lujvo>, I<fu'ivla>, and I<cmene> should not be attempted without rewriting
the module.

Note that, when an object of this class is created, the only things it knows
about the associated I<valsi> are what was contained in the string used to
construct it; e.g., if a C<Lojban::Valsi> is made from a I<cmavo> entry taken
from the I<gismu> list, the object will not know the I<cmavo>'s I<selma'o>.
One way to solve this is to create an object for both the I<cmavo> list entry
and the I<gismu> list entry and then merge them with the C<xrejmina> method.

=head1 CONSTRUCTOR

=over

=item cminiho STRING

The single constructor for the Lojban::Valsi class takes as its argument a
string containing one (1) line from either the official I<gismu> list or the
official I<cmavo> list (excluding the first line of each, of course) or a line
from elsewhere that is formatted identically to the entries in the appropriate
list.  If an improperly formatted string is supplied, the results are
undefined.

=back

=head1 INSTANCE METHODS

=over

=item djuvla

Returns the hint word for the invocant or false if there is none.

=item isCmavo

Returns true if the invocant is a I<cmavo> or compound I<cmavo>, false
otherwise.

=item isCompoundCmavo

Returns true if the invocant is a compound I<cmavo>, false otherwise.  This is
tested by seeing whether the I<selma'o> contains an asterisk.

=item isGismu

Returns true if the invocant is a I<gismu>, false otherwise.

=item notci

Returns the notes for the invocant or false if there are none.

=item printEntry [FILE]

Prints a neatly-formatted dictionary entry for the invocant to the given
filehandle or to the currently selected filehandle if none is specified.

=item rafsi

Returns the three-letter I<rafsi> of the invocant as an array of strings.

=item ralvla

Returns the keyword or gloss for the invocant.

=item selmaho

Returns the I<selma'o> of the invocant or false if there is none.

=item selvla

Returns the definition of the invocant.

=item klesi

Returns the string C<"gismu"> if the invocant is a I<gismu> or C<"cmavo"> if
the invocant is a I<cmavo>.

=item valsi

Returns the actual word that the invocant represents.

=back

=head1 CLASS METHODS

Note that these functions are not class methods in the strictest sense; though
they can be invoked as C<karbi Lojban::Valsi $pa, $re>, using them as instance
methods (e.g., C<< $pa->karbi($re) >>) will have the same effect, and they can
even be used as non-OO subroutines (C<Lojban::Valsi::karbi($pa, $re)>).  How
you invoke them is pretty much up to you.

=over

=item girxre LIST

This method sorts a list of Lojban::Valsi objects, calls C<xrejmina> to merge
together any with the same I<valsi>, and returns the resulting list.

=item karbi VALSI, VALSI

This method lexically compares the I<valsi> of the objects passed to it and
returns -1 if the first I<valsi> comes after the second, 1 if the first comes
before the second, or 0 if they are equal.  All letters are made lowercase,
apostrophes are converted to 'h's, and periods & commas are removed before
comparison.  A string may be supplied as the second argument instead of a
Lojban::Valsi object.

=item xrejmina VALSI, VALSI

This method combines the data for two Lojban::Valsi objects into a new object.
The objects must have the same I<valsi>; if they do not, a warning is printed
and C<undef> is returned.

Each field of the objects (other than the I<valsi> and I<rafsi>) is merged as
follows: If the field only exists in one object but not the other, the extant
value is used.  If the objects have the same value for the field, that value is
used.  If the objects have different values, then, due to the lack of a better
algorithm, those values are concatenated together with C<" --- "> and used as
the new value for the field.  The I<rafsi> of the objects are merged into a
single list, with any duplicates removed.

=back

=head1 OVERLOADED OPERATORS

In order to make using the Lojban::Valsi class a little easier, three operators
have been overloaded for it.  The C<< <=> >> and C<cmp> operators are mapped to
C<karbi> so that comparisons & sorting of Lojban::Valsi objects can be
accomplished easily, and "stringification" is mapped to the C<valsi> method so
that any Lojban::Valsi object used as a string will be replaced by its
I<valsi>.

=head1 SEE ALSO

L<sisku(1)>, Lojban::Vlasisku, Lojban::Vlatai

I<The Complete Lojban Language> by John Woldemar Cowan

L<http://www.lojban.org/>

=head1 AUTHOR

John T. "kamymecraijun." Wodder II <minimiscience@gmail.com>

=head1 LICENSE

Feel free to do whatever the I<bais.> you want with this.

=cut
