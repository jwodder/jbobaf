package Lojban::Vlatid::Sampu;
use overload '<>' => 'cpacu';
use Carp;
use Lojban::Valsi;

our @CARP_NOT = ('Lojban::Valsi', 'Lojban::Vlasisku');

sub cnino {
 my($class, $file) = @_;
 my $self = {};
 open $self->{fh}, '<', $file or croak "$file: $!";
 $self->{i} = 0;
 if (<$self->{fh}> =~ /\t/) {
  $self->{type} = 1;
  seek $self->{fh}, 0, 0;
 } else { $self->{type} = 0 }
 bless $self, $class;
}

sub rapcfa {
 my $self = shift;
 seek $self->{fh}, 0, 0;
 $self->{i} = 0;
 scalar <$self->{fh}> if $self->{type} == 0;
}

sub cpacu {
 my $self = shift;
 my $line = <$self->{fh}>;
 if (defined $line) {
  $self->{i}++;
  return $self->{type} ? Lojban::Valsi->cminiho($line)
   : Lojban::Valsi->catniho($line);
 } else { return undef }
}

sub selzva { $_[0]{i} }
sub sisti { close $_[0]{fh} }
