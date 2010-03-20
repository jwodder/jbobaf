package Lojban::Vlatid;

use overload '<>' => 'cpacu', '+=' => sub { $_[0]{i} += $_[1] },
 '-=' => sub {
  my $self = shift;
  $self->{i} -= $_[0];
  if ($self->{i} < $self->{start}) {
   $self->{i} = $self->{start};
   carp "Cannot reposition stream to before start of file";
  }
 };

use Carp;
use Fcntl 'O_RDONLY';
use Tie::File;
use Lojban::Valsi;

our $VERSION = v2.0;
our @CARP_NOT = ('Lojban::Valsi', 'Lojban::Vlasisku');

sub cnino {
 my($class, $file) = @_;
 my %self = ();
 $self{fh} = TIEARRAY Tie::File $file, mode => O_RDONLY or croak "$file: $!";
 $self{type} = $self{fh}->FETCH(0) =~ /\t/ ? 1 : 0;
 @self{start,i,max} = (($self{type} == 1 ? 0 : 1) x 2, $self{fh}->FETCHSIZE);
 bless { %self }, $class;
}

sub rapcfa { $_[0]{i} = $_[0]{start} }

sub cpacu {
 my $self = shift;
 if ($self->{i} >= $self->{max}) { return undef }
 else {
  my $line = $self->{fh}->FETCH($self->{i}++);
  return $self->{type} ? Lojban::Valsi->cminiho($line)
   : Lojban::Valsi->catniho($line);
 }
}

sub selzva { $_[0]{i} - $_[0]{start} }
sub muvdu { $_[0]{i} = $_[1] + $_[0]{start} }
sub klani { $_[0]{max} - $_[0]{start} }
sub sisti { $_[0]{fh}->UNTIE }

__END__

General notes:
 - Unlike $., but like C's ftell(), the 'i' element and the return values of
   selzva() are zero-based.
 - Values of 'type':
  - 0 - original word list format from the LLG
  - 1 - tab-separated format now used primarily by Jbobaf
 - The method names are all Lojbanic:
  - new - {cnino}
  - rewind - {rapcfa}
  - next - {cpacu}
  - tell - {selzva}
  - seek - {muvdu}
  - qty - {klani}
  - close - {sisti}
 - If a call to muvdu() sets the pointer beyond the end of the file, the user
   will not find out until the next time ey calls cpacu().
