#!/usr/bin/perl -wT
# This program was written by John T. "kamymecraijun." Wodder II
# <minimiscience+jvozba@gmail.com>.  Feel free to do whatever the {bais.} you
# want with it.
use strict;
use CGI qw< :standard start_table start_Tr start_td -nosticky >;
use CGI::Carp 'fatalsToBrowser';
use Lojban::Vlatai qw< :fancu :lerpoi >;
use Lojban::Vlasisku qw< :DEFAULT :stodi >;
use Lojban::Valsi;

print header, start_html(-title => 'Jvozba', -style => {-src => 'jbobaf.css'});
print start_table({-align => 'center', -border => 0, -class => 'main'});
print start_Tr, start_td, h2('Jvozba');

print h4('Create a', tt('lujvo'));
print start_form(-method => 'GET');
print p({-class => 'link'}, b(tt('tanru:')), textfield('tanru', '', 32, 128));
print p({-class => 'link'}, radio_group(-name => 'vlalei', -values =>
 ['brivla', 'pluja', 'cmene'], -default => 'brivla', -labels => {brivla =>
 'brivla only', pluja => 'brivla and cmene', cmene => 'cmene only'}));
# Attempting to wrap the Lojban words in the radio button descriptions in tt()
# doesn't work, as CGI.pm HTML-encodes all form data.
print p({-class => 'link'}, submit(-label => 'zbasu'));
print endform;

IFBLOCK: { if (url_param('tanru')) {
 my @valsi = map { /$c$c/ ? $_ : split /(?<=.)(?=$c)/ } split ' ',
  lc url_param('tanru');
 if (@valsi < 2) {
  print p({-class => 'link'}, b('Error:'), 'You need at least two'
   . ' <tt>valsi</tt> to make a <tt>lujvo</tt>');
  last IFBLOCK;
 }
 my @rafsi;
 my $i=0;
 for (@valsi) {
  my $valsi = getGismu($_, VLASISKU_LITERAL | VLASISKU_ANCHORED);
  if (!$valsi) {
   print p({-class => 'link'}, b('Error:'), 'No <tt>rafsi</tt> found for "' .
    tt(escapeHTML $_) . '"');
   last IFBLOCK;
  }
  my @valraf = $valsi->rafsi();
  if ($i == $#valsi) {
   if (url_param('vlalei') eq 'cmene') { @valraf = grep !/[aeiou]$/, @valraf }
   elsif (url_param('vlalei') ne 'pluja') { @valraf = grep /[aeiou]$/, @valraf }
   push @valraf, $valsi->valsi() if $valsi->isGismu();
  } elsif ($valsi->isGismu()) {
   (my $vla = $valsi->valsi()) =~ s/[aeiou]$/y/;
   push @valraf, $vla;
  }
  if (!@valraf) {
   print p({-class => 'link'}, b('Error:'), '"' . tt(escapeHTML $_)
    . '" has no appropriate <tt>rafsi</tt>.');
   last IFBLOCK;
  }
  $rafsi[$i++] = [ @valraf ];
 }
 my %jvoste;  # mapping from lujvo to score
 my @rafyzva = (0) x @rafsi;
 do {
  $i = 0;
  my $lujvo = rafyjongau map { $rafsi[$i++][$_] } @rafyzva;
  my $termre = jvomre $lujvo;
  $lujvo =~ s/[aeiou]$// if url_param('vlalei') eq 'cmene';
  $jvoste{$lujvo} = $termre;
  for ($i = @rafsi - 1; $i >= 0; $i--) {
   if (++$rafyzva[$i] >= @{$rafsi[$i]}) { $rafyzva[$i] = 0 }
   else { last }
  }
 } while ($i >= 0);
 print start_table({-align => 'center', -border => 0});
 print Tr(th(tt('lujvo')), th('Score'));
 print Tr(td(tt($_)), td($jvoste{$_}))
  for sort { $jvoste{$a} <=> $jvoste{$b} } keys %jvoste;
 print end_table;
} }

print hr;

print h4('Split a', tt('lujvo'));
print start_form(-method => 'GET');
print p({-class => 'link'}, b(tt('lujvo:')), textfield('lujvo', '', 32, 128),
 submit(-label => 'katna'));
print endform;

if (url_param('lujvo')) {
 for (terjvo(lc url_param('lujvo'), 1)) {
  if (ref) { printValsi($_) }
  else {
   print p({-class => 'link'}, b('Error:'), 'No match found for "' .
    tt(escapeHTML $_) . '"')
  }
 }
}

print p({-class => 'link'}, a({-href => 'http://github.com/jwodder/jbobaf'},
 'Jbobaf'), '|', a({-href => 'vlasisku.cgi'}, 'Vlasisku'), '|',
 a({-href => 'ralju.html'}, 'ralju'));

print end_table, end_Tr, end_td, end_html;

sub printValsi {
 my $vla = shift;
 print start_table({-align => 'center', -border => 0});
 for my $attr (qw< valsi selmaho rafsi ralvla djuvla selvla notci >) {
  next if !defined $vla->$attr() || !$vla->$attr() && !($attr ne 'rafsi' &&
   $vla->$attr() eq '0');
  print Tr(td({-class => 'kaicme'}, $attr), td({-width => 450},
   tt(escapeHTML("@{[ $vla->$attr() ]}"))));
 }
 print end_table, br;
}

__END__

TO DO
- Handle {tanru} that already contain {lujvo}
- Improve the word splitting of {tanru}
- Support {lujvo} formed with "{zei}"
- Try to get printValsi() to show the complete definitions for {cmavo} rather
  than just their {gismu} list entries.  Once this is done, have {selma'o} be
  labeled as "selma'o" instead of "selmaho".
- Add a note saying that any & all problems & suggestions should be e-mailed to
  me
