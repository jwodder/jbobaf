#!/usr/bin/perl -wT
# This program was written by John T. "kamymecraijun." Wodder II
# <minimiscience+vlasisku@gmail.com>.  Feel free to do whatever the {bais.} you
# want with it.
use strict;
use CGI qw< :standard start_table start_div >;
use CGI::Carp 'fatalsToBrowser';
use Lojban::Valsi;
use Lojban::Vlasisku qw< :DEFAULT :stodi >;

print header, start_html(-title => 'Vlasisku', -style => {-src => 'jbobaf.css'},
 -script => {-src => 'sisydju.js', -language => 'JavaScript'},
 -onLoad => 'initFormAddition()');
print start_div({-class => 'main'}), h2('Vlasisku');

my(@fields, @relations, @queries, @bools);
print start_form;
print start_div({-id => 'subqueries'});
if (param()) {
 @fields = param('field');
 @relations = param('relation');
 @queries = param('query');
 @bools = (0, param('bool'));
 print subquery($bools[$_], $fields[$_], $relations[$_], $queries[$_])
  for 0..$#fields;
} else { print subquery(0) }
print end_div;

print p({-class => 'link'},
 button(-id => 'suhi', -value => '+', -disabled => 1,
	-onClick => 'addSubquery()'),
 button(-id => 'vuhu', -value => '-', -disabled => (@fields <= 1),
	-onClick => 'removeSubquery()'));

print p({-class => 'link'},
 checkbox(-name => 'gismu', -checked => 1, -label => 'Search gismu'),
 checkbox(-name => 'cmavo', -id => 'cmavo', -checked => 1,
	  -label => 'Search cmavo', -onClick => 'toggleCmavo()'),
 checkbox(-name => 'lujmaho', -id => 'lujmaho', -checked => 0,
	  -label => 'Search compound cmavo',
	  -disabled => param() && !param('cmavo')));

print p({-class => 'link'}, submit('mode', 'sisku'), submit('mode', 'cunso'));
print endform;

print hr;

if (param('mode')) {
 use constant {GIMSISKU => 1, MAhOSISKU => 2, LUJMAhOSISKU => 4};
 my $vlalei = 0;
 $vlalei |= GIMSISKU if param('gismu');
 if (param('cmavo')) {
  $vlalei |= MAhOSISKU;
  $vlalei |= LUJMAhOSISKU if param('lujmaho');
 }
 $vlalei = GIMSISKU | MAhOSISKU if $vlalei == 0;
 if (param('mode') eq 'cunso') {
  my $valsi;
  if ($vlalei & GIMSISKU) {
   $valsi = cungihu();
   unless ($vlalei & MAhOSISKU) { $valsi = cungiho() until $valsi->isGismu() }
  } else {
   $valsi = cunmaho();
   unless ($vlalei & LUJMAhOSISKU) {
    $valsi = cunmaho() while $valsi->isCompoundCmavo()
   }
  }
  printValsi($valsi);
 } elsif (param('mode') eq 'sisku') {
  my($wordSearch, $keySearch, $defSearch) =
   $vlalei & GIMSISKU && $vlalei & MAhOSISKU
    ? (\&getValsi, \&getValsiByKeyword, \&getValsiByDefinition)
    : $vlalei & GIMSISKU
     ? (\&getGismu, \&getGismuByKeyword, \&getGismuByDefinition)
     : (\&getCmavo, \&getCmavoByKeyword, \&getCmavoByDefinition);
  $bools[0] = 'or';
  my @valsi = ();
  for my $i (0..$#fields) {
   next unless $fields[$i] && $relations[$i] && defined $queries[$i]
    && $queries[$i] ne '' && $bools[$i];
   if ($bools[$i] eq 'or') {
    my $flags = $relations[$i] eq 'is' ? VLASISKU_LITERAL | VLASISKU_ANCHORED
     : $relations[$i] eq 'contains' ? VLASISKU_LITERAL : 0;
    my @results;
    if ($fields[$i] eq 'valsi') {
     @results = &$wordSearch($queries[$i], $flags)
    } elsif ($fields[$i] eq 'rafsi') {
     @results = getValsiByRafsi($queries[$i], $flags)
    } elsif ($fields[$i] eq 'glico') {
     @results = &$keySearch($queries[$i], $flags);
     push @results, &$defSearch($queries[$i], $flags);
    } elsif ($fields[$i] eq 'ralvla') {
     @results = &$keySearch($queries[$i], $flags)
    } elsif ($fields[$i] eq 'selvla') {
     @results = &$defSearch($queries[$i], $flags)
    } else { next }
    @valsi = Lojban::Valsi::girxre(@valsi, grep {
      $vlalei & GIMSISKU && $_->isGismu()
      || $vlalei & MAhOSISKU && $_->isCmavo()
      && (!$_->isCompoundCmavo() || $vlalei & LUJMAhOSISKU)
     } @results);
   } elsif ($bools[$i] eq 'and') {
    my $query = $relations[$i] eq 'is' ? qr/^\Q$queries[$i]\E$/
     : $relations[$i] eq 'contains' ? qr/\Q$queries[$i]\E/ : qr/$queries[$i]/;
    if ($fields[$i] eq 'glico') {
     @valsi = grep { grep /$query/, $_->selvla(), $_->ralvla() } @valsi
    } elsif (grep { $fields[$i] eq $_ } qw< valsi rafsi ralvla selvla >) {
     my $f = $fields[$i];
     @valsi = grep { grep /$query/, $_->$f() } @valsi;
    }
   }
  }
  print h4(@valsi . " match" . (@valsi == 1 ? '' : 'es') . " found");
  printValsi($_) for @valsi;
 }
 print hr;
}

print <<EOT;
<h4><tt>skicu</tt></h4>

<p class="text">This page can be used to search through the official Lojban
<tt>gismu</tt> and <tt>cmavo</tt> lists.  Unlike <a
href="http://jbovlaste.lojban.org"><tt>jbovlaste</tt></a>, you can search the
full text of definitions using multiple logically joined queries.  On the
downside, searches are limited to what is found in the official (English) word
lists, you can't edit anything, and there are no hyperlinks between word
entries.</p>

<p class="text">Use of the query form should mostly be self-explanatory.  Regex
matching is performed with <a
href="http://perldoc.perl.org/perlretut.html">Perl 5 regular expressions</a>,
and the boolean search operators are left, er, top-associative (I'm working on
a way to implement grouping).  Any lines with empty fields (or, if you can
manage it, options that shouldn't exist) are ignored.  Lastly, to search, click
"<tt>sisku</tt>"; "<tt>cunso</tt>" will give you a random word.</p>
EOT

print p({-class => 'link'}, a({-href => 'http://github.com/jwodder/jbobaf'},
 'Jbobaf'), '|', a({-href => 'jvozba.cgi'}, 'Jvozba'), '|',
 a({-href => 'ralju.html'}, 'ralju'));

print end_div, end_html;

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

sub subquery {
 my $bool = shift;
 my $field = shift || 'valsi';
 my $relation = shift || 'is';
 my $query = shift || '';
 return p({-class => 'link'},
  $bool ? popup_menu(-name => 'bool', -values => ['and', 'or'], -default =>
   $bool, -override => 1) : '',
  popup_menu(-name => 'field', -values => ['valsi', 'rafsi', 'glico', 'ralvla',
   'selvla'], -default => $field, -override => 1, -labels => {valsi => 'valsi',
   rafsi => 'at least one rafsi', glico => 'keyword or definition', ralvla =>
   'keyword', selvla => 'definition'}),
  popup_menu(-name => 'relation', -values => ['is', 'contains', 'regex'],
   -default => $relation, -override => 1, -labels => {regex =>
   'matches regex'}),
  textfield(-name => 'query', -value => $query, -override => 1, -size => 20,
   -maxlength => 128));
}

__END__

Features to add:
 - case-insensitivity
 - matching on word boundaries
 - grouping of boolean operators
 - searching by hints?
 - searching by notes
 - searching by {selma'o}
 - submitting via GET?

Other to-do:
 - Replace the flat files with a SQLite3 database (though Perl regex support
   will have to be dropped)
 - Add a <noscript> tag
 - In printValsi(), have {selma'o} be labeled as "selma'o" instead of
   "selmaho".
 - Add a note saying that any & all problems & suggestions should be e-mailed
   to me
