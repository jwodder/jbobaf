=== jboval - v.1.1 - README.txt ===

0. CONTENTS

0. Contents
1. About
2. File List
3. Installation
4. Documentation
5. Version History


1. ABOUT

jboval is a small set of Perl modules & scripts for handling Lojban words,
primarily in finding entries in the official {gismu} & {cmavo} lists and
creating & splitting {lujvo}.  Besides the Perl modules, the package contains a
program for searching the word lists and another one for forming {lujvo} out of
{tanru}.  The official {gismu} & {cmavo} lists themselves are also included for
convenience.  See section 3 of this README for installation instructions.

jboval was written by John T. "kamymecraijun." Wodder II
<minimiscience@gmail.com> and is distributed under a "do whatever the {bais.}
you want with this" license.  The latest "official" version of this package can
be found at <http://jwodder.freeshell.org/downloads/jboval.tgz>.

IMPORTANT: As the code uses object-oriented Perl, if you are using Red Hat
Linux or one of its derivatives, you MUST read this before going any further:

   <http://blog.vipul.net/2008/08/24/redhat-perl-what-a-tragedy/>


2. FILE LIST

Lojban/ - directory containing the Lojban Perl modules
 Lojban/Valsi.pm - class for Lojban::Valsi objects
 Lojban/Vlasisku.pm - functions for searching the word lists
 Lojban/Vlatai.pm - a few simple morphology routines
Makefile - file for automatically generating the manpages for `sisku` and
    `jvozba`; of no interest to most people
README.txt - this file
TODO.txt - list of things that should be done or could be done; only of
    interest if you plan to rewrite any of the code
cmavo.txt - the official {cmavo} list
gismu.txt - the official {gismu} list
jvozba.1 - the manpage for `jvozba`
jvozba.pl - a program for creating {lujvo} out of {tanru}
sisku.1 - the manpage for `sisku`
sisku.pl - a program for searching the word lists for {valsi}


3. INSTALLATION

Obviously, you will need Perl installed on your system in order for jboval to
work.  The code was written for v.5.8.8 and hopefully should work with any
version of Perl close to that.  Also, if the Perl on your system is not located
at /usr/bin/perl, you will need to change the first line of sisku.pl and
jvozba.pl to use the correct path.  If you encounter any compatibility
problems, please let me know.

First, you need to copy gismu.txt and cmavo.txt to a directory somewhere on
your system.  By default, the Perl modules assume that the word lists are
located in the `share' directory in the user's home directory, though this
setting can be changed (see below).  If you already have a copy of the official
word lists, you may use those instead; if you have an unofficial copy, it MUST
be formatted in the same way as the official lists.

If you choose to install the word lists in a different directory than ~/share,
you must edit Lojban/Vlasisku.pm so that it uses the new paths.  The path for
the {gismu} list is assigned to `$gimste' on line 14, and the {cmavo} list's
path is assigned to `$mahoste' on line 15.  The paths should be absolute paths
enclosed in double quotes, and the user's home directory should be represented
by "$ENV{HOME}".  Also, if your copy of the lists uses a CRLF sequence for its
line terminators (the copies distributed with this package do not, but the ones
available on lojban.org do), set `$dosyste' to 1 on line 16.

You must now copy the whole of the Lojban/ directory into one of the locations
that Perl will search for modules; the complete list of such locations for your
system can be seen by running the command:

    $ perl -le 'print for @INC'

Choose one of these directories (other than the current directory) and copy
Lojban/ (including the Lojban/ directory itself) into it, e.g.:

    $ cp -R Lojban /usr/local/lib/perl

...assuming that /usr/local/lib/perl exists and is one of the directories that
Perl will search for modules.

Finally, copy sisku.pl and jvozba.pl into a directory in your PATH (you may
drop the ".pl" extensions if you wish), and copy sisku.1 and jvozba.1 into the
man1/ subdirectory of a directory in your MANPATH.  If you don't know what that
means, Google it; I'm not helping you.


4. DOCUMENTATION

If everything is installed correctly, you can view the documentation for
sisku.pl and jvozba.pl by typing `man sisku` and `man jvozba`, respectively.
If you wish to see the documentation for one of the modules (which is only
useful if you're programming with them), type `perldoc Lojban::Valsi`, `perldoc
Lojban::Vlasisku`, or `perldoc Lojban::Vlatai` as appropriate.


5. VERSION HISTORY

7 Dec 2008 - v.1.0 - initial release
7 Feb 2009 - v.1.1:
 - The package is renamed from "Vlasisku" to "jboval."
 - Lojban::Vlatai::terjvo() now returns Lojban::Valsi objects instead of
   strings.
 - Lojban::Vlatai now makes a number of basic morphology regexes available for
   export.
 - Lojban::Vlatai::rafyjongau() (and thus also jvozba.pl) now inserts hyphens
   between {rafsi} that would create impermissible consonant triples.
 - jvozba.pl can now handle compound {cmavo} on the command line.
 - jvozba.pl now converts H's in command-line arguments to apostrophes.


$Id$
