Brief yet important notes on setting these CGI scripts up at your own site:
 - Both jvozba.cgi and vlasisku.cgi depend on jbobaf.css.
 - vlasisku.cgi depends on sisydju.js.
 - The Lojban/ modules need to be installed somewhere that Perl will find them
   (i.e., in one of the directories listed in @INC).  If a non-@INC directory
   is used, you will need to add a switch of the form "-I/path/to/modules" to
   the #! line at the top of the scripts.
 - At time of writing, these CGI scripts are intended for use with the Lojban
   modules in jboval-1.1.  If more recent modules are used, the behavior is
   undefined.
