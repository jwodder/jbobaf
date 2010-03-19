class Lojban::Valsi {
 has Str $.valsi;
 has Str $.selma'o;
 # The {selma'o} accessor should be modified so that it takes an optional
 # :jicmu adverb that indicates that the base form of the {selma'o} should be
 # returned.
 has Str @.rafsi;
 has Str $.ralvla;
 has Str $.djuvla;
 has Str $.selvla;
 has Str $.notci;

 # The below attempts at caching/delayed execution may or may not work right.

 our enum Vlalei < gismu cmavo lujma'o cmene lujvo fu'ivla >;

 has Vlalei $.klesi = lazy { Lojban::Vlatai::vlalei $.valsi };

 has Str @.krarafsi = lazy {
  $.klesi ~~ Vlalei::lujvo ?? Lojban::Vlatai::jvokatna $.valsi !! Nil
 };

 # Implement this one:
 has Str | Lojban::Valsi @.veljvo = lazy { !!! };

 has Int $.termre = lazy {
  @.krarafsi ?? Lojban::Vlatai::jvomre($.valsi, @.krarafsi) !! undef
 };

 has Bool $.xuvladra = lazy { Lojban::Vlatai::vlalei($.valsi) eq $.klesi };
 # This ^^ needs to make sure the {klesi} returned are not undef.
 # &Lojban::Vlatai::vlalei should be memoized or whatever in order to avoid
 # computing the {klesi} twice.


 # Implement the various methods....

 # Create a constructor which requires a {valsi} field and prohibits a
 # {xuvladra} field.


 method Str { $.valsi }  # I think this is right.

 method fadni( --> Lojban::Valsi) {
  # This method returns a new Valsi with a normalized $.valsi field.  If the
  # user wishes to mutate the invocant, ey simply calls "$valsi.=fadni".

  self.clone: valsi => Lojban::Vlatai::fadygau($.valsi)
  # This needs to handle @.krafsi, @.veljvo, and other $.valsi-based attributes
  # somehow.
 }

}
