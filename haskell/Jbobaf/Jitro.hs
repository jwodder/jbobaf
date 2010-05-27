-- |Run-time configuration options and a combined Reader-Error monad for
-- keeping track of them

module Jbobaf.Jitro (
  -- * Jvacux type
  JvacuxT, Jvacux, isOpt, isNopt, xusnada, nupre, fliba, fliba', kavbu,
  -- ** Re-exported from Control.Monad.* 
  runReaderT, throwError, catchError,
  -- * Options
  Tercuxna(..), defaults,
  -- * Errors
  Srelei(..), Selsrera(..),
  -- ** Error messages
  sreski, sreskicu
 ) where
 import Array
 import List (intersperse)
 import Data.Set (Set, member, notMember, fromList)
 import Control.Monad.Error
 import Control.Monad.Reader

 type JvacuxT m a = ReaderT (Set Tercuxna) m a
 type Jvacux a = JvacuxT (Either Selsrera) a

 isOpt, isNopt :: Monad m => Tercuxna -> JvacuxT m Bool
 isOpt  = asks . member
 isNopt = asks . notMember

 xusnada :: Jvacux a -> Jvacux Bool
 xusnada m = (m >> return True) `mplus` return False

 nupre :: Jvacux a -> Set Tercuxna -> a
 nupre jct opts = either (error . sreskicu True) id $ runReaderT jct opts

 -- Are the following three functions really necessary and/or useful?
 fliba :: Selsrera -> Jvacux a
 fliba = throwError

 fliba' :: String -> Jvacux a
 fliba' = throwError . strMsg

 kavbu :: Jvacux a -> (Selsrera -> Jvacux a) -> Jvacux a
 kavbu = catchError

-----------------------------------------

 data Tercuxna =
  Use_dotside
    -- ^The strings \"la\", \"lai\", \"la'i\", and \"doi\" will be allowed to
    -- appear freely inside /cmevla/ (which must then always be preceded by a
    -- period or whitespace).
  | Allow_triphthongs
    -- ^Triphthongs (consisting of an \'i\' or a \'u\' followed by one of the
    -- four falling diphthongs) will be allowed in Lojban words.
  | No_commas_in_cmavo  -- ^/Cmavo/ will not be allowed to contain commas.
  | Ignore_brivla_emphasis
    -- ^The presence or absence of capitalized/accented syllables will be
    -- ignored when determining whether a string is a valid /brivla/ or not.
    -- This option has no effect on word splitting.
  | Allow_Y_in_fu'ivla  -- ^The letteral \'y\' will be allowed in /fu'ivla/.
  | Allow_ndj_in_fu'ivla
    -- ^The consonant clusters \"ndj,\" \"ndz,\" \"ntc,\" and \"nts\" will be
    -- permitted within /fu'ivla/.
  | Allow_ndj_in_cmevla
    -- ^The consonant clusters \"ndj,\" \"ndz,\" \"ntc,\" and \"nts\" will be
    -- permitted within /cmevla/.

  | Allow_accents
    -- ^'fadgau' will implicitly convert vowels with acute accent marks into
    -- uppercase unaccented vowels (except for an accented \'y\', which will
    -- always be treated as lowercase).  Note that only precomposed characters
    -- (e.g., U+00E1, not U+0061+0301) are recognized; decomposed character
    -- sequences will be treated as an unaccented character followed by a
    -- separate combining character.
  | Allow_H  -- ^'fadgau' will convert H's in Lojban text into apostrophes.
  | Ignore_naljbo_chars
    -- ^'fadgau' will remove any & all non-space characters outside of the
    -- Lojban alphabet, including accented vowels if 'Allow_accents' is not in
    -- effect and decimal digits if 'Translate_digits' is not in effect.
  | Translate_digits
    -- ^'fadgau' will replace any & all decimal digits in Lojban text with the
    -- characters for the corresponding PA /cmavo/.  Note that no syllable or
    -- word boundaries are assumed or imposed for the purposes of this
    -- transformation, so \"@0i@\" becomes \"@noi@\", not \"@no,i@\" or
    -- \"@no.i@\".  However, if 'Split_bad_diphthongs' is in effect, \"@0e@\"
    -- will become \"@no,e@\", and similarly for other bad vowel combinations.
  | Split_bad_diphthongs
    -- ^'fadgau' will break apart any invalid vowel clusters (including
    -- triphthongs when 'Allow_triphthongs' is not in effect) by inserting
    -- commas, using a \"maximal munch\" rule that favors leading
    -- diphthongs/triphthongs (so \"aui\" will become \"au,i\", not \"a,ui\").

  | LOhU_disables_ZO
    -- ^\"/zo/\" will lose its magic word status inside \"/lo'u/ ... /le'u/\"
    -- quotes.
  | LOhU_disables_ZEI
    -- ^\"/zei/\" will lose its magic word status inside \"/lo'u/ ... /le'u/\"
    -- quotes.
  | LOhU_disables_ZOI
    -- ^\"/zoi/\" will lose its magic word status inside \"/lo'u/ ... /le'u/\"
    -- quotes.
  | LOhU_disables_FAhO
    -- ^\"/fa'o/\" will lose its magic word status inside \"/lo'u/ ... /le'u/\"
    -- quotes.
  | Ignore_FAhO
    -- ^'lerfendi' will continue performing word splitting even after a /fa'o/
    -- is encountered (The /fa'o/ will still be returned in the list of words).
    -- When this option is not in effect, any text coming after a /fa'o/ will
    -- be returned as a 'String' at the end of the word list.
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Ix)

 defaults :: Set Tercuxna
 defaults = fromList [Use_dotside, Allow_accents, Ignore_naljbo_chars,
  Allow_triphthongs, Allow_H, Allow_ndj_in_fu'ivla, Allow_ndj_in_cmevla,
  No_commas_in_cmavo, Translate_digits, Split_bad_diphthongs]

---------------------------------------

 data Srelei =
  SRE_internal_error  -- something that is not supposed to happen
  | SRE_invalid_word_form  -- generic morphological failure
  | SRE_empty_string
  | SRE_invalid_emphasis
  | SRE_bad_consonant_pair
  | SRE_bad_consonant_triple
  | SRE_tosmabru_failure
  | SRE_slinku'i_failure
  | SRE_bad_vowel_sequence  -- bad diphthong, triphthong, etc.
  | SRE_no_spaces_allowed  -- internal spaces/periods not allowed
  | SRE_lacks_cluster  -- consonant cluster absent from {fu'ivla}
  | SRE_non_Lojban_char  -- non-Lojbanic character in string
  | SRE_misplaced_apostrophe
  | SRE_no_commas_allowed
  | SRE_no_Ys_allowed  -- applies only to {fu'ivla}?
  | SRE_na'e_fu'ivla   -- proposed {fu'ivla} is actually a {gismu} or {lujvo}
  | SRE_bad_rn_hyphen  -- includes superfluous r/n-hyphens
  | SRE_missing_rn_hyphen
  | SRE_too_much_before_cluster
    -- two many letters or {ma'osmi} before a consonant cluster in a {brivla}
  | SRE_extra_Y_hyphen
    -- sre_valsi !! 2 == the normalized portion of the {lujvo} up through the Y
    -- sre_valsi !! 3 == the normalized portion of the {lujvo} after the Y
  | SRE_invalid_rafsi
    -- sre_valsi !! 2 == the {lujvo} up through the end of the bad {rafsi}
    -- sre_valsi !! 3 == the {lujvo} after the bad {rafsi}
  | SRE_la_in_cmevla  -- when the dotside is not in effect
  | SRE_not_enough_rafsi
  | SRE_not_enough_syllables  -- vocalic syllables, that is
  | SRE_must_end_with_vowel
  | SRE_must_end_with_consonant
  | SRE_breaks_apart  -- into smaller words
  | SRE_non_initial_start  -- begins with non-initial consonant cluster
  | SRE_consonant_inside_cmavo  -- includes single consonants as {cmavo}
  | SRE_other_error
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Ix)

 data Selsrera = Selsrera {
   sre_velski :: [String],
   -- description of the error; first element is usually the name of the
   -- function that threw it, second element (if present) is the erroneous
   -- argument to the function, third element (if present) is the problematic
   -- substring of the second element
   sre_klesi :: Srelei
  } deriving (Eq, Ord, Read, Show)

 instance Error Selsrera where
  noMsg    = Selsrera ["noMsg"]     SRE_other_error
  strMsg s = Selsrera ["strMsg", s] SRE_other_error

 sreski :: Array Srelei (String, String)
 sreski = array (minBound, maxBound) [
  (SRE_internal_error,
   ("Internal error (this is not supposed to happen)",
    "canti pe'a selsre sei ba'a na fasnu")),  -- Rethink this translation.
  (SRE_invalid_word_form, ("Invalid word form", "naldra vlatai")),
  (SRE_empty_string, ("An empty string is not a word.", ".e'anai nonporsi")),
  (SRE_invalid_emphasis,
   ("Invalid {brivla} emphasis",
    "naldra brivla basnymo'a")),
  (SRE_bad_consonant_pair,
   ("Invalid consonant pair",
    "naldra zunsna bo remei")),
  (SRE_bad_consonant_triple,
   ("Invalid consonant triple",
    "naldra zunsna bo cimei")),
  (SRE_tosmabru_failure,
   ("{lujvo} missing tosmabru hyphen",
    "le lujvo cu claxu me'o .ybu noi sarcu fi tu'a la'o gy. tosmabru .gy.")),
  (SRE_slinku'i_failure,
   ("{fu'ivla} may not fail the slinku'i test.",
    ".e'anai lo fu'ivla cu fliba lo cipra pe la'o gy. slinku'i .gy.")),
  (SRE_bad_vowel_sequence,
   ("Invalid vowel sequence detected",
    "naldra voksna bo porsi")),
  (SRE_no_spaces_allowed,
   ("{valsi} may not have internal spaces or periods.",
    ".e'anai lo valsi cu se nenri me'o tersei bu .a denpa bu")),
    -- Check the translation of "space."
  (SRE_lacks_cluster,
   ("{brivla} must contain a consonant cluster.",
    ".ei lo brivla cu vasru lo zunsna porsi")),
  (SRE_non_Lojban_char,
   ("Non-Lojbanic character in string",
    ".e'anai naljbo lerfu")),
  (SRE_misplaced_apostrophe,
   ("Apostrophe found in an invalid location",
    "me'o .y'y. naldra zvati")),
  (SRE_no_commas_allowed,
   ("Commas are prohibited.",
    "me'o slaka bu jai se tolcru")),
  (SRE_no_Ys_allowed,
   ("{fu'ivla} may not contain Y's.",
    ".e'anai lo fu'ivla cu vasru me'o .ybu")),
  (SRE_na'e_fu'ivla,
   ("{fu'ivla} may not be {gismu} or {lujvo}.",
    ".e'anai lo fu'ivla cu gismu ja lujvo")),
  (SRE_bad_rn_hyphen,
   ("Invalid r/n-hyphen in {lujvo}",
    "le terjo'e pe me'o ry. .a ny. naldra fi le lujvo")),
  (SRE_missing_rn_hyphen,
   ("R/n-hyphen missing from {lujvo}",
    "lo lujvo cu claxu lo sarcu terjo'e pe me'o ry. .a ny.")),
  (SRE_too_much_before_cluster,
   ("Too much before consonant cluster in {brivla}",
    "lo dukse cu lidne lo zunsna porsi ne'i lo brivla")),
  (SRE_extra_Y_hyphen,
   ("Superfluous Y-hyphen in {lujvo}",
    "dukse me'o .ybu ne'i lo lujvo")),
  (SRE_invalid_rafsi, ("Invalid {rafsi} form", "naldra raftai")),
  (SRE_la_in_cmevla,
   ("{cmevla} may not contain \"la\", \"lai\", \"la'i\", or \"doi\".",
    ".e'anai lo cmevla cu vasru zo la .a zo lai .a zo la'i .a zo doi")),
  (SRE_not_enough_rafsi,
   ("{lujvo} must contain at least two {rafsi}.",
    ".ei lo lujvo cu vasru su'o re rafsi")),
  (SRE_not_enough_syllables,
   ("{brivla} must contain two or more vocalic syllables.",
    ".ei lo brivla cu vasru su'o re voksna slaka")),
  (SRE_must_end_with_vowel,
   ("{brivla} must end with a vowel.",
    ".ei lo brivla cu se fanmo lo voksna")),
  (SRE_must_end_with_consonant,
   ("{cmevla} must end with a consonant.",
    ".ei lo cmevla cu se fanmo lo zunsna")),
  (SRE_breaks_apart,
   ("{valsi} may not break apart into smaller words.",
    ".e'anai lo valsi cu porpi lo cmalu valsi")),
  (SRE_non_initial_start,
   ("Non-initial consonant cluster at beginning of string",
    ".e'anai lo na'e lidne zunsna bo porsi cu krasi")),
  (SRE_consonant_inside_cmavo,
   ("{cmavo} may not contain internal or trailing consonants.",
    ".e'anai lo cmavo cu se nenri ja se fanmo lo zunsna")),
  (SRE_other_error, ("Unknown error", "fange selsre"))]

 sreskicu :: Bool -> Selsrera -> String
 sreskicu jbo sre = concat (intersperse ": " $ sre_velski sre) ++ ": "
  ++ (if jbo then snd else fst) (sreski ! (sre_klesi sre))
