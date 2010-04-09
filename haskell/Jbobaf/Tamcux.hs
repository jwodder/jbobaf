-- |Options for the morphology functions and a Reader monad used to keep track
-- of them

module Jbobaf.Tamcux where
 import Ix
 import Data.Set (Set)
 import qualified Data.Set as Set

 data Tercuxna =
  Use_dotside
    -- ^Allow \"la,\" \"lai,\" \"la'i,\" and \"doi\" to appear freely inside
    -- /cmevla/ (which must then be preceded by a period or whitespace)
  | Allow_accents
    -- ^Vowels with acute accent marks will be allowed and implicitly converted
    -- to uppercase unaccented vowels (except for an accented \'y\', which will
    -- always be treated as lowercase).  Note that only precomposed characters
    -- (e.g., U+00E1, not U+0061+0301) are recognized; decomposed character
    -- sequences will be treated as an unaccented character followed by a
    -- separate combining character.
  | Allow_triphthongs
    -- ^Triphthongs (consisting of an \'i\' or a \'u\' followed by one of the
    -- four falling diphthongs) will be allowed in Lojban words.
  | LOhU_disables_ZO
    -- ^\"/zo/\" loses its magic word status inside \"/lo'u/ ... /le'u/\"
    -- quotes.
  | LOhU_disables_ZEI
    -- ^\"/zei/\" loses its magic word status inside \"/lo'u/ ... /le'u/\"
    -- quotes.
  | LOhU_disables_ZOI
    -- ^\"/zoi/\" loses its magic word status inside \"/lo'u/ ... /le'u/\"
    -- quotes.
  | LOhU_disables_FAhO
    -- ^\"/fa'o/\" loses its magic word status inside \"/lo'u/ ... /le'u/\"
    -- quotes.
  | Allow_Y_in_fu'ivla  -- ^The letteral \'y\' will be allowed in /fu'ivla/.
  | No_commas_in_cmavo  -- ^/Cmavo/ will not be allowed to contain commas.
  | Ignore_FAhO
    -- ^When performing word splitting, keep splitting even after a /fa'o/ is
    -- encountered (The /fa'o/ will still be returned in the list of words).
    -- When this option is not in effect, any text coming after a /fa'o/ will
    -- be returned as a 'String' at the end of the word list.
  | Ignore_naljbo_chars
    -- ^All non-space characters outside of the Lojban alphabet are ignored &
    -- removed (including accented vowels if 'Allow_accents' is not in effect
    -- and decimal digits if 'Translate_digits' is not in effect).
  | Split_bad_diphthongs
    -- ^Vowel clusters which do not form valid diphthongs (or triphthongs if
    -- 'Allow_triphthongs' is in effect) will have commas implicitly inserted
    -- in them as early & often as necessary.
  | Ignore_brivla_emphasis
    -- ^Ignore the presence or absence of capitalized/accented characters when
    -- determining whether a string is a valid /brivla/ or not.  This option
    -- has no effect on word splitting.
  | Allow_H  -- ^H's in Lojban text will be converted into apostrophes.
  | Allow_ndj_in_fu'ivla
    -- ^The consonant clusters \"ndj,\" \"ndz,\" \"ntc,\" and \"nts\" will be
    -- permitted within /fu'ivla/.
  | Allow_ndj_in_cmevla
    -- ^The consonant clusters \"ndj,\" \"ndz,\" \"ntc,\" and \"nts\" will be
    -- permitted within /cmevla/.
  | Translate_digits
    -- ^Decimal digits encountered in Lojban text will be replaced by the
    -- characters in the corresponding PA /cmavo/.  Note that no syllable or
    -- word boundaries are assumed for the purposes of this transformation, so
    -- \"@0i@\" becomes \"@noi@\", not \"@no,i@\" or \"@no.i@\"; however, if
    -- 'Split_bad_diphthongs' is in effect, \"@0e@\" will become \"@no,e@\".
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Ix)

 defaults :: Set Tercuxna
 defaults = Set.fromList [Use_dotside, Allow_accents, Ignore_naljbo_chars,
  Allow_triphthongs, Allow_H, Allow_ndj_in_fu'ivla, Allow_ndj_in_cmevla,
  No_commas_in_cmavo, Translate_digits]

 newtype Tamcux a = Tamcux {tamcuxna :: Set Tercuxna -> a}

 instance Monad Tamcux where
  return = Tamcux . const
  Tamcux f >>= g = Tamcux $ \opt -> tamcuxna (g $ f opt) opt

 isOpt, isNopt :: Tercuxna -> Tamcux Bool
 isOpt = Tamcux . Set.member
 isNopt = Tamcux . Set.notMember
