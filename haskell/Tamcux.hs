-- |Options for the morphology functions and a Reader monad used to keep track
-- of them

module Jbobaf.Tamcux where
 import Ix
 import Data.Set (Set)
 import qualified Data.Set as Set

 data FendiOption =
  Use_dotside
    -- ^Allow "la," "lai," "la'i," and "doi" to appear freely inside /cmevla/
    -- (which must then be preceded by a period or whitespace)
  | Allow_Y_in_fu'ivla
  | Allow_accents
  | Allow_triphthongs
  | LOhU_disables_ZO
  | LOhU_disables_ZEI
  | LOhU_disables_ZOI
  | LOhU_disables_FAhO
  | Ignore_FAhO
    -- ^When performing word splitting, keep splitting even after a /fa'o/ is
    -- encountered (The /fa'o/ will still be returned in the list of words).
    -- When this option is not in effect, any text coming after a /fa'o/ will
    -- be returned as a 'String' at the end of the word list.
  | Ignore_naljbo_chars
    -- ^Ignore & remove all non-space characters outside of the Lojban alphabet
    -- (including accented vowels if 'Allow_accents' is not in effect)
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
    -- ^The consonant clusters "ndj," "ndz," "ntc," and "nts" will be permitted
    -- within /fu'ivla/.
  | Allow_ndj_in_cmevla
    -- ^The consonant clusters "ndj," "ndz," "ntc," and "nts" will be permitted
    -- within /cmevla/.
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Ix)

 defaults :: Set FendiOption
 defaults = Set.fromList [Use_dotside, Allow_accents, Ignore_naljbo_chars,
  Allow_triphthongs, Allow_H, Allow_ndj_in_fu'ivla, Allow_ndj_in_cmevla]

 newtype Tamcux a = Tamcux {tamcuxna :: Set FendiOption -> a}

 instance Monad Tamcux where
  return = Tamcux . const
  Tamcux f >>= g = Tamcux $ \opt -> tamcuxna (g $ f opt) opt

 isOpt, isNopt :: FendiOption -> Tamcux Bool
 isOpt = Tamcux . Set.member
 isNopt = Tamcux . Set.notMember
