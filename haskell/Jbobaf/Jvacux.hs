-- |Run-time configuration options and a Reader monad for keeping track of them

module Jbobaf.Jvacux (module Jbobaf.Jvacux, runReaderT, throwError) where
 import Ix
 import Data.Set (Set)
 import Control.Monad.Error
 import Control.Monad.Identity
 import Control.Monad.Reader
 import qualified Data.Set as Set

 type JvacuxT m a = ReaderT (Set Tercuxna) m a
 type Jvacux a = ReaderT (Set Tercuxna) Identity a
 type Jvacuxtoi a = ReaderT (Set Tercuxna) (Either String) a

 isOpt, isNopt :: Monad m => Tercuxna -> JvacuxT m Bool
 isOpt = asks . Set.member
 isNopt = asks . Set.notMember

 nupre :: Jvacuxtoi a -> Jvacux a
 nupre = mapReaderT (\(Right a) -> return a)

 troci :: Jvacuxtoi a -> Jvacux (Either String a)
 troci = mapReaderT (return . id)

 kavbu :: Jvacuxtoi a -> (String -> Jvacux a) -> Jvacux a
 kavbu jct f = ask >>= either f return . runReaderT jct

 snada :: Jvacux a -> Jvacuxtoi a
 snada = mapReaderT (return . runIdentity)

 -- fliba :: String -> Jvacuxtoi a  -- Is this function necessary/useful?
 -- fliba = throwError

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
 defaults = Set.fromList [Use_dotside, Allow_accents, Ignore_naljbo_chars,
  Allow_triphthongs, Allow_H, Allow_ndj_in_fu'ivla, Allow_ndj_in_cmevla,
  No_commas_in_cmavo, Translate_digits, Split_bad_diphthongs]
