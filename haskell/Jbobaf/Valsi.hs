module Jbobaf.Valsi (
  -- * Types
  Vlalei(..), Valsi,
  -- * Fields
  valsi, klesi, selma'o, rafsi, ralvla, djuvla, selvla, notci,
  -- * Construction from strings
  toValsi, toCmavo, toCmevla, toBrivla, toGismu, toLujvo, toFu'ivla
 ) where
 import Char (toLower)
 import Ix
 import Monad (mplus)
 import Jbobaf.Canti (findC_C)
 import Jbobaf.Jitro
 import Jbobaf.Vlatai

 data Vlalei = Gismu | Lujvo | Fu'ivla | Cmavo {- | Lujma'o -} | Cmevla
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Ix)

 -- In the interest of keeping the Valsi type abstract and thus preventing
 -- users from messing with its values, the field names (which shall not be
 -- exported) shall all begin with "ck_" (for "{ckaji}"), and prefix-less
 -- accessors shall be created for each field.

 data Valsi = Vla1 {ck_valsi :: String, ck_klesi :: Vlalei}
  -- ck_valsi stores the normalized form of the word (converted to all
  -- lowercase for non-{cmevla}).
  | Vla2 {
   ck_valsi :: String,
   ck_klesi :: Vlalei,
   ck_selma'o, ck_ralvla, ck_djuvla, ck_selvla, ck_notci :: Maybe String,
   ck_rafsi :: [String]
  } deriving (Eq, Ord, Read, Show)

 valsi :: Valsi -> String
 valsi = ck_valsi

 klesi :: Valsi -> Vlalei
 klesi = ck_klesi

 selma'o, ralvla, djuvla, selvla, notci :: Valsi -> Maybe String
 selma'o (Vla1 {}) = Nothing
 selma'o (Vla2 {ck_selma'o = s}) = s
 ralvla (Vla1 {}) = Nothing
 ralvla (Vla2 {ck_ralvla = r}) = r
 djuvla (Vla1 {}) = Nothing
 djuvla (Vla2 {ck_djuvla = d}) = d
 selvla (Vla1 {}) = Nothing
 selvla (Vla2 {ck_selvla = s}) = s
 notci (Vla1 {}) = Nothing
 notci (Vla2 {ck_notci = n}) = n

 rafsi :: Valsi -> [String]
 rafsi (Vla1 {}) = []
 rafsi (Vla2 {ck_rafsi = r}) = r

 toValsi, toCmavo, toCmevla, toBrivla, toGismu, toLujvo, toFu'ivla
  :: String -> Jvacux Valsi

 toValsi [] = throwError "Empty strings are not {valsi}."
 toValsi str = fadgau str >>= \fadni ->
  if isC (last fadni) then toCmevla fadni
  else maybe (toCmavo fadni) (const $ toBrivla fadni) (findC_C fadni)

 toCmevla str = do
  f <- fadgau str
  cmevla_xusra' f
  return $ miniMake f Cmevla

 toCmavo str = do
  f <- fadgau str
  cmavo_xusra' f
  return $ miniMake (map toLower f) Cmavo

 toBrivla str = do
  f <- fadgau str
  kle <- (gismu_xusra' f >> return Gismu)
   `mplus` (lujvo_xusra' f >> return Lujvo)
   `mplus` (fu'ivla_xusra' f >> return Fu'ivla)
   `mplus` throwError "You can't make a non-{brivla} into a Brivla object!"
  return $ miniMake (map toLower f) kle

 toGismu str = do
  f <- fadgau str
  gismu_xusra' f
  return $ miniMake (map toLower f) Gismu

 toLujvo str = do
  f <- fadgau str
  lujvo_xusra' f
  return $ miniMake (map toLower f) Lujvo

 toFu'ivla str = do
  f <- fadgau str
  fu'ivla_xusra' f
  return $ miniMake (map toLower f) Fu'ivla

 miniMake str kle = Vla1 {ck_valsi = str, ck_klesi = kle}  -- not exported
