module Jbobaf.Valsi (
  -- * Types
  Vlalei(..), Valsi(..), kunti_valsi,
  -- * Construction from strings
  toValsi, toCmavo, toCmevla, toBrivla, toGismu, toLujvo, toFu'ivla
 ) where
 import Control.Monad (mplus)
 import Data.Char (toLower)
 import Data.Ix
 import Jbobaf.Canti (findC_C)
 import Jbobaf.Jitro
 import Jbobaf.Vlatai

 data Vlalei = Gismu | Lujvo | Fu'ivla | Cmavo | {- Lujma'o | -} Cmevla
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Ix)

 data Valsi = Valsi {
   valsi :: String,
   -- ck_valsi stores the normalized form of the word (converted to all
   -- lowercase for non-{cmevla}).
   klesi :: Vlalei,
   selma'o, ralvla, djuvla, selvla, notci :: Maybe String,
   rafsi :: [String]
  } deriving (Eq, Ord, Read, Show)

 kunti_valsi :: Valsi
 kunti_valsi = Valsi "" Gismu Nothing Nothing Nothing Nothing Nothing []

 toValsi, toCmavo, toCmevla, toBrivla, toGismu, toLujvo, toFu'ivla
  :: String -> Jvacux Valsi

 toValsi [] = throwError $ Selsrera ["toValsi"] SRE_empty_string
 toValsi str = fadgau str >>= \fad -> if isC (last fad) then toCmevla fad
				      else maybe (toCmavo fad)
					    (const $ toBrivla fad) (findC_C fad)

 toCmevla str = do f <- fadgau str
		   cmevla_xusra' f
		   return $ miniMake f Cmevla

 toCmavo str = do f <- fadgau str
		  cmavo_xusra' f
		  return $ miniMake (map toLower f) Cmavo

 toBrivla str = do
  f <- fadgau str
  kle <- (gismu_xusra' f >> return Gismu)
   `mplus` (lujvo_xusra' f >> return Lujvo)
   `mplus` (fu'ivla_xusra' f >> return Fu'ivla)
   --`mplus` throwError (Selsrera ["toBrivla", str] SRE_invalid_word_form)
  return $ miniMake (map toLower f) kle

 toGismu str = do f <- fadgau str
		  gismu_xusra' f
		  return $ miniMake (map toLower f) Gismu

 toLujvo str = do f <- fadgau str
		  lujvo_xusra' f
		  return $ miniMake (map toLower f) Lujvo

 toFu'ivla str = do f <- fadgau str
		    fu'ivla_xusra' f
		    return $ miniMake (map toLower f) Fu'ivla

 miniMake :: String -> Vlalei -> Valsi
 miniMake str kle = kunti_valsi {valsi = str, klesi = kle}  -- not exported
