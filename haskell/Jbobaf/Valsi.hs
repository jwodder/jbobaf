module Jbobaf.Valsi (
  -- * Types
  Vlalei(..), Valsi,
  -- * Fields
  valsi, klesi, selma'o, rafsi, ralvla, djuvla, selvla, notci,
  -- * Conversion from strings
  toValsi, toCmavo, toCmevla, toBrivla, toGismu, toLujvo, toFu'ivla
 ) where
 import Char (toLower)
 import Ix
 import Jbobaf.Internals
 import Jbobaf.Tamcux (Tamcux)
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
  :: String -> Tamcux (Maybe Valsi)

 toValsi [] = return Nothing
 toValsi str = if isC (last str) then toCmevla str
	       else maybe (toCmavo str) (const $ toBrivla str) (findCC str)

 toCmevla str = fadgau str >>= \fadni -> case fadni of
  Just f -> xucmevla' f >>= return . (?: Just (miniMake f Cmevla) :? Nothing)
  Nothing -> return Nothing

 toCmavo str = fadgau str >>= \fadni -> case fadni of
  Just f -> xucmavo' f >>= return . (?: Just (miniMake (map toLower f) Cmavo)
   :? Nothing)
  Nothing -> return Nothing

 toBrivla str = fadgau str >>= \fadni -> case fadni of
  Just f -> do gim <- xugismu' f
	       luj <- xulujvo' f
	       fui <- xufu'ivla' f
	       return $ if gim then Just $ miniMake (map toLower f) Gismu
			else if luj then Just $ miniMake (map toLower f) Lujvo
			else if fui then Just $ miniMake (map toLower f) Fu'ivla
			else Nothing
  Nothing -> return Nothing

 toGismu str = fadgau str >>= \fadni -> case fadni of
  Just f -> xugismu' f >>= return . (?: Just (miniMake (map toLower f) Gismu)
   :? Nothing)
  Nothing -> return Nothing

 toLujvo str = fadgau str >>= \fadni -> case fadni of
  Just f -> xulujvo' f >>= return . (?: Just (miniMake (map toLower f) Lujvo)
   :? Nothing)
  Nothing -> return Nothing

 toFu'ivla str = fadgau str >>= \fadni -> case fadni of
  Just f -> xufu'ivla' f >>=
   return . (?: Just (miniMake (map toLower f) Fu'ivla) :? Nothing)
  Nothing -> return Nothing

 miniMake str kle = Vla1 {ck_valsi = str, ck_klesi = kle}  -- not exported
