module Jbobaf.Valsi where
 import Char (toLower)
 import Ix

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
   ck_rafsi :: [String],
  } deriving (Eq, Ord, Read, Show)

 -- Computable properties: termre, krarafsi, veljvo

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

 toValsi :: String -> Tamcux (Maybe Valsi)
 toValsi str = ?????

 toCmavo, toCmevla, toBrivla :: String -> Tamcux (Maybe Valsi)
 toGismu, toLujvo, toFu'ivla :: String -> Tamcux (Maybe Valsi)

 toCmevla str = do
  fadni <- fadgau str
  drani <- xucmevla' fadni
  return if drani then Just (miniMake fadni Cmevla) else Nothing

 toCmavo str = do
  fadni <- fadgau str
  drani <- xucmavo' fadni
  return if drani then Just $ miniMake (map toLower fadni) Cmavo
	 else Nothing

 toBrivla str = do
  fadni <- fadgau str
  gim <- xugismu' fadni
  luj <- xulujvo' fadni
  fu'i <- xufu'ivla' fadni
  return if gim then Just $ miniMake (map toLower fadni) Gismu
	 else if luj then Just $ miniMake (map toLower fadni) Lujvo
	 else if fu'i then Just $ miniMake (map toLower fadni) Fu'ivla
	 else Nothing

 toGismu str = do
  fadni <- fadgau str
  drani <- xugismu' fadni
  return if drani then Just $ miniMake (map toLower fadni) Gismu
	 else Nothing

 toLujvo str = do
  fadni <- fadgau str
  drani <- xulujvo' fadni
  return if drani then Just $ miniMake (map toLower fadni) Lujvo
	 else Nothing

 toFu'ivla str = do
  fadni <- fadgau str
  drani <- xufu'ivla' fadni
  return if drani then Just $ miniMake (map toLower fadni) Fu'ivla
	 else Nothing

 miniMake str type = Vla1 {ck_valsi = str, ck_klesi = type}  -- not exported
