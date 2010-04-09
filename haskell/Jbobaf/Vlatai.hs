module Jbobaf.Vlatai (
  -- * Basic character sequences
  isV, isVy, isC, isCC, isC_C, lidne, fadni,
  -- * Word classification & validity
  xubrivla, xugismu, xulujvo, xufu'ivla, xucmevla, xucmavo,
  -- ** Pre-normalized
  xubrivla', xugismu', xulujvo', xufu'ivla', xucmevla', xucmavo',
  -- * Normalization
  fadgau,
  -- * /Lujvo/ manipulation
  jvokatna, jvokatna', Raftai(..), raftai
 ) where
 import Char
 import Ix
 import List (findIndices)
 import qualified Data.Set as Set
 import Jbobaf.Internals
 import Jbobaf.Jvacux

 isV, isVy, isC :: Char -> Bool
 isV = (`elem` "aeiou") . toLower
 isVy = (`elem` "aeiouy") . toLower
 isC = (`elem` "bcdfgjklmnprstvxz") . toLower

 isCC, isC_C :: String -> Bool
 isCC = (`Set.member` lidne) . map toLower
 isC_C cc = Set.member cc' fadni || Set.member cc' lidne
  where cc' = map toLower cc

 lidne, fadni :: Set.Set String
 lidne = Set.fromAscList ["bl", "br", "cf", "ck", "cl", "cm", "cn", "cp", "cr",
  "ct", "dj", "dr", "dz", "fl", "fr", "gl", "gr", "jb", "jd", "jg", "jm", "jv",
  "kl", "kr", "ml", "mr", "pl", "pr", "sf", "sk", "sl", "sm", "sn", "sp", "sr",
  "st", "tc", "tr", "ts", "vl", "vr", "xl", "xr", "zb", "zd", "zg", "zm", "zv"]

 fadni = Set.fromAscList ["bd", "bg", "bj", "bm", "bn", "bv", "bz", "db", "dg",
  "dl", "dm", "dn", "dv", "fc", "fk", "fm", "fn", "fp", "fs", "ft", "fx", "gb",
  "gd", "gj", "gm", "gn", "gv", "gz", "jl", "jn", "jr", "kc", "kf", "km", "kn",
  "kp", "ks", "kt", "lb", "lc", "ld", "lf", "lg", "lj", "lk", "lm", "ln", "lp",
  "lr", "ls", "lt", "lv", "lx", "lz", "mb", "mc", "md", "mf", "mg", "mj", "mk",
  "mn", "mp", "ms", "mt", "mv", "mx", "nb", "nc", "nd", "nf", "ng", "nj", "nk",
  "nl", "nm", "np", "nr", "ns", "nt", "nv", "nx", "nz", "pc", "pf", "pk", "pm",
  "pn", "ps", "pt", "px", "rb", "rc", "rd", "rf", "rg", "rj", "rk", "rl", "rm",
  "rn", "rp", "rs", "rt", "rv", "rx", "rz", "sx", "tf", "tk", "tl", "tm", "tn",
  "tp", "tx", "vb", "vd", "vg", "vj", "vm", "vn", "vz", "xf", "xm", "xn", "xp",
  "xs", "xt", "zl", "zn", "zr"]

 xubrivla, xugismu, xulujvo, xufu'ivla, xucmevla, xucmavo,
  xubrivla', xugismu', xulujvo', xufu'ivla', xucmevla', xucmavo'
   :: String -> Jvacux Bool
  -- The "prime" forms of the xu* functions assume that their arguments are
  -- already normalized.  The non-prime functions do not.

 xubrivla str = fadgau str >>= maybe (return False) xubrivla'
 xubrivla' str = do
  gismu <- xugismu' str
  lujvo <- xulujvo' str
  fu'ivla <- xufu'ivla' str
  return $ gismu || lujvo || fu'ivla
 
 xugismu str = fadgau str >>= maybe (return False) xugismu'
 xugismu' [a, b, c, d, e] = do
  noemph <- isOpt Ignore_brivla_emphasis
  return $ isC a && isC d && isV e && (isV b && isC c && isC_C [c, d]
   || isC b && isV c && isCC [a, b]) && (noemph || not (isUpper e))
 xugismu' _ = return False

 xulujvo str = fadgau str >>= maybe (return False) xulujvo'
 xulujvo' str = do
  noemph <- isOpt Ignore_brivla_emphasis
  rafsi <- jvokatna' str
  let sylls = syllabicate str
      emphQty = length $ filter (not . null . filter isUpper) sylls
  return $ not (null rafsi) && (noemph || emphQty == 0 || emphQty == 1
   && not (null $ filter isUpper $ last $ init $ filter voc sylls))

 xufu'ivla str = fadgau str >>= maybe (return False) xufu'ivla'
 xufu'ivla' str = do
  noemph <- isOpt Ignore_brivla_emphasis
  canY <- isOpt Allow_Y_in_fu'ivla
  ndj <- isOpt Allow_ndj_in_fu'ivla
  xugim <- xugismu' str
  xuluj <- xulujvo' str
  toluj <- xulujvo' $ 't':'o':str
  let vocSyls = filter voc $ syllabicate str
      emphQty = length $ filter (not . null . filter isUpper) vocSyls
  if not (null str) && notElem ' ' str && isV (last str) && noBadCC str
   && (ndj || not (hasNDJ str)) && (canY || notElem 'y' str)
   && length vocSyls >= 2 && (noemph || emphQty == 0
    || emphQty == 1 && not (null $ filter isUpper $ last $ init vocSyls))
   && not xugim && not xuluj && not (isC (head str) && toluj)
   then case findCC str of
	 Just ccLoc -> do
	  let (clust, rest) = span (\c -> isC c || c == 'y') (drop ccLoc str)
	      preclust = take ccLoc str
	      preCs = length $ filter isC preclust
	  slinky <- xulujvo' $ 't':'o':drop ccLoc str
	  if elem 'y' clust || has_C_C clust
	      || length (filter voc $ syllabicate rest) == 1
	      || ccLoc /= 0 && slinky
	   then return $ ccLoc /= 0
		 && length (filter (`notElem` "',y") preclust) <= 3
		 && (preCs == 1 && isC (head preclust) || preCs == 0)
	   else return (ccLoc == 0)
	 Nothing -> return False
   else return False

 xucmevla str = fadgau str >>= maybe (return False) xucmevla'
 xucmevla' [] = return False
 xucmevla' str = do
  dotty <- isOpt Use_dotside
  ndj <- isOpt Allow_ndj_in_cmevla
  return $ isC (last str) && notElem ' ' str && noBadCC str
	   && case (dotty, findLa str, ndj, hasNDJ str) of
	       (False, Just _, _, _) -> False
	       (_, _, False, True) -> False
	       _ -> True

 xucmavo str = fadgau str >>= maybe (return False) xucmavo'
 xucmavo' [] = return False
 xucmavo' str@(c:xs) = do
  commas <- isNopt No_commas_in_cmavo
  let maho = if isC c then xs else str
  return $ not (null maho) && null (filter (\c -> isSpace c || isC c) maho)
   && (commas || notElem ',' maho)

 -- |@fadgau@ is a basic "cleanup" routine used by various functions in Jbobaf
 -- for converting Lojban strings into a more regular, "normalized" form.  It
 -- performs the following operations:
 --
 -- * Leading & trailing whitespace, periods, and commas are removed.
 --
 -- * Internal strings of whitespace and/or periods (along with any adjacent
 --   commas) are converted into a single space each.
 --
 -- * If 'Ignore_naljbo_chars' is in effect, any non-Lojbanic characters
 --   (including accented vowels and H's if 'Allow_accents' and 'Allow_H',
 --   respectively, are not in effect) are removed.  Otherwise, if the string
 --   does contain any non-Lojban characters, 'Nothing' is returned, indicating
 --   invalid input.
 --
 -- * If 'Allow_accents' is in effect, vowels with acute accents are converted
 --   to uppercase ASCII letters (except for accented Y's, which are converted
 --   to lowercase).
 --
 -- * If 'Allow_H' is in effect, H's are converted to apostrophes.
 --
 -- * Consecutive commas are merged together.
 --
 -- * All consonants and Y's are converted to lowercase.
 --
 -- * Any occurrences of the right single quotation mark (\'’\', U+2019) are
 --   converted to apostrophes.
 --
 -- TO IMPLEMENT:
 --
 -- * Commas not between two vowels are removed.  (Currently, only commas next
 --   to apostrophes are removed.)
 --
 -- * Commas are inserted in invalid vowel clusters if 'Split_bad_diphthongs'
 --   is in effect; otherwise, an invalid vowel cluster causes 'Nothing' to be
 --   returned.
 --
 -- * Digits are converted to their corresponding /cmavo/.
 --
 -- * Consecutive apostrophes are merged together?
 --
 -- * If an apostrophe is found in an invalid location (i.e., next to a
 --   consonant or at the the beginning or end of the string), 'Nothing' is
 --   returned.  (Currently, only apostrophes at the end of the string are
 --   detected.)
 --
 -- * If an invalid consonant pair is detected, 'Nothing' is returned?
 --
 -- * H's and ’'s need to have the same effects on their surroundings as
 --   apostrophes.

 fadgau :: String -> Jvacux (Maybe String)
 fadgau str = do
  accents <- isOpt Allow_accents
  ignoring <- isOpt Ignore_naljbo_chars
  hasH <- isOpt Allow_H
  digits <- isOpt Translate_digits
  let goodchr c = elem (toLower c) "',.abcdefgijklmnoprstuvxyz’"
		   || accents && elem (toLower c) "áéíóúý"
		   || hasH && toLower c == 'h'
		   || digits && isDigit c
      lerfad "'" = Nothing
      lerfad ('\'':',':xs) = lerfad ('\'':xs)
      lerfad "," = Just []
      lerfad (',':'\'':xs) = lerfad ('\'':xs)
      lerfad (',':',':xs) = lerfad (',':xs)
      lerfad (',':'.':xs) = lerfad (' ':xs)
      lerfad (',':c:xs) | isSpace c = lerfad (c:xs)
      lerfad (' ':'.':xs) = lerfad (' ':xs)
      lerfad (' ':',':xs) = lerfad (' ':xs)
      lerfad (' ':c:xs) | isSpace c = lerfad (' ':xs)
      lerfad "." = Just []
      lerfad ('.':xs) = lerfad (' ':xs)
      lerfad [c] | isSpace c = Just []
      lerfad (c:xs) | isSpace c = lerfad (' ':xs)
      lerfad (c:xs) | not (goodchr c) = if ignoring then lerfad xs else Nothing
      lerfad ('á':xs) = 'A' ~: lerfad xs
      lerfad ('Á':xs) = 'A' ~: lerfad xs
      lerfad ('é':xs) = 'E' ~: lerfad xs
      lerfad ('É':xs) = 'E' ~: lerfad xs
      lerfad ('h':xs) = lerfad $ '\'':xs
      lerfad ('H':xs) = lerfad $ '\'':xs
      lerfad ('’':xs) = lerfad $ '\'':xs
      lerfad ('í':xs) = 'I' ~: lerfad xs
      lerfad ('Í':xs) = 'I' ~: lerfad xs
      lerfad ('ó':xs) = 'O' ~: lerfad xs
      lerfad ('Ó':xs) = 'O' ~: lerfad xs
      lerfad ('ú':xs) = 'U' ~: lerfad xs
      lerfad ('Ú':xs) = 'U' ~: lerfad xs
      lerfad ('Y':xs) = 'y' ~: lerfad xs
      lerfad ('ý':xs) = 'y' ~: lerfad xs
      lerfad ('Ý':xs) = 'y' ~: lerfad xs
      lerfad ('0':xs) = lerfad $ 'n':'o':xs
      lerfad ('1':xs) = lerfad $ 'p':'a':xs
      lerfad ('2':xs) = lerfad $ 'r':'e':xs
      lerfad ('3':xs) = lerfad $ 'c':'i':xs
      lerfad ('4':xs) = lerfad $ 'v':'o':xs
      lerfad ('5':xs) = lerfad $ 'm':'u':xs
      lerfad ('6':xs) = lerfad $ 'x':'a':xs
      lerfad ('7':xs) = lerfad $ 'z':'e':xs
      lerfad ('8':xs) = lerfad $ 'b':'i':xs
      lerfad ('9':xs) = lerfad $ 's':'o':xs
      lerfad (c:xs) = (if isC c then toLower c else c) ~: lerfad xs
      lerfad [] = Just []
  return $ lerfad $ dropWhile (\c -> isSpace c || c == '.' || c == ',') str

 jvokatna, jvokatna' :: String -> Jvacux [String]
  -- Although jvokatna' currently doesn't use any Jvacux options, it is still
  -- wrapped in the Jvacux monad in preparation for the day that it does.
 jvokatna str = fadgau str >>= maybe (return []) jvokatna'
 jvokatna' str =
  -- As part of the assumption that str is normalized, all vowel pairs are
  -- assumed to be valid diphthongs and all consonants & Y's are assumed to be
  -- lowercase.
  let (pre, fanmo) = case lertype (reverse str) of
	V v2 : Apos : V v1 : C c1 : xs -> (xs, [[c1, v1, '\'', v2]])
	V v2 : V v1 : C c1 : xs | notElem v1 "iuIU" -> (xs, [[c1, v1, v2]])
	V v2 : C c3 : V v1 : C c2 : C c1 : xs
	 | isCC [c1, c2] -> (xs, [[c1, c2, v1, c3, v2]])
	V v2 : C c3 : C c2 : V v1 : C c1 : xs
	 | Set.member [c2, c3] fadni -> (xs, [[c1, v1, c2, c3, v2]])
	V v1 : C c2 : C c1 : xs | isCC [c1, c2] -> ccv' xs [[c1, c2, v1]]
	 where ccv' (V v1' : C c2' : C c1' : xs') ccvs | isCC [c1', c2']
		= ccv' xs' ([c1', c2', v1'] : ccvs)
	       ccv' (V _:C _: _) _ = case xs of
		 V v' : C c' : xs' -> (xs', [[c', v', c1, c2, v1]])
		 _ -> ([], [])  -- This shouldn't happen, but just in case...
	       ccv' xs' ccvs = (xs', ccvs)
	_ -> ([], [])  -- invalid {lujvo}
      katna [] rafs = rafs
      katna (V v : C c2 : C c1 : xs) rafs | isCC [c1, c2]
       = katna xs ([c1, c2, v] : rafs)
      katna (V v2 : V v1 : C c : xs@(_:_)) rafs | notElem v1 "iuIU"
       = katna xs ([c, v1, v2] : rafs)
      katna (V v2 : Apos : V v1 : C c : xs@(_:_)) rafs
       = katna xs ([c, v1, '\'', v2] : rafs)
      katna (C c2 : V v : C c1 : xs) rafs = katna xs ([c1, v, c2] : rafs)
      katna (Y : C c3 : C c2 : V v : C c1 : xs) rafs | isC_C [c2, c3]
       = katna xs ([c1, v, c2, c3] : rafs)
      katna (Y : C c2 : V v : C c1 : xs) rafs =
       let ccvc' (C c2' : V _ : C c1' : xs') p = isCC [c2', p] && ccvc' xs' c1'
	   ccvc' (C c:_) prec = isCC [c, prec]
	   ccvc' _ _ = False
       in case (ccvc' xs c1, xs) of
        (True, C c0 : xs') -> katna xs' ([c0, c1, v, c2] : rafs)
	_ -> if isC_C [c2, head (head rafs)] && not (hasNDJ $ c2 : head rafs)
	     then if isCC [c2, head (head rafs)]
		  then katna xs ([c1, v, c2] : [] : rafs)
		  else []
	     else katna xs ([c1, v, c2] : "y" : rafs)
      katna [rn, V v2, V v1, C c] rafs =
       if rn == C (head (head rafs) == 'r' ?: 'n' :? 'r')
	&& (length rafs > 1 || raftai (head rafs) /= CCV) && notElem v1 "iuIU"
       then [c, v1, v2] : rafs
       else []
      katna [rn, V v2, Apos, V v1, C c] rafs =
       if rn == C (head (head rafs) == 'r' ?: 'n' :? 'r')
	&& (length rafs > 1 || raftai (head rafs) /= CCV)
       then [c, v1, '\'', v2] : rafs
       else []
      katna [V v2, V v1, C c] rafs =
       if length rafs == 1 && raftai (head rafs) == CCV && notElem v1 "iuIU"
       then [c, v1, v2] : rafs
       else []
      katna [V v2, Apos, V v1, C c] rafs =
       if length rafs == 1 && raftai (head rafs) == CCV
       then [c, v1, '\'', v2] : rafs
       else []
      katna _ _ = []
      rolrafsi = katna pre fanmo
      mulrafsi = filter (\r -> not (null r) && r /= "y") rolrafsi
  in if hasNDJ str || not (noBadCC str) || null fanmo
      || (null pre && length fanmo < 2)
      || length (filter null rolrafsi) > 1
      || length mulrafsi < 2
     then return []
     else return $ case span (\r -> raftai r == CVC || null r) rolrafsi of
      (cvcs@(_:tsb:_), "y":_) ->
       if has_C_C (concat cvcs)  -- has_C_C ⇒ no need for a tosmabru hyphen
	?: null (filter null rolrafsi) :? null tsb then mulrafsi else []
      (cvcs, [[_,_,c1,c2,_]]) | isCC [c1, c2] ->
       if has_C_C (concat rolrafsi)  -- has_C_C ⇒ no need for a tosmabru hyphen
	 ?: null (filter null rolrafsi) :? length cvcs > 1 && null (cvcs !! 1)
	then mulrafsi else []
      _ -> if null (filter null rolrafsi) then mulrafsi else []

 data Raftai = CVV | CCV | CVC | CCVC | CVC_C | CCVCV | CVC_CV | Srerafsi
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix)

 raftai :: String -> Raftai
 -- assumes its argument is normalized; should this use a Jvacux monad?
 raftai [c, v1, v2] | isC c && isV v1 && isV v2 && notElem v1 "iuIU" = CVV
 raftai [c, v1, '\'', v2] | isC c && isV v1 && isV v2 = CVV
 raftai [c1, c2, v] | isCC [c1, c2] && isV v = CCV
 raftai [c1, v, c2] | isC c1 && isV v && isC c2 = CVC
 raftai [c1, c2, v, c3] | isCC [c1, c2] && isV v && isC c3 = CCVC
 raftai [c1, c2, v, c3, 'y'] | isCC [c1, c2] && isV v && isC c3 = CCVC
 raftai [c1, v, c2, c3] | isC c1 && isV v && isC_C [c2, c3] = CVC_C
 raftai [c1, v, c2, c3, 'y'] | isC c1 && isV v && isC_C [c2, c3] = CVC_C
 raftai [c1, c2, v1, c3, v2] | isCC [c1, c2] && isV v1 && isC c3 && isV v2
  = CCVCV
 raftai [c1, v1, c2, c3, v2] | isC c1 && isV v1 && isC_C [c2, c3] && isV v2
  = CVC_CV
 raftai _ = Srerafsi

-- Unexported functions: ------------------------------------------------------

 hasNDJ :: String -> Bool
 hasNDJ str = case dropWhile (/= 'n') str of
  'n':'d':'j':_ -> True
  'n':'d':'z':_ -> True
  'n':'t':'c':_ -> True
  'n':'t':'s':_ -> True
  'n':xs -> hasNDJ xs
  [] -> False

 noBadCC :: String -> Bool
 noBadCC str = null $ filter (\i -> let cc = take 2 (drop i str)
  in length cc /= 1 && (isC $ cc !! 1) && not (isC_C cc)) (findIndices isC str)
 
 data Lertype = C Char | V Char | Y | Apos | BadCh
  deriving (Eq, Ord, Read, Show)
 
 lertype :: String -> [Lertype]
 -- Pre-classifying letterals as consonants & vowels cuts down on obsessive
 -- checking later.
 lertype [] = []
 lertype ('y':xs) = Y : lertype xs
 lertype ('Y':xs) = Y : lertype xs
 lertype ('\'':xs) = Apos : lertype xs
 lertype (c:xs)
  | isC c = C c : lertype xs
  | isV c = V c : lertype xs
  | otherwise = BadCh : lertype xs

 has_C_C :: String -> Bool
 -- checks for the presence of a non-initial valid consonant pair
 has_C_C str = not $ null $ filter (\i -> Set.member (take 2 $ drop i str)
  fadni) (findIndices isC str)
