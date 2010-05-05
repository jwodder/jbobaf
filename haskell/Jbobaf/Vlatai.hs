module Jbobaf.Vlatai (
  -- * Basic character sequences
  isV, isVy, isC, isCC, isC_C, lidne, fadni,
  -- * Word classification & validity
  xubrivla, xugismu, xulujvo, xufu'ivla, xucmevla, xucmavo,
  -- ** Pre-normalized
  xubrivla', xugismu', xulujvo', xufu'ivla', xucmevla', xucmavo',
  -- ** Asserting validity
  brivla_xusra, gismu_xusra, lujvo_xusra, fu'ivla_xusra, cmevla_xusra,
  cmavo_xusra,
  -- ** Asserting validity (pre-normalized)
  brivla_xusra', gismu_xusra', lujvo_xusra', fu'ivla_xusra', cmevla_xusra',
  cmavo_xusra',
  -- * Normalization
  fadgau,
  -- * /Lujvo/ manipulation
  jvokatna, jvokatna', Raftai(..), raftai
 ) where
 import Char
 import Ix
 import List (findIndices)
 import Monad (mplus)
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

 -- The "prime" forms of the xu* and *_xusra functions assume that their
 -- arguments are already normalized.  The non-prime functions do not.

 xubrivla, xugismu, xulujvo, xufu'ivla, xucmevla, xucmavo,
  xubrivla', xugismu', xulujvo', xufu'ivla', xucmevla', xucmavo'
  :: String -> Jvacux Bool
 xugismu    = xusnada . gismu_xusra
 xugismu'   = xusnada . gismu_xusra'
 xulujvo    = xusnada . lujvo_xusra
 xulujvo'   = xusnada . lujvo_xusra'
 xufu'ivla  = xusnada . fu'ivla_xusra
 xufu'ivla' = xusnada . fu'ivla_xusra'
 xucmevla   = xusnada . cmevla_xusra
 xucmevla'  = xusnada . cmevla_xusra'
 xucmavo    = xusnada . cmavo_xusra
 xucmavo'   = xusnada . cmavo_xusra'
 xubrivla   = xusnada . brivla_xusra
 xubrivla'  = xusnada . brivla_xusra'

 brivla_xusra, brivla_xusra' :: String -> Jvacux ()
 brivla_xusra str = fadgau str >>= brivla_xusra'
 brivla_xusra' str = gismu_xusra' str
  `mplus` lujvo_xusra' str
  `mplus` fu'ivla_xusra' str
  `mplus` throwError "This string is not a {brivla}."
 
 gismu_xusra, gismu_xusra' :: String -> Jvacux ()
 gismu_xusra str = fadgau str >>= gismu_xusra'
 gismu_xusra' [a, b, c, d, e] = do
  noemph <- isOpt Ignore_brivla_emphasis
  xusra (isC a && isC d && isV e && (isV b && isC c && isC_C [c, d]
   || isC b && isV c && isCC [a, b])) "Invalid {gismu} form"
  xusra (noemph || not (isUpper e)) "Invalid {brivla} emphasis"
 gismu_xusra' _ = throwError "{gismu} must be five letterals long."

 lujvo_xusra, lujvo_xusra' :: String -> Jvacux ()
 lujvo_xusra str = fadgau str >>= lujvo_xusra'
 lujvo_xusra' str = do
  noemph <- isOpt Ignore_brivla_emphasis
  let sylls = syllabicate str
      emphQty = length $ filter (not . null . filter isUpper) sylls
  jvokatna' str
  xusra (noemph || emphQty == 0 || emphQty == 1
   && not (null $ filter isUpper $ last $ init $ filter voc sylls))
   "Invalid {brivla} emphasis"

 fu'ivla_xusra, fu'ivla_xusra' :: String -> Jvacux ()
 fu'ivla_xusra str = fadgau str >>= fu'ivla_xusra'
 fu'ivla_xusra' str = do
  noemph <- isOpt Ignore_brivla_emphasis
  canY   <- isOpt Allow_Y_in_fu'ivla
  ndj    <- isOpt Allow_ndj_in_fu'ivla
  let vocSyls = filter voc $ syllabicate str
      emphQty = length $ filter (not . null . filter isUpper) vocSyls
  xusra (not $ null str) "{fu'ivla} must be non-empty."
  xugismu' str >>= flip xusra "{fu'ivla} may not be {gismu}." . not
  xulujvo' str >>= flip xusra "{fu'ivla} may not be {lujvo}." . not
  if isC (head str) then (xulujvo' $ 't':'o':str)
    >>= flip xusra "{fu'ivla} may not fail the tosmabru test" . not
   else return ()
  xusra (notElem ' ' str) "{fu'ivla} may not have internal spaces or periods."
  xusra (isV $ last str) "{fu'ivla} must end with a vowel."
  xusra (noBadCC str) "{fu'ivla} may not contain any invalid consonant pairs."
  xusra (ndj || not (hasNDJ str)) "{fu'ivla} may not contain the strings\
   \ \"ndj\", \"ndz\", \"ntc\", or \"nts\"."
  xusra (canY || notElem 'y' str) "{fu'ivla} may not contain Y's."
  xusra (length vocSyls >= 2)
   "{fu'ivla} must contain two or more vocalic syllables."
  xusra (noemph || emphQty == 0 || emphQty == 1
   && not (null $ filter isUpper $ last $ init vocSyls))
   "Invalid {brivla} emphasis"
  case findC_C str of
   Just ccLoc -> do
    let (clust, rest) = span (\c -> isC c || c == 'y') (drop ccLoc str)
	preclust = take ccLoc str
	preCs = length $ filter isC preclust
    slinky <- xulujvo' $ 't':'o':drop ccLoc str
    if elem 'y' clust || has_C_C clust
	|| length (filter voc $ syllabicate rest) == 1
	|| ccLoc /= 0 && slinky
     then do
      xusra (ccLoc /= 0) "Non-initial consonant clusters may not occur at the\
       \ start of a {fu'ivla}."
      xusra (length (filter (`notElem` "',y") preclust) <= 3) "The consonant\
       \ cluster in a {fu'ivla} may be preceded by no more than three letters."
      xusra (preCs == 1 && isC (head preclust) || preCs == 0) "A consonant\
       \ cluster in a {fu'ivla} must be preceded by no more than one {ma'osmi}."
     else xusra (ccLoc == 0) "{fu'ivla} may not break apart into smaller words."
   Nothing -> throwError "{fu'ivla} must contain a consonant cluster."

 cmevla_xusra, cmevla_xusra' :: String -> Jvacux ()
 cmevla_xusra str = fadgau str >>= cmevla_xusra'
 cmevla_xusra' [] = throwError "{cmevla} must be non-empty."
 cmevla_xusra' str = do
  dotty <- isOpt Use_dotside
  ndj   <- isOpt Allow_ndj_in_cmevla
  xusra (isC $ last str) "{cmevla} must end with a consonant."
  xusra (notElem ' ' str) "{cmevla} may not have internal spaces or periods."
  xusra (noBadCC str) "{cmevla} may not contain any invalid consonant pairs."
  case (dotty, findLa str, ndj, hasNDJ str) of
   (False, Just _, _, _) -> throwError "{cmevla} may not contain the strings\
    \ \"la\", \"lai\", \"la'i\", or \"doi\"."
   (_, _, False, True) -> throwError "{cmevla} may not contain the strings\
    \ \"ndj\", \"ndz\", \"ntc\", or \"nts\"."
   _ -> return ()

 cmavo_xusra, cmavo_xusra' :: String -> Jvacux ()
 cmavo_xusra str = fadgau str >>= cmavo_xusra'
 cmavo_xusra' [] = throwError "{cmavo} must be non-empty."
 cmavo_xusra' str@(c:xs) = do
  commas <- isNopt No_commas_in_cmavo
  let maho = if isC c then xs else str
  xusra (not $ null maho) "A single consonant is not a {cmavo}."
  xusra (null $ filter (\c -> isSpace c || isC c) maho)
   "{cmavo} may not have internal spaces, periods, or consonants."
  xusra (commas || notElem ',' maho) "{cmavo} may not contain commas."

 -- |@fadgau@ is a basic \"cleanup\" routine used by various functions in
 -- Jbobaf for converting Lojban text into a more regular, \"normalized\" form.
 -- It performs the following operations:
 --
 -- * Leading & trailing whitespace, periods, and commas are removed.
 --
 -- * Internal strings of whitespace and/or periods (along with any adjacent
 --   commas) are converted into a single space each.
 --
 -- * If 'Ignore_naljbo_chars' is in effect, any non-Lojbanic characters
 --   (including accented vowels and H's if 'Allow_accents' and 'Allow_H',
 --   respectively, are not in effect) are removed.  Otherwise, if the string
 --   does contain any non-Lojban characters, an error is thrown.
 --
 -- * If 'Allow_accents' is in effect, vowels with acute accents are converted
 --   to uppercase ASCII letters (except for accented Y's, which are converted
 --   to lowercase).
 --
 -- * If 'Translate_digits' is in effect, decimal digits are converted to their
 --   corresponding /cmavo/.
 --
 -- * If 'Allow_H' is in effect, H's are converted to apostrophes.
 --
 -- * All consonants and Y's are converted to lowercase.
 --
 -- * Any occurrences of the right single quotation mark (\'&#x2019;\', U+2019)
 --   are converted to apostrophes.
 --
 -- * Consecutive apostrophes are merged together.
 --
 -- * If an apostrophe is found in an invalid location (i.e., next to a
 --   non-vowel or at the the beginning or end of the string), an error is
 --   thrown.
 --
 -- * Consecutive commas are merged together, and commas not between two vowels
 --   are removed.
 --
 -- * Commas are inserted in invalid vowel clusters if 'Split_bad_diphthongs'
 --   is in effect; otherwise, an invalid vowel cluster causes an error to be
 --   thrown

 fadgau :: String -> Jvacux String
 fadgau str = do
  accents     <- isOpt Allow_accents
  ignoring    <- isOpt Ignore_naljbo_chars
  hasH        <- isOpt Allow_H
  digits      <- isOpt Translate_digits
  splitDiphth <- isOpt Split_bad_diphthongs
  triphth     <- isOpt Allow_triphthongs
  let goodchr c = elem (toLower c) "',.abcdefgijklmnoprstuvxyz’"
		   || accents && elem (toLower c) "áéíóúý"
		   || hasH && toLower c == 'h'
		   || digits && isDigit c
      lerfad (c:xs) | isSpace c = ' ' ~: lerfad xs
      lerfad (c:xs) | not (goodchr c) =
       if ignoring then lerfad xs
       else throwError "Non-Lojbanic character in string"
      lerfad ('.':xs) = ' ' ~: lerfad xs
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
      lerfad [] = return []
      -- All of the tests for adjacent characters and characters at the
      -- beginning or end of the string rely on first removing all non-Lojbanic
      -- characters and normalizing the remaining individuals.  Thus, input to
      -- fadgau must be run through lerfad first and then through porfad and
      -- slakate (which could possibly be merged into porfad, but the code is
      -- ugly enough already).
      porfad (c:',':xs) | not (isVy c) = porfad (c:xs)
      porfad (',':c:xs) | not (isVy c) = porfad (c:xs)
      porfad ('\'':'\'':xs) = porfad ('\'':xs)
      porfad (c:'\'':xs)
       | not (isVy c) = throwError "Apostrophe next to a non-vowel detected."
      porfad ('\'':c:xs)
       | not (isVy c) = throwError "Apostrophe next to a non-vowel detected."
      porfad "'"= throwError "Apostrophes may not occur at the end of a string."
      porfad "," = return []
      porfad " " = return []
      porfad (' ':' ':xs) = porfad (' ':xs)
      porfad (c:xs) = c ~: porfad xs
      porfad [] = return []
      isDiphth v1 v2 = v1 `elem` "iuIU"
       || v1 `elem` "aeoAEO" && toLower v2 == 'i'
       || toLower v1 == 'a' && toLower v2 == 'u'
      vokfed [] = return []
      vokfed [v] = return [v]
      vokfed [v1, v2] = if isDiphth v1 v2 then return [v1, v2]
			else if splitDiphth then return [v1, ',', v2]
			else throwError "Invalid diphthong detected"
      vokfed (v1:v2:v3:xs) =
       if triphth && v1 `elem` "iuIU" && isDiphth v2 v3
       then return [v1, v2, v3] ~~ (null xs ?: return [] :? splitDiphth
	?: ',' ~: vokfed xs :? throwError "Invalid 4-vowel sequence detected")
       else if splitDiphth then
	if isDiphth v1 v2
	then return [v1, v2, ','] ~~ vokfed (v3:xs)
	else return [v1, ','] ~~ vokfed (v2:v3:xs)
       else throwError "Invalid triphthong detected"
      slakate [] = return []
      slakate str = return cs ~~ vokfed vs ~~ slakate rest
       where (cs, r) = break isVy str
	     (vs, rest) = span isVy r
  str' <- lerfad str >>= return . dropWhile (\c -> isSpace c || c == ',')
  if take 1 str' == "'"
     then throwError "Apostrophes may not occur at the beginning of a string."
     else porfad str' >>= slakate

 jvokatna, jvokatna' :: String -> Jvacux [String]
 jvokatna str = fadgau str >>= jvokatna'
 jvokatna' str = do
  xusra (not $ hasNDJ str) "Invalid consonant triple in {lujvo}"
  xusra (noBadCC str) "Invalid consonant pair in {lujvo}"
  (pre, fanmo) <- case lertype (reverse str) of
    V v2 : Apos : V v1 : C c1 : xs -> return (xs, [[c1, v1, '\'', v2]])
    V v2 : V v1 : C c1 : xs | notElem v1 "iuIU" -> return (xs, [[c1, v1, v2]])
    V v2 : C c3 : V v1 : C c2 : C c1 : xs
     | isCC [c1, c2] -> return (xs, [[c1, c2, v1, c3, v2]])
    V v2 : C c3 : C c2 : V v1 : C c1 : xs
     | Set.member [c2, c3] fadni -> return (xs, [[c1, v1, c2, c3, v2]])
    V v1 : C c2 : C c1 : xs | isCC [c1, c2] -> ccv' xs [[c1, c2, v1]]
      where ccv' (V v1' : C c2' : C c1' : xs') ccvs | isCC [c1', c2']
	     = ccv' xs' ([c1', c2', v1'] : ccvs)
	    ccv' (V _:C _: _) _ = case xs of
	      V v' : C c' : xs' -> return (xs', [[c', v', c1, c2, v1]])
	      _ -> throwError "jvokatna': Internal error #1: Don't panic"
	    ccv' xs' ccvs = return (xs', ccvs)
    _ -> throwError "Invalid final {rafsi}"
  xusra (not (null pre) || length fanmo >= 2)
   "{lujvo} must contain at least two {rafsi}."
  let katna [] rafs = return rafs
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
		  else throwError "Superfluous Y-hyphen in {lujvo}"
	     else katna xs ([c1, v, c2] : "y" : rafs)
      katna [rn, V v2, V v1, C c] rafs =
       if rn == C (head (head rafs) == 'r' ?: 'n' :? 'r')
	&& (length rafs > 1 || raftai (head rafs) /= CCV) && notElem v1 "iuIU"
       then return $ [c, v1, v2] : rafs
       else throwError "Invalid r/n-hyphen in {lujvo}"
      katna [rn, V v2, Apos, V v1, C c] rafs =
       if rn == C (head (head rafs) == 'r' ?: 'n' :? 'r')
	&& (length rafs > 1 || raftai (head rafs) /= CCV)
       then return $ [c, v1, '\'', v2] : rafs
       else throwError "Invalid r/n-hyphen in {lujvo}"
      katna [V v2, V v1, C c] rafs =
       if length rafs == 1 && raftai (head rafs) == CCV && notElem v1 "iuIU"
       then return $ [c, v1, v2] : rafs
       else throwError "R/n-hyphen missing from {lujvo}"
      katna [V v2, Apos, V v1, C c] rafs =
       if length rafs == 1 && raftai (head rafs) == CCV
       then return $ [c, v1, '\'', v2] : rafs
       else throwError "R/n-hyphen missing from {lujvo}"
      katna _ _ = throwError "Invalid {rafsi} form"
  rolrafsi <- katna pre fanmo
  let mulrafsi = filter (\r -> not (null r) && r /= "y") rolrafsi
  xusra (length (filter null rolrafsi) <= 1) "Superfluous Y-hyphen in {lujvo}"
  xusra (length mulrafsi >= 2) "{lujvo} must contain at least two {rafsi}."
  case span (\r -> raftai r == CVC || null r) rolrafsi of
   (cvcs@(_:tsb:_), "y":_) ->
    if has_C_C (concat cvcs)  -- has_C_C ⇒ no need for a tosmabru hyphen
    then xusra (null $ filter null rolrafsi) "Superfluous Y-hyphen in {lujvo}"
    else xusra (null tsb) "{lujvo} missing tosmabru hyphen"
   (cvcs, [[_,_,c1,c2,_]]) | isCC [c1, c2] ->
    if has_C_C (concat rolrafsi)  -- has_C_C ⇒ no need for a tosmabru hyphen
    then xusra (null $ filter null rolrafsi) "Superfluous Y-hyphen in {lujvo}"
    else xusra (length cvcs > 1 && null (cvcs !! 1))
     "{lujvo} missing tosmabru hyphen"
   _ -> xusra (null $ filter null rolrafsi) "Superfluous Y-hyphen in {lujvo}"
  return mulrafsi

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

 xusra :: Bool -> String -> Jvacux ()
 xusra True _ = return ()
 xusra False s = throwError s
