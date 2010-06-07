module Jbobaf.Vlatai (
  -- * Basic character sequences
  isV, isVy, isVV, isVVV, isC, isCC, isC_C, lidne, fadni,
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
  jvokatna, jvokatna', Raftai(..), raftai, rafyjongau
 ) where
 import Char
 import Ix
 import List (findIndices)
 import Monad (mplus, when, unless)
 import qualified Data.Set as Set
 import Jbobaf.Canti
 import Jbobaf.Jitro

 isV, isVy, isC :: Char -> Bool
 isV = (`elem` "aeiou") . toLower
 isVy = (`elem` "aeiouy") . toLower
 isC = (`elem` "bcdfgjklmnprstvxz") . toLower

 isVV :: String -> Bool
 isVV [v1, v2] = v1 `elem` "iuIU" && isVy v2
  || v1 `elem` "aeoAEO" && toLower v2 == 'i'
  || toLower v1 == 'a' && toLower v2 == 'u'
 isVV _ = False

 isVVV :: String -> Bool
 isVVV [v1, v2, v3] = v1 `elem` "iuIU" && v2 `notElem` "iuIU" && isVV [v2, v3]
 isVVV _ = False

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
  --`mplus` throwError (Selsrera ["brivla_xusra", str] SRE_invalid_word_form)

 gismu_xusra, gismu_xusra' :: String -> Jvacux ()
 gismu_xusra str = fadgau str >>= gismu_xusra'
 gismu_xusra' [] = throwError $ Selsrera ["gismu_xusra"] SRE_empty_string
 gismu_xusra' s@[a, b, c, d, e] = do
  noemph <- isOpt Ignore_brivla_emphasis
  unless (isC a && isC d && isV e && (isV b && isC c && isC_C [c, d]
   || isC b && isV c && isCC [a, b]))
   (throwError $ Selsrera ["gismu_xusra", s] SRE_invalid_word_form)
  when (not noemph && isUpper e) (throwError $ Selsrera ["gismu_xusra", s]
   SRE_invalid_emphasis)
 gismu_xusra' s = throwError $ Selsrera ["gismu_xusra", s] SRE_invalid_word_form

 lujvo_xusra, lujvo_xusra' :: String -> Jvacux ()
 lujvo_xusra str = fadgau str >>= lujvo_xusra'
 lujvo_xusra' str = do
  noemph <- isOpt Ignore_brivla_emphasis
  let sylls = syllabicate str
      emphQty = length $ filter (any isUpper) sylls
  jvokatna' str
  unless (noemph || emphQty == 0 || emphQty == 1 && any isUpper (last $ init
   $ filter voc sylls)) (throwError $ Selsrera ["lujvo_xusra", str]
   SRE_invalid_emphasis)

 fu'ivla_xusra, fu'ivla_xusra' :: String -> Jvacux ()
 fu'ivla_xusra str = fadgau str >>= fu'ivla_xusra'
 fu'ivla_xusra' [] = throwError $ Selsrera ["fu'ivla_xusra"] SRE_empty_string
 fu'ivla_xusra' str = do
  noemph <- isOpt Ignore_brivla_emphasis
  canY   <- isOpt Allow_Y_in_fu'ivla
  ndj    <- isOpt Allow_ndj_in_fu'ivla
  commas <- isNopt No_commas_in_fu'ivla
  let vocSyls = filter voc $ syllabicate str
      emphQty = length $ filter (any isUpper) vocSyls
      sregau = throwError . Selsrera ["fu'ivla_xusra", str]
  xugismu' str >>= flip when (sregau SRE_na'e_fu'ivla)
  xulujvo' str >>= flip when (sregau SRE_na'e_fu'ivla)
  when (isC $ head str)
   $ xulujvo' ('t':'o':str) >>= flip when (sregau SRE_slinku'i_failure)
  when (elem ' ' str) (sregau SRE_no_spaces_allowed)
  unless (commas || notElem ',' str) (sregau SRE_no_commas_allowed)
  unless (isV $ last str) (sregau SRE_must_end_with_vowel)
  checkCC "fu'ivla_xusra" str
  unless ndj $ checkNDJ "fu'ivla_xusra" str
  unless (canY || notElem 'y' str) (sregau SRE_no_Ys_allowed)
  when (length vocSyls < 2) (sregau SRE_not_enough_syllables)
  unless (noemph || emphQty == 0 || emphQty == 1 && any isUpper (last
   $ init vocSyls)) (sregau SRE_invalid_emphasis)
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
      when (ccLoc == 0) (sregau SRE_non_initial_start)
      unless (length (filter (`notElem` "',y") preclust) <= 3)
       (sregau SRE_too_much_before_cluster)
      unless (preCs == 1 && isC (head preclust) || preCs == 0)
       (sregau SRE_too_much_before_cluster)
     else unless (ccLoc == 0) (sregau SRE_breaks_apart)
   Nothing -> sregau SRE_lacks_cluster

 cmevla_xusra, cmevla_xusra' :: String -> Jvacux ()
 cmevla_xusra str = fadgau str >>= cmevla_xusra'
 cmevla_xusra' [] = throwError $ Selsrera ["cmevla_xusra"] SRE_empty_string
 cmevla_xusra' str = do
  dotty <- isOpt Use_dotside
  ndj   <- isOpt Allow_ndj_in_cmevla
  slaka <- isNopt No_commas_in_cmevla
  let sregau = throwError . Selsrera ["cmevla_xusra", str]
  unless (isC $ last str) (sregau SRE_must_end_with_consonant)
  when (elem ' ' str) (sregau SRE_no_spaces_allowed)
  unless (slaka || notElem ',' str) (sregau SRE_no_commas_allowed)
  checkCC "cmevla_xusra" str
  unless ndj $ checkNDJ "cmevla_xusra" str
  unless dotty (case findLa str of
   Just (_, la, _) -> throwError $ Selsrera ["cmevla_xusra", str, la]
		       SRE_la_in_cmevla
   Nothing -> return ())

 cmavo_xusra, cmavo_xusra' :: String -> Jvacux ()
 cmavo_xusra str = fadgau str >>= cmavo_xusra'
 cmavo_xusra' [] = throwError $ Selsrera ["cmavo_xusra"] SRE_empty_string
 cmavo_xusra' str@(c:xs) = do
  commas <- isNopt No_commas_in_cmavo
  let maho = if isC c then xs else str
      sregau = throwError . Selsrera ["cmavo_xusra", str]
  when (elem ' ' str) (sregau SRE_no_spaces_allowed)
  when (null maho) (sregau SRE_consonant_inside_cmavo)
  when (any isC maho) (sregau SRE_consonant_inside_cmavo)
  unless (commas || notElem ',' maho) (sregau SRE_no_commas_allowed)

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
       else throwError $ Selsrera ["fadgau", str, [c]] SRE_non_Lojban_char
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
      porfad (c:'\'':xs) | not (isVy c) =
       throwError $ Selsrera ["fadgau", str, [c, '\'']] SRE_misplaced_apostrophe
      porfad ('\'':c:xs) | not (isVy c) =
       throwError $ Selsrera ["fadgau", str, ['\'', c]] SRE_misplaced_apostrophe
      porfad "'"= throwError $ Selsrera ["fadgau", str] SRE_misplaced_apostrophe
      porfad "," = return []
      porfad " " = return []
      porfad (' ':' ':xs) = porfad (' ':xs)
      porfad (c:xs) = c ~: porfad xs
      porfad [] = return []
      vokfed [] = return []
      vokfed [v] = return [v]
      vokfed [v1, v2] = if isVV [v1, v2] then return [v1, v2]
			else if splitDiphth then return [v1, ',', v2]
			else throwError $ Selsrera ["fadgau", str, [v1, v2]]
			 SRE_bad_vowel_sequence
      vokfed vs@(v1:v2:v3:xs) =
       if triphth && isVVV [v1, v2, v3]
       then return [v1, v2, v3] ~~ (null xs ?: return [] :? splitDiphth
	?: ',' ~: vokfed xs
	:? throwError (Selsrera ["fadgau", str, vs] SRE_bad_vowel_sequence))
       else if splitDiphth then
	if isVV [v1, v2]
	then return [v1, v2, ','] ~~ vokfed (v3:xs)
	else return [v1, ','] ~~ vokfed (v2:v3:xs)
       else throwError $ Selsrera ["fadgau", str, vs] SRE_bad_vowel_sequence
      slakate [] = return []
      slakate str = return cs ~~ vokfed vs ~~ slakate rest
       where (cs, r) = break isVy str
	     (vs, rest) = span isVy r
  str' <- lerfad str >>= return . dropWhile (\c -> isSpace c || c == ',')
  if take 1 str' == "'"
     then throwError $ Selsrera ["fadgau", str] SRE_misplaced_apostrophe
     else porfad str' >>= slakate

 jvokatna, jvokatna' :: String -> Jvacux [String]
 jvokatna str = fadgau str >>= jvokatna'
 jvokatna' [] = throwError $ Selsrera ["jvokatna"] SRE_empty_string
 jvokatna' str = do
  let sregau vel lei = throwError $ Selsrera ("jvokatna" : str : vel) lei
  when (elem ' ' str) (sregau [] SRE_no_spaces_allowed)
  when (elem ',' str) (sregau [] SRE_no_commas_allowed)
  checkCC "jvokatna" str
  checkNDJ "jvokatna" str
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
	      _ -> sregau ["Internal error #1"] SRE_internal_error
	    ccv' xs' ccvs = return (xs', ccvs)
    xs -> sregau [reverse $ unlertype xs, ""] SRE_invalid_rafsi
  when (null pre && length fanmo < 2) (sregau [] SRE_not_enough_rafsi)
  let unraf = concatMap (\r -> r == [] ?: "y" :? r)
      katna [] rafs = return rafs
      katna (V v : C c2 : C c1 : xs) rafs | isCC [c1, c2]
       = katna xs ([c1, c2, v] : rafs)
      katna (V v2 : V v1 : C c : xs@(_:_)) rafs | notElem v1 "iuIU"
       = katna xs ([c, v1, v2] : rafs)
      katna (V v2 : Apos : V v1 : C c : xs@(_:_)) rafs
       = katna xs ([c, v1, '\'', v2] : rafs)
      katna (C c2 : V v : C c1 : xs) rafs = katna xs ([c1, v, c2] : rafs)
      katna (Y : C c3 : C c2 : V v : C c1 : xs) rafs | isC_C [c2, c3]
       = katna xs ([c1, v, c2, c3] : "y" : rafs)
      katna ys@(Y : C c2 : V v : C c1 : xs) rafs@((cA:cB:_):_) =
       let ccvc' (C c2' : V _ : C c1' : xs') p = isCC [c2', p] && ccvc' xs' c1'
	   ccvc' (C c:_) prec = isCC [c, prec]
	   ccvc' _ _ = False
       in case (ccvc' xs c1, xs) of
        (True, C c0 : xs') -> katna xs' $ [c0, c1, v, c2] : "y" : rafs
	_ -> if isC_C [c2,cA] && notElem [c2,cA,cB] ["ndj", "ndz", "ntc", "nts"]
	     then if isCC [c2, head (head rafs)]
		  then katna xs $ [c1, v, c2]:[]:rafs
		  else sregau [reverse $ unlertype ys, unraf rafs]
		   SRE_extra_Y_hyphen
	     else katna xs $ [c1, v, c2] : "y" : rafs
      katna [rn, V v2, V v1, C c] rafs =
       if rn == C (head (head rafs) == 'r' ?: 'n' :? 'r')
	&& (length rafs > 1 || raftai (head rafs) /= CCV) && notElem v1 "iuIU"
       then return $ [c, v1, v2] : rafs
       else sregau [] SRE_bad_rn_hyphen
      katna [rn, V v2, Apos, V v1, C c] rafs =
       if rn == C (head (head rafs) == 'r' ?: 'n' :? 'r')
	&& (length rafs > 1 || raftai (head rafs) /= CCV)
       then return $ [c, v1, '\'', v2] : rafs
       else sregau [] SRE_bad_rn_hyphen
      katna [V v2, V v1, C c] rafs =
       if length rafs == 1 && raftai (head rafs) == CCV && notElem v1 "iuIU"
       then return $ [c, v1, v2] : rafs
       else sregau [] SRE_missing_rn_hyphen
      katna [V v2, Apos, V v1, C c] rafs =
       if length rafs == 1 && raftai (head rafs) == CCV
       then return $ [c, v1, '\'', v2] : rafs
       else sregau [] SRE_missing_rn_hyphen
      katna mal rafs = sregau [reverse $ unlertype mal, unraf rafs]
       SRE_invalid_rafsi
  rolrafsi <- katna pre fanmo
  let mulrafsi = filter (\r -> not (null r) && r /= "y") rolrafsi
      tosCheck [] = return ()
      tosCheck (x:_) = sregau [unraf pre, unraf post] SRE_extra_Y_hyphen
       where (pre, post) = splitAt (x+1) rolrafsi
  tosCheck $ drop 1 $ findIndices null rolrafsi
  when (length mulrafsi < 2) (sregau [] SRE_not_enough_rafsi)
   -- Can ^this^ even happen at this point?
  case span (\r -> raftai r == CVC || null r) rolrafsi of
   (cvcs@(_:tsb:_), "y":_) ->
    if has_C_C (concat cvcs)  -- has_C_C ⇒ no need for a tosmabru hyphen
    then tosCheck $ findIndices null rolrafsi
    else unless (null tsb) (sregau [] SRE_tosmabru_failure)
   (cvcs, [[_,_,c1,c2,_]]) | isCC [c1, c2] ->
    if has_C_C (concat rolrafsi)  -- has_C_C ⇒ no need for a tosmabru hyphen
    then tosCheck $ findIndices null rolrafsi
    else unless (length cvcs > 1 && null (cvcs !! 1))
     (sregau [] SRE_tosmabru_failure)
   _ -> tosCheck $ findIndices null rolrafsi
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

 rafyjongau :: [String] -> Jvacux String
 -- assumes that the {rafsi} are pre-normalized
 -- This function *should* support construction of {lujvo cmevla}.
 rafyjongau rafs = do
  when (length rafs < 2)
   (throwError $ Selsrera ("rafyjongau":rafs) SRE_not_enough_rafsi)
  let insertYs _ [] = return []
      insertYs prev (raf:xs) = do
	let tai = raftai raf
	when (tai == Srerafsi)
	 (throwError $ Selsrera ["rafyjongau", raf] SRE_invalid_rafsi)
	let y1 = if not (null prev) && isC (last prev)
		  && (not (isC_C [last prev, head raf]) || last prev == 'n'
		   && take 2 raf `elem` ["dj", "dz", "tc", "ts"])
		 then ("y" ~:)
		 else id
	let (y2, p) = if tai == CCVC || tai == CVC_C && not (null xs)
		      then (("y" ~:), "y")
		      else if null prev && tai == CVV
		       && (length xs > 1 || raftai (head xs) /= CCV)
		      then (((take 1 (head xs) == "r" ?: "n" :? "r") ~:), "y")
		      -- Using "take 1" avoids problems with null {rafsi}.
		      else (id, prev)
	y1 $ raf ~: y2 (insertYs p xs)
  rafs2 <- insertYs [] rafs
  return $ concat $ case span ((== CVC) . raftai) rafs2 of
   (cvcs@(r1:r2:xs), "y":rest) ->
    has_C_C (concat cvcs) ?: rafs2 :? (r1:"y":r2:xs ++ "y":rest)
   (cvcs@(r1:xs), rest@[[_,_,c1,c2,_]]) | isCC [c1, c2] ->
    has_C_C (concat rafs2) ?: rafs2 :? (r1:"y":xs ++ rest)
   _ -> rafs2

-- Unexported functions: ------------------------------------------------------

 checkNDJ :: String -> String -> Jvacux ()
 checkNDJ f str = ndj str
  where ndj s = case dropWhile (/= 'n') s of
		 'n':'d':'j':_ -> flib "dj"
		 'n':'d':'z':_ -> flib "dz"
		 'n':'t':'c':_ -> flib "tc"
		 'n':'t':'s':_ -> flib "ts"
		 'n':xs -> ndj xs
		 [] -> return ()
	flib dj = throwError $ Selsrera [f,str,'n':dj] SRE_bad_consonant_triple

 checkCC :: String -> String -> Jvacux ()
 checkCC f str = case [cc | i <- findIndices isC str,
   let cc = take 2 (drop i str), length cc /= 1, isC (cc !! 1), not (isC_C cc)]
  of cc:_ -> throwError $ Selsrera [f, str, cc] SRE_bad_consonant_pair
     []   -> return ()

 data Lertype = C Char | V Char | Y | Apos | BadCh Char deriving (Eq, Ord, Show)

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
  | otherwise = BadCh c : lertype xs

 unlertype :: [Lertype] -> String
 unlertype (C c : xs) = c : unlertype xs
 unlertype (V v : xs) = v : unlertype xs
 unlertype (Y : xs) = 'y' : unlertype xs
 unlertype (Apos : xs) = '\'' : unlertype xs
 unlertype (BadCh c : xs) = c : unlertype xs
 unlertype [] = []
