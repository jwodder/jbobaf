module Jbobaf.Vlatai where
 import Char
 import qualified Data.Set as Set
 import Jbobaf.Internals

 isV = (`elem` "aeiou") . toLower
 isVy = (`elem` "aeiouy") . toLower
 isC = (`elem` "bcdfgjklmnprstvxz") . toLower
 isCC = (`Set.member` lidne) . map toLower
 isC_C cc = Set.member cc' fadni || Set.member cc' lidne
  where cc' = map toLower cc

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
 xugismu', xulujvo', xufu'ivla', xucmevla', xucmavo' :: String -> Tamcux Bool
 -- The "prime" forms of the xu* functions assume that their arguments are
 -- already normalized.  The non-prime functions do not.

 xubrivla str = do
  gismu <- xugismu str
  lujvo <- xulujvo str
  fu'ivla <- xufu'ivla str
  return $ gismu || lujvo || fu'ivla
 
 xugismu str = fadgau str >>= maybe (return False) xugismu'
 xugismu' [a, b, c, d, e] = do
  noemph <- isOpt Ignore_brivla_emphasis
  return $ isC a && isC d && isV e && (isV b && isC c && isC_C [c, d]
   || isC b && isV c && isCC [a, b]) && (noemph || not (isUpper e))
 xugismu' _ = return False

 xulujvo str = fadgau str >>= maybe (return False) xulujvo'
 xulujvo' str = ?????

 xufu'ivla str = fadgau str >>= maybe (return False) xufu'ivla'
 xufu'ivla' str = ?????

 xucmevla str = fadgau str >>= maybe (return False) xucmevla'
 xucmevla' str = ?????

 xucmavo str = fadgau str >>= maybe (return False) xucmavo'
 xucmavo' str = ?????

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

 fadgau :: String -> Tamcux (Maybe String)
 fadgau str = do
  accents <- isOpt Allow_accents
  ignoring <- isOpt Ignore_naljbo_chars
  hasH <- isOpt Allow_H
  let goodchr c = elem (toLower c) "',.abcdefgijklmnoprstuvxyz"
		   || accents && elem (toLower c) "áéíóú"
		   || hasH && toLower c == 'h'
      lerfad ('\'':',':xs) = lerfad ('\'':xs)
      lerfad (',':'\'':xs) = lerfad ('\'':xs)
      lerfad (',':',':xs) = lerfad (',':xs)
      lerfad (',':'.':xs) = lerfad (' ':xs)
      lerfad (',':c:xs) | isSpace c = lerfad (c:xs)
      lerfad (' ':'.':xs) = lerfad (' ':xs)
      lerfad (' ':',':xs) = lerfad (' ':xs)
      lerfad (' ':c:xs) | isSpace c = lerfad (' ':xs)
      lerfad ('.':xs) = lerfad (' ':xs)
      lerfad "," = Just []
      lerfad "." = Just []
      lerfad [c] | isSpace c = Just []
      lerfad (c:xs) | isSpace c = lerfad (' ':xs)
      lerfad (c:xs) | not (goodchr c) = if ignoring then lerfad xs else Nothing
      lerfad ('á':xs) = 'A' ~: lerfad xs
      lerfad ('Á':xs) = 'A' ~: lerfad xs
      lerfad ('é':xs) = 'E' ~: lerfad xs
      lerfad ('É':xs) = 'E' ~: lerfad xs
      lerfad ('h':xs) = '\'' ~: lerfad xs
      lerfad ('H':xs) = '\'' ~: lerfad xs
      lerfad ('í':xs) = 'I' ~: lerfad xs
      lerfad ('Í':xs) = 'I' ~: lerfad xs
      lerfad ('ó':xs) = 'O' ~: lerfad xs
      lerfad ('Ó':xs) = 'O' ~: lerfad xs
      lerfad ('ú':xs) = 'U' ~: lerfad xs
      lerfad ('Ú':xs) = 'U' ~: lerfad xs
      lerfad ('Y':xs) = 'y' ~: lerfad xs
      lerfad ('ý':xs) = 'y' ~: lerfad xs
      lerfad ('Ý':xs) = 'y' ~: lerfad xs
      lerfad (c:xs) = if isC c then toLower c else c
      lerfad [] = Just []
  lerfad $ dropWhile (\c -> isSpace c || c == '.' || c == ',') str
