module Jbobaf.Selmaho (Selma'o(..), getSelma'o, rolma'o) where
 import Ix
 import qualified Data.Map as Map
 import Jbobaf.Valsi
 
 data Selma'o = BRIVLA | CMEVLA
  | A | BAI | BAhE | BE | BEI | BEhO | BIhE | BIhI | BO | BOI | BU | BY | CAI
  | CAhA | CEI | CEhE | CO | COI | CU | CUhE | DAhO | DOI | DOhU | FA | FAhA
  | FAhO | FEhE | FEhU | FIhO | FOI | FUhA | FUhE | FUhO | GA | GAhO | GEhU
  | GI | GIhA | GOI | GOhA | GUhA | I | JA | JAI | JOI | JOhI | KE | KEI | KEhE
  | KI | KOhA | KU | KUhE | KUhO | LA | LAU | LAhE | LE | LEhU | LI | LIhU
  | LOhO | LOhU | LU | LUhU | MAI | MAhO | ME | MEhU | MOI | MOhE | MOhI | NA
  | NAI | NAhE | NAhU | NIhE | NIhO | NOI | NU | NUhA | NUhI | NUhU | PA | PEhE
  | PEhO | PU | RAhO | ROI | SA | SE | SEI | SEhU | SI | SOI | SU | TAhE | TEI
  | TEhU | TO | TOI | TUhE | TUhU | UI | VA | VAU | VEI | VEhA | VEhO | VIhA
  | VUhO | VUhU | XI | Y | ZAhO | ZEI | ZEhA | ZI | ZIhE | ZO | ZOI | ZOhU
  | UNKNOWN
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix)

 getSelma'o :: Valsi -> Selma'o
 getSelma'o vla = case klesi vla of
  Gismu -> BRIVLA
  Lujvo -> BRIVLA
  Fu'ivla -> BRIVLA
  Cmavo -> case Map.lookup (valsi vla) rolma'o of
   Just s -> s
   Nothing -> UNKNOWN
  -- Lujma'o -> undefined
  Cmevla -> CMEVLA

 rolma'o :: Map.Map String Selma'o
 rolma'o = Map.fromAscList [
  ("a", A), ("a'a", UI), ("a'e", UI), ("a'i", UI), ("a'o", UI), ("a'u", UI),
  ("ai", UI), ("au", UI), ("ba", PU), ("ba'a", UI), ("ba'e", BAhE),
  ("ba'i", BAI), ("ba'o", ZAhO), ("ba'u", UI), ("bai", BAI), ("bau", BAI),
  ("be", BE), ("be'a", FAhA), ("be'e", COI), ("be'i", BAI), ("be'o", BEhO),
  ("be'u", UI), ("bei", BEI), ("bi", PA), ("bi'e", BIhE), ("bi'i", BIhI),
  ("bi'o", BIhI), ("bi'u", UI), ("bo", BO), ("boi", BOI), ("bu", BU),
  ("bu'a", GOhA), ("bu'e", GOhA), ("bu'i", GOhA), ("bu'o", UI), ("bu'u", FAhA),
  ("by", BY), ("ca", PU), ("ca'a", CAhA), ("ca'e", UI), ("ca'i", BAI),
  ("ca'o", ZAhO), ("ca'u", FAhA), ("cai", CAI), ("cau", BAI), ("ce", JOI),
  ("ce'a", LAU), ("ce'e", CEhE), ("ce'i", PA), ("ce'o", JOI), ("ce'u", KOhA),
  ("cei", CEI), ("ci", PA), ("ci'e", BAI), ("ci'i", PA), ("ci'o", BAI),
  ("ci'u", BAI), ("co", CO), ("co'a", ZAhO), ("co'e", GOhA), ("co'i", ZAhO),
  ("co'o", COI), ("co'u", ZAhO), ("coi", COI), ("cu", CU), ("cu'a", VUhU),
  ("cu'e", CUhE), ("cu'i", CAI), ("cu'o", MOI), ("cu'u", BAI), ("cy", BY),
  ("da", KOhA), ("da'a", PA), ("da'e", KOhA), ("da'i", UI), ("da'o", DAhO),
  ("da'u", KOhA), ("dai", UI), ("dau", PA), ("de", KOhA), ("de'a", ZAhO),
  ("de'e", KOhA), ("de'i", BAI), ("de'o", VUhU), ("de'u", KOhA), ("dei", KOhA),
  ("di", KOhA), ("di'a", ZAhO), ("di'e", KOhA), ("di'i", TAhE), ("di'o", BAI),
  ("di'u", KOhA), ("do", KOhA), ("do'a", UI), ("do'e", BAI), ("do'i", KOhA),
  ("do'o", KOhA), ("do'u", DOhU), ("doi", DOI), ("du", GOhA), ("du'a", FAhA),
  ("du'e", PA), ("du'i", BAI), ("du'o", BAI), ("du'u", NU), ("dy", BY),
  ("e", A), ("e'a", UI), ("e'e", UI), ("e'i", UI), ("e'o", UI), ("e'u", UI),
  ("ei", UI), ("fa", FA), ("fa'a", FAhA), ("fa'e", BAI), ("fa'i", VUhU),
  ("fa'o", FAhO), ("fa'u", JOI), ("fai", FA), ("fau", BAI), ("fe", FA),
  ("fe'a", VUhU), ("fe'e", FEhE), ("fe'i", VUhU), ("fe'o", COI),
  ("fe'u", FEhU), ("fei", PA), ("fi", FA), ("fi'a", FA), ("fi'e", BAI),
  ("fi'i", COI), ("fi'o", FIhO), ("fi'u", PA), ("fo", FA), ("fo'a", KOhA),
  ("fo'e", KOhA), ("fo'i", KOhA), ("fo'o", KOhA), ("fo'u", KOhA), ("foi", FOI),
  ("fu", FA), ("fu'a", FUhA), ("fu'e", FUhE), ("fu'i", UI), ("fu'o", FUhO),
  ("fu'u", VUhU), ("fy", BY), ("ga", GA), ("ga'a", BAI), ("ga'e", BY),
  ("ga'i", UI), ("ga'o", GAhO), ("ga'u", FAhA), ("gai", PA), ("gau", BAI),
  ("ge", GA), ("ge'a", VUhU), ("ge'e", UI), ("ge'i", GA), ("ge'o", BY),
  ("ge'u", GEhU), ("gei", VUhU), ("gi", GI), ("gi'a", GIhA), ("gi'e", GIhA),
  ("gi'i", GIhA), ("gi'o", GIhA), ("gi'u", GIhA), ("go", GA), ("go'a", GOhA),
  ("go'e", GOhA), ("go'i", GOhA), ("go'o", GOhA), ("go'u", GOhA), ("goi", GOI),
  ("gu", GA), ("gu'a", GUhA), ("gu'e", GUhA), ("gu'i", GUhA), ("gu'o", GUhA),
  ("gu'u", GUhA), ("gy", BY), ("i", I), ("i'a", UI), ("i'e", UI), ("i'i", UI),
  ("i'o", UI), ("i'u", UI), ("ia", UI), ("ie", UI), ("ii", UI), ("io", UI),
  ("iu", UI), ("ja", JA), ("ja'a", NA), ("ja'e", BAI), ("ja'i", BAI),
  ("ja'o", UI), ("jai", JAI), ("jau", PA), ("je", JA), ("je'a", NAhE),
  ("je'e", COI), ("je'i", JA), ("je'o", BY), ("je'u", UI), ("jei", NU),
  ("ji", A), ("ji'a", UI), ("ji'e", BAI), ("ji'i", PA), ("ji'o", BAI),
  ("ji'u", BAI), ("jo", JA), ("jo'a", UI), ("jo'e", JOI), ("jo'i", JOhI),
  ("jo'o", BY), ("jo'u", JOI), ("joi", JOI), ("ju", JA), ("ju'a", UI),
  ("ju'e", JOI), ("ju'i", COI), ("ju'o", UI), ("ju'u", VUhU), ("jy", BY),
  ("ka", NU), ("ka'a", BAI), ("ka'e", CAhA), ("ka'i", BAI), ("ka'o", PA),
  ("ka'u", UI), ("kai", BAI), ("kau", UI), ("ke", KE), ("ke'a", KOhA),
  ("ke'e", KEhE), ("ke'i", GAhO), ("ke'o", COI), ("ke'u", UI), ("kei", KEI),
  ("ki", KI), ("ki'a", UI), ("ki'e", COI), ("ki'i", BAI), ("ki'o", PA),
  ("ki'u", BAI), ("ko", KOhA), ("ko'a", KOhA), ("ko'e", KOhA), ("ko'i", KOhA),
  ("ko'o", KOhA), ("ko'u", KOhA), ("koi", BAI), ("ku", KU), ("ku'a", JOI),
  ("ku'e", KUhE), ("ku'i", UI), ("ku'o", KUhO), ("ku'u", BAI), ("ky", BY),
  ("la", LA), ("la'a", UI), ("la'e", LAhE), ("la'i", LA), ("la'o", ZOI),
  ("la'u", BAI), ("lai", LA), ("lau", LAU), ("le", LE), ("le'a", BAI),
  ("le'e", LE), ("le'i", LE), ("le'o", UI), ("le'u", LEhU), ("lei", LE),
  ("li", LI), ("li'a", UI), ("li'e", BAI), ("li'i", NU), ("li'o", UI),
  ("li'u", LIhU), ("lo", LE), ("lo'a", BY), ("lo'e", LE), ("lo'i", LE),
  ("lo'o", LOhO), ("lo'u", LOhU), ("loi", LE), ("lu", LU), ("lu'a", LAhE),
  ("lu'e", LAhE), ("lu'i", LAhE), ("lu'o", LAhE), ("lu'u", LUhU), ("ly", BY),
  ("ma", KOhA), ("ma'a", KOhA), ("ma'e", BAI), ("ma'i", BAI), ("ma'o", MAhO),
  ("ma'u", PA), ("mai", MAI), ("mau", BAI), ("me", ME), ("me'a", BAI),
  ("me'e", BAI), ("me'i", PA), ("me'o", LI), ("me'u", MEhU), ("mei", MOI),
  ("mi", KOhA), ("mi'a", KOhA), ("mi'e", COI), ("mi'i", BIhI), ("mi'o", KOhA),
  ("mi'u", UI), ("mo", GOhA), ("mo'a", PA), ("mo'e", MOhE), ("mo'i", MOhI),
  ("mo'o", MAI), ("mo'u", ZAhO), ("moi", MOI), ("mu", PA), ("mu'a", UI),
  ("mu'e", NU), ("mu'i", BAI), ("mu'o", COI), ("mu'u", BAI), ("my", BY),
  ("na", NA), ("na'a", BY), ("na'e", NAhE), ("na'i", UI), ("na'o", TAhE),
  ("na'u", NAhU), ("nai", NAI), ("nau", CUhE), ("ne", GOI), ("ne'a", FAhA),
  ("ne'i", FAhA), ("ne'o", VUhU), ("ne'u", FAhA), ("nei", GOhA), ("ni", NU),
  ("ni'a", FAhA), ("ni'e", NIhE), ("ni'i", BAI), ("ni'o", NIhO), ("ni'u", PA),
  ("no", PA), ("no'a", GOhA), ("no'e", NAhE), ("no'i", NIhO), ("no'o", PA),
  ("no'u", GOI), ("noi", NOI), ("nu", NU), ("nu'a", NUhA), ("nu'e", COI),
  ("nu'i", NUhI), ("nu'o", CAhA), ("nu'u", NUhU), ("ny", BY), ("o", A),
  ("o'a", UI), ("o'e", UI), ("o'i", UI), ("o'o", UI), ("o'u", UI), ("oi", UI),
  ("pa", PA), ("pa'a", BAI), ("pa'e", UI), ("pa'i", VUhU), ("pa'o", FAhA),
  ("pa'u", BAI), ("pai", PA), ("pau", UI), ("pe", GOI), ("pe'a", UI),
  ("pe'e", PEhE), ("pe'i", UI), ("pe'o", PEhO), ("pe'u", COI), ("pei", CAI),
  ("pi", PA), ("pi'a", VUhU), ("pi'e", PA), ("pi'i", VUhU), ("pi'o", BAI),
  ("pi'u", JOI), ("po", GOI), ("po'e", GOI), ("po'i", BAI), ("po'o", UI),
  ("po'u", GOI), ("poi", NOI), ("pu", PU), ("pu'a", BAI), ("pu'e", BAI),
  ("pu'i", CAhA), ("pu'o", ZAhO), ("pu'u", NU), ("py", BY), ("ra", KOhA),
  ("ra'a", BAI), ("ra'e", PA), ("ra'i", BAI), ("ra'o", RAhO), ("ra'u", UI),
  ("rai", BAI), ("rau", PA), ("re", PA), ("re'a", VUhU), ("re'e", UI),
  ("re'i", COI), ("re'o", FAhA), ("re'u", ROI), ("rei", PA), ("ri", KOhA),
  ("ri'a", BAI), ("ri'e", UI), ("ri'i", BAI), ("ri'o", VUhU), ("ri'u", FAhA),
  ("ro", PA), ("ro'a", UI), ("ro'e", UI), ("ro'i", UI), ("ro'o", UI),
  ("ro'u", UI), ("roi", ROI), ("ru", KOhA), ("ru'a", UI), ("ru'e", CAI),
  ("ru'i", TAhE), ("ru'o", BY), ("ru'u", FAhA), ("ry", BY), ("sa", SA),
  ("sa'a", UI), ("sa'e", UI), ("sa'i", VUhU), ("sa'o", VUhU), ("sa'u", UI),
  ("sai", CAI), ("sau", BAI), ("se", SE), ("se'a", UI), ("se'e", BY),
  ("se'i", UI), ("se'o", UI), ("se'u", SEhU), ("sei", SEI), ("si", SI),
  ("si'a", UI), ("si'e", MOI), ("si'i", VUhU), ("si'o", NU), ("si'u", BAI),
  ("so", PA), ("so'a", PA), ("so'e", PA), ("so'i", PA), ("so'o", PA),
  ("so'u", PA), ("soi", SOI), ("su", SU), ("su'a", UI), ("su'e", PA),
  ("su'i", VUhU), ("su'o", PA), ("su'u", NU), ("sy", BY), ("ta", KOhA),
  ("ta'a", COI), ("ta'e", TAhE), ("ta'i", BAI), ("ta'o", UI), ("ta'u", UI),
  ("tai", BAI), ("tau", LAU), ("te", SE), ("te'a", VUhU), ("te'e", FAhA),
  ("te'o", PA), ("te'u", TEhU), ("tei", TEI), ("ti", KOhA), ("ti'a", FAhA),
  ("ti'e", UI), ("ti'i", BAI), ("ti'o", SEI), ("ti'u", BAI), ("to", TO),
  ("to'a", BY), ("to'e", NAhE), ("to'i", TO), ("to'o", FAhA), ("to'u", UI),
  ("toi", TOI), ("tu", KOhA), ("tu'a", LAhE), ("tu'e", TUhE), ("tu'i", BAI),
  ("tu'o", PA), ("tu'u", TUhU), ("ty", BY), ("u", A), ("u'a", UI), ("u'e", UI),
  ("u'i", UI), ("u'o", UI), ("u'u", UI), ("ua", UI), ("ue", UI), ("ui", UI),
  ("uo", UI), ("uu", UI), ("va", VA), ("va'a", VUhU), ("va'e", MOI),
  ("va'i", UI), ("va'o", BAI), ("va'u", BAI), ("vai", PA), ("vau", VAU),
  ("ve", SE), ("ve'a", VEhA), ("ve'e", VEhA), ("ve'i", VEhA), ("ve'o", VEhO),
  ("ve'u", VEhA), ("vei", VEI), ("vi", VA), ("vi'a", VIhA), ("vi'e", VIhA),
  ("vi'i", VIhA), ("vi'o", COI), ("vi'u", VIhA), ("vo", PA), ("vo'a", KOhA),
  ("vo'e", KOhA), ("vo'i", KOhA), ("vo'o", KOhA), ("vo'u", KOhA), ("voi", NOI),
  ("vu", VA), ("vu'a", FAhA), ("vu'e", UI), ("vu'i", LAhE), ("vu'o", VUhO),
  ("vu'u", VUhU), ("vy", BY), ("xa", PA), ("xe", SE), ("xi", XI), ("xo", PA),
  ("xu", UI), ("xy", BY), ("y", Y), ("y'y", BY), ("za", ZI), ("za'a", UI),
  ("za'e", BAhE), ("za'i", NU), ("za'o", ZAhO), ("za'u", PA), ("zai", LAU),
  ("zau", BAI), ("ze", PA), ("ze'a", ZEhA), ("ze'e", ZEhA), ("ze'i", ZEhA),
  ("ze'o", FAhA), ("ze'u", ZEhA), ("zei", ZEI), ("zi", ZI), ("zi'e", ZIhE),
  ("zi'o", KOhA), ("zo", ZO), ("zo'a", FAhA), ("zo'e", KOhA), ("zo'i", FAhA),
  ("zo'o", UI), ("zo'u", ZOhU), ("zoi", ZOI), ("zu", ZI), ("zu'a", FAhA),
  ("zu'e", BAI), ("zu'i", KOhA), ("zu'o", NU), ("zu'u", UI), ("zy", BY)]
