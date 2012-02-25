module Jbobaf.Lerfendi (lerfendi) where
 import Data.Char
 import Control.Monad.Identity
 import Control.Monad.Reader
 import Jbobaf.Canti
 import Jbobaf.Jitro
 import Jbobaf.Valsi
 import Jbobaf.Vlatai

 -- |@lerfendi@ takes a string of Lojban text and splits it into a mixture of
 -- /valsi/s and invalid character sequences.  The string is processed one
 -- "chunk" at a time, where a chunk is a nonempty maximal string of non-space,
 -- non-period characters.  Each chunk is normalized with @fadgau@ and split up
 -- into words, which are then made into 'Valsi' objects.  Anything that
 -- returns 'Nothing' from normalization or that just can't be split up is left
 -- as a 'String'.  In general, if a chunk contains anything invalid, how the
 -- chunk gets handled & split up should be considered undefined, undocumented,
 -- and subject to random change.
 --
 -- After splitting, the contents of a chunk are checked for magic words that
 -- impact lexing (ZOI and FAhO) and magic words that impact magic words that
 -- impact lexing (ZO, ZEI, and LOhU).  Whether LOhU cancels anything inside it
 -- and whether lexing should stop after a FAhO or not can be configured via
 -- the 'Jvacux' options 'LOhU_disables_ZO', 'LOhU_disables_ZEI',
 -- 'LOhU_disables_ZOI', 'LOhU_disables_FAhO', and 'Ignore_FAhO'; by default,
 -- none of these are in effect.
 --
 -- Miscellaneous features and things to watch out for:
 --
 -- * Despite the tendency of Error monads to force strict evaluation,
 --   @lerfendi@ uses a couple of graceless hacks to achieve non-strict
 --   evaluation internally, so you can process its return values while still
 --   feeding it input.
 --
 -- * If the text after a FAhO is nonempty after stripping leading whitespace &
 --   periods, it is returned as a @Left String@.
 --
 -- * The end of a ZOI quote is searched for by reading a chunk at a time and
 --   performing normal word splitting on it.  If the first word in the chunk
 --   equals the delimiter, the end has been found; otherwise, the raw text of
 --   the chunk (unnormalized, with leading spaces & periods preserved) is
 --   absorbed into the ZOI text buffer and processing moves on to the next
 --   chunk.
 --
 -- * If a ZOI is followed by an invalid word (i.e., a @Left String@), this
 --   invalid string will be used as the delimiter, and the ZOI will only be
 --   terminated by an identical non-word or by end-of-text.
 --
 -- * A ZOI quote is always returned in four parts: the ZOI, the delimiter, the
 --   contents (a @Left String@, possibly empty), and the delimiter again.
 --   However, if the end of the text is reached while searching for the
 --   closing delimiter, the contents of the ZOI will be everything in the text
 --   after the opening delimiter, and no closing delimiter will be present in
 --   the returned list.
 --
 -- * The contents of a ZOI quote are preserved character-for-character, except
 --   that leading & trailing whitespace & periods are removed.  EXCEPTION: If
 --   the opening delimiter of a ZOI quote has text after it in its chunk
 --   (which it shouldn't anyway), that text has already been normalized &
 --   split into words by the time @lerfendi@ realizes this, and so the
 --   concatenated string representations of the items in the rest of the chunk
 --   (@valsi@ for @Right Valsi@, the string itself for @Left String@) will
 --   form the beginning of the ZOI text (unless the ending delimiter is also
 --   in the chunk, in which case only the items up to the delimiter will be
 --   made into ZOI text).
 --
 -- * As far as @lerfendi@ is concerned, ZO and ZEI differ only in spelling.
 --   Whether this actually leads to any problems has yet to be determined.

 lerfendi :: String -> Jvacux [Either String Valsi]
 lerfendi = mapReaderT (return . runIdentity) . lerfendi' (Fadni, False)

----------------------------------------

 type Jvacux' a = JvacuxT Identity a

 data Flezvalei = Fadni | After_ZO_ZEI | After_ZOI (Maybe String) String
  deriving (Eq, Ord, Read, Show)
  -- Flezvalei are accompanied by a Bool that is true iff we're currently
  -- inside an error quote.

----------------------------------------

 lerfendi' :: (Flezvalei, Bool) -> String -> Jvacux' [Either String Valsi]
 lerfendi' makfa@(_, lohu) str =
  let (ca, ba) = spicpa str
  in if null ca then return []
     else mapReaderT return (fadgau ca) >>= \ca' -> case ca' of
      Left  _  -> Left ca ~: lerfendi' (Fadni, lohu) ba
      Right [] -> lerfendi' makfa ba
      Right x  -> fendi x >>= mafygau makfa ba

----------------------------------------

 mafygau :: (Flezvalei, Bool) -> String -> [Either String Valsi]
  -> Jvacux' [Either String Valsi]

 -- How to search for ending ZOI delimiters: While the end has not been found,
 -- get the next chunk from the stream and split it into words.  If the first
 -- word equals the delimiter, the end has been found.  Otherwise, consume the
 -- raw chunk from the stream and keep searching.

 mafygau (After_ZOI (Just d) trail, lohu) ba [] =
  let (ca, ba') = spicpa ba
  in if null ca
     then return $ null trail ?: [] :? [Left trail]
     else do
      ca'  <- mapReaderT return $ fadgau ca
      vals <- either (const $ return []) fendi ca'
      case (ca', vals) of
       (Right (_:_), v:alsi)
        | esv2str v == d -> Left trail ~: v ~: mafygau (Fadni, lohu) ba' alsi
       _ -> let (a, a') = span xudenpa ba
		(b, b') = break xudenpa a'
	    in mafygau (After_ZOI (Just d) (trail ++ a ++ b), lohu) b' []

 mafygau makfa ba [] = lerfendi' makfa ba

 mafygau (After_ZOI Nothing [], lohu) ba (v:alsi) =
  v ~: mafygau (After_ZOI (Just $ esv2str v) [], lohu)
	(null alsi ?: dropWhile xudenpa ba :? ba) alsi

 mafygau (After_ZOI (Just d) trail, lohu) ba (v:alsi) =
  let v' = esv2str v
  in if v' == d
     then Left trail ~: v ~: mafygau (Fadni, lohu) ba alsi
     else mafygau (After_ZOI (Just d) (trail ++ v'), lohu) ba alsi

 mafygau (After_ZO_ZEI, lohu) ba (v:alsi) = v ~: mafygau (Fadni, lohu) ba alsi

 mafygau makfa ba (Left s : alsi) = Left s ~: mafygau makfa ba alsi

 mafygau (Fadni, False) ba (Right v : alsi) = Right v ~: case valsi v of
  "zo"   -> mafygau (After_ZO_ZEI, False) ba alsi
  "zei"  -> mafygau (After_ZO_ZEI, False) ba alsi
  "zoi"  -> mafygau (After_ZOI Nothing [], False) ba alsi
  "la'o" -> mafygau (After_ZOI Nothing [], False) ba alsi
  "lo'u" -> mafygau (Fadni, True) ba alsi
  "fa'o" -> do ignore <- isOpt Ignore_FAhO
	       if ignore then mafygau (Fadni, False) ba alsi
		  else let trail = concatMap esv2str alsi++dropWhile xudenpa ba
		       in return (null trail ?: [] :? [Left trail])
  _ -> mafygau (Fadni, False) ba alsi

 mafygau c@(Fadni, True) ba (Right v : alsi) = Right v ~: case valsi v of
  "zo"   -> do disabled <- isOpt LOhU_disables_ZO
	       mafygau (disabled ?: c :? (After_ZO_ZEI, True)) ba alsi
  "zei"  -> do disabled <- isOpt LOhU_disables_ZEI
	       mafygau (disabled ?: c :? (After_ZO_ZEI, True)) ba alsi
  "zoi"  -> do disabled <- isOpt LOhU_disables_ZOI
	       mafygau (disabled ?: c :? (After_ZOI Nothing [], True)) ba alsi
  "la'o" -> do disabled <- isOpt LOhU_disables_ZOI
	       mafygau (disabled ?: c :? (After_ZOI Nothing [], True)) ba alsi
  "le'u" -> mafygau (Fadni, False) ba alsi
  "fa'o" -> do disabled <- isOpt LOhU_disables_FAhO
	       ignored <- isOpt Ignore_FAhO
	       if disabled || ignored then mafygau c ba alsi
		  else let trail = concatMap esv2str alsi ++ ba
		       in return (null trail ?: [] :? [Left trail])
  _ -> mafygau c ba alsi

----------------------------------------

 fendi :: String -> Jvacux' [Either String Valsi]
 fendi [] = return []
 fendi (',':xs) = fendi xs
  -- This ^^ was at one point possible due to some other bit of code.  Can this
  -- still occur?

 fendi str | isC (last str) = do
  dotty <- isOpt Use_dotside
  case (dotty, findLa str) of
   (False, Just (a, b, z)) -> fendi a ~~ mkCmavo b ~~ mkCmevla z
   _ -> mkCmevla str

 fendi str | last str == 'y' = case finalMa'osmi str of
  Just (pre, post) -> fendi pre ~~ mkCmavo post
  Nothing -> return [Left str]

 fendi str = case findC_C str of
  Just n -> let (alpha, omega) = splitAt n str
		alvocs = filter voc $ syllabicate alpha
		omsyls = syllabicate omega
		findUltima pre s = case break voc s of
		 -- pre = portion of omsyls through the first emphed syllable
		 -- s = rest of omsyls
		 (_, [])     -> shiftCy alpha omsyls
		 (_, [u])    -> (emphed u ?: shiftCy :? brivlate) alpha omsyls
		 (c, d:e:ys) -> if emphed d || head e `elem` "'," || has_C_C e
				then shiftCy alpha omsyls
				else brivlate alpha (pre ++ c ++ [d])
				      ~~ fendi (concat $ e:ys)
	    in if not (null alvocs) && emphed (last alvocs)
	       then findUltima [] omsyls
	       else case break emphed omsyls of
		     (_, [])   -> brivlate alpha omsyls
		     (a, b:xs) -> findUltima (a ++ [b]) xs
  Nothing -> ma'ocpa str
  where ma'ocpa []  = return []
	ma'ocpa str = mkCmavo (c ++ v) ~~ ma'ocpa w
	 where (c, v') = span isC str; (v, w) = break isC v'

----------------------------------------

 spicpa :: String -> (String, String)  -- gets the next "word" from the string
 spicpa = break xudenpa . dropWhile xudenpa

 finalMa'osmi :: String -> Maybe (String, String)
 finalMa'osmi str = case break isC (reverse str) of
  ([], _)     -> Nothing
  (_, [])     -> Just ([], str)
  (_, [_])    -> Just ([], str)
  (xs, a:b:ys) | not (isC b) -> Just (reverse (b:ys), reverse (xs ++ [a]))
	       | otherwise   -> Nothing

 brivlate :: String -> [String] -> Jvacux' [Either String Valsi]
 brivlate pre body@(b1:_) = do
  -- pre = stuff before the consonant cluster
  -- body = syllables from the consonant cluster through the ultima
  Right slinku'i <- mapReaderT return $ xulujvo' $ 't':'o':concat body
  let allInit (c1:c2:xs) = if isV c2 then True
			   else if isCC [c1, c2] then allInit (c2:xs)
			   else False
  let (a, b, b') = if allInit b1 && length (filter voc body) > 1 && not slinku'i
   -- a = parts of `pre' not to prepend to body
   -- b = stuff to prepend to body
   -- b' = whether `pre' met our needs
		   then (pre, [], True)
		   else case finalMa'osmi pre of
		    Just (pa, pb) -> length (filter (`notElem` "',y") pb) < 4
				      ?: (pa, pb, True) :? (pre, [], False)
		    Nothing -> (pre, [], False)
  let beta = b ++ concat body
  Right xubriv <- mapReaderT return $ xubrivla' beta
  fendi a ~~ if b' && xubriv then mkBrivla beta else shiftCy b body

 esv2str :: Either String Valsi -> String
 esv2str (Left str) = str
 esv2str (Right v) = valsi v

 xudenpa :: Char -> Bool
 xudenpa c = isSpace c || c == '.'

 shiftCy :: String -> [String] -> Jvacux' [Either String Valsi]
 shiftCy pre (cy@[_,'y']:rest) = fendi pre ~~ mkCmavo cy ~~ fendi (concat rest)
 shiftCy pre blob = return [Left $ pre ++ concat blob]
  -- The `pre' argument exists so that it can be prepended to an invalid string
  -- so the output doesn't look like invalid words were split up excessively.

 emphed :: String -> Bool
 emphed = any isUpper

 mkCmevla, mkCmavo, mkBrivla :: String -> Jvacux' [Either String Valsi]
 mkCmevla = mkValsi toCmevla
 mkCmavo  = mkValsi toCmavo
 mkBrivla = mkValsi toBrivla

 mkValsi :: (String -> Jvacux Valsi) -> String -> Jvacux' [Either String Valsi]
 mkValsi _ []  = return []
 mkValsi f str = mapReaderT (return . either (const [Left str]) ((:[]) . Right))
  (f str)
