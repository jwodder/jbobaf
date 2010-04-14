module Jbobaf.Lerfendi (lerfendi) where
 import Char
 import List
 import Jbobaf.Internals
 import Jbobaf.Jvacux
 import Jbobaf.Valsi
 import Jbobaf.Vlatai

 -- |@lerfendi@ takes a string of Lojban text and splits it into a mixture of
 -- /valsi/s and invalid character sequences.  The string is processed one
 -- "chunk" at a time, where a chunk is a nonempty maximal string of non-space,
 -- non-period characters.  Each chunk is normalized with @fadgau@ and split up
 -- into words, which are then made into 'Valsi' objects.  Anything that can't
 -- be split is left as a 'String'.  In general, if a chunk contains anything
 -- invalid, how the rest of the chunk gets split up should be considered
 -- undefined, undocumented, and subject to random change.
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
 -- * If the text after a FAhO is nonempty (after stripping leading -- but not
 --   trailing -- whitespace & periods), it is returned as a @Left String@.
 --
 -- * The end of a ZOI quote is searched for by reading a chunk at a time and
 --   performing normal word splitting on it.  If the first word in the chunk
 --   equals the delimiter, the end has been found; otherwise, the raw text of
 --   the chunk (unnormalized, with leading spaces & periods) is absorbed into
 --   the ZOI-text buffer and processing moves on to the next chunk.
 --
 -- * If a ZOI is followed by an invalid word (i.e., a @Left String@), this
 --   invalid string will be used as the delimiter, and the ZOI will only be
 --   terminated by an identical non-word or by end-of-text.
 --
 -- * A ZOI quote always consists of four parts: the ZOI, the delimiter, the
 --   contents (as a @Left String@, possibly empty), and the delimiter again.
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
 lerfendi = lerfendi' Fadni

----------------------------------------

 data Flezvalei =
  Fadni
  | After_ZO_ZEI
    -- ZO and ZEI are essentially the same in how they treat the next word,
    -- right?
  | After_ZOI (Maybe String) String
  | ErrorQuote
  | After_ZO_ZEI_error
  | After_ZOI_error (Maybe String) String
  deriving (Eq, Ord, Read, Show)

----------------------------------------

 lerfendi' :: Flezvalei -> String -> Jvacux [Either String Valsi]
 lerfendi' makfa str =
  let (ca, ba) = spicpa str
  in if null ca then return [] else fadgau ca >>= \ca' -> case ca' of
   Just [] -> lerfendi' makfa ba
   Just x -> fendi x >>= mafygau makfa ba
   Nothing -> Left ca~:lerfendi' (makfa < ErrorQuote ?: Fadni :? ErrorQuote) ba

----------------------------------------

 mafygau :: Flezvalei -> String -> [Either String Valsi]
  -> Jvacux [Either String Valsi]

 -- How to search for ending ZOI delimiters: While the end has not been found,
 -- get the next chunk from the stream and split it into words.  If the first
 -- word equals the delimiter, the end has been found.  Otherwise, consume the
 -- raw chunk from the stream and keep searching.

 mafygau (After_ZOI (Just d) trail) ba [] = do
  let (ca, ba') = spicpa ba
  if null ca
   then return (null trail ?: [] :? [Left trail])
   else do
    ca' <- fadgau ca
    vals <- maybe (return []) fendi ca'
    case (ca', vals) of
     (Just x@(_:_), v:alsi)
      | esv2str v == d -> Left trail ~: v ~: mafygau Fadni ba' alsi
     _ -> let (a, a') = span xudenpa ba
	      (b, b') = break xudenpa a'
	  in mafygau (After_ZOI (Just d) (trail ++ a ++ b)) b' []

 mafygau (After_ZOI_error (Just d) trail) ba [] = do
  let (ca, ba') = spicpa ba
  if null ca
   then return (null trail ?: [] :? [Left trail])
   else do
    ca' <- fadgau ca
    vals <- maybe (return []) fendi ca'
    case (ca', vals) of
     (Just x@(_:_), v:alsi)
      | esv2str v == d -> Left trail ~: v ~: mafygau ErrorQuote ba' alsi
     _ -> let (a, a') = span xudenpa ba
	      (b, b') = break xudenpa a'
	  in mafygau (After_ZOI_error (Just d) (trail ++ a ++ b)) b' []

 mafygau makfa ba [] = lerfendi' makfa ba

 mafygau (After_ZOI Nothing []) ba (v:alsi) = v ~:
  mafygau (After_ZOI (Just $ esv2str v) [])
   (null alsi ?: dropWhile xudenpa ba :? ba) alsi

 mafygau (After_ZOI_error Nothing []) ba (v:alsi) = v ~:
  mafygau (After_ZOI_error (Just $ esv2str v) [])
   (null alsi ?: dropWhile xudenpa ba :? ba) alsi

 mafygau (After_ZOI (Just d) trail) ba (v:alsi) =
  let v' = esv2str v
  in if v' == d then Left trail ~: v ~: mafygau Fadni ba alsi
     else mafygau (After_ZOI (Just d) (trail ++ v')) ba alsi

 mafygau (After_ZOI_error (Just d) trail) ba (v:alsi) =
  let v' = esv2str v
  in if v' == d then Left trail ~: v ~: mafygau ErrorQuote ba alsi
     else mafygau (After_ZOI_error (Just d) (trail ++ v')) ba alsi

 mafygau After_ZO_ZEI ba (v:alsi) = v ~: mafygau Fadni ba alsi

 mafygau After_ZO_ZEI_error ba (v:alsi) = v ~: mafygau ErrorQuote ba alsi

 mafygau makfa ba (Left s : alsi) = Left s ~: mafygau makfa ba alsi

 mafygau Fadni ba (Right v : alsi) = Right v ~: case valsi v of
  "zo" -> mafygau After_ZO_ZEI ba alsi
  "zei" -> mafygau After_ZO_ZEI ba alsi
  "zoi" -> mafygau (After_ZOI Nothing []) ba alsi
  "la'o" -> mafygau (After_ZOI Nothing []) ba alsi
  "lo'u" -> mafygau ErrorQuote ba alsi
  "fa'o" -> do
   ignore <- isOpt Ignore_FAhO
   if ignore then mafygau Fadni ba alsi
    else let trail = concatMap esv2str alsi ++ dropWhile xudenpa ba
	 in return (null trail ?: [] :? [Left trail])
  _ -> mafygau Fadni ba alsi

 mafygau ErrorQuote ba (Right v : alsi) = Right v ~: case valsi v of
  "zo" -> do
   disabled <- isOpt LOhU_disables_ZO
   mafygau (disabled ?: ErrorQuote :? After_ZO_ZEI_error) ba alsi
  "zei" -> do
   disabled <- isOpt LOhU_disables_ZEI
   mafygau (disabled ?: ErrorQuote :? After_ZO_ZEI_error) ba alsi
  "zoi" -> do
   disabled <- isOpt LOhU_disables_ZOI
   mafygau (disabled ?: ErrorQuote :? After_ZOI_error Nothing []) ba alsi
  "la'o" -> do
   disabled <- isOpt LOhU_disables_ZOI
   mafygau (disabled ?: ErrorQuote :? After_ZOI_error Nothing []) ba alsi
  "le'u" -> mafygau Fadni ba alsi
  "fa'o" -> do
   disabled <- isOpt LOhU_disables_FAhO
   ignored <- isOpt Ignore_FAhO
   if disabled || ignored then mafygau ErrorQuote ba alsi
    else let trail = concatMap (either id valsi) alsi ++ ba
	 in return (null trail ?: [] :? [Left trail])
  _ -> mafygau ErrorQuote ba alsi

----------------------------------------

 fendi :: String -> Jvacux [Either String Valsi]
 fendi [] = return []
 fendi (',':xs) = fendi xs
  -- This ^^ was at one point possible due to some other bit of code.  Given
  -- the implementation of fadgau, can this still occur?

 fendi str | isC (last str) = do
  dotty <- isOpt Use_dotside
  case (dotty, findLa str) of
   (False, Just (a, b, z)) -> fendi a ~~ mkCmavo b ~~ mkCmevla z
   _ -> mkCmevla str

 fendi str | last str == 'y' = case finalMa'osmi str of
  Just n -> fendi (take n str) ~~ mkCmavo (drop n str)
  Nothing -> return [Left str]

 fendi str = case findCC str of
  Nothing -> ma'ocpa str
  Just n -> let (alpha, omega) = splitAt n str
		alvocs = filter voc $ syllabicate alpha
		omsyls = syllabicate omega
		findUltima pre s = case break voc s of
		 (_, []) -> return [Left str]  -- This should never happen.
		 (_, [_]) -> brivlate alpha omsyls
		 (c, d:e:ys) -> if emphed d || head e == '\'' || has_C_C e
				then shiftCy alpha omsyls
				else brivlate alpha (pre ++ c ++ [d])
				      ~~ fendi (concat $ e:ys)
	    in if null alvocs || not (emphed $ last alvocs)
	       then case break emphed omsyls of
		     (_, [])       -> brivlate alpha omsyls
		     (_, [_])      -> shiftCy alpha omsyls
		     (_, [_, ult]) -> (emphed ult ?: shiftCy :? brivlate)
				       alpha omsyls
		     (a, b:xs) -> findUltima (a ++ [b]) xs
	       else findUltima [] omsyls

----------------------------------------

 spicpa :: String -> (String, String)  -- gets the next "word" from the string
 spicpa = break xudenpa . dropWhile xudenpa

 finalMa'osmi :: String -> Maybe Int
 -- How should this handle commas?
 finalMa'osmi = fmas 0 0 False False
  -- first boolean: whether the previous character was a consonant
  -- second boolean: whether the most recent consonant was preceded by a
  --  consonant
  where fmas _     _   _  True  []     = Nothing
	fmas start _   _  False []     = Just start
	fmas _     pos tf _     (c:xs) | isC c = fmas pos (pos+1) True tf xs
	fmas start pos _  tf    (_:xs) = fmas start (pos+1) False tf xs

 ma'ocpa :: String -> Jvacux [Either String Valsi]
 ma'ocpa str = mp $ 0 : findIndices isC str
  where mp [] = mkCmavo str
	mp (0:0:xs) = mp (0:xs)
	mp (a:b:xs) = mkCmavo (take (b-a) $ drop a str) ~~ mp (b:xs)
	mp [a] = mkCmavo $ drop a str

 brivlate :: String -> [String] -> Jvacux [Either String Valsi]
 brivlate pre body@(b1:bxs) = do
  tosmabru <- xulujvo' $ 't':'o':concat body
  let allInit (c1:c2:xs) = if isV c2 then True
			   else if isCC [c1, c2] then allInit (c2:xs)
			   else False
  let (a, b, b') = if allInit b1 && length (filter voc body) > 1 && not tosmabru
   -- a = parts of `pre' not to prepend to body
   -- b = stuff to prepend to body
   -- b' = whether `pre' met our needs
		   then (pre, [], True)
		   else case finalMa'osmi pre of
		    Just i -> let (pa, pb) = splitAt i pre
			      in if length (filter (\c -> isC c || isV c) pb)<4
				 then (pa, pb, True)
				 else (pre, [], False)
		    Nothing -> (pre, [], False)
  let beta = b ++ concat body
  xubriv <- xubrivla' beta
  fendi a ~~ if b' && xubriv then mkBrivla beta else shiftCy b body

 esv2str :: Either String Valsi -> String
 esv2str (Left str) = str
 esv2str (Right v) = valsi v

 xudenpa :: Char -> Bool
 xudenpa c = isSpace c || c == '.'

 shiftCy :: String -> [String] -> Jvacux [Either String Valsi]
 shiftCy pre (cy@[_,'y']:rest) = fendi pre ~~ mkCmavo cy ~~ fendi (concat rest)
 shiftCy pre blob = return [Left $ pre ++ concat blob]
 -- The `pre' argument exists so that it can be prepended to an invalid string
 -- so that the output doesn't look like invalid words were split up
 -- excessively.

 emphed :: String -> Bool
 emphed = not . null . filter isUpper

 mkCmevla, mkCmavo, mkBrivla :: String -> Jvacux [Either String Valsi]
 mkCmevla [] = return []
 mkCmevla str = toCmevla str >>= return . (: []) . maybe (Left str) Right
 mkCmavo  [] = return []
 mkCmavo  str = toCmavo  str >>= return . (: []) . maybe (Left str) Right
 mkBrivla [] = return []
 mkBrivla str = toBrivla str >>= return . (: []) . maybe (Left str) Right
