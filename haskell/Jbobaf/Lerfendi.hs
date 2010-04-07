module Jbobaf.Lerfendi (lerfendi) where
 import Char
 import List
 import Jbobaf.Internals
 import Jbobaf.Tamcux
 import Jbobaf.Valsi
 import Jbobaf.Vlatai

 lerfendi :: String -> Tamcux [Either String Valsi]
 lerfendi = lerfendi' Fadni

{- Miscellaneous features of lerfendi:
  - If the text after a FAhO is nonempty, it is returned as a Left String.
  - If an active ZOI is followed by an invalid word (i.e., a Left String), it
    will be used as the delimiter, and the ZOI will only be terminated by an
    identical non-word or by end-of-text.
  - A ZOI quote always consists of four parts: the ZOI, the delimiter, the
    contents (as a Left String, possibly empty), and the delimiter again.
    However, if the end of the text is reached while searching for the closing
    delimiter, the contents of the ZOI will be everything in the text after the
    opening delimiter, and no closing delimiter will be present in the returned
    list.
  - The contents of a ZOI quote are preserved character-for-character, except
    that leading & trailing whitespace & periods are removed.
  - Exception to the above: If the opening delimiter of a ZOI quote has text
    after it in its "chunk" (which it shouldn't), that text has already been
    normalized & split into words by the time @lerfendi@ realizes this, and so
    the concatenated string representations of the items in the rest of the
    chunk (@valsi@ for Right Valsi, the string itself for Left String) will
    form the beginning of the ZOI text (unless the ending delimiter is also in
    the chunk, in which case only the items up to the delimiter will be made
    into ZOI text).
-}

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

 lerfendi' :: Flezvalei -> String -> Tamcux [Either String Valsi]
 lerfendi' makfa str =
  let (ca, ba) = spicpa str
  in if null ca then return [] else fadgau ca >>= \ca' -> case ca' of
   Just [] -> lerfendi' makfa ba
   Just x -> fendi x >>= mafygau makfa ba
   Nothing -> Left ca~:lerfendi' (makfa < ErrorQuote ?: Fadni :? ErrorQuote) ba

----------------------------------------

 mafygau :: Flezvalei -> String -> [Either String Valsi]
  -> Tamcux [Either String Valsi]

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
    else let trail = concatMap (either id valsi) alsi ++ ba
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

 fendi :: String -> Tamcux [Either String Valsi]
 fendi [] = return []
 fendi (',':xs) = fendi xs
  -- This ^^ was at one point possible due to some other bit of code.  Given
  -- the implementation of fadgau, can this still occur?

 fendi str | isC (last str) = do
  dotty <- isOpt Use_dotside
  case (dotty, findLa str) of
   (False, Just n) -> fendi (take n str) ~~ mkCmevla (drop n str)
   _ -> mkCmevla str

 fendi str | toLower (last str) == 'y' = case finalMa'osmi str of
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
		 (c, d:ys) ->
		  if head (head ys) == '\'' then return [Left str]
		   -- TO DO: Make sure `d' isn't capitalized!  It is also an
		   -- error if `head ys' begins with a non-initial consonant
		   -- cluster.
		  else brivlate alpha (pre ++ c ++ [d]) ~~ fendi (concat ys)
	    in if null alvocs || null (filter isUpper $ last alvocs)
	       then case span (null . filter isUpper) omsyls of
		     (_, []) -> brivlate alpha omsyls
		     (_, [_]) -> return [Left str]
		      -- only last syllable emphasized; error?
		     (_, [_, _]) -> brivlate alpha omsyls
		      -- TO DO: Make sure the last syllable isn't capitalized!
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

 ma'ocpa :: String -> Tamcux [Either String Valsi]
 ma'ocpa str = mp $ 0 : findIndices isC str
  where mp [] = mkCmavo str
	mp (0:0:xs) = mp (0:xs)
	mp (a:b:xs) = mkCmavo (take (b-a) $ drop a str) ~~ mp (b:xs)
	mp [a] = mkCmavo $ drop a str

 brivlate :: String -> [String] -> Tamcux [Either String Valsi]
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
  fendi a ~~ if b' && xubriv then mkBrivla beta
	     else if toLower (b1 !! 1) == 'y'
	     then mkCmavo b ~~ mkCmavo b1 ~~ fendi (concat bxs)
	     else return [Left beta]

 esv2str :: Either String Valsi -> String
 esv2str (Left str) = str
 esv2str (Right v) = valsi v

 xudenpa :: Char -> Bool
 xudenpa c = isSpace c || c == '.'

 mkCmevla, mkCmavo, mkBrivla :: String -> Tamcux [Either String Valsi]
 mkCmevla str = toCmevla str >>= return . (: []) . maybe (Left str) Right
 mkCmavo  str = toCmavo  str >>= return . (: []) . maybe (Left str) Right
 mkBrivla str = toBrivla str >>= return . (: []) . maybe (Left str) Right
