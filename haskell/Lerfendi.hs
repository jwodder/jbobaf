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

 mafygau (After_ZOI (Just d) trail) ba [] = ???
 mafygau (After_ZOI_error (Just d) trail) ba [] = ???
 mafygau makfa ba [] = lerfendi' makfa ba

 mafygau (After_ZOI Nothing trail) ba (v:alsi) = ???
 mafygau (After_ZOI_error Nothing trail) ba (v:alsi) = ???
 mafygau (After_ZOI (Just d) trail) ba (v:alsi) = ???
 mafygau (After_ZOI_error (Just d) trail) ba (v:alsi) = ???

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
   ignored <- Ignore_FAhO
   if disabled || ignored then mafygau ErrorQuote ba alsi
   else let trail = concatMap (either id valsi) alsi ++ ba
	in return (null trail ?: [] :? [Left trail])
  _ -> mafygau ErrorQuote ba alsi

----------------------------------------

 fendi :: String -> Tamcux [Either String Valsi]
 fendi [] = []
 fendi (',':xs) = fendi xs

 fendi str | isC (last str) = do
  dotty <- isOpt Use_dotside
  case (dotty, findLa ca) of
   (False, Just n) -> fendi (take n str) ~~ mkCmevla (drop n str)
   _ -> mkCmevla str

 fendi str | toLower (last str) == 'y' =
  case finalMa'osmi str of
   Just n -> fendi (take n str) ~~ mkCmavo (drop n str)
   Nothing -> return [Left str]

 fendi str = case findCC str of
  Nothing -> ma'ocpa str
  Just n -> let (alpha, omega) = splitAt n str
		omsyls = syllabicate omega
	    in case span (null . filter (\c -> isV c && isUpper c)) omsyls of
	     (_, []) -> brivlate alpha omsyls
	     (_, [_]) -> [Left str]  -- only last syllable emphasized; error?
	     (_, [_, _]) -> brivlate alpha omsyls
	      -- make sure the last syllable isn't capitalized
	     (a, b:xs) -> case break voc xs of
	      (_, []) -> [Left str]  -- This is never supposed to happen.
	      (_, [_]) -> brivlate alpha omsyls
	      (c, d:ys) -> 
	       -- make sure `d' isn't capitalized
	       if head (head ys) == '\'' then [Left str]
		-- It is also an error if `head ys' begins with a non-initial
		-- consonant cluster.
	       else brivlate alpha (a ++ b:c ++ [d]) ~~ fendi (concat ys)

----------------------------------------

 spicpa :: String -> (String, String)  -- gets the next "word" from the string
 spicpa str = break (\c -> isSpace c || c == '.')
  $ dropWhile (\c -> isSpace c || c == '.')

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

 ma'ocpa :: String -> [Either String Valsi]
 ma'ocpa str = mp $ 0 : findIndices isC str
  where mp [] = mkCmavo str
	mp (0:0:xs) = mp (0:xs)
	mp (a:b:xs) = mkCmavo (take (b-a) $ drop a str) ~~ mp (b:xs)
	mp [a] = mkCmavo $ drop a str

 voc :: String -> Bool
 -- Tests whether a syllable contains a non-Y vowel and is thus accentable.
 -- The short name is solely for aesthetic reasons.
 voc = not . null . filter isV

 syllabicate :: String -> [String]
 -- How should this handle consonant clusters, especially non-initial ones?
 syllabicate [] = []
 syllabicate ('\'':xs) = ('\'':a) : syllabicate b where (a, b) = span isVy xs
 syllabicate (',':xs) = (',':a) : syllabicate b where (a, b) = span isVy xs
 syllabicate str = (c ++ v) : syllabicate rest
  where (c, r) = span isC str
        (v, rest) = span isVy r

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
			      in if filter (\c -> isC c || isV c) pb <= 3
				 then (pa, pb, True)
				 else (pre, [], False)
		    Nothing -> (pre, [], False)
  let beta = b ++ concat body
  xubriv <- xubrivla' beta
  fendi a ~~ if b' && xubriv then mkBrivla beta
	     else if toLower (b1 !! 1) == 'y'
	     then mkCmavo b ~~ mkCmavo b1 ~~ fendi (concat bxs)
	     else return [Left beta]

 mkCmevla, mkCmavo, mkBrivla :: String -> Tamcux [Either String Valsi]
 mkCmevla str = toCmevla str >>= return . (: []) . maybe (Left str) Right
 mkCmavo  str = toCmavo str >>= return . (: []) . maybe (Left str) Right
 mkBrivla str = toBrivla str >>= return . (: []) . maybe (Left str) Right
