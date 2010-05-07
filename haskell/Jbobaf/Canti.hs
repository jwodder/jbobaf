{-# OPTIONS_HADDOCK hide #-}

module Jbobaf.Canti where
 import Char (toLower)
 import List (find, findIndices)
 import Monad (liftM2)
 import {-# SOURCE #-} Jbobaf.Vlatai (isC, isVy, isV, isCC, isC_C)

 infixr 5 ~:, ~~, ~:~
 infixr 0 ?:, :?

 (~:) :: Monad m => a -> m [a] -> m [a]
 a ~: xs = xs >>= return . (a :)

 (~~) :: Monad m => m [a] -> m [a] -> m [a]
 (~~) = liftM2 (++)

 (~:~) :: Monad m => m a -> m [a] -> m [a]
 (~:~) = liftM2 (:)

 data TernaryBranch a = a :? a deriving (Eq, Ord, Read, Show, Bounded)

 instance Functor TernaryBranch where fmap f (x :? y) = f x :? f y

 (?:) :: Bool -> TernaryBranch a -> a
 True ?: (y :? _) = y
 False ?: (_ :? z) = z

 findC_C :: String -> Maybe Int
 findC_C str = find (\i -> isC (str !! (i+1)) || str !! (i+1) == 'y'
  && isC (str !! (i+2))) $ findIndices isC str

 findLa :: String -> Maybe (String, String, String)
 findLa str = fla 0 False $ map toLower str
  where fla _ _ [] = Nothing
	fla pos False ('l':'a':'\'':'i':c:xs) =
	 if notElem c "',aeiouy"
	 then Just (take pos str, "la'i", drop (pos+4) str)
	 else fla (pos+5) False xs
	fla pos False ('l':'a':'i':c:xs) =
	 if notElem c "',aeiouy"
	 then Just (take pos str, "lai", drop (pos+3) str)
	 else fla (pos+4) False xs
	fla pos False ('l':'a':c:xs) =
	 if notElem c "',aeiouy"
	 then Just (take pos str, "la", drop (pos+2) str)
	 else fla (pos+3) False xs
	fla pos False ('d':'o':'i':c:xs) =
	 if notElem c "',aeiouy"
	 then Just (take pos str, "doi", drop (pos+3) str)
	 else fla (pos+4) False xs
	fla pos _ (c:xs) = fla (pos+1) (isC c) xs

 syllabicate :: String -> [String]
 -- How should this handle consonant clusters, especially non-initial ones?
 syllabicate [] = []
 syllabicate ('\'':xs) = ('\'':a) : syllabicate b where (a, b) = span isVy xs
 syllabicate (',':xs) = (',':a) : syllabicate b where (a, b) = span isVy xs
 syllabicate str = (c ++ v) : syllabicate rest
  where (c, r) = span isC str
        (v, rest) = span isVy r

 voc :: String -> Bool
 -- Tests whether a syllable contains a non-Y vowel and is thus accentable.
 -- The short name is solely for aesthetic reasons.
 voc = not . null . filter isV

 has_C_C :: String -> Bool
 -- checks for the presence of a non-initial valid consonant pair
 has_C_C str = not $ null $ filter (\i -> let cc = take 2 $ drop i str
  in isC_C cc && not (isCC cc)) (findIndices isC str)
