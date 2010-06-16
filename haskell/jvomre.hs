jvomre_A :: String -> Jvacux Int
jvomre_A lujvo = jvokatna lujvo >>= jvomre_X lujvo

jvomre_B :: String -> Jvacux Int
jvomre_B str = fadgau str >>= jvomre_A

jvomre_C :: [String] -> Jvacux Int
jvomre_C rafsi = rafyjongau rafsi >>= flip jvomre_X rafsi

jvomre_X :: String -> [String] -> Jvacux Int
jvomre_X lujvo rafsi = do
 let rafsi' = map raftai rafsi
     l = length lujvo
     a = length $ filter (== '\'') lujvo
     h = length (filter (== 'y') lujvo) + fromEnum (head rafsi' == CVV
	  && (length rafsi > 2 || last rafsi' /= CCV))
     rafmre _ CVC_CV = 1
     rafmre _ CVC_C  = 2
     rafmre _ CCVCV  = 3
     rafmre _ CCVC   = 4
     rafmre _ CVC    = 5
     rafmre r CVV    = elem '\'' r ?: 6 :? 8
     rafmre _ CCV    = 7
     r = sum $ zipWith rafmre rafsi rafsi'
     v = length $ filter isV lujvo
 when (length rafsi < 2) ( ### throw error ### )
 when (Srerafsi `elem` rafsi') ( ### throw error ### )
 return $ (1000*l) - (500*a) + (100*h) - (10*r) - v

jvomre_Y :: [String] -> Jvacux Int
jvomre_Y rafhyph =  -- `rafhyph' is a list of {rafsi} and hyphens.
 if length rafhyph < 2 then ### throw error ###
 else foldM (\score raf -> do
  r <- case raftai raf of
	CVC_CV   -> return (-10)
	CVC_C    -> return (-20)
	CCVCV    -> return (-30)
	CCVC     -> return (-40)
	CVC      -> return (-50)
	CVV      -> return $ elem '\'' raf ?: -60 :? -80
	CCV      -> return (-70)
	Srerafsi -> if elem raf ["y", "r", "n"] then 100
		    else ### throw error ###
  return $ score + 1000 * length raf - 500 * length (filter (== '\'') raf) + r
   - length (filter isV raf)) 0 rafhyph
