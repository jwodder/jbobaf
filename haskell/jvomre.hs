jvomre_A :: String -> Jvacux Int
jvomre_A lujvo = jvokatna lujvo >>= jvomre_X lujvo

jvomre_B :: String -> Jvacux Int
jvomre_B str = fadgau str >>= jvomre_A

jvomre_C :: [String] -> Jvacux Int
jvomre_C rafsi = rafyjongau rafsi >>= flip jvomre_X rafsi

jvomre_X :: String -> [String] -> Jvacux Int
jvomre_X lujvo rafsi = return $ (1000*l) - (500*a) + (100*h) - (10*r) - v
 where rafsi' = map raftai rafsi
       l = length lujvo
       a = length $ filter (== '\'') lujvo
       h = length (filter (== 'y') lujvo) + fromEnum (head rafsi' == CVV
	    && (length rafsi > 2 || last rafsi' /= CCV))
       r = sum $ zipWith rafmre rafsi rafsi'
       rafmre _ CVC_CV = 1
       rafmre _ CVC_C  = 2
       rafmre _ CCVCV  = 3
       rafmre _ CCVC   = 4
       rafmre _ CVC    = 5
       rafmre r CVV    = elem '\'' r ?: 6 :? 8
       rafmre _ CCV    = 7
       v = length $ filter isV lujvo
