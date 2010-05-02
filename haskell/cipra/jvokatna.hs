import System (getArgs)
import Jbobaf.Jvacux
import Jbobaf.Vlatai

main = do
 argv <- getArgs
 input <- if null argv then getContents else readFile (head argv)
 mapM_ (\(jvo, rafs) ->
   let rafs' = either (const []) id (runReaderT (jvokatna jvo) defaults)
   in if rafs /= rafs'
      then putStrLn $ "Failure splitting " ++ jvo ++ ": expected " ++ show rafs
       ++ ", got " ++ show rafs'
      else return ())
      --else putStrLn $ "jvokatna " ++ show jvo ++ " = " ++ show rafs')
  $ map read (lines input)
