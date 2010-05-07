import System (getArgs, exitFailure)
import Jbobaf.Jitro
import Jbobaf.Vlatai

main = do
 argv <- getArgs
 luj <- if null argv
	then putStrLn "lo lujvo lonu katna cu jai sarcu" >> exitFailure
	else return (head argv)
 case runReaderT (jvokatna luj) defaults of
  Left str -> putStrLn $ "na drani lujvo: " ++ str
  Right rafs -> mapM_ putStrLn rafs
