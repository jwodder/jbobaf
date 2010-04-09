import System (getArgs, exitFailure)
import Jbobaf.Jvacux
import Jbobaf.Vlatai

main = do
 argv <- getArgs
 luj <- if null argv
	then putStrLn "lo lujvo lonu katna cu jai sarcu" >> exitFailure
	else return (head argv)
 let rafs = jvacuxna (jvokatna luj) defaults
 if null rafs then putStrLn "na drani lujvo" else mapM_ putStrLn rafs
