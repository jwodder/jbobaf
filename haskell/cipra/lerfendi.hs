import System (getArgs)
import Jbobaf.Lerfendi
import Jbobaf.Jvacux
import Jbobaf.Valsi

main = do
 argv <- getArgs
 input <- if null argv then getContents else readFile (head argv)
 let jarco (Left str) = putStrLn $ "Naljbo: " ++ str
     jarco (Right vla) = putStrLn $ show (klesi vla) ++ ": " ++ valsi vla
 mapM_ jarco $ jvacuxna (lerfendi input) defaults
