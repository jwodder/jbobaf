import System.Environment (getArgs)
import Data.Set (fromList)
import Jbobaf.Lerfendi
import Jbobaf.Jitro
import Jbobaf.Valsi

main = do
 argv <- getArgs
 (opts, argv') <- case argv of
   "-c":rc:xs      -> readFile rc >>= readIO >>= \o -> return (fromList o, xs)
   ('-':'c':rc):xs -> readFile rc >>= readIO >>= \o -> return (fromList o, xs)
   _               -> return (defaults, argv)
 input <- if null argv' then getContents else readFile (head argv')
 let jarco (Left str) = putStrLn $ "Naljbo: " ++ str
     jarco (Right vla) = putStrLn $ show (klesi vla) ++ ": " ++ valsi vla
     input' = unlines $ map (takeWhile (/= '#')) $ lines input
 mapM_ jarco $ nupre (lerfendi input') opts
