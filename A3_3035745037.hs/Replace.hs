import System.Environment
import Data.List

main :: IO ()
main = 
    do args <- getArgs
       fileContent <- readFile $ head args
       putStrLn $ replaceStr (args !! 1) fileContent (args !! 2)
       return ()
       
-- Returns true if xs matches the prefix of ys
matchStrPrefix :: String -> String -> Bool
matchStrPrefix [] ys         = True
matchStrPrefix xs []         = False
matchStrPrefix (x:xs) (y:ys) = if x == y then matchStrPrefix xs ys else False

replaceStr :: String -> String -> String -> String
replaceStr [] (y:ys) z = y:ys
replaceStr xs [] z     = []
replaceStr xs (y:ys) z = if matchStrPrefix xs (y:ys) then 
                           z ++ replaceStr xs (drop (length xs) (y:ys)) z
                         else 
                           y:replaceStr xs ys z