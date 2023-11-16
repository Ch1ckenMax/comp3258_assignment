module Pager where

import System.Environment (getArgs)
import System.Process
import qualified System.Info as SystemInfo
import System.IO
import Data.List

runPager :: IO ()
runPager = do
           filename <- handleArgs
           contents <- readFile filename
           termSize <- getTerminalSize
           let pages                      = paginate termSize contents
               ScreenDimensions rows cols = termSize
           showPages filename (rows - 1) pages

main = runPager 

data ScreenDimensions = ScreenDimensions
                { screenRows :: Int
                , screenColumns :: Int
                } deriving Show

getTerminalSize :: IO ScreenDimensions
getTerminalSize = case SystemInfo.os of
        "darwin" -> tputScreenDimensions
        "linux" -> tputScreenDimensions
        _other -> pure $ ScreenDimensions 25 80
        -- for other systems, it's okay to return a fixed size
        where tputScreenDimensions :: IO ScreenDimensions
              tputScreenDimensions = do
                lines <- readProcess "tput" ["lines"] ""
                cols <- readProcess "tput" ["cols"] ""
                let lines' = read $ init lines
                    cols' = read $ init cols
                return $ ScreenDimensions lines' cols'

clearScreen :: IO ()
clearScreen = putStr "\^[[1J\^[[1;1H"

handleArgs :: IO FilePath
handleArgs = do args <- getArgs 
                return $ head args

wordWrap :: Int -> String -> [String]
wordWrap 0 lineText          = []
wordWrap lineLength lineText = if length lineText <= lineLength then 
                                 [lineText] 
                               else 
                                 take lineLength lineText : wordWrap lineLength (drop lineLength lineText)

paginate :: ScreenDimensions -> String -> [String]
paginate (ScreenDimensions rows cols) text = foldr aux [] lines
                                           where 
                                             lineSize = cols
                                             lines    = splitAtNewLine text
                                             aux :: String -> [String] -> [String]
                                             aux line wrappedLines = wordWrap lineSize line ++ wrappedLines
                                               
showPages :: String -> Int -> [String] -> IO ()
showPages fileName rowsOnScreen lines = do hSetBuffering stdin NoBuffering
                                           hSetEcho stdin False
                                           showPage fileName rowsOnScreen lines 0
               

-- Prints one page
showPage :: String -> Int -> [String] -> Int -> IO ()
showPage fileName rowsOnScreen lines currentPage = let totalLines = length lines
                                                       totalPages = totalLines `div` rowsOnScreen + if totalLines `mod` rowsOnScreen == 0 then 0 else 1
                                                       startLine  = if totalPages == 2 && currentPage == 1 then totalLines - rowsOnScreen else currentPage * rowsOnScreen
                                                       endLine    = min (startLine + rowsOnScreen) totalLines
                                                       pageStats  = (" Page:" ++ show (currentPage + 1) ++ "/" ++ show totalPages)
                                                       lineStats  = (" Lines:" ++ show (startLine + 1) ++ "~" ++ show endLine ++ "/" ++ show totalLines) 
                                                       showPage'  = showPage fileName rowsOnScreen lines in
                                                    do clearScreen
                                                       printLines lines startLine endLine
                                                       putStr (fileName ++ pageStats ++ lineStats)
                                                       input <- getChar
                                                       case input of 
                                                         ' ' -> showPage' $ min (currentPage + 1) (totalPages - 1) 
                                                         'b' -> showPage' $ max (currentPage - 1) 0
                                                         'q' -> do putStrLn ""
                                                                   return ()
                                                         _   -> showPage' currentPage

-- Prints the [start, end) lines (starting index at 0)
printLines :: [String] -> Int -> Int -> IO ()
printLines lines start end | start == end          = return () -- Nothing needs to be printed
                           | start > end           = error ("Trying to print from " ++ show start ++ "-th line til " ++ show end ++ "-th line.")
                           | start < 0             = error ("Trying to print starting from negative line: " ++ show start)
                           | start >= length lines = error ("Trying to print from " ++ show start ++ "-th line, but there are only " ++ (show $ length lines) ++ " lines.")
                           | end > length lines    = error ("Trying to print til " ++ show end ++ "-th line, but there are only " ++ (show $ length lines) ++ " lines.")
                           | otherwise             = printFirstNLines (drop start lines) (end - start)

-- Print the first n elements of the string list
printFirstNLines :: [String] -> Int -> IO ()
printFirstNLines lines 0        = return ()
printFirstNLines [] n           = error "Too many lines to print!"
printFirstNLines (line:lines) n = do putStrLn line
                                     printFirstNLines lines $ n - 1
                          
-- Split a string to multiple strings separated by new line character
splitAtNewLine :: String -> [String]
splitAtNewLine ""     = []
splitAtNewLine (x:xs) = if x == '\n' then 
                          "":splitAtNewLine xs 
                        else
                           case splitAtNewLine xs of
                             []     -> [[x]]
                             (y:ys) -> (x:y):ys
                     