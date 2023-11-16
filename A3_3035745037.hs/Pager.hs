module Pager where

import System.Environment (getArgs)
import System.Process
import qualified System.Info as SystemInfo
import System.IO
import Data.List

runPager :: IO ()
runPager = do
           filenames <- handleArgs
           contents <- readFiles filenames
           termSize <- getTerminalSize
           let fileLines                  = paginateFiles termSize contents
               ScreenDimensions rows cols = termSize
           showPages filenames (rows - 1) fileLines

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

handleArgs :: IO [FilePath]
handleArgs = getArgs 

-- Given a list of file paths, read the files and return a list of strings
readFiles :: [FilePath] -> IO [String]
readFiles []     = return []
readFiles (f:fs) = do s  <- readFile f
                      ss <- readFiles fs
                      return (s:ss) 

wordWrap :: Int -> String -> [String]
wordWrap 0 lineText          = []
wordWrap lineLength lineText = if length lineText <= lineLength then 
                                 [lineText] 
                               else 
                                 take lineLength lineText : wordWrap lineLength (drop lineLength lineText)

-- Given a list of strings, paginate each files and return a list of lists of strings (where each element of the list corresponds to one file)
paginateFiles :: ScreenDimensions -> [String] -> [[String]]
paginateFiles scrdim fs = [paginate scrdim f | f <- fs]

paginate :: ScreenDimensions -> String -> [String]
paginate (ScreenDimensions rows cols) text = foldr aux [] lines
                                           where 
                                             lineSize = cols
                                             lines    = splitAtNewLine text
                                             aux :: String -> [String] -> [String]
                                             aux line wrappedLines = wordWrap lineSize line ++ wrappedLines
                                               
showPages :: [String] -> Int -> [[String]] -> IO ()
showPages fileNames rowsOnScreen fileLines = do hSetBuffering stdin NoBuffering
                                                hSetEcho stdin False
                                                showPage 0 0
  where 
    totalFiles = length fileNames
    showPage :: Int -> Int -> IO ()
    showPage currentFile currentPage = let -- Get the data of the current file
                                          lines      = fileLines !! currentFile
                                          fileName   = fileNames !! currentFile
                                          totalLines = length lines
                                          totalPages = totalLines `div` rowsOnScreen + if totalLines `mod` rowsOnScreen == 0 then 0 else 1
                                          startLine  = if totalPages > 1 && totalPages == currentPage + 1 then 
                                                         totalLines - rowsOnScreen 
                                                       else 
                                                         currentPage * rowsOnScreen
                                          endLine    = min (startLine + rowsOnScreen) totalLines
                                          -- Status bar strings
                                          pageStats  = (" Page:" ++ show (currentPage + 1) ++ "/" ++ show totalPages)
                                          lineStats  = (" Lines:" ++ show (startLine + 1) ++ "~" ++ show endLine ++ "/" ++ show totalLines) 
                                          fileStats  = (" Files:" ++ show (currentFile + 1) ++ "/" ++ show totalFiles) in 
                                       do clearScreen
                                          printLines lines startLine endLine
                                          putStr (fileName ++ pageStats ++ lineStats ++ fileStats)
                                          input <- getChar
                                          case input of 
                                            ' ' -> if currentPage + 1 >= totalPages then -- Next file
                                                     if currentFile + 1 < totalFiles then
                                                       showPage (currentFile + 1) 0
                                                     else -- Current file is already the last file
                                                       showPage currentFile currentPage
                                                   else -- Current file
                                                     showPage currentFile (currentPage + 1)  
                                            'b' -> if currentPage - 1 < 0 then -- Previous file
                                                     if currentFile - 1 >= 0 then 
                                                       showPage (currentFile - 1) $ getLastPageOfFile (currentFile - 1)
                                                     else -- Current file is already the first file
                                                       showPage currentFile currentPage
                                                   else
                                                     showPage currentFile (currentPage - 1)
                                            'q' -> do clearScreen
                                                      return ()
                                            _   -> showPage currentFile currentPage  
    -- Given an index of file, find the index of its last page
    getLastPageOfFile :: Int -> Int
    getLastPageOfFile file = totalPages - 1
      where 
        totalLines = length $ fileLines !! file
        totalPages = totalLines `div` rowsOnScreen + if totalLines `mod` rowsOnScreen == 0 then 0 else 1
                       
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
                     