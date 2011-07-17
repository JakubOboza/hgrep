import System.Environment (getArgs)
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import IO
import Text.Regex.Posix

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", "..", ".git"]) names  --  don't scan .git
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

scanForPattern [] _ = do
  return ()

scanForPattern (fileName:paths) pattern = do
  file <- openFile fileName ReadMode
  scanFile file fileName pattern 0
  hClose file
  scanForPattern paths pattern

scanFile file fileName pattern n = do
    readable <- hIsReadable file
    if not readable
        then return ()
        else do
            ineof <- hIsEOF file
            if ineof
              then return ()
              else do
                    line <- hGetLine file
                    if line =~ pattern
                       then do
                         putStr fileName
                         putStr ":"
                         putStr $ show n
                         putStrLn line
                       else return ()
                    scanFile file fileName pattern (n + 1)

main = do
         args <- getArgs
         paths <- getRecursiveContents "."
         case args of
           [pattern] -> scanForPattern paths pattern
           _ -> putStrLn "USAGE: hrgrep <pattern> "
