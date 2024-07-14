module Base.Interface (
  prompt, putLine, readOrMakeFile
) where

import Control.Exception ( catch, throwIO )

import System.IO ( hFlush, stdout )
import System.IO.Error ( isDoesNotExistError )

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

putLine :: IO ()
putLine = putStrLn ""

readOrMakeFile :: FilePath -> String -> IO String
readOrMakeFile path def = catch (readFile path) recover where
  recover e
    | isDoesNotExistError e = writeFile path def >> pure def
    | otherwise = throwIO e

