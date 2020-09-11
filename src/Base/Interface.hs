module Base.Interface (
  Action, Interactive(..), application,
  prompt, putLine, readOrMakeFile
) where

import Control.Monad ( forever )
import Control.Monad.Reader ( ReaderT(..) )
import Control.Monad.State ( StateT(..), evalStateT )
import Control.Exception ( catch, throwIO )

import System.IO ( hFlush, stdout )
import System.IO.Error ( isDoesNotExistError )

type Action r s = ReaderT r (StateT s IO)

data Interactive r s = Interactive {
  setup :: IO (r, s),
  loop :: Action r s ()
}

application :: Interactive r s -> IO ()
application app = do
  (env, state) <- setup app
  let theLoop = forever $ loop app
  evalStateT (runReaderT theLoop env) state

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

