module Lib (
  Interactive(..), application,
  prompt, putLine
) where

import Control.Monad ( forever )
import Control.Monad.State ( StateT, evalStateT )
import System.IO ( hFlush, stdout )

data Interactive a = Interactive {
  setup :: IO a,
  loop :: StateT a IO ()
}

application :: Interactive a -> IO ()
application app = do
  p <- setup app
  evalStateT (forever $ loop app) p

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

putLine :: IO ()
putLine = putStrLn ""
