module DataEdit.App ( main ) where

import Control.Monad.IO.Class ( MonadIO(..) )

import System.Environment ( getArgs )

import Base.Interface

data Environment = Environment {
  script :: String,
  database :: FilePath
}

data DataState = DataState {
}

main :: IO ()
main = do
  putStrLn "-----------------------"
  putStrLn "   [DataEdit v0.1.0]   "
  putStrLn "-----------------------"

  args <- getArgs
  path <- case filter ((/= "--") . take 2) args of
    [path] -> pure path
    _ -> fail "Data not specified"
  pure ()



-- Add row with condition
-- Find row with key

