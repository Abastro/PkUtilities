module DataEdit.App ( app ) where

import Control.Monad.IO.Class ( MonadIO(..) )

import System.Environment ( getArgs )

import Base.Interface

data Environment = Environment {
  script :: String,
  database :: FilePath
}

data DataState = DataState {
}

app :: Interactive Environment DataState
app = Interactive {
  setup = do
    putStrLn "-----------------------"
    putStrLn "   [DataEdit v0.1.0]   "
    putStrLn "-----------------------"

    args <- getArgs
    path <- case filter ((/= "--") . take 2) args of
      [path] -> pure path
      _ -> fail "Data not specified"

    let environment = Environment {
      script = "", database = path
    }
    pure (environment, DataState),
  loop = do
    cmd <- liftIO $ prompt "|dataedit|> "
    pure ()
}


-- Add row with condition
-- Find row with key
-- Produce table
