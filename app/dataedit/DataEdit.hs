{-------------------------------------
        Simplistic Data Editor
--------------------------------------}
module Main (main) where

import Lib
import System.Environment (getArgs)
import Control.Monad.IO.Class (MonadIO(..))
import Command

data Environment = Environment {
  script :: String,
  database :: FilePath
}

data DataState = DataState {
}

main :: IO ()
main = application app

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

cmdView :: CmdParse (Action Environment DataState ())
cmdView = undefined

