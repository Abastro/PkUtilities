module DataEdit.App ( main ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State

import System.Environment ( getArgs )

import Base.Interface
import Base.Command
import DataEdit.SQLData

type DataHandle = IO

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

commands :: CommandHandle (IO ())
commands = CommandHandle {
  commandMap = mkCmdMap [
    ("help", mkCommand cmdHelp "Shows commands and their usages")
    , ("with", mkCommand cmdWith "Operate upon the table")
  ]
  , illformed = undefined
  , wrongCommand = undefined
  , wrongFormat = undefined
} where
  cmdHelp = undefined -- TODO For later
  cmdWith = withTable <$> getIdentCmd "<table>" where
    withTable table = do
      flip evalStateT (STyped undefined $ SQLFrom table) $ forever $ do
        cmd <- liftIO $ prompt "|table:|>> "
        unless (null cmd) $ runCommand cmdWithTable cmd
      -- TODO initiate?
      undefined


type TableHandle = StateT (STyped SQLColumn) IO

cmdWithTable :: CommandHandle (TableHandle ())
cmdWithTable = CommandHandle {
  commandMap = mkCmdMap [
    ("where", mkCommand cmdWhere "")
  ]
  , illformed = undefined
  , wrongCommand = undefined
  , wrongFormat = undefined
} where
  cmdWhere = undefined

-- Add row with condition
-- Find row with key

