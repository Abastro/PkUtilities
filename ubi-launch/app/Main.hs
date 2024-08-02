{-# LANGUAGE LambdaCase #-}

module Main where

import Command hiding (metavar)
import Control.Monad
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
import Toml qualified

newtype Args = Args FilePath

args :: Parser Args
args = Args <$> argument str (metavar "CONFIG")

opts :: ParserInfo Args
opts = info (args <**> helper) fullDesc

main :: IO ()
main = execParser opts >>= handle

handle :: Args -> IO ()
handle (Args path) = do
  Toml.decode @(M.Map T.Text Command) <$> T.readFile path >>= \case
    Toml.Failure errs -> putStrLn $ unlines ("errors occurred" : errs)
    Toml.Success warns commands -> do
      unless (null warns) $ putStrLn $ unlines ("warnings occurred" : warns)
      -- TODO Placeholder
      for_ (M.assocs commands) $ \(name, cmd) -> do
        putStrLn $ T.unpack name <> ":"
        print cmd

