{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command (
  Command (..),
  Option (..),
  MetaVar (..),
  Arity (..),
) where

import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Vector qualified as V
import Toml.Schema

-- | Possibly nested commands.
data Command = Command
  { cmdDescription :: Maybe T.Text,
    arguments :: V.Vector MetaVar,
    options :: M.Map T.Text Option,
    subCommands :: M.Map T.Text Command
  }
  deriving (Show)

instance FromValue Command where
  fromValue :: Value' l -> Matcher l Command
  fromValue =
    parseTableFromValue $
      Command
        <$> optKey "description"
        <*> (maybe V.empty V.fromList <$> optKey "arguments")
        <*> (fromMaybe M.empty <$> optKey "options")
        <*> (fromMaybe M.empty <$> optKey "command")

data Option = Option
  { optDescription :: Maybe T.Text,
    metavar :: Maybe MetaVar
  }
  deriving (Show)

instance FromValue Option where
  fromValue :: Value' l -> Matcher l Option
  fromValue =
    parseTableFromValue $
      Option
        <$> optKey "description"
        <*> optKey "argument"

data MetaVar = MetaVar
  { varName :: T.Text,
    arity :: Arity,
    type_ :: T.Text
  }
  deriving (Show)

instance FromValue MetaVar where
  fromValue :: Value' l -> Matcher l MetaVar
  fromValue =
    parseTableFromValue $
      MetaVar <$> reqKey "name" <*> reqKey "arity" <*> reqKey "type"

data Arity = One | Many
  deriving (Show)

instance FromValue Arity where
  fromValue :: Value' l -> Matcher l Arity
  fromValue = \case
    Text' _ "one" -> pure One
    Text' _ "many" -> pure Many
    _ -> fail "arity: one|many"
