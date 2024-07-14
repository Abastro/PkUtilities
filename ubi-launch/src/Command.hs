module Command where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V

-- Common parts:
-- USAGE: ..

-- | Possibly nested commands.
data Command = Command
  { command :: T.Text,
    info :: CommandInfo
  }

-- | Command information.
-- The arguments are assumed to be taken from front to back.
data CommandInfo
  = Group (V.Vector Command)
  | Options (M.Map T.Text Option) CommandInfo
  | End

data Option = Option
  { metavar :: Maybe MetaVar,
    helpText :: Maybe T.Text
  }

data MetaVar
  = TextVar T.Text
  | FileVar FilePath
