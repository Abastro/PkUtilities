module Command where

import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V

-- | Possibly nested commands.
data Command = Command
  { command :: T.Text,
    info :: CommandInfo
  }

-- | Command information.
-- The arguments are assumed to be taken from front to back.
data CommandInfo
  = Group (V.Vector Command)
  | Options (V.Vector Option) CommandInfo
  | End

data Option = Option
  { aliases :: S.Set T.Text,
    metavar :: Maybe MetaVar,
    helpText :: Maybe T.Text
  }

data MetaVar
  = TextVar T.Text
  | FileVar FilePath
  | DirVar FilePath
