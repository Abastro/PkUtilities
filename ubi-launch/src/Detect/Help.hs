module Detect.Help where

import Command
import Control.Monad
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void
import Text.Megaparsec qualified as P

-- Help result usually have this parts.
-- Usage: foo [options] command
--
-- This will likely be enough to determine the usages.

-- Erm, not getting point of this.

data CmdSpec
  = Composite (V.Vector (Reqs, Arity, CmdSpec))
  | Fixed T.Text
  | Meta T.Text

data Reqs = Required | Optional

signalUsage :: P.Parsec Void T.Text ()
signalUsage = void (P.chunk "Usage:")

parseUsage :: T.Text -> P.Parsec Void T.Text Command
parseUsage cmdName = do
  _ <- P.chunk cmdName
  undefined
