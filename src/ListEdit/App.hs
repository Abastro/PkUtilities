{--------------------------------------
        Minimalistic List Editor
---------------------------------------}
module ListEdit.App ( app ) where

import Control.Monad ( when, unless, guard, forever, (>=>) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Reader ( MonadReader(..), asks )
import Control.Monad.State ( MonadState(..), gets, modify )
import Control.Exception ( evaluate )

import Data.Maybe ( isNothing, isJust )
import Data.List ( intercalate )
import Data.Foldable ( traverse_ )
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq

import Text.Read.Lex ( numberToInteger )

import System.Exit ( exitSuccess )
import System.Environment ( getArgs )
import GHC.Exts ( IsList (..) )

import Base.Interface
import Base.Command

type AppState = Seq String

app :: Interactive FilePath AppState
app = Interactive {
  setup = do
    putStrLn "-----------------------"
    putStrLn "   [ListEdit v0.2.0]   "
    putStrLn "-----------------------"

    args <- getArgs
    path <- case filter ((/= "--") . take 2) args of
      [] -> pure "list"
      [path] -> pure path
      _ -> fail "Too many arguments"

    list <- load path
    when ("--view" `elem` args) $ do
      traverse_ putStrLn $ numbering list
      putLine
      exitSuccess
    pure (path, list)

  ,loop = do
    cmd <- liftIO $ prompt "|listedit|> "
    unless (null cmd) $ case processCommand commands cmd of
      Right action -> do
        action >> liftIO putLine
      Left (FChoice []) -> liftIO $ do
        putStrLn "Invalid operation"
        putLine
      Left format -> liftIO $ do
        showUsage format
        putLine
}

load :: FilePath -> IO (Seq String)
load path = fromList . lines <$> readOrMakeFile path ""

save :: FilePath -> Seq String -> IO ()
save path = writeFile path . unlines . toList

showUsage :: Format -> IO ()
showUsage format = putStrLn $ "Usage: " <> show format

inRange :: Int -> Action FilePath AppState () -> Action FilePath AppState ()
inRange i r = do
  list <- get
  if i > 0 && i <= length list then r
  else liftIO $ putStrLn "Out of range"

numbering :: Seq String -> Seq String
numbering = Seq.mapWithIndex (\i -> (<>) $ show (i + 1) <> ". ")


{- Actions -}

printIt :: Action r AppState ()
printIt = get >>= traverse_ (liftIO . putStrLn) . numbering


{- Commands -}

intCmd :: String -> CmdParse Int
intCmd name =
  failOn (fmap fromInteger . numberToInteger) (numberCmd name)

commands :: Commands (Action FilePath AppState ())
commands = mkCommands [
  ("help", cmdHelp)
  , ("view", cmdView)
  , ("quit", cmdQuit)
  , ("revert", cmdRevert)
  , ("save", cmdSave)
  , ("add", cmdAdd)
  , ("remove", cmdRemove)
  , ("move", cmdMove)
  , ("clear", cmdClear)
  ] where
  cmdHelp = help <$> maybeCmd (getIdentCmd "<command>") where
    help (Just cmd) = liftIO $ do
      let format = cmdFormatOf cmd commands
      maybe (putStrLn "Command does not exist") showUsage format
    help Nothing = liftIO $ do
      putStrLn "Available commands:"
      traverse_ putStrLn $ cmdKeys commands

  cmdView = pure printIt
  cmdQuit = pure $ do -- Currently always save on quit
    saving <- asks save
    get >>= liftIO . saving
    liftIO $ putStrLn "Saved to disk"
    liftIO $ putLine >> exitSuccess
  cmdRevert = pure $ do
    loading <- asks load
    liftIO loading >>= put
    printIt
  cmdSave = pure $ do
    saving <- asks save
    get >>= liftIO . saving
    liftIO $ putStrLn "Saved"

  cmdAdd = add <$> maybeCmd (intCmd "index") <*> stringCmd "content" where
    add (Just num) content = inRange num $ do
      modify (Seq.insertAt (num - 1) content)
      printIt
    add Nothing content = do
      modify (flip (Seq.|>) content)
      printIt

  cmdRemove = remove <$> maybeCmd (intCmd "index") where
    remove (Just num) = inRange num $ do
      modify (Seq.deleteAt (num - 1))
      printIt
    remove Nothing = do
      modify (Seq.drop 1)
      printIt

  cmdMove = move <$> intCmd "from" <*> intCmd "to" where
    move from to = inRange from . inRange to $ do
      item <- gets (flip Seq.index (from - 1))
      modify (Seq.insertAt (to - 1) item
        . Seq.deleteAt (from - 1))
      printIt

  cmdClear = pure $ do
    put Seq.empty
    liftIO $ putStrLn "Cleared"
