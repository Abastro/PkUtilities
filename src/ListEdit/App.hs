{--------------------------------------
        Minimalistic List Editor
---------------------------------------}
module ListEdit.App ( main ) where

import Control.Monad ( when, unless, guard, forever, (>=>) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe ( isNothing, isJust )
import Data.List ( isPrefixOf, intercalate )
import Data.Foldable ( traverse_ )
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq

import Text.Read.Lex ( numberToInteger )

import System.Exit ( exitSuccess )
import System.Environment ( getArgs )
import GHC.Exts ( IsList (..) )

import Base.Interface
import Base.Command

data AppState = AppState {
  content :: Seq String,
  isSaved :: Bool
}

setSaved :: AppState -> AppState
setSaved s = s { isSaved = True }

chContent :: (Seq String -> Seq String) -> (AppState -> AppState)
chContent f s = AppState { content = f $ content s, isSaved = False }

type ListHandle = ReaderT FilePath (StateT AppState IO)

main :: IO ()
main = do
  putStrLn "-----------------------"
  putStrLn "   [ListEdit v0.2.0]   "
  putStrLn "-----------------------"

  args <- getArgs
  path <- case filter (not . isPrefixOf "--") args of
    [] -> pure "list"
    [path] -> pure path
    _ -> fail "Too many arguments"

  list <- load path
  when ("--view" `elem` args) $ do
    traverse_ putStrLn $ numbering list
    putLine
    exitSuccess

  flip evalStateT AppState { content = list, isSaved = True }
    $ flip runReaderT path
    $ forever loop

loop :: ListHandle ()
loop = do
  cmd <- liftIO $ prompt "|listedit|> "
  unless (null cmd) $ case processCommand commands cmd of
    Right action -> action
    Left (FChoice []) -> liftIO $ putStrLn "Invalid operation"
    Left format -> liftIO $ showUsage format
  liftIO putLine

load :: FilePath -> IO (Seq String)
load path = fromList . lines <$> readOrMakeFile path ""

showUsage :: Format -> IO ()
showUsage format = putStrLn $ "Usage: " <> show format

inRange :: Int -> ListHandle () -> ListHandle ()
inRange i r = do
  list <- gets content
  if i > 0 && i <= length list then r
  else liftIO $ putStrLn "Out of range"

numbering :: Seq String -> Seq String
numbering = Seq.mapWithIndex (\i -> (<>) $ show (i + 1) <> ". ")

saveIt :: ListHandle ()
saveIt = do
  saving <- asks $ writeFile
  gets content >>= liftIO . saving . unlines . toList
  modify setSaved

printIt :: ListHandle ()
printIt = gets content >>= traverse_ (liftIO . putStrLn) . numbering


{- Commands -}

intCmd :: String -> CmdParse Int
intCmd name =
  failOn (fmap fromInteger . numberToInteger) (numberCmd name)

commands :: Commands (ListHandle ())
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
  cmdQuit = pure $ do
    saved <- gets isSaved
    unless saved $ do
      ans <- liftIO $ prompt "Save changes? (y/n): "
      unless (ans == "n") $ saveIt >> liftIO (putStrLn "Changes saved")
    liftIO $ putLine >> exitSuccess
  cmdRevert = pure $ do
    loading <- asks load
    liftIO loading >>= modify . chContent . const
    printIt
  cmdSave = pure $ do
    saveIt
    liftIO $ putStrLn "Saved"

  cmdAdd = add <$> maybeCmd (intCmd "index") <*> stringCmd "content" where
    add (Just num) content = inRange num $ do
      modify . chContent $ Seq.insertAt (num - 1) content
      printIt
    add Nothing content = do
      modify . chContent $ flip (Seq.|>) content
      printIt

  cmdRemove = remove <$> maybeCmd (intCmd "index") where
    remove (Just num) = inRange num $ do
      modify . chContent $ Seq.deleteAt (num - 1)
      printIt
    remove Nothing = do
      modify . chContent $ Seq.drop 1
      printIt

  cmdMove = move <$> intCmd "from" <*> intCmd "to" where
    move from to = inRange from . inRange to $ do
      item <- gets $ flip Seq.index (from - 1) . content
      modify . chContent $ (Seq.insertAt (to - 1) item
        . Seq.deleteAt (from - 1))
      printIt

  cmdClear = pure $ do
    modify . chContent $ const Seq.empty
    liftIO $ putStrLn "Cleared"
