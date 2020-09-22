{--------------------------------------
        Minimalistic List Editor
---------------------------------------}
module ListEdit.App ( main ) where

import Control.Monad ( when, unless, guard, forever, (>=>) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Reader ( ReaderT(..), asks )
import Control.Monad.State ( StateT(..), gets, modify, evalStateT )
import Control.Monad.Except ( runExceptT )

import Data.List ( isPrefixOf, intercalate )
import Data.Foldable ( traverse_ )
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq

import Text.Read.Lex ( numberToInteger )
import Text.Printf ( printf )

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
    $ forever $ do
    cmd <- liftIO $ prompt "|listedit|> "
    unless (null cmd) $ runCommand commands cmd
    liftIO putLine


load :: FilePath -> IO (Seq String)
load path = fromList . lines <$> readOrMakeFile path ""

inRange :: Int -> ListHandle () -> ListHandle ()
inRange i r = do
  list <- gets content
  if i > 0 && i <= length list then r
  else liftIO $ putStrLn "Out of range"

numbering :: Seq String -> Seq String
numbering = Seq.mapWithIndex (printf "%i. %s" . (+1))

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

commands :: CommandHandle (ListHandle ())
commands = CommandHandle {
  commandMap = mkCmdMap [
  ("help", mkCommand cmdHelp "Shows commands and their usages")
  , ("view", mkCommand cmdView "Views the list")
  , ("quit", mkCommand cmdQuit "Quit")
  , ("revert", mkCommand cmdRevert "Reverts to the last saved state")
  , ("save", mkCommand cmdSave "Saves the list to the disk")
  , ("add", mkCommand cmdAdd "Adds an element to the list, to the bottom by default")
  , ("remove", mkCommand cmdRemove "Remove an element from the list, from the top by default")
  , ("move", mkCommand cmdMove "Moves an element")
  , ("clear", mkCommand cmdClear "Clears the list to be empty")
  ],
  illformed = liftIO . putStrLn $ "Invalid operation",
  wrongCommand = \cmd -> liftIO . putStrLn $ "Wrong command: " <> cmd,
  wrongFormat = \_ format -> liftIO . putStrLn $ "Usage: " <> show format
} where
  cmdHelp = help <$> maybeCmd (getIdentCmd "<command>") where
    viewCommand key command = do
      putStrLn $ printf "<< %s >>" key
      putStrLn $ description command
      putStrLn $ printf "Usage: %s %s" key $ show (format command)
    help (Just key) = liftIO $ do
      let command = getCommand commands key
      maybe (putStrLn "Command does not exist") (viewCommand key) command
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
