module Main where

import Control.Monad ( when, unless, guard, forever, (>=>) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Reader ( MonadReader(..) )
import Control.Monad.State ( evalStateT, modify, MonadState(..) )
import Control.Exception ( evaluate )

import Data.Maybe ( isNothing, isJust )
import Data.List ( intercalate )

import Text.Read
  ( Read(..), ReadPrec(..), lexP, choice, readMaybe, (+++) )
import Text.ParserCombinators.ReadP
  ( string, munch, skipSpaces )
import Text.Read.Lex ( Lexeme(..), numberToInteger )
import qualified Text.Read as Read

import System.Exit ( exitSuccess )
import System.Environment ( getArgs )

import Lib
import Command


main :: IO ()
main = application app

app :: Interactive FilePath [String]
app = Interactive {
  setup = do
    putStrLn "---------------------"
    putStrLn "   [ListEdit v0.1]   "
    putStrLn "---------------------"

    args <- getArgs
    path <- case filter ((/= "--") . take 2) args of
      [] -> pure "list"
      [path] -> pure path
      _ -> fail "Too many arguments"

    list <- lines <$> readOrMakeFile path ""
    evaluate $ length list -- Reads to the end

    when ("--view" `elem` args) $ do
      putStrLn . unlines . numbering $ list
      exitSuccess

    pure (path, list)

  ,loop = do
    cmd <- liftIO $ prompt "|listedit|> "
    unless (null cmd) $ case processCommand commands cmd of
      Right action -> action
      Left (FChoice []) -> do
        liftIO $ putStrLn "Invalid operation"
        liftIO putLine
      Left format -> do
        showUsage format
        liftIO putLine
}


numbering :: [String] -> [String]
numbering = zipWith (<>) $ (<> ". ") . show <$> [1..]

{- Actions -}

modifyIt :: ([String] -> [String]) -> Action FilePath [String]
modifyIt f = do
  modify f
  path <- ask
  get >>= liftIO . writeFile path . unlines

printIt :: Action FilePath [String]
printIt = get >>= liftIO . putStrLn . unlines . numbering

showUsage :: Format -> Action FilePath [String]
showUsage format = liftIO . putStrLn $ "Usage: " <> show format


{- Commands -}

readMaybeInt :: String -> CmdParse (Maybe Int)
readMaybeInt name = maybeCmd $
  failOn (fmap fromInteger . numberToInteger) (numberCmd name)

commands :: Commands (Action FilePath [String])
commands = mkCommands [
  ("help", cmdHelp)
  , ("add", cmdAdd)
  , ("remove", cmdRemove)
  , ("clear", cmdClear)
  , ("view", cmdView)
  , ("quit", cmdQuit)
  ]

cmdHelp :: CmdParse (Action FilePath [String])
cmdHelp = help <$> maybeCmd (getIdentCmd "<command>") where
  help (Just cmd) = do
    let format = cmdFormatOf cmd commands
    maybe (liftIO $ putStrLn "Command does not exist") showUsage format
    liftIO putLine
  help Nothing = do
    liftIO . putStrLn $ "Available commands: " <> unwords (cmdKeys commands)
    liftIO putLine

cmdAdd :: CmdParse (Action FilePath [String])
cmdAdd = add <$> readMaybeInt "index" <*> stringCmd "content" where
  add (Just num) str = do
    modifyIt (intercalate [str] . pairToList . splitAt (num - 1))
    printIt
  add Nothing str = do
    modifyIt (<> [str])
    printIt

cmdRemove :: CmdParse (Action FilePath [String])
cmdRemove = remove <$> readMaybeInt "index" where
  remove (Just num) = do
    modifyIt (fmap snd . filter ((/= num) . fst) . zip [1..])
    printIt
  remove Nothing = do
    modifyIt $ drop 1
    printIt

cmdClear :: CmdParse (Action FilePath [String])
cmdClear = pure clear where
  clear = do
    modifyIt $ const []
    liftIO $ putStrLn "Cleared" >> putLine

cmdView :: CmdParse (Action FilePath [String])
cmdView = pure printIt

cmdQuit :: CmdParse (Action FilePath [String])
cmdQuit = pure $ liftIO exitSuccess
