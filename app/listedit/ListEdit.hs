module Main where

import Control.Monad ( when, guard, forever )
import Control.Monad.State
  ( MonadTrans(..), modify, MonadState(..) )

import System.Exit ( exitSuccess )

import Data.Maybe ( isNothing, isJust )
import Data.List ( intercalate )

import Text.Read
  ( Read(..), lexP, choice, readMaybe )
import Text.ParserCombinators.ReadP
  ( string, munch, skipSpaces )
import Text.Read.Lex ( Lexeme(..), numberToInteger )
import qualified Text.Read as Read

import Lib


data Option =
  Add String
  | Remove (Maybe Int)
  | View
  | Quit
  | Empty

instance Read Option where
  readPrec = choice [
    pure Empty
    ,do
      Ident "add" <- lexP
      content <- lexP
      pure . Add $ case content of
        String str -> str
        Ident str -> str
    ,do
      Ident "remove" <- lexP
      Remove <$> choice [
        do
          (Number num) <- lexP
          let index = numberToInteger num
          guard $ isJust index
          pure $ fromInteger <$> index
        , pure Nothing
        ]
    ,do
      Ident "view" <- lexP
      pure View
    ,do
      Ident "quit" <- lexP
      pure Quit
    ]


main :: IO ()
main = application app

app :: Interactive [String]
app = Interactive {
  setup = do
    putLine
    putStrLn "[ListEdit v0.1]"
    putLine
    pure []
  ,loop = do
    line <- lift $ prompt "|listedit|> "
    result <- sequenceA $ do
      option <- readMaybe line
      pure $ case option of
        Add str -> do
          modify (<> [str])
          printIt
        Remove Nothing -> do
          modify $ drop 1
          printIt
        Remove (Just num) -> do
          modify (fmap snd . filter ((/= num) . fst) . zip [1..])
          printIt
        View -> printIt
        Quit -> lift exitSuccess
        Empty -> pure ()
    case result of
      Just _ -> lift $ putLine
      Nothing -> lift $ putStrLn "Invalid operation" >> putLine
} where
  numbering = zipWith (<>) $ (<> ". ") . show <$> [1..]
  printIt = get >>= (lift . putStrLn . intercalate "\n" . numbering)
