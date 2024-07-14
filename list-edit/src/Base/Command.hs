module Base.Command where

import Control.Monad ( when )

import Data.Maybe
import Data.List
import Data.Functor.Compose ( Compose(..) )
import Data.Map ( Map )
import qualified Data.Map as Map

import Text.Read
import Text.Read.Lex ( Number )
import Text.ParserCombinators.ReadP ( eof )

-- |Parsing format. First parameter is name
data Format =
  FIdent String
  | FString String
  | FNumber String
  | FConcat [Format]
  | FMaybe Format
  | FChoice [Format]
  deriving Eq

instance Show Format where
  show (FIdent i) = i
  show (FString s) = show s
  show (FNumber n) = "num_" <> n
  show (FConcat list) = unwords $ show <$> list
  show (FMaybe f) = "[" <> show f <> "]"
  show (FChoice list) = "(" <> intercalate "|" (show <$> list) <> ")"

instance Semigroup Format where
  FConcat l <> FConcat m = FConcat (l <> m)
  FConcat l <> f = FConcat (l <> [f])
  f <> FConcat m = FConcat (f : m)
  f <> g = FConcat [f, g]

instance Monoid Format where
  mempty = FConcat []


-- |Applicative for building a command parsing
type CmdParse = Compose ((,) Format) ReadPrec

formatOf :: CmdParse a -> Format
formatOf = fst . getCompose


choiceCmd :: [CmdParse a] -> CmdParse a
choiceCmd list = Compose (
    FChoice $ fst . getCompose <$> list,
    choice $ snd . getCompose <$> list)

identCmd :: String -> CmdParse ()
identCmd t = Compose (FIdent t, do
  Ident x <- lexP
  when (x /= t) pfail)

getIdentCmd :: String -> CmdParse String
getIdentCmd name = Compose (FIdent name, do
  Ident x <- lexP
  pure x)


stringCmd :: String -> CmdParse String
stringCmd name = Compose (FString name, do
  String s <- lexP
  pure s)

numberCmd :: String -> CmdParse Number
numberCmd name = Compose (FNumber name, do
  Number n <- lexP
  pure n)

maybeCmd :: CmdParse a -> CmdParse (Maybe a)
maybeCmd (Compose (f, p)) = Compose (FMaybe f,
  (Just <$> p) <++ pure Nothing)

failOn :: (a -> Maybe b) -> CmdParse a -> CmdParse b
failOn f = Compose . fmap (\x -> do
  Just y <- f <$> x
  pure y) . getCompose


data Command a = Command {
  parser :: ReadPrec a,
  format :: Format,
  description :: String
}

mkCommand :: CmdParse a -> String -> Command a
mkCommand (Compose (format, parser)) desc = Command {
  parser = parser,
  format = format,
  description = desc
}

mkCmdMap :: [(String, Command a)] -> Map String (Command a)
mkCmdMap = Map.fromList

data CommandHandle a = CommandHandle {
  commandMap :: Map String (Command a),
  illformed :: a,
  wrongCommand :: String -> a,
  wrongFormat :: String -> Format -> a
}

getCommand :: CommandHandle a -> String -> Maybe (Command a)
getCommand = flip Map.lookup . commandMap

cmdKeys :: CommandHandle a -> [String]
cmdKeys = Map.keys . commandMap

runCommand :: CommandHandle a -> String -> a
runCommand cmdHandle input = let
  process = do
    Ident i <- lexP
    case getCommand cmdHandle i of
      Nothing -> pure $ wrongCommand cmdHandle i
      Just command -> do
        let formatErr = wrongFormat cmdHandle i $ FIdent i <> format command
        (parser command <* lift eof) <++ pure formatErr
  in fromMaybe (illformed cmdHandle) . listToMaybe
  $ fst <$> readPrec_to_S process minPrec input
