module Base.Command where

import Control.Monad ( when )

import Data.Maybe ( listToMaybe )
import Data.Functor.Compose ( Compose(..) )

import Data.List ( intercalate )
import Data.Map ( Map )
import qualified Data.Map as Map

import Text.Read
    ( ReadPrec,
      lexP,
      choice,
      lift,
      minPrec,
      pfail,
      readPrec_to_S,
      (<++),
      Lexeme(..) )
import Text.Read.Lex ( Number )
import Text.ParserCombinators.ReadP ( munch )

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
  show (FConcat list) = intercalate " " $ show <$> list
  show (FMaybe f) = "[" <> show f <> "]"
  show (FChoice list) = "(" <> (intercalate "|" $ show <$> list) <> ")"

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
  when (x /= t) $ pfail
  pure ())

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
failOn f = Compose . (fmap $ \x -> do
  Just y <- f <$> x
  pure y) . getCompose


type Commands a = Map String (CmdParse a)

mkCommands :: [(String, CmdParse a)] -> Commands a
mkCommands = Map.fromList

cmdKeys :: Commands a -> [String]
cmdKeys = Map.keys

cmdFormatOf :: String -> Commands a -> Maybe Format
cmdFormatOf key = fmap (FIdent key <>) . fmap formatOf . Map.lookup key

-- |Processes command.
-- When parse error occurred, gives desired format.
-- When no command is selected, gives Left (FChoice [])
processCommand :: Commands a -> String -> Either Format a
processCommand cmds input = let
  process = do
    Ident i <- lexP
    Just parse <- pure $ Map.lookup i cmds
    let (format, parser) = getCompose parse
    (Right <$> parser) <++
      ((Left $ FIdent i <> format) <$ lift (munch $ const True))
  in maybe (Left $ FChoice []) id . listToMaybe $ do
    (act, "") <- readPrec_to_S process minPrec input
    pure act
