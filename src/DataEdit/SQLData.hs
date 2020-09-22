module DataEdit.SQLData where

import Data.Int (Int64)
import Data.Text (Text)
import Data.ByteString (ByteString)

import Control.Monad (guard)

data SQLPrim =
  DataInt Int64
  | DataFloat Double
  | DataText Text
  | DataBytes ByteString
  | DataNull

typeName :: SQLPrim -> String
typeName (DataInt _) = "int"
typeName (DataFloat _) = "float"
typeName (DataText _) = "text"
typeName (DataBytes _) = "string"
typeName (DataNull) = "*"

data SQLType =
  Primitive String
  | Composite [(String, SQLType)]

instance Eq SQLType where
  -- * can match with any type
  Primitive "*" == Primitive _ = True
  Primitive _ == Primitive "*" = True
  Primitive x == Primitive y = x == y
  -- Ignores field names for equality check
  Composite l == Composite m = (snd <$> l) == (snd <$> m)
  _ == _ = False

data JoinType = InnerJoin | LeftJoin | RightJoin | FullJoin

data SQLMod =
  SQLJoin JoinType SQLColumn SQLCond
  | SQLWhere SQLCond
  | SQLOrder SQLSort

data SQLColumn =
  SQLFrom String
  | SQLMod SQLMod SQLColumn

data SQLCond =
  SAnd SQLCond SQLCond
  | SOr SQLCond SQLCond
  | SNot SQLCond
  | SCompare Bool Ordering SQLField SQLField
  | SContain SQLField SQLColumn

data SQLSort =
  SReverse SQLSort
  | SChain SQLSort SQLSort
  | SOrderBy SQLField

data SQLField =
  FieldVal SQLPrim
  | FieldFrom SQLColumn
  | FieldComp [SQLField]
  | FieldPart SQLField String


data STyped a = STyped {
  sqlType :: SQLType,
  content :: a
} deriving Functor

{- Conditions & Sortings -}
sqlValue :: SQLPrim -> STyped SQLField
sqlValue p = STyped (Primitive $ typeName p) (FieldVal p)

sqlCompare :: Bool -> Ordering ->
  STyped SQLField -> STyped SQLField -> Maybe SQLCond
sqlCompare f ord (STyped s a) (STyped t b) = do
  Primitive s' <- pure s
  Primitive t' <- pure t
  guard (s' == t')
  pure $ SCompare f ord a b

sqlContain :: STyped SQLField -> STyped SQLColumn -> Maybe SQLCond
sqlContain (STyped s a) (STyped t b) = do
  Primitive s' <- pure s
  Primitive t' <- pure t
  guard (s' == t')
  pure $ SContain a b

sqlOrderBy :: STyped SQLField -> Maybe SQLSort
sqlOrderBy (STyped s a) = do
  Primitive s' <- pure s
  pure $ SOrderBy a

{- Field manipulations -}
sqlPart :: String -> STyped SQLField -> Maybe (STyped SQLField)
sqlPart name (STyped s a) = do
  Composite tp <- pure s
  t <- lookup name tp
  pure $ STyped t $ FieldPart a name

sqlComp :: [(String, STyped SQLField)] -> STyped SQLField
sqlComp list = let
  comp = content . snd <$> list
  tp = Composite $ fmap sqlType <$> list
  in STyped tp $ FieldComp comp

-- Representative of a database
type SQLDatabase = [(String, STyped SQLColumn)]
