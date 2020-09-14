{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module DataEdit.DataRep where

import Data.Int (Int64)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Foldable (Foldable(..))
import Data.Maybe (maybeToList, fromJust)

import Control.Monad (ap, guard)
import Control.Monad.Reader ( MonadReader(..), ReaderT(..) )
import Control.Monad.Except ( MonadError(..), ExceptT(..) )
import Control.Monad.Identity ( Identity )
import Control.Applicative (Const)

data DataPrim =
  DataInt Int64
  | DataFloat Double
  | DataText Text
  | DataBytes ByteString
  | DataNull

class (Eq a, Ord a) => DataField a where
  typeName :: a -> String
  readPrim :: DataPrim -> Maybe a
  writePrim :: a -> DataPrim

-- |Class for data key. 
class DataField k => DataKey k

data DataAny = forall a. (DataField a) => DataAny a
data KeyAny = forall a. (DataKey a) => KeyAny a

data Composite = Composite {
  getData :: [(String, DataAny)]
}

class Table t where
  singleton :: a -> t a
  merge :: [t a] -> t a
  asList :: t a -> [a]
  search :: t a -> KeyAny -> Maybe a


