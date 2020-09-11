{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module DataEdit.DataRep where

import Data.Int (Int64)
import Data.Text (Text)
import Data.ByteString (ByteString)

data DataPrim =
  DataInt Int64
  | DataFloat Double
  | DataText Text
  | DataBytes ByteString
  | DataNull

class (Eq a, Ord a) => DataField a where
  readPrim :: DataPrim -> Maybe a
  writePrim :: a -> DataPrim

class DataField k => DataKey k where

class Bundle t where
  singleton :: DataField a => a -> t a
  -- |Should be O(1). Errors out when out of bound
  atIndex :: DataField a => t a -> Int -> a
  -- |Should be O(1). gives nothing When key doesn't exist
  findIndex :: DataKey k => (t k -> k -> Maybe Int)
  merge :: DataField a => [t a] -> t a

data Column t = forall a. (DataField a) => Column (t a)

data Table t = Table {
  keyCol :: String,
  -- Small # of columns
  getColumn :: [(String, Column t)]
}

data View t = View {
  indices :: [Int],
  viewColumn :: [(String, Column t)]
}


