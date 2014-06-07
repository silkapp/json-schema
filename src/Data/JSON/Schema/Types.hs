{-# LANGUAGE
    FlexibleInstances
  , TypeSynonymInstances
  , OverloadedStrings
  #-}
-- | Types for defining JSON schemas.
module Data.JSON.Schema.Types where

import Data.Maybe
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32)
import qualified Data.HashMap.Strict as H
import qualified Data.Map            as M
import qualified Data.Vector         as V
import qualified Data.Aeson.Types    as Aeson

-- | A schema for a JSON value.
data Schema =
    Choice [Schema] -- ^ A choice of multiple values, e.g. for sum types.
  | Object [Field]  -- ^ A JSON object.
  | Map    Schema   -- ^ A JSON object with arbitrary keys.
  | Array Bound Bool Schema -- ^ An array. The integers represent the
                              -- lower and upper bound of the array
                              -- size. The value -1 indicates no bound.
                              -- The boolean denotes whether items have
                              -- to unique.
  | Tuple [Schema]  -- ^ A fixed-length tuple of different values.
  | Value Bound     -- ^ A string. The integers denote the lower and
                    -- upper bound of the length of the string. The
                    -- value -1 indicates no bound.
  | Boolean
  | Number Bound    -- ^ A number. The integers denote the lower and
                    -- upper bound on the value. The value -1
                    -- indicates no bound.
  | Constant Aeson.Value
  | Null
  | Any
  deriving (Eq, Show)

data Bound = Bound (Maybe Int) (Maybe Int) deriving (Eq, Show)

unbounded = Bound Nothing Nothing

-- | A field in an object.
data Field = Field { key :: Text, required :: Bool, content :: Schema } deriving (Eq, Show)

-- | Class representing JSON schemas
class JSONSchema a where
  schema :: Proxy a -> Schema

instance JSONSchema () where
  schema _ = Null

instance JSONSchema Int where
  schema _ = Number unbounded

instance JSONSchema Integer where
  schema _ = Number unbounded

instance JSONSchema Word32 where
  schema _ = Number unbounded

instance JSONSchema Bool where
  schema _ = Boolean

instance JSONSchema Text where
  schema _ = Value unbounded

instance JSONSchema a => JSONSchema (Maybe a) where
  schema p = Choice [Object [Field "Just" True $ schema $ fmap fromJust p], Object [Field "Nothing" True Null]]

instance JSONSchema a => JSONSchema [a] where
  schema = Array unbounded False . schema . fmap head

instance JSONSchema a => JSONSchema (Vector a) where
  schema = Array unbounded False . schema . fmap V.head

instance (IsString k, JSONSchema v) => JSONSchema (M.Map k v) where
  schema = Map . schema . fmap (head . M.elems)

instance (IsString k, JSONSchema v) => JSONSchema (H.HashMap k v) where
  schema = Map . schema . fmap (head . H.elems)
