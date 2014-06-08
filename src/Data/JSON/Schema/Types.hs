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
  | Array LengthBound Bool Schema -- ^ An array. The LengthBound represent the
                              -- lower and upper bound of the array
                              -- size. The value 'unboundedLength' indicates no bound.
                              -- The boolean denotes whether items have
                              -- to unique.
  | Tuple [Schema]  -- ^ A fixed-length tuple of different values.
  | Value LengthBound     -- ^ A string. The LengthBound denote the lower and
                    -- upper bound of the length of the string. The
                    -- value 'unboundedLength' indicates no bound.
  | Boolean
  | Number Bound    -- ^ A number. The Bound denote the lower and
                    -- upper bound on the value. The value 'unbounded'
                    -- indicates no bound.
  | Constant Aeson.Value
  | Null
  | Any
  deriving (Eq, Show)

-- | A type for bounds on number domains. Use Nothing when no lower or upper bound makes sense
data Bound = Bound { lower :: Maybe Int, upper :: Maybe Int } deriving (Eq, Show)
-- | A type for bounds on lengths for strings and arrays. Use Nothing when no lower or upper bound makes sense
data LengthBound = LengthBound { lowerLength :: Maybe Int, upperLength :: Maybe Int } deriving (Eq, Show)

unbounded = Bound Nothing Nothing
unboundedLength = LengthBound Nothing Nothing

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
  schema _ = Value unboundedLength

instance JSONSchema a => JSONSchema (Maybe a) where
  schema p = Choice [Object [Field "Just" True $ schema $ fmap fromJust p], Object [Field "Nothing" True Null]]

instance JSONSchema a => JSONSchema [a] where
  schema = Array unboundedLength False . schema . fmap head

instance JSONSchema a => JSONSchema (Vector a) where
  schema = Array unboundedLength False . schema . fmap V.head

instance (IsString k, JSONSchema v) => JSONSchema (M.Map k v) where
  schema = Map . schema . fmap (head . M.elems)

instance (IsString k, JSONSchema v) => JSONSchema (H.HashMap k v) where
  schema = Map . schema . fmap (head . H.elems)
