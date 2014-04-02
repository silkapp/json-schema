{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-- | Types for defining JSON schemas.
module Data.JSON.Schema.Types where

import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Word (Word32)

-- | A schema is any JSON value.
type Schema = Value

-- | A schema for a JSON value.
data Value =
    Choice [Value] -- ^ A choice of multiple values, e.g. for sum types.
  | Object [Field] -- ^ A JSON object.
  | Array Int Int Bool Value -- ^ An array. The integers represent the
                             -- lower and upper bound of the array
                             -- size. The value -1 indicates no bound.
                             -- The boolean denotes whether items have
                             -- to unique.
  | Tuple [Value]   -- ^ A fixed-length tuple of different values.
  | Value Int Int   -- ^ A string. The integers denote the lower and
                    -- upper bound of the length of the string. The
                    -- value -1 indicates no bound.
  | Boolean
  | Number Int Int  -- ^ A number. The integers denote the lower and
                    -- upper bound on the value. The value -1
                    -- indicates no bound.
  | Null
  deriving (Eq, Show)

-- | A field in an object.
data Field = Field { key :: String, required :: Bool, content :: Schema } deriving (Eq, Show)

-- | Class representing JSON schemas
class JSONSchema a where
  schema :: Proxy a -> Schema

instance JSONSchema () where
  schema _ = Null

instance JSONSchema Int where
  schema _ = Number 0 (-1)

instance JSONSchema Integer where
  schema _ = Number 0 (-1)

instance JSONSchema Word32 where
  schema _ = Number 0 4294967295

instance JSONSchema Bool where
  schema _ = Boolean

instance JSONSchema Text where
  schema _ = Value 0 (-1)

instance JSONSchema a => JSONSchema (Maybe a) where
  schema p = Choice [Object [Field "Just" True $ schema $ fmap fromJust p], Object [Field "Nothing" True Null]]

instance JSONSchema a => JSONSchema [a] where
  schema = Array 0 (-1) False . schema . fmap head
