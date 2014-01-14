{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Data.JSON.Schema.Types where

import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Word (Word32)

-- | Based on the syntax specification of http://www.json.org/

data Field = Field { key :: String, required :: Bool, content :: Schema } deriving (Eq, Show)

data Value =
    Choice [Value]
  | Object [Field]
  | Array Int Int Bool Value -- The integers represent the lower and upper bound of the array size
                             -- The boolean denotes whether items have to unique
  | Tuple [Value]
  | Value Int Int   -- The integers denote the lower and upper bound of the length of the string
  | Boolean
  | Number Int Int  -- The integers denote the lower and upper bound on the value
  | Null
  deriving (Eq, Show)

type Schema = Value

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
