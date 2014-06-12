{-# LANGUAGE
    FlexibleInstances
  , OverloadedStrings
  , TypeSynonymInstances
  #-}
-- | Types for defining JSON schemas.
module Data.JSON.Schema.Types
  ( JSONSchema (..)
  , Schema (..)
  , Field (..)
  , Bound (..)
  , LengthBound (..)
  , unbounded
  , unboundedLength
  ) where

import Data.Maybe
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32)
import qualified Data.Aeson.Types    as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Map            as M
import qualified Data.Vector         as V

-- | A schema for a JSON value.
data Schema =
    Choice [Schema]      -- ^ A choice of multiple values, e.g. for sum types.
  | Object [Field]       -- ^ A JSON object.
  | Map    Schema        -- ^ A JSON object with arbitrary keys.
  | Array LengthBound Bool Schema
                         -- ^ An array. The LengthBound represent the
                         -- lower and upper bound of the array
                         -- size. The value 'unboundedLength' indicates no bound.
                         -- The boolean denotes whether items have
                         -- to be unique.
  | Tuple [Schema]       -- ^ A fixed-length tuple of different values.
  | Value LengthBound    -- ^ A string. The LengthBound denote the lower and
                         -- upper bound of the length of the string. The
                         -- value 'unboundedLength' indicates no bound.
  | Boolean              -- ^ A Bool.
  | Number Bound         -- ^ A number. The Bound denote the lower and
                         -- upper bound on the value. The value 'unbounded'
                         -- indicates no bound.
  | Constant Aeson.Value -- ^ A Value that never changes. Can be
                         -- combined with Choice to create enumerables.
  | Null                 -- ^ Only null is allowed.
  | Any                  -- ^ Anything value is allowed.
  deriving (Eq, Show)

-- | A type for bounds on number domains. Use Nothing when no lower or upper bound makes sense
data Bound = Bound
  { lower :: Maybe Int
  , upper :: Maybe Int
  } deriving (Eq, Show)

-- | A type for bounds on lengths for strings and arrays. Use Nothing when no lower or upper bound makes sense
data LengthBound = LengthBound
  { lowerLength :: Maybe Int
  , upperLength :: Maybe Int
  } deriving (Eq, Show)

unbounded :: Bound
unbounded = Bound Nothing Nothing

unboundedLength :: LengthBound
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

instance (JSONSchema a, JSONSchema b) => JSONSchema (a, b) where
  schema s = Tuple
    [ schema . fmap fst $ s
    , schema . fmap snd $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c) => JSONSchema (a, b, c) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_) -> a) $ s
    , schema . fmap (\(_,b,_) -> b) $ s
    , schema . fmap (\(_,_,c) -> c) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d) => JSONSchema (a, b, c, d) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_) -> c) $ s
    , schema . fmap (\(_,_,_,d) -> d) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e) => JSONSchema (a, b, c, d, e) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e) -> e) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f) => JSONSchema (a, b, c, d, e, f) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f) -> f) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g) => JSONSchema (a, b, c, d, e, f, g) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g) -> g) $ s
    ]
