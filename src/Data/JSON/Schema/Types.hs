{-# LANGUAGE
    FlexibleInstances
  , OverloadedStrings
  , TypeSynonymInstances
  , ScopedTypeVariables
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

import Data.Fixed
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.Scientific
import Data.String
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Vector (Vector)
import Data.Word
import qualified Data.Aeson.Types    as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text.Lazy      as L
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
  | Any                  -- ^ Any value is allowed.
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

integralSchema :: forall a. (Bounded a, Integral a) => Proxy a -> Schema
integralSchema _ =
  Number $ Bound (Just $ fromIntegral (minBound::a))
                 (Just $ fromIntegral (maxBound::a))

unboundedLength :: LengthBound
unboundedLength = LengthBound Nothing Nothing

-- | A field in an object.
data Field = Field { key :: Text, required :: Bool, content :: Schema } deriving (Eq, Show)

-- | Class representing JSON schemas
class JSONSchema a where
  schema :: Proxy a -> Schema

instance JSONSchema () where
  schema _ = Constant Aeson.Null

instance JSONSchema Int where
  schema _ = Number unbounded

instance JSONSchema Integer where
  schema _ = Number unbounded

instance JSONSchema Int8 where
  schema = integralSchema

instance JSONSchema Int16 where
  schema = integralSchema

instance JSONSchema Int32 where
  schema _ = Number unbounded

instance JSONSchema Int64 where
  schema _ = Number unbounded

instance JSONSchema Word where
  schema _ = Number (Bound (Just 0) Nothing)

instance JSONSchema Word8 where
  schema = integralSchema

instance JSONSchema Word16 where
  schema = integralSchema

instance JSONSchema Word32 where
  schema _ = Number (Bound (Just 0) Nothing)

instance JSONSchema Word64 where
  schema _ = Number (Bound (Just 0) Nothing)

instance JSONSchema Float where
  schema _ = Number unbounded

instance JSONSchema Double where
  schema _ = Number unbounded

instance HasResolution a => JSONSchema (Fixed a) where
  schema _ = Number unbounded

instance JSONSchema Scientific where
  schema _ = Number unbounded

instance JSONSchema Bool where
  schema _ = Boolean

instance JSONSchema Text where
  schema _ = Value unboundedLength

instance JSONSchema L.Text where
  schema _ = Value unboundedLength

instance JSONSchema a => JSONSchema (Maybe a) where
  schema p = Choice [Object [Field "Just" True $ schema $ fmap fromJust p], Object [Field "Nothing" True (Constant Aeson.Null)]]

instance JSONSchema a => JSONSchema [a] where
  schema = Array unboundedLength False . schema . fmap head

instance JSONSchema a => JSONSchema (Vector a) where
  schema = Array unboundedLength False . schema . fmap V.head

instance (IsString k, JSONSchema v) => JSONSchema (M.Map k v) where
  schema = Map . schema . fmap (head . M.elems)

instance (IsString k, JSONSchema v) => JSONSchema (H.HashMap k v) where
  schema = Map . schema . fmap (head . H.elems)

instance JSONSchema UTCTime where
  schema _ = Value LengthBound { lowerLength = Just 20, upperLength = Just 24 }

instance JSONSchema a => JSONSchema (S.Set a) where
  schema = Array unboundedLength True . schema . fmap S.findMin

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

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h) => JSONSchema (a, b, c, d, e, f, g, h) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h) -> h) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i) => JSONSchema (a, b, c, d, e, f, g, h, i) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i) -> i) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j) => JSONSchema (a, b, c, d, e, f, g, h, i, j) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j) -> j) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k) -> k) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k, JSONSchema l) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k, l) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k,_) -> k) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,l) -> l) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k, JSONSchema l, JSONSchema m) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_,_,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k,_,_) -> k) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,l,_) -> l) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,m) -> m) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k, JSONSchema l, JSONSchema m, JSONSchema n) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_,_,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_,_,_,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k,_,_,_) -> k) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,l,_,_) -> l) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,m,_) -> m) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,n) -> n) $ s
    ]

instance (JSONSchema a, JSONSchema b, JSONSchema c, JSONSchema d, JSONSchema e, JSONSchema f, JSONSchema g, JSONSchema h, JSONSchema i, JSONSchema j, JSONSchema k, JSONSchema l, JSONSchema m, JSONSchema n, JSONSchema o) => JSONSchema (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  schema s = Tuple
    [ schema . fmap (\(a,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> a) $ s
    , schema . fmap (\(_,b,_,_,_,_,_,_,_,_,_,_,_,_,_) -> b) $ s
    , schema . fmap (\(_,_,c,_,_,_,_,_,_,_,_,_,_,_,_) -> c) $ s
    , schema . fmap (\(_,_,_,d,_,_,_,_,_,_,_,_,_,_,_) -> d) $ s
    , schema . fmap (\(_,_,_,_,e,_,_,_,_,_,_,_,_,_,_) -> e) $ s
    , schema . fmap (\(_,_,_,_,_,f,_,_,_,_,_,_,_,_,_) -> f) $ s
    , schema . fmap (\(_,_,_,_,_,_,g,_,_,_,_,_,_,_,_) -> g) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,h,_,_,_,_,_,_,_) -> h) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,i,_,_,_,_,_,_) -> i) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,j,_,_,_,_,_) -> j) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,k,_,_,_,_) -> k) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,l,_,_,_) -> l) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,m,_,_) -> m) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,n,_) -> n) $ s
    , schema . fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,o) -> o) $ s
    ]
