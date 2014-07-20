module Data.JSON.Schema.Validate (validate) where

import Data.Scientific
import Data.Vector (Vector)
import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as HS
import qualified Data.Text           as T
import qualified Data.Vector         as V

import qualified Data.JSON.Schema as S

validate :: S.Schema -> A.Value -> Bool
validate s v = case (s, v) of
  ( S.Any       , _          ) -> True
  ( S.Null      , A.Null     ) -> True
  ( S.Boolean   , A.Bool{}   ) -> True
  ( S.Constant x, _          ) -> x == v
  ( S.Number   b, A.Number n ) -> inLower b n
                               && inUpper b n
  ( S.Choice  xs, _          ) -> any (`validate` v) xs
  ( S.Tuple   xs, A.Array vs ) -> and $ (length xs == V.length vs) : zipWith validate xs (V.toList vs)
  ( S.Object  fs, A.Object h ) -> all (`validateField` h) fs
  ( S.Map      x, A.Object h ) -> all (validate x) . H.elems $ h
  ( S.Value    b, A.String w ) -> inLowerLength b (T.length w)
                               && inUpperLength b (T.length w)
  ( S.Array b u x, A.Array vs) -> inLowerLength b (V.length vs)
                               && inUpperLength b (V.length vs)
                               && if u then unique vs else True
                               && V.all (validate x) vs
  ( S.Null      , _          ) -> False
  ( S.Boolean   , _          ) -> False
  ( S.Number  {}, _          ) -> False
  ( S.Tuple   {}, _          ) -> False
  ( S.Object  {}, _          ) -> False
  ( S.Map     {}, _          ) -> False
  ( S.Value   {}, _          ) -> False
  ( S.Array   {}, _          ) -> False

validateField :: S.Field -> A.Object -> Bool
validateField f o = maybe (not $ S.required f) (validate $ S.content f) $ H.lookup (S.key f) o

unique :: Vector A.Value -> Bool
unique vs = (== V.length vs) . HS.size . HS.fromList . V.toList $ vs

inLower :: S.Bound -> Scientific -> Bool
inLower b v = maybe True ((<= v) . fromIntegral) . S.lower $ b

inUpper :: S.Bound -> Scientific -> Bool
inUpper b v = maybe True ((>= v) . fromIntegral) . S.upper $ b

inLowerLength :: S.LengthBound -> Int -> Bool
inLowerLength b v = maybe True (<= v) . S.lowerLength $ b

inUpperLength :: S.LengthBound -> Int -> Bool
inUpperLength b v = maybe True (>= v) . S.upperLength $ b
