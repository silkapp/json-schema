{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  #-}
module Data.JSON.Schema.Validate (isValid, validate) where

import Control.Applicative
import Control.Monad.RWS.Strict
import Data.Aeson (Value)
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as HS
import qualified Data.Text           as T
import qualified Data.Vector         as V

import Data.JSON.Schema (Schema)
import qualified Data.JSON.Schema as S

data Err = Err [Text] Err'
data Err'
  = Mismatch Schema Value
  | BoundError S.Bound Scientific
  | LengthBoundError S.LengthBound Int
  | TupleLength Int (Vector Value)
  | MissingRequiredField Text
  | ChoiceError [[Err]] Value
  | NonUniqueArray (Vector Value)

newtype M a = M { unM :: RWS [Text] [Err] () a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter [Err]
    , MonadReader [Text]
    )

ok :: M ()
ok = return ()

err :: Err' -> M ()
err e = do
  path <- ask
  tell . (: []) . Err path $ e

cond :: Err' -> Bool -> M ()
cond e p = if p then ok else err e

isValid :: Schema -> Value -> Bool
isValid s v = null $ validate s v

validate :: Schema -> Value -> [Err]
validate s v = (\(_,_,errs) -> errs) $ runRWS (unM $ validate' s v) [] ()

validate' :: Schema -> Value -> M ()
validate' sch val = case (sch, val) of
  ( S.Any       , _          ) -> ok
  ( S.Null      , A.Null     ) -> ok
  ( S.Boolean   , A.Bool{}   ) -> ok
  ( S.Constant x, _          ) -> cond (Mismatch sch val) (x == val)
  ( S.Number   b, A.Number n ) ->
    do inLower b n
       inUpper b n
  ( S.Tuple   xs, A.Array vs ) ->
    do cond (TupleLength (length xs) vs) (length xs == V.length vs)
       sequence_ $ zipWith3
         (\i s v -> local (T.pack (show i) :) $ validate' s v)
         [0..] xs (V.toList vs)
  ( S.Map      x, A.Object h ) ->
    do let kvs = H.toList h
       mapM_ (\(k,v) -> local (k :) $ validate' x v) kvs
  ( S.Object  fs, A.Object h ) -> mapM_ (`validateField` h) fs
  ( S.Choice   s, _          ) ->
    do let errs = map (`validate` val) s
       if all null errs
         then ok
         else err $ ChoiceError errs val
  ( S.Value    b, A.String w ) ->
    do inLowerLength b (T.length w)
       inUpperLength b (T.length w)
  ( S.Array b u s, A.Array vs) ->
    do inLowerLength b (V.length vs)
       inUpperLength b (V.length vs)
       if u then unique vs else ok
       sequence_ $ zipWith
         (\i -> local (T.pack (show i) :) . validate' s)
         [0..] (V.toList vs)
  ( S.Null    {}, _          ) -> err $ Mismatch sch val
  ( S.Boolean {}, _          ) -> err $ Mismatch sch val
  ( S.Number  {}, _          ) -> err $ Mismatch sch val
  ( S.Tuple   {}, _          ) -> err $ Mismatch sch val
  ( S.Object  {}, _          ) -> err $ Mismatch sch val
  ( S.Map     {}, _          ) -> err $ Mismatch sch val
  ( S.Value   {}, _          ) -> err $ Mismatch sch val
  ( S.Array   {}, _          ) -> err $ Mismatch sch val

validateField :: S.Field -> A.Object -> M ()
validateField f o = maybe req (validate' $ S.content f) $ H.lookup (S.key f) o
  where
    req | not (S.required f) = ok
        | otherwise          = err $ MissingRequiredField (S.key f)

unique :: Vector Value -> M ()
unique vs =
  when (not . (== V.length vs) . HS.size . HS.fromList . V.toList $ vs) $
    err (NonUniqueArray vs)

inLower :: S.Bound -> Scientific -> M ()
inLower b v =
  if (maybe True ((<= v) . fromIntegral) . S.lower $ b)
    then ok
    else err (BoundError b v)

inUpper :: S.Bound -> Scientific -> M ()
inUpper b v =
  if (maybe True ((>= v) . fromIntegral) . S.upper $ b)
    then ok
    else err (BoundError b v)

inLowerLength :: S.LengthBound -> Int -> M ()
inLowerLength b v =
  if (maybe True (<= v) . S.lowerLength $ b)
    then ok
    else err (LengthBoundError b v)

inUpperLength :: S.LengthBound -> Int -> M ()
inUpperLength b v =
  if (maybe True (>= v) . S.upperLength $ b)
    then ok
    else err (LengthBoundError b v)
