{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  #-}
module Data.JSON.Schema.Validate
  ( isValid
  , validate
  , ValidationError (..)
  , ErrorType (..)
  ) where

import Control.Applicative
import Control.Monad.RWS.Strict
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap)
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import qualified Data.Vector         as V

import Data.JSON.Schema (Schema)
import qualified Data.JSON.Schema as S

-- | Validates a value against a schema returning errors.
validate :: Schema -> Value -> Vector ValidationError
validate s v = (\(_,_,errs) -> errs) $ runRWS (unM $ validate' s v) V.empty ()

-- | Predicate version of 'validate'.
isValid :: Schema -> Value -> Bool
isValid s v = V.null $ validate s v

data ValidationError = ValidationError
  { path      :: Vector Text -- ^ The Path to the property where the error occured, empty if the error is on the top level.
  , errorType :: ErrorType
  } deriving (Eq, Show)

data ErrorType
  = Mismatch             Schema Value                            -- ^ General type error.
  | BoundError           S.Bound Scientific                      -- ^ Number out of bounds.
  | LengthBoundError     S.LengthBound Int                       -- ^ String or Array out of bounds.
  | TupleLength          Int Int                                 -- ^ Expected and actual tuple length.
  | MissingRequiredField Text                                    -- ^ A required field is missing.
  | ChoiceError          (Vector (Vector ValidationError)) Value -- ^ All choices failed, contains the error of each branch.
  | NonUniqueArray       (HashMap Value Int)                    -- ^ The elements in the array that are duplicated with the number of occurences (at least 2).
  deriving (Eq, Show)

newtype M a = M { unM :: RWS (Vector Text) (Vector ValidationError) () a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter (Vector ValidationError)
    , MonadReader (Vector Text)
    )

ok :: M ()
ok = return ()

err :: ErrorType -> M ()
err e = do
  pth <- ask
  tell . V.singleton . ValidationError pth $ e

cond :: ErrorType -> Bool -> M ()
cond e p = if p then ok else err e

nestPath :: Text -> M a -> M a
nestPath p = local (`V.snoc` p)

validate' :: Schema -> Value -> M ()
validate' sch val = case (sch, val) of
  ( S.Any       , _          ) -> ok
  ( S.Boolean   , A.Bool{}   ) -> ok
  ( S.Constant x, _          ) -> cond (Mismatch sch val) (x == val)
  ( S.Number   b, A.Number n ) ->
    do inLower b n
       inUpper b n
  ( S.Tuple   xs, A.Array vs ) ->
    do let vlen = V.length vs
       let xlen = length xs
       cond (TupleLength xlen vlen) (xlen == vlen)
       sequence_ $ zipWith3
         (\i s -> nestPath (T.pack (show i)) . validate' s)
         [(0::Int)..] xs (V.toList vs)
  ( S.Map      x, A.Object h ) ->
    do let kvs = H.toList h
       mapM_ (\(k,v) -> nestPath k $ validate' x v) kvs
  ( S.Object  fs, A.Object h ) -> mapM_ (`validateField` h) fs
  ( S.Choice   s, _          ) ->
    do let errs = map (`validate` val) s
       if any V.null errs
         then ok
         else err $ ChoiceError (V.fromList errs) val
  ( S.Value    b, A.String w ) ->
    do inLowerLength b (T.length w)
       inUpperLength b (T.length w)
  ( S.Array b u s, A.Array vs) ->
    do inLowerLength b (V.length vs)
       inUpperLength b (V.length vs)
       if u then unique vs else ok
       zipWithM_
         (\i -> nestPath (T.pack (show i)) . validate' s)
         [(0::Int)..] (V.toList vs)
  ( S.Boolean {}, _          ) -> err $ Mismatch sch val
  ( S.Number  {}, _          ) -> err $ Mismatch sch val
  ( S.Tuple   {}, _          ) -> err $ Mismatch sch val
  ( S.Object  {}, _          ) -> err $ Mismatch sch val
  ( S.Map     {}, _          ) -> err $ Mismatch sch val
  ( S.Value   {}, _          ) -> err $ Mismatch sch val
  ( S.Array   {}, _          ) -> err $ Mismatch sch val

validateField :: S.Field -> A.Object -> M ()
validateField f o = maybe req (nestPath (S.key f) . validate' (S.content f)) $ H.lookup (S.key f) o
  where
    req | not (S.required f) = ok
        | otherwise          = err $ MissingRequiredField (S.key f)

unique :: Vector Value -> M ()
unique vs = do
  let dups = H.filter (>= 2) . V.foldl' (\h v -> H.insertWith (+) v 1 h) H.empty $ vs
  unless (H.null dups) $
    err (NonUniqueArray dups)

inLower :: S.Bound -> Scientific -> M ()
inLower b v =
  if maybe True ((<= v) . fromIntegral) . S.lower $ b
    then ok
    else err (BoundError b v)

inUpper :: S.Bound -> Scientific -> M ()
inUpper b v =
  if maybe True ((>= v) . fromIntegral) . S.upper $ b
    then ok
    else err (BoundError b v)

inLowerLength :: S.LengthBound -> Int -> M ()
inLowerLength b v =
  if maybe True (<= v) . S.lowerLength $ b
    then ok
    else err (LengthBoundError b v)

inUpperLength :: S.LengthBound -> Int -> M ()
inUpperLength b v =
  if maybe True (>= v) . S.upperLength $ b
    then ok
    else err (LengthBoundError b v)
