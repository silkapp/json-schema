{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  #-}
module Test.Util where

import Data.Aeson hiding (Result)
import Data.Aeson.Parser ()
import Data.Aeson.Utils (eitherDecodeV)
import Data.Attoparsec.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (intersperse)
import Data.Proxy
import Test.Tasty.HUnit
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Types  as A
import qualified Data.Vector       as V

import Data.JSON.Schema
import Data.JSON.Schema.Validate

-- | Serializes a value to an aeson Value and validates that against the type's schema
valid :: forall a . (Show a, ToJSON a, FromJSON a, JSONSchema a) => a -> Assertion
valid v = do
  case eitherDecodeV (encode v) of
    Left err -> error err
    Right r  -> case V.toList $ validate sch r of
      []   -> assertBool "x" True
      errs -> assertFailure ("schema validation for " ++ show v ++ " value: " ++ show r ++ " schema: " ++ show sch ++ " errors: " ++ show errs)
  where
    sch = schema (Proxy :: Proxy a)

encDec :: (FromJSON a, ToJSON a) => a -> Either String a
encDec a = case (parse A.value . encode) a of
  Done _ r -> case fromJSON r of A.Success v -> Right v; Error s -> Left $ "fromJSON r=" ++ show r ++ ", s=" ++ s
  Fail _ ss e -> Left . concat $ intersperse "," (ss ++ [e])

unsafeParse :: ByteString -> Value
unsafeParse b = fromResult . parse A.value $ b
  where
    fromResult (Done _ r) = r
    fromResult _ = error $ "unsafeParse failed on: " ++ unpack b

eq :: (Show a, Eq a) => a -> a -> Assertion
eq = (@=?)

bidir ::  (FromJSON a, ToJSON a, Show a, Eq a) => ByteString -> a -> IO ()
bidir s d = do
  eq (unsafeParse s)
     (toJSON d)
  eq (Right d)
     (encDec d)
