{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , TemplateHaskell
  , TypeFamilies
  #-}
module Main (main) where

import Data.Aeson hiding (Result)
import Data.Proxy
import Data.JSON.Schema (JSONSchema (..), gSchema, Field (..))
import qualified Data.JSON.Schema as S
import Data.Aeson.Parser
import Data.Attoparsec.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse)
import GHC.Generics (Generic)
import Generics.Generic.Aeson
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Data.Aeson.Types as A

data SingleCons = SingleCons deriving (Generic, Show, Eq)
instance ToJSON   SingleCons where toJSON    = gtoJson
instance FromJSON SingleCons where parseJSON = gparseJson
instance JSONSchema SingleCons where schema = gSchema

case_constructorWithoutFields = do
  eq (unsafeParse "\"singleCons\"", Right SingleCons)
     (toJSON SingleCons , encDec SingleCons)
  eq (S.Constant (A.String "singleCons"))
     (schema (Proxy :: Proxy SingleCons))

data Record = Record { field :: Int } deriving (Generic, Show, Eq)
instance ToJSON   Record where toJSON    = gtoJson
instance FromJSON Record where parseJSON = gparseJson
instance JSONSchema Record where schema = gSchema
case_record = do
  eq (unsafeParse "{\"field\":1}"          , Right (Record { field = 1 }))
     (toJSON Record { field = 1 }, encDec Record { field = 1 })
  eq (S.Object [S.Field {S.key = "field", S.required = True, S.content = S.Number 0 (-1)}])
     (schema (Proxy :: Proxy Record))

data RecordTwoFields = D { d1 :: Int, d2 :: String } deriving (Generic, Show, Eq)
instance ToJSON   RecordTwoFields where toJSON = gtoJson
instance FromJSON RecordTwoFields where parseJSON = gparseJson
instance JSONSchema RecordTwoFields where schema = gSchema
case_recordWithFields = do
  eq (unsafeParse "{\"d1\":1,\"d2\":\"aap\"}"  , Right (D {d1 = 1, d2 = "aap"}))
     (toJSON D { d1 = 1, d2 = "aap" }, encDec D { d1 = 1, d2 = "aap" })
  eq (S.Object [Field {key = "d1", required = True, content = S.Number 0 (-1)},Field {key = "d2", required = True, content = S.Value 0 (-1)}])
     (schema (Proxy :: Proxy RecordTwoFields))

data E = E Int deriving (Generic, Show, Eq)
instance ToJSON   E where toJSON = gtoJson
instance FromJSON E where parseJSON = gparseJson
instance JSONSchema E where schema = gSchema
case_constructorOneField = do
  eq (unsafeParse "1"        , Right (E 1))
     (toJSON (E 1) , encDec (E 1))
  eq (S.Number 0 (-1))
     (schema (Proxy :: Proxy E))

data F = F Int String deriving (Generic, Show, Eq)
instance ToJSON   F where toJSON = gtoJson
instance FromJSON F where parseJSON = gparseJson
instance JSONSchema F where schema = gSchema
case_constructorWithFields = do
  eq (unsafeParse "[1,\"aap\"]",Right (F 1 "aap"))
     (toJSON (F 1 "aap"), encDec (F 1 "aap"))
  eq (S.Tuple [S.Number 0 (-1),S.Value 0 (-1)])
     (schema (Proxy :: Proxy F))

data G = G1 Int | G2 String deriving (Generic, Show, Eq)
instance ToJSON   G where toJSON = gtoJson
instance FromJSON G where parseJSON = gparseJson
instance JSONSchema G where schema = gSchema
case_sumConstructorsWithField = do
  eq (unsafeParse "{\"g1\":1}",unsafeParse "{\"g2\":\"aap\"}",Right (G1 1),Right (G2 "aap"))
     (toJSON (G1 1), toJSON (G2 "aap"), encDec (G1 1), encDec (G2 "aap"))
  eq (S.Choice [S.Object [Field {key = "g1", required = True, content = S.Number 0 (-1)}],S.Object [Field {key = "g2", required = True, content = S.Value 0 (-1)}]])
     (schema (Proxy :: Proxy G))

data H = H1 { h1 :: Int } | H2 { h2 :: String } deriving (Generic, Show, Eq)
instance ToJSON   H where toJSON = gtoJson
instance FromJSON H where parseJSON = gparseJson
instance JSONSchema H where schema = gSchema
case_sumRecord = do
  eq (unsafeParse "{\"h1\":{\"h1\":1}}",unsafeParse "{\"h2\":{\"h2\":\"aap\"}}",Right (H1 {h1 = 1}),Right (H2 {h2 = "aap"}))
     (toJSON (H1 1), toJSON (H2 "aap"), encDec (H1 1), encDec (H2 "aap"))
  eq (S.Choice [S.Object [Field {key = "h1", required = True, content = S.Object [Field {key = "h1", required = True, content = S.Number 0 (-1)}]}],S.Object [Field {key = "h2", required = True, content = S.Object [Field {key = "h2", required = True, content = S.Value 0 (-1)}]}]])
     (schema (Proxy :: Proxy H))

data J = J1 { j1 :: Int, j2 :: String } | J2 deriving (Generic, Show, Eq)
instance ToJSON   J where toJSON = gtoJson
instance FromJSON J where parseJSON = gparseJson
instance JSONSchema J where schema = gSchema
case_sumRecordConstructorWithoutFields = do
  eq (unsafeParse "{\"j1\":{\"j1\":1,\"j2\":\"aap\"}}",unsafeParse "{\"j2\":{}}",Right (J1 {j1 = 1, j2 = "aap"}),Right J2)
     (toJSON (J1 1 "aap"), toJSON J2, encDec (J1 1 "aap"), encDec J2)
  eq (S.Choice [S.Object [Field {key = "j1", required = True, content = S.Object [Field {key = "j1", required = True, content = S.Number 0 (-1)},Field {key = "j2", required = True, content = S.Value 0 (-1)}]}],S.Object [Field {key = "j2", required = True, content = S.Object []}]])
     (schema (Proxy :: Proxy J))

data L = L1 | L2 Int String deriving (Generic, Show, Eq)
instance ToJSON   L where toJSON = gtoJson
instance FromJSON L where parseJSON = gparseJson
instance JSONSchema L where schema = gSchema
case_sumConstructorWithoutFieldsConstructorWithFields = do
  eq (unsafeParse "{\"l1\":{}}",unsafeParse "{\"l2\":[1,\"aap\"]}",Right L1,Right (L2 1 "aap"))
     (toJSON L1, toJSON (L2 1 "aap"), encDec L1, encDec (L2 1 "aap"))
  eq (S.Choice [S.Object [Field {key = "l1", required = True, content = S.Object []}],S.Object [Field {key = "l2", required = True, content = S.Tuple [S.Number 0 (-1),S.Value 0 (-1)]}]])
     (schema (Proxy :: Proxy L))

data M = M1 | M2 Int M deriving (Generic, Show, Eq)
instance ToJSON   M where toJSON = gtoJson
instance FromJSON M where parseJSON = gparseJson
instance JSONSchema M where schema = gSchema
case_sumConstructorWithoutFieldsConstructorWithRecursiveField = do
  eq (unsafeParse "{\"m1\":{}}",unsafeParse "{\"m2\":[1,{\"m1\":{}}]}",unsafeParse "{\"m2\":[1,{\"m2\":[2,{\"m1\":{}}]}]}",Right M1,Right (M2 1 M1),Right (M2 1 (M2 2 M1)))
     (toJSON M1, toJSON (M2 1 M1), toJSON (M2 1 (M2 2 M1)), encDec M1, encDec (M2 1 M1), encDec (M2 1 (M2 2 M1)))
-- TODO Recursion
--  eq (S.Any)
--     (schema (Proxy :: Proxy x))

data N = N1 | N2 { n1 :: Int, n2 :: N } deriving (Generic, Show, Eq)
instance ToJSON   N where toJSON = gtoJson
instance FromJSON N where parseJSON = gparseJson
instance JSONSchema N where schema = gSchema
case_sum_constructorWithoutFields_record = do
  eq (unsafeParse "{\"n1\":{}}",unsafeParse "{\"n2\":{\"n2\":{\"n1\":{}},\"n1\":1}}",unsafeParse "{\"n2\":{\"n1\":1,\"n2\":{\"n2\":{\"n1\":2,\"n2\":{\"n1\":{}}}}}}",Right N1,Right (N2 {n1 = 1, n2 = N1}),Right (N2 {n1 = 1, n2 = N2 {n1 = 2, n2 = N1}}))
     (toJSON N1, toJSON (N2 1 N1), toJSON (N2 1 (N2 2 N1)), encDec N1, encDec (N2 1 N1), encDec (N2 1 (N2 2 N1)))
  -- TODO Recursive types produce infinite schemas
  -- schema (Proxy :: Proxy N) @=? schema (Proxy :: Proxy N)

data O = O { o :: [Int] } deriving (Generic, Show, Eq)
instance ToJSON   O where toJSON = gtoJson
instance FromJSON O where parseJSON = gparseJson
instance JSONSchema O where schema = gSchema
case_recordListField = do
  eq (unsafeParse "{\"o\":[1,2,3]}",Right (O {o = [1,2,3]}))
     (toJSON (O [1,2,3]), encDec (O [1,2,3]))
  eq (S.Object [Field {key = "o", required = True, content = S.Array 0 (-1) False (S.Number 0 (-1))}])
     (schema (Proxy :: Proxy O))

data P = P [Int] deriving (Generic, Show, Eq)
instance ToJSON   P where toJSON = gtoJson
instance FromJSON P where parseJSON = gparseJson
instance JSONSchema P where schema = gSchema
case_constructorListField = do
  eq (unsafeParse "[1,2,3]",Right (P [1,2,3]))
     (toJSON (P [1,2,3]), encDec (P [1,2,3]))
  eq (S.Array 0 (-1) False (S.Number 0 (-1)))
     (schema (Proxy :: Proxy P))

data Q = Q Int Int Int deriving (Generic, Show, Eq)
instance ToJSON   Q where toJSON = gtoJson
instance FromJSON Q where parseJSON = gparseJson
instance JSONSchema Q where schema = gSchema
case_ConstructorSameTypedFields = do
  eq (unsafeParse "[1,2,3]",Right (Q 1 2 3))
     (toJSON (Q 1 2 3), encDec (Q 1 2 3))
  eq (S.Tuple [S.Number 0 (-1),S.Number 0 (-1),S.Number 0 (-1)])
     (schema (Proxy :: Proxy Q))

data T = T { r1 :: Maybe Int } deriving (Generic, Show, Eq)
instance ToJSON   T where toJSON = gtoJson
instance FromJSON T where parseJSON = gparseJson
instance JSONSchema T where schema = gSchema
case_RecordMaybeField = do
  eq (unsafeParse "{}", unsafeParse "{\"r1\":1}",Right (T {r1 = Nothing}),Right (T {r1 = Just 1}))
     (toJSON (T Nothing), toJSON (T (Just 1)), encDec (T Nothing), encDec (T (Just 1)))
  eq (S.Object [Field {key = "r1", required = False, content = S.Number 0 (-1)}])
     (schema (Proxy :: Proxy T))

data V = V1 | V2 | V3 deriving (Generic, Show, Eq)
instance ToJSON   V where toJSON = gtoJson
instance FromJSON V where parseJSON = gparseJson
instance JSONSchema V where schema = gSchema
case_constructorsWithoutFields = do
  eq (unsafeParse "\"v1\"",unsafeParse "\"v2\"",Right V1,Right V2)
     (toJSON V1, toJSON V2, encDec V1, encDec V2)
  eq (S.Choice [S.Constant (A.String "v1"), S.Constant (A.String "v2"), S.Constant (A.String "v3")])
     (schema (Proxy :: Proxy V))

data W = W { underscore1_ :: Int, _underscore2 :: Int } deriving (Generic, Show, Eq)
instance ToJSON   W where toJSON = gtoJson
instance FromJSON W where parseJSON = gparseJson
instance JSONSchema W where schema = gSchema
case_recordWithUnderscoredFields = do
  eq (unsafeParse "{\"underscore1\":1,\"underscore2\":2}",Right (W {underscore1_ = 1, _underscore2 = 2}))
     (toJSON (W 1 2), encDec (W 1 2))
  eq (S.Object [Field {key = "underscore1_", required = True, content = S.Number 0 (-1)},Field {key = "_underscore2", required = True, content = S.Number 0 (-1)}])
     (schema (Proxy :: Proxy W))

-- Helpers

encDec :: (FromJSON a, ToJSON a) => a -> Either String a
encDec a = case (parse value . encode) a of
  Done _ r -> case fromJSON r of A.Success v -> Right v; Error s -> Left $ "fromJSON r=" ++ show r ++ ", s=" ++ s
  Fail _ ss e -> Left . concat $ intersperse "," (ss ++ [e])

unsafeParse :: ByteString -> Value
unsafeParse = fromResult . parse value

fromResult (Done _ r) = r
fromResult _ = error "Boo"

eq :: (Show a, Eq a) => a -> a -> Assertion
eq = (@=?)

tests :: TestTree
tests = $testGroupGenerator

main :: IO ()
main = defaultMain $ testGroup "generic-aeson" [tests]
