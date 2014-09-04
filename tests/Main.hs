{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  #-}
module Main (main) where

import Data.Aeson hiding (Result)
import Data.Proxy
import GHC.Generics (Generic)
import Generics.Generic.Aeson
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Data.Aeson.Types as A

import Data.JSON.Schema
import Data.JSON.Schema.Combinators
import Test.Util
import qualified Data.JSON.Schema as S
import qualified Test.Validate as Validate

data SingleCons = SingleCons deriving (Generic, Show, Eq)
instance ToJSON   SingleCons where toJSON    = gtoJson
instance FromJSON SingleCons where parseJSON = gparseJson
instance JSONSchema SingleCons where schema = gSchema

case_constructorWithoutFields = do
  bidir "\"singleCons\"" SingleCons
  eq (S.Constant (A.String "singleCons"))
     (schema (Proxy :: Proxy SingleCons))
  valid SingleCons

data Record = Record { recordField :: Int } deriving (Generic, Show, Eq)
instance ToJSON   Record where toJSON    = gtoJson
instance FromJSON Record where parseJSON = gparseJson
instance JSONSchema Record where schema = gSchema
case_record = do
  bidir "{\"recordField\":1}" Record { recordField = 1 }
  eq (S.Object [S.Field {S.key = "recordField", S.required = True, S.content = S.Number S.unbounded}])
     (schema (Proxy :: Proxy Record))
  valid Record { recordField = 1 }

data RecordTwoFields = D { d1 :: Int, d2 :: String } deriving (Generic, Show, Eq)
instance ToJSON   RecordTwoFields where toJSON = gtoJson
instance FromJSON RecordTwoFields where parseJSON = gparseJson
instance JSONSchema RecordTwoFields where schema = gSchema
case_recordWithFields = do
  bidir "{\"d1\":1,\"d2\":\"aap\"}"D {d1 = 1, d2 = "aap"}
  eq (S.Object [Field {key = "d1", required = True, content = S.Number S.unbounded}, Field {key = "d2", required = True, content = S.Value S.unboundedLength }])
     (schema (Proxy :: Proxy RecordTwoFields))
  valid D { d1 = 1, d2 = "aap"}

data E = E Int deriving (Generic, Show, Eq)
instance ToJSON   E where toJSON = gtoJson
instance FromJSON E where parseJSON = gparseJson
instance JSONSchema E where schema = gSchema
case_constructorOneField = do
  bidir "1" (E 1)
  eq (S.Number S.unbounded)
     (schema (Proxy :: Proxy E))
  valid $ E 1

data F = F Int String deriving (Generic, Show, Eq)
instance ToJSON   F where toJSON = gtoJson
instance FromJSON F where parseJSON = gparseJson
instance JSONSchema F where schema = gSchema
case_constructorWithFields = do
  bidir "[1,\"aap\"]" (F 1 "aap")
  eq (S.Tuple [S.Number S.unbounded, S.Value S.unboundedLength])
     (schema (Proxy :: Proxy F))
  valid $ F 1 "aap"

data G = G1 Int | G2 String deriving (Generic, Show, Eq)
instance ToJSON   G where toJSON = gtoJson
instance FromJSON G where parseJSON = gparseJson
instance JSONSchema G where schema = gSchema
case_sumConstructorsWithField = do
  bidir "{\"g1\":1}" (G1 1)
  bidir "{\"g2\":\"aap\"}" (G2 "aap")
  eq (S.Choice [S.Object [Field {key = "g1", required = True, content = S.Number S.unbounded}],S.Object [Field {key = "g2", required = True, content = S.Value S.unboundedLength }]])
     (schema (Proxy :: Proxy G))
  valid $ G1 1
  valid $ G2 "aap"

data H = H1 { h1 :: Int } | H2 { h2 :: String } deriving (Generic, Show, Eq)
instance ToJSON   H where toJSON = gtoJson
instance FromJSON H where parseJSON = gparseJson
instance JSONSchema H where schema = gSchema
case_sumRecord = do
  bidir "{\"h1\":{\"h1\":1}}" H1 { h1 = 1 }
  bidir "{\"h2\":{\"h2\":\"aap\"}}" H2 { h2 = "aap" }
  eq (S.Choice [S.Object [Field {key = "h1", required = True, content = S.Object [Field {key = "h1", required = True, content = S.Number S.unbounded}]}],S.Object [Field {key = "h2", required = True, content = S.Object [Field {key = "h2", required = True, content = S.Value S.unboundedLength}]}]])
     (schema (Proxy :: Proxy H))
  valid $ H1 1
  valid $ H2 "aap"

data J = J1 { j1 :: Int, j2 :: String } | J2 deriving (Generic, Show, Eq)
instance ToJSON   J where toJSON = gtoJson
instance FromJSON J where parseJSON = gparseJson
instance JSONSchema J where schema = gSchema
case_sumRecordConstructorWithoutFields = do
  bidir "{\"j1\":{\"j1\":1,\"j2\":\"aap\"}}" J1 {j1 = 1, j2 = "aap"}
  bidir  "{\"j2\":{}}" J2
  eq (S.Choice [S.Object [Field {key = "j1", required = True, content = S.Object [Field {key = "j1", required = True, content = S.Number S.unbounded},Field {key = "j2", required = True, content = S.Value S.unboundedLength }]}],S.Object [Field {key = "j2", required = True, content = S.Object []}]])
     (schema (Proxy :: Proxy J))
  valid $ J1 1 "aap"
  valid $ J2

data L = L1 | L2 Int String deriving (Generic, Show, Eq)
instance ToJSON   L where toJSON = gtoJson
instance FromJSON L where parseJSON = gparseJson
instance JSONSchema L where schema = gSchema
case_sumConstructorWithoutFieldsConstructorWithFields = do
  bidir "{\"l1\":{}}" L1
  bidir "{\"l2\":[1,\"aap\"]}" (L2 1 "aap")
  eq (S.Choice [S.Object [Field {key = "l1", required = True, content = S.Object []}],S.Object [Field {key = "l2", required = True, content = S.Tuple [S.Number S.unbounded, S.Value S.unboundedLength]}]])
     (schema (Proxy :: Proxy L))
  valid L1
  valid (L2 1 "aap")

data M = M1 | M2 Int M deriving (Generic, Show, Eq)
instance ToJSON   M where toJSON = gtoJson
instance FromJSON M where parseJSON = gparseJson
instance JSONSchema M where schema = gSchema
case_sumConstructorWithoutFieldsConstructorWithRecursiveField = do
  let a = M1
  let b = M2 1 M1
  let c = M2 1 (M2 2 M1)
  bidir "{\"m1\":{}}" a
  bidir "{\"m2\":[1,{\"m1\":{}}]}" b
  bidir "{\"m2\":[1,{\"m2\":[2,{\"m1\":{}}]}]}" c
  -- Infinite schema, so we just validate
  valid a
  valid b
  valid c

data N = N1 | N2 { n1 :: Int, n2 :: N } deriving (Generic, Show, Eq)
instance ToJSON   N where toJSON = gtoJson
instance FromJSON N where parseJSON = gparseJson
instance JSONSchema N where schema = gSchema
case_sum_constructorWithoutFields_record = do
  bidir "{\"n1\":{}}" N1
  bidir "{\"n2\":{\"n2\":{\"n1\":{}},\"n1\":1}}" N2 { n1 = 1, n2 = N1 }
  bidir "{\"n2\":{\"n1\":1,\"n2\":{\"n2\":{\"n1\":2,\"n2\":{\"n1\":{}}}}}}" N2 { n1 = 1, n2 = N2 { n1 = 2, n2 = N1 } }
  -- Infinite schema, so we just validate
  valid $ N1
  valid $ N2 1 (N2 2 N1)
  valid $ N2 1 N1

data O = O { o :: [Int] } deriving (Generic, Show, Eq)
instance ToJSON   O where toJSON = gtoJson
instance FromJSON O where parseJSON = gparseJson
instance JSONSchema O where schema = gSchema
case_recordListField = do
  bidir "{\"o\":[1,2,3]}" O {o = [1,2,3]}
  eq (S.Object [Field {key = "o", required = True, content = S.Array S.unboundedLength False (S.Number S.unbounded)}])
     (schema (Proxy :: Proxy O))
  valid $ O [1,2,3]

data P = P [Int] deriving (Generic, Show, Eq)
instance ToJSON   P where toJSON = gtoJson
instance FromJSON P where parseJSON = gparseJson
instance JSONSchema P where schema = gSchema
case_constructorListField = do
  bidir "[1,2,3]" (P [1,2,3])
  eq (S.Array S.unboundedLength False (S.Number S.unbounded))
     (schema (Proxy :: Proxy P))
  valid $ P [1,2,3]

data Q = Q Int Int Int deriving (Generic, Show, Eq)
instance ToJSON   Q where toJSON = gtoJson
instance FromJSON Q where parseJSON = gparseJson
instance JSONSchema Q where schema = gSchema
case_ConstructorSameTypedFields = do
  bidir "[1,2,3]" (Q 1 2 3)
  eq (S.Tuple [S.Number S.unbounded, S.Number S.unbounded, S.Number S.unbounded])
     (schema (Proxy :: Proxy Q))
  valid $ Q 1 2 3

data T = T { r1 :: Maybe Int } deriving (Generic, Show, Eq)
instance ToJSON   T where toJSON = gtoJson
instance FromJSON T where parseJSON = gparseJson
instance JSONSchema T where schema = gSchema
case_RecordMaybeField = do
  bidir "{}" T { r1 = Nothing }
  bidir "{\"r1\":1}" T { r1 = Just 1 }
  eq (S.Object [Field {key = "r1", required = False, content = S.Number S.unbounded}])
     (schema (Proxy :: Proxy T))
  valid $ T Nothing
  valid $ T (Just 1)

data V = V1 | V2 | V3 deriving (Generic, Show, Eq)
instance ToJSON   V where toJSON = gtoJson
instance FromJSON V where parseJSON = gparseJson
instance JSONSchema V where schema = gSchema
case_constructorsWithoutFields = do
  bidir "\"v1\"" V1
  bidir "\"v2\"" V2
  eq (S.Choice [S.Constant (A.String "v1"), S.Constant (A.String "v2"), S.Constant (A.String "v3")])
     (schema (Proxy :: Proxy V))
  valid V1
  valid V2

data W = W { underscore1_ :: Int, _underscore2 :: Int } deriving (Generic, Show, Eq)
instance ToJSON   W where toJSON = gtoJson
instance FromJSON W where parseJSON = gparseJson
instance JSONSchema W where schema = gSchema
case_recordWithUnderscoredFields = do
  bidir "{\"underscore1\":1,\"underscore2\":2}" W {underscore1_ = 1, _underscore2 = 2}
  eq (S.Object [Field {key = "underscore1", required = True, content = S.Number S.unbounded},Field {key = "underscore2", required = True, content = S.Number S.unbounded}])
     (schema (Proxy :: Proxy W))
  valid $ W 1 2

data Strip = Strip { stripA :: Int, strip_B :: Int, stripC_ :: Int, strip :: Int } deriving (Generic, Show, Eq)
stripSettings :: Settings
stripSettings = defaultSettings { stripPrefix = Just "strip" }
instance ToJSON     Strip where toJSON    = gtoJsonWithSettings    stripSettings
instance FromJSON   Strip where parseJSON = gparseJsonWithSettings stripSettings
instance JSONSchema Strip where schema    = gSchemaWithSettings    stripSettings
case_strip = do
  bidir "{\"a\":1,\"b\":2,\"c\":3,\"strip\":4}" Strip { stripA = 1, strip_B = 2, stripC_ = 3, strip = 4 }
  eq (S.Object [ Field { key = "a"    , required = True, content = S.Number S.unbounded }
               , Field { key = "b"    , required = True, content = S.Number S.unbounded }
               , Field { key = "c"    , required = True, content = S.Number S.unbounded }
               , Field { key = "strip", required = True, content = S.Number S.unbounded }
               ])
     (schema (Proxy :: Proxy Strip))
  valid (Strip 1 2 3 4)

data Stat = StatA | StatB (Maybe Prog)
  deriving (Eq, Generic, Show)
data Prog = Prog { aff :: Int }
  deriving (Eq, Generic, Show)
instance ToJSON     Prog where toJSON    = gtoJson
instance FromJSON   Prog where parseJSON = gparseJson
instance JSONSchema Prog where schema    = gSchema
instance ToJSON     Stat where toJSON    = gtoJson
instance FromJSON   Stat where parseJSON = gparseJson
instance JSONSchema Stat where schema    = gSchema
case_stat = do
  let a = StatB (Just Prog { aff = 1 })
  bidir "{\"statB\":{\"aff\":1}}" a
  valid a

  let b = StatB Nothing
  bidir "{\"statB\":null}" b
  eq (field "statA" True empty <|> field "statB" True (nullable $ field "aff" True number))
     (schema (Proxy :: Proxy Stat))
  valid b

-- https://github.com/silkapp/generic-aeson/issues/2
data X = X (Maybe Int) Int deriving (Eq, Generic, Show)
instance ToJSON     X where toJSON    = gtoJson
instance FromJSON   X where parseJSON = gparseJson
instance JSONSchema X where schema    = gSchema
case_constructorWithMaybeField = do
  let a = X (Just 1) 2
  bidir "[1,2]" a
  valid a

  let b = X Nothing 2
  bidir "[null,2]" b
  valid b

  eq (Left "when expecting a Int, encountered Boolean instead" :: Either String X)
     (eitherDecode "[true,2]")

  eq (Tuple [nullable number, number])
     (schema (Proxy :: Proxy X))

data X1 = X1 { x1a :: Maybe Int, x1b :: Int } deriving (Eq, Generic, Show)
instance ToJSON     X1 where toJSON    = gtoJson
instance FromJSON   X1 where parseJSON = gparseJson
instance JSONSchema X1 where schema    = gSchema
case_recordWithMaybeField = do
  let a = X1 { x1a = Just 1, x1b = 2 }
  bidir "{\"x1a\" : 1, \"x1b\" : 2}}" a
  valid a

  let b = X1 Nothing 2
  bidir "{ \"x1b\" : 2 }" b
  eq (Nothing :: Maybe X1)
     (decode "{\"x1a\":true,\"x1b\":2}")
  valid b

  eq (S.Object [Field "x1a" False number, Field "x1b" True number])
     (schema (Proxy :: Proxy X1))

  -- Regression test
  eq (Nothing :: Maybe X1)
     (decode "[true,2]")

-- https://github.com/silkapp/generic-aeson/issues/3
data X2 = X2 { x2 :: Maybe Int }
  deriving (Eq, Generic, Show)
instance ToJSON   X2 where toJSON = gtoJson
instance FromJSON X2 where parseJSON = gparseJson
instance JSONSchema X2 where schema = gSchema
case_recordWithOnlyOneMaybeField = do
  eq (Nothing :: Maybe X2)
     (decode "[{\"x2\":1}]")
  bidir "{\"x2\":1}" X2 { x2 = Just 1 }
  valid $ X2 (Just 1)

data MaybeStringCon = MaybeStringCon (Maybe String)
  deriving (Eq, Generic, Show)
instance ToJSON     MaybeStringCon where toJSON    = gtoJson
instance FromJSON   MaybeStringCon where parseJSON = gparseJson
instance JSONSchema MaybeStringCon where schema    = gSchema
data MaybeString = MaybeString { ms :: Maybe String }
  deriving (Eq, Generic, Show)
instance ToJSON     MaybeString where toJSON    = gtoJson
instance FromJSON   MaybeString where parseJSON = gparseJson
instance JSONSchema MaybeString where schema    = gSchema
case_maybeString = do
  let a = MaybeStringCon (Just "x")
  bidir "\"x\"" a

  eq (nullable value)
     (schema (Proxy :: Proxy MaybeStringCon))

  valid a

  let b = MaybeString { ms = Just "x" }
  bidir "{\"ms\":\"x\"}" b

  valid b

  let c = MaybeString Nothing
  bidir "{}" c

  valid c

  eq (field "ms" False value)
     (schema (Proxy :: Proxy MaybeString))

tests :: TestTree
tests = $testGroupGenerator

main :: IO ()
main = do
  defaultMain $ testGroup "generic-aeson" [tests, Validate.tests]
