{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  #-}
module Test.Validate (tests) where

import Data.Aeson as A
import Data.Aeson.Parser as A
import Data.Attoparsec.Lazy
import Data.ByteString.Lazy
import Data.JSON.Schema as S
import Data.JSON.Schema.Combinators as S
import Data.JSON.Schema.Validate
import Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

tests :: TestTree
tests = $testGroupGenerator

case_valid = do
  valid number (toJSON (1::Int))
  valid S.value (unsafeParse "\"x\"")
  valid (field "p" True number) (unsafeParse "{\"p\" : 1}")

case_lengthBound = do
  v [Err (v []) (LengthBoundError (LengthBound (Just 0) (Just 1)) 3)]
    @=? validate (Value (LengthBound (Just 0) (Just 1))) (unsafeParse "\"xyz\"")

case_requiredProp = do
  v [Err (v []) (MissingRequiredField "a")] @=? validate (field "a" True number) (unsafeParse "{}")
  v [Err (v []) (MissingRequiredField "a")] @=? validate (field "a" True (field "b" True number)) (unsafeParse "{}")
  v [Err (v ["a"]) (MissingRequiredField "b")] @=? validate (field "a" True (field "b" True number)) (unsafeParse "{\"a\":{}}")
  v [Err (v ["a","b"]) (MissingRequiredField "c")] @=? validate (field "a" True (field "b" True (field "c" True number))) (unsafeParse "{\"a\":{\"b\":{}}}")

valid s val = v [] @=? validate s val

v = V.fromList

unsafeParse :: ByteString -> Value
unsafeParse = fromResult . parse A.value
  where
    fromResult (Done _ r) = r
    fromResult _ = error "unsafeParse failed"
