{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  #-}
module Test.Validate (tests) where

import Data.ByteString.Lazy
import Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Data.JSON.Schema
import Data.JSON.Schema.Combinators
import Data.JSON.Schema.Validate
import Test.Util

tests :: TestTree
tests = $testGroupGenerator

case_valid = do
  check [] number "1"
  check [] value "\"x\""
  check [] (field "p" True number) "{\"p\" : 1}"

case_lengthBound = do
  check [Err (v []) (LengthBoundError (LengthBound (Just 0) (Just 1)) 3)]
        (Value (LengthBound (Just 0) (Just 1)))
        "\"xyz\""

case_requiredProp = do
  check [Err (v []) (MissingRequiredField "a")]
        (field "a" True number)
        "{}"
  check [Err (v []) (MissingRequiredField "a")]
        (field "a" True (field "b" True number))
        "{}"
  check [Err (v ["a"]) (MissingRequiredField "b")]
        (field "a" True (field "b" True number))
        "{\"a\":{}}"
  check [Err (v ["a","b"]) (MissingRequiredField "c")]
        (field "a" True (field "b" True (field "c" True number)))
        "{\"a\":{\"b\":{}}}"

check :: [Err] -> Schema -> ByteString -> Assertion
check errs s val = v errs @=? validate s (unsafeParse val)

v :: [a] -> Vector a
v = V.fromList
