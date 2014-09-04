{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  #-}
module Test.Validate (tests) where

import Data.Aeson as A
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
  val' number (toJSON (1::Int))
  val' value (unsafeParse "\"x\"")
  val' (field "p" True number) (unsafeParse "{\"p\" : 1}")

case_lengthBound = do
  eq (v [Err (v []) (LengthBoundError (LengthBound (Just 0) (Just 1)) 3)])
     (validate (Value (LengthBound (Just 0) (Just 1))) (unsafeParse "\"xyz\""))

case_requiredProp = do
  eq (v [Err (v []) (MissingRequiredField "a")])
     (validate (field "a" True number)
               (unsafeParse "{}"))
  eq (v [Err (v []) (MissingRequiredField "a")])
     (validate (field "a" True (field "b" True number))
               (unsafeParse "{}"))
  eq (v [Err (v ["a"]) (MissingRequiredField "b")])
     (validate (field "a" True (field "b" True number))
               (unsafeParse "{\"a\":{}}"))
  eq (v [Err (v ["a","b"]) (MissingRequiredField "c")])
     (validate (field "a" True (field "b" True (field "c" True number)))
               (unsafeParse "{\"a\":{\"b\":{}}}"))

val' s val = v [] @=? validate s val

v = V.fromList
