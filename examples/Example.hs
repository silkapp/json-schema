{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  #-}
module Example where

import Prelude.Compat

import Control.Monad.Compat
import Data.Aeson hiding (Object)
import Data.Text (Text)
import GHC.Generics
import Generics.Generic.Aeson
import qualified Data.Aeson as A

import Data.JSON.Schema

-- Manually defined instances

data User = User { name :: Text, age :: Int }

instance ToJSON User where
  toJSON u = object [ "name" .= name u
                    , "age"  .= age u
                    ]

instance FromJSON User where
  parseJSON (A.Object u) = User <$> u .: "name" <*> u .: "age"
  parseJSON _            = mzero

instance JSONSchema User where
  schema pu = Object [ Field { key = "name", required = True, content = schema $ fmap name pu }
                     , Field { key = "age" , required = True, content = schema $ fmap age  pu }
                     ]

-- Using generic-aeson

data User2 = User2 { name2 :: Text, age2 :: Text }
  deriving Generic

instance ToJSON User2 where
  toJSON = gtoJson

instance FromJSON User2 where
  parseJSON = gparseJson

instance JSONSchema User2 where
  schema = gSchema
