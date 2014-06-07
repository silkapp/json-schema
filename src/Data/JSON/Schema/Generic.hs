{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , OverlappingInstances
  , ScopedTypeVariables
  , TupleSections
  , TypeFamilies
  , TypeOperators
  #-}
-- | Generic derivation of schemas. The schemas generated match the
-- JSON generated by type 'generic-aeson' package. See that package
-- for documentation on the format and examples of it.
module Data.JSON.Schema.Generic (gSchema) where

import Control.Applicative hiding (empty, (<|>))
import Data.Char
import Data.JSON.Schema.Combinators
import Data.JSON.Schema.Types
import Data.Maybe
import Data.Proxy
import GHC.Generics
import Generics.Deriving.ConNames
import Generics.Generic.IsEnum
import qualified Data.Aeson.Types as Aeson
import Data.Text (pack)

class GJSONSCHEMA f where
  gSchema' :: Bool -> [String] -> Proxy (f a) -> Schema

-- Recursive positions disabled for now, it causes infintite data structures. This is a problem to be solved!
{-
instance GJSONSCHEMA I where
  gSchema' _ f = f . fmap unI
-}

instance JSONSchema c => GJSONSCHEMA (K1 i c) where
  gSchema' _ _ = schema . fmap unK1

instance GJSONSCHEMA (K1 i String) where
  gSchema' _ _ _ = Value unbounded

instance GJSONSCHEMA U1 where
  gSchema' _ _ _ = empty

instance (GJSONSCHEMA f, GJSONSCHEMA g) => GJSONSCHEMA (f :+: g) where
  gSchema' enm names p =
        gSchema' enm names (gL <$> p)
    <|> gSchema' enm names (gR <$> p)
    where
      gL :: (f :+: g) r -> f r
      gL _ = undefined
      gR :: (f :+: g) r -> g r
      gR _ = undefined

gFst :: (f :*: g) r -> f r
gFst (f :*: _) = f

gSnd :: (f :*: g) r -> g r
gSnd (_ :*: g) = g

pv :: Proxy a -> a
pv _ = undefined

toConstant :: String -> Schema
toConstant = Constant . Aeson.String . pack . firstLetterToLower

instance (GJSONSCHEMA f, GJSONSCHEMA g) => GJSONSCHEMA (f :*: g) where
  gSchema' enm names p = gSchema' enm names (gFst <$> p) `merge` gSchema' enm names (gSnd <$> p)

instance (Constructor c, GJSONSCHEMA f) => GJSONSCHEMA (M1 C c f) where
  gSchema' True _ = toConstant . conName . pv
    where
  gSchema' enm names = wrap . gSchema' enm names . fmap unM1
    where
      wrap = if multipleConstructors names
             then field (firstLetterToLower $ conName (undefined :: M1 C c f p)) True
             else id

instance GJSONSCHEMA f => GJSONSCHEMA (M1 D c f) where
  gSchema' True names p | multipleConstructors names = const (Choice . fmap toConstant $ names) $ p
  gSchema' enm names p = gSchema' enm names . fmap unM1 $ p

firstLetterToLower :: String -> String
firstLetterToLower ""     = ""
firstLetterToLower (l:ls) = toLower l : ls

instance (Selector c, JSONSchema a) => GJSONSCHEMA (M1 S c (K1 i (Maybe a))) where
  gSchema' _ _ = field (selName (undefined :: M1 S c f p)) False . schema . fmap (fromJust . unK1 . unM1)

-- TODO This instance does not correspond to the generic-aeson representation for Maybe
instance Selector c => GJSONSCHEMA (M1 S c (K1 i (Maybe String))) where
  gSchema' _ _ _ = field (selName (undefined :: M1 S c f p)) False $ Value unbounded

instance (Selector c, GJSONSCHEMA f) => GJSONSCHEMA (M1 S c f) where
  gSchema' enm names = wrap . gSchema' enm names . fmap unM1
    where
      wrap = case selName (undefined :: M1 S c f p) of
        "" -> id
        s -> field s True

multipleConstructors :: [String] -> Bool
multipleConstructors = (> 1) . length

-- | Derive a JSON schema for types with an instance of 'Generic'.
gSchema :: (Generic a, GJSONSCHEMA (Rep a), ConNames (Rep a), GIsEnum (Rep a)) => Proxy a -> Schema
gSchema p = gSchema' (isEnum p) ((conNames . pv) p) (fmap from p)
