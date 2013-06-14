{-# LANGUAGE TypeOperators
           , TupleSections
           , ScopedTypeVariables
           , FlexibleContexts
           , FlexibleInstances
           , OverlappingInstances
           , TypeFamilies
           #-}
module Data.JSON.Schema.Generic (gSchema) where

import Generics.Regular
import Generics.Regular.IsEnum
import Control.Applicative hiding (empty, (<|>))
import Data.Char
import Data.Maybe
import Data.JSON.Schema.Types
import Data.JSON.Schema.Combinators
import Data.Proxy

class GJSONSCHEMA f where
  gSchema' :: Bool -> Bool -> (Proxy a -> Schema) -> Proxy (f a) -> Schema

-- Recursive positions disabled for now, it casauses infintite data strcutres. This is a problem to be solved!
{-
instance GJSONSCHEMA I where
  gSchema' _ f = f . fmap unI
-}

instance JSONSchema a => GJSONSCHEMA (K a) where
  gSchema' _ _ _ = schema . fmap unK

instance GJSONSCHEMA (K String) where
  gSchema' _ _ _ _ = Value 0 (-1)

instance GJSONSCHEMA U where
  gSchema' _ _ _ _ = empty

instance (GJSONSCHEMA f, GJSONSCHEMA g) => GJSONSCHEMA (f :+: g) where
  gSchema' mc enm f p =
        gSchema' mc enm f (gL <$> p)
    <|> gSchema' mc enm f (gR <$> p)
    where
      gL :: (f :+: g) r -> f r
      gL _ = undefined
      gR :: (f :+: g) r -> g r
      gR _ = undefined

gFst :: (f :*: g) r -> f r
gFst (f :*: _) = f

gSnd :: (f :*: g) r -> g r
gSnd (_ :*: g) = g

instance (GJSONSCHEMA f, GJSONSCHEMA g) => GJSONSCHEMA (f :*: g) where
  gSchema' mc enm f p = gSchema' mc enm f (gFst <$> p) `merge` gSchema' mc enm f (gSnd <$> p)

instance (Constructor c, GJSONSCHEMA f) => GJSONSCHEMA (C c f) where
  gSchema' _  True _ = const $ Value 0 (-1)
  gSchema' mc enm f = wrap . gSchema' mc enm f . fmap unC
    where
      wrap = if mc
             then field (firstLetterToLower $ conName (undefined :: C c f r)) True
             else id

firstLetterToLower :: String -> String
firstLetterToLower ""     = ""
firstLetterToLower (l:ls) = toLower l : ls

instance (Selector s, JSONSchema a) => GJSONSCHEMA (S s (K (Maybe a))) where
  gSchema' _ _ _ = field (selName (undefined :: S s f r)) False . schema . fmap (fromJust . unK . unS)

instance Selector s => GJSONSCHEMA (S s (K (Maybe String))) where
  gSchema' _ _ _ _ = field (selName (undefined :: S s f r)) False $ Value 0 (-1)

instance (Selector s, GJSONSCHEMA f) => GJSONSCHEMA (S s f) where
  gSchema' mc enm f = field (selName (undefined :: S s f r)) True . gSchema' mc enm f . fmap unS

multipleConstructors :: (Regular a, ConNames (PF a)) => Proxy a -> Bool
multipleConstructors = (> 1) . length . conNames . pv
  where pv :: Proxy a -> a
        pv _ = undefined

gSchema :: (Regular a, GJSONSCHEMA (PF a), GIsEnum (PF a), ConNames (PF a)) => Proxy a -> Schema
gSchema p = gSchema' (multipleConstructors p) (isEnum p) gSchema (fmap from p)
