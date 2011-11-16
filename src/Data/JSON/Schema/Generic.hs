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
import Control.Applicative hiding (empty, (<|>))
import Data.Char
import Data.Maybe
import Data.JSON.Schema.Types
import Data.JSON.Schema.Combinators
import Data.Proxy

class GJSONSCHEMA f where
  gSchema' :: Bool -> (Proxy a -> Schema) -> Proxy (f a) -> Schema

-- Recursive positions disabled for now, it casauses infintite data strcutres. This is a problem to be solved!
{-
instance GJSONSCHEMA I where
  gSchema' _ f = f . fmap unI
-}

instance JSONSchema a => GJSONSCHEMA (K a) where
  gSchema' _ _ = schema . fmap unK

instance GJSONSCHEMA (K String) where
  gSchema' _ _ _ = Value 0 (-1)

instance GJSONSCHEMA U where
  gSchema' _ _ _ = empty

instance (GJSONSCHEMA f, GJSONSCHEMA g) => GJSONSCHEMA (f :+: g) where
  gSchema' mc f p = gSchema' mc f (gL <$> p)
                <|> gSchema' mc f (gR <$> p)
            where gL :: (f :+: g) r -> f r
                  gL _ = undefined
                  gR :: (f :+: g) r -> g r
                  gR _ = undefined

gFst :: (f :*: g) r -> f r
gFst (f :*: _) = f

gSnd :: (f :*: g) r -> g r
gSnd (_ :*: g) = g

instance (GJSONSCHEMA f, GJSONSCHEMA g) => GJSONSCHEMA (f :*: g) where
  gSchema' mc f p = gSchema' mc f (gFst <$> p) `merge` gSchema' mc f (gSnd <$> p)

instance (Constructor c, GJSONSCHEMA f) => GJSONSCHEMA (C c f) where
  gSchema' mc f = wrap . gSchema' mc f . fmap unC
    where
      wrap = if mc
             then field (firstLetterToLower $ conName (undefined :: C c f r)) True
             else id

firstLetterToLower :: String -> String
firstLetterToLower ""     = ""
firstLetterToLower (l:ls) = toLower l : ls

instance (Selector s, JSONSchema a) => GJSONSCHEMA (S s (K (Maybe a))) where
  gSchema' _ _ = field (selName (undefined :: S s f r)) False . schema . fmap (fromJust . unK . unS)

instance Selector s => GJSONSCHEMA (S s (K (Maybe String))) where
  gSchema' _ _ _ = field (selName (undefined :: S s f r)) False $ Value 0 (-1)

instance (Selector s, GJSONSCHEMA f) => GJSONSCHEMA (S s f) where
  gSchema' mc f = field (selName (undefined :: S s f r)) True . gSchema' mc f . fmap unS

multipleConstructors :: (Regular a, ConNames (PF a)) => Proxy a -> Bool
multipleConstructors = (> 1) . length . conNames . pv
  where pv :: Proxy a -> a
        pv _ = undefined

gSchema :: (Regular a, GJSONSCHEMA (PF a), ConNames (PF a)) => Proxy a -> Schema
gSchema p = gSchema' (multipleConstructors p) gSchema (fmap from p)