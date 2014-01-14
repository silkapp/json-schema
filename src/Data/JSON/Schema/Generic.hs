{-# LANGUAGE TypeOperators
           , TupleSections
           , ScopedTypeVariables
           , FlexibleContexts
           , FlexibleInstances
           , OverlappingInstances
           , TypeFamilies
           #-}
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

class GJSONSCHEMA f where
  gSchema' :: Bool -> Bool -> Proxy (f a) -> Schema

-- Recursive positions disabled for now, it causes infintite data structures. This is a problem to be solved!
{-
instance GJSONSCHEMA I where
  gSchema' _ f = f . fmap unI
-}

instance JSONSchema c => GJSONSCHEMA (K1 i c) where
  gSchema' _ _ = schema . fmap unK1

instance GJSONSCHEMA (K1 i String) where
  gSchema' _ _ _ = Value 0 (-1)

instance GJSONSCHEMA U1 where
  gSchema' _ _ _ = empty

instance (GJSONSCHEMA f, GJSONSCHEMA g) => GJSONSCHEMA (f :+: g) where
  gSchema' mc enm p =
        gSchema' mc enm (gL <$> p)
    <|> gSchema' mc enm (gR <$> p)
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
  gSchema' mc enm p = gSchema' mc enm (gFst <$> p) `merge` gSchema' mc enm (gSnd <$> p)

instance (Constructor c, GJSONSCHEMA f) => GJSONSCHEMA (M1 C c f) where
  gSchema' _  True = const $ Value 0 (-1)
  gSchema' mc enm = wrap . gSchema' mc enm . fmap unM1
    where
      wrap = if mc
             then field (firstLetterToLower $ conName (undefined :: M1 C c f p)) True
             else id

instance GJSONSCHEMA f => GJSONSCHEMA (M1 D c f) where
  gSchema' mc enm = gSchema' mc enm . fmap unM1

firstLetterToLower :: String -> String
firstLetterToLower ""     = ""
firstLetterToLower (l:ls) = toLower l : ls

instance (Selector c, JSONSchema a) => GJSONSCHEMA (M1 S c (K1 i (Maybe a))) where
  gSchema' _ _ = field (selName (undefined :: M1 S c f p)) False . schema . fmap (fromJust . unK1 . unM1)

instance Selector c => GJSONSCHEMA (M1 S c (K1 i (Maybe String))) where
  gSchema' _ _ _ = field (selName (undefined :: M1 S c f p)) False $ Value 0 (-1)

instance (Selector c, GJSONSCHEMA f) => GJSONSCHEMA (M1 S c f) where
  gSchema' mc enm = field (selName (undefined :: M1 S c f p)) True . gSchema' mc enm . fmap unM1

multipleConstructors :: (Generic a, ConNames (Rep a)) => Proxy a -> Bool
multipleConstructors = (> 1) . length . conNames . pv
  where pv :: Proxy a -> a
        pv _ = undefined

gSchema :: (Generic a, GJSONSCHEMA (Rep a), ConNames (Rep a), GIsEnum (Rep a)) => Proxy a -> Schema
gSchema p = gSchema' (multipleConstructors p) (isEnum p) (fmap from p)
