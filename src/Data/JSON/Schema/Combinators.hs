module Data.JSON.Schema.Combinators where

import Data.JSON.Schema.Types

type SchemaC = Schema -> Schema

-- | Optionality operator for schemas
infixl 3 <|>
(<|>) :: Schema -> Schema -> Schema
(Choice s1) <|> (Choice s2) = Choice $ s1 ++ s2
(Choice s1) <|> v           = Choice $ s1 ++ [v]
v           <|> (Choice s2) = Choice $ v : s2
v1          <|> v2          = Choice $ [v1, v2]

-- | Tupling
infixl 4 <+>
(<+>) :: Schema -> Schema -> Schema
(Tuple t1) <+> (Tuple t2) = Tuple $ t1 ++ t2
(Tuple t1) <+> v          = Tuple $ t1 ++ [v]
v          <+> (Tuple t2) = Tuple $ v : t2
v1         <+> v2         = Tuple $ [v1, v2]

-- | Merges the fields of an object, otherwise creates a tuple
merge :: Schema -> Schema -> Schema
merge (Object f1) (Object f2) = Object $ f1 ++ f2
merge v1          v2          = v1 <+> v2

field :: String -> Bool -> Schema -> Schema
field k r v = Object [Field k r v]

value :: Schema
value = Value 0 (-1)

number :: Schema
number = Number 0 (-1)

array :: Schema -> Schema
array = Array 0 (-1) False

addField :: String -> Bool -> Schema -> SchemaC
addField k r v = merge $ field k r v

addFields :: [(String, Bool, Schema)] -> SchemaC
addFields = flip $ foldr (\(k, r, v) -> addField k r v)

empty :: Schema
empty = Choice []