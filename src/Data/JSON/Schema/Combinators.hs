-- | Combinators for creating JSON schemas.
module Data.JSON.Schema.Combinators where

import Data.JSON.Schema.Types

-- | A schema combinator.
type SchemaC = Schema -> Schema

-- | Optionality operator for schemas.
infixl 3 <|>
(<|>) :: Schema -> Schema -> Schema
(Choice s1) <|> (Choice s2) = Choice $ s1 ++ s2
(Choice s1) <|> v           = Choice $ s1 ++ [v]
v           <|> (Choice s2) = Choice $ v : s2
v1          <|> v2          = Choice $ [v1, v2]

-- | Tupling.
infixl 4 <+>
(<+>) :: Schema -> Schema -> Schema
(Tuple t1) <+> (Tuple t2) = Tuple $ t1 ++ t2
(Tuple t1) <+> v          = Tuple $ t1 ++ [v]
v          <+> (Tuple t2) = Tuple $ v : t2
v1         <+> v2         = Tuple $ [v1, v2]

-- | If passed two objects, merges the fields. Otherwise creates a
-- tuple.
merge :: Schema -> Schema -> Schema
merge (Object f1) (Object f2) = Object $ f1 ++ f2
merge v1          v2          = v1 <+> v2

-- | Create an object with a single field.
field :: String -> Bool -> Schema -> Schema
field k r v = Object [Field k r v]

-- | An unbounded string.
value :: Schema
value = Value unbounded

-- | An unbounded number.
number :: Schema
number = Number unbounded

-- | An unbounded array with non-unique values.
array :: Schema -> Schema
array = Array unbounded False

-- | Add a field to an object, or tuple if passed a non-object.
addField :: String -> Bool -> Schema -> SchemaC
addField k r v = merge $ field k r v

-- | Add multiple fields to an object, or tuple if passed a
-- non-object.
addFields :: [(String, Bool, Schema)] -> SchemaC
addFields = flip $ foldr (\(k, r, v) -> addField k r v)

-- | An empty object.
empty :: Schema
empty = Object []
