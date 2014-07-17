## 0.6.1

* Add JSONSchema instances for tuples up to length 15 (matching aeson 0.8 instances)
* Specify uniqueness for JSONSchema instances for Set.

## 0.6

Breaking changes:

* Add `Constant Aeson.Value` type to `Schema`
* Change Number to take a `Bound` for the boundary. Before this the upper bound could not be `-1`.
* Change `Value` and `Array` to take a `LengthBound`. The difference from Number is that these values should always be `>= 0` if present.

Minor:

* Add `Bound { lower :: Maybe Int, upper :: Maybe Int }`
* Add `unbounded` as a shorthand for a `Bound` without restrictions
* Add `LengthBound { lowerLength :: Maybe Int, upperLength :: Maybe Int }`
* Add `unboundedLength` as a shorthand for a `LengthBound` without restrictions
* Add remaining `JSONSchema` instances based on existing Aeson instances. `UTCTime`, `Set`, Lazy `Text`, and tuples up to length 7.
* Add `enum` combinator as a shorthand for creating a `Choice` of `Constant`s

## 0.5

* `JSONSchema` instances for `Data.Vector`, `Data.Map`, and `Data.HashMap`
* Add `Map` type for json objects with arbitrary keys
* Add `Any` type for any json value
* Don't generate empty field names in ojbects for constructors without labeled fields
