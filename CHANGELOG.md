# Changelog

#### 0.7.4.1

* aeson-0.10 produces new error messages so the test-suite was updated to reflect this.

### 0.7.4.0

* Raise upper length limit of `UTCTime` since aeson-0.9 increased the precision.
* Add `JSONSchema Aeson.Value` instance.

#### 0.7.3.7

* Allow `generic-deriving 1.8.*`
* Allow `vector 0.11.*`

#### 0.7.3.6

* Allow `aeson 0.9.*`

#### 0.7.3.5

* Allow `attoparsec 0.13.*`

#### 0.7.3.4

* Fix compilation on GHC 7.2 and 7.4

#### 0.7.3.3

* Allow `tagged 0.8.*` in test-suite

#### 0.7.3.2

* Allow `tagged 0.8.*`

#### 0.7.3.1

* test-suite: Allow `aeson-utils 0.3.*`

### 0.7.3.0

* Add `JSONSchema` instances for all standard Num types and Scientific.

### 0.7.2.0

* Add `JSONSchema` instances for Double, Float, Fixed

#### 0.7.1.1

* Allow `generic-deriving 1.7.*`

### 0.7.1.0

* Export `GJSONSchema` type to allow clients to write type signatures for `gSchema` and `gSchemaWithSettings`.

#### 0.7.0.2

* Allow time 1.5.*

#### 0.7.0.1

* Drop support for old tasty versions

## 0.7.0.0

* Removed the `Null` constructor from `Schema`, use `Data.JSON.Schema.Combinators.nullable` instead.

* Added the `Validation` module that can be used to validate a json
  object against a schema and to get descriptive error messages.

* Updates for `Maybe` fixes in `generic-aeson 0.2.0.0` including more
  thorough test cases.

#### 0.6.1.1

* Bugfix: Remove underscores from fields and constructors in generated schemas to match generic-aeson.

### 0.6.1

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
