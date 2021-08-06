# Change log

## in develop

* Set `CommandLineTool.successCodes` to `Set(0)` if not specified

## v0.5.1 (2021-07-28)

* Fixes error when trying to finalize a File value without location or path
* Updates dxCommon to 0.2.15-SNAPSHOT
* Uses `FileSource.listing` to determine directory listing during finalization
* Updates to dxCommon 0.7.0

## v0.5.0 (2021-03-28)

* *Breaking change*: schema types now have `id: Option[Identifier]` rather than `name: Option[String]`
* Parser can now handle `$graph` style CWL documents
* Adds dependency on `dxCommon` library  
* Improves finalization of file values
* Fixes coercion of StringValue to CwlEnum
* other bugfixes

## v0.4.1 (2021-03-11)

* Adds `EvaluatorContext.createInputs` to create an `EvaluatorContext` from input values
* Performs "finalization" of input values (setting of missing attributes on File and Directory values) when using `EvaluatorContext.createInputs` or `EvaluatorContext.createStaticInputs`

## v0.4.0 (2021-03-04)

* Parser bugfixes
* Incorporate `cwljava/39`, which fixes workflow parsing issues
* Allow duplicate Requirements/Hints and specify priority rules
* *Breaking change*: added new `CwlMulti` type and removed all uses of `Vector[CwlType]`

## v0.3.4 (2021-02-03)

* Fix parsing of SchemaDefRequirement with nested type references
* Fix coercion of null and optional ctypes

## v0.3.3 (2021-02-02)

* Rename all `CwlValue.apply(JsValue)` functions to `deserialize` - fixes bug with deserializing string values

## v0.3.2 (2021-01-27)

* Handle CwlAny type in Evaluator
* Update cwljava, which fixes remaning CommandLineTool parsing errors
* Convert all tool compliance tests to unit tests

## v0.3.1 (2021-01-21)

* Update cwljava, which fixes most CommandLineTool parsing errors
* Normalize baseuri so schemadef names and type references match

## v0.3.0 (2021-01-20)

* Implement workflow support
* Use separate classes for input and output record schemas
* Use SeqMap for RecordSchema type and Object value

## v0.2.9 (2021-01-05)

* Add optional name parameter to `Parser.parse*` functions, for tools/workflows that do not specify `id`
* Fixed bug with `Runtime.create`
* Include `class` attribute when serializing `File` and `Directory` values

## v0.2.8 (2020-12-14)

* Fix hint parsing bugs

## v0.2.7 (2020-12-14)

* Implement coercion of values
* In `Evaluator`, have all functions that take a Vector of types return the actual type of the evaluated value
* Add convenience functions to `Evaluator`
* Parse hints

## v0.2.6 (2020-12-03)

* Fix several issues with CwlTypes
* Add `Parser.canParse(String)` and `Parser.parseString` functions
* Add default values and `merge` function to `ReSourceRequirement`

## v0.2.5 (2020-11-20)

* Updated cwljava
  - fixes some issues with requirements parsing
  - fixes some tests cases and breaks others

## v0.2.4 (2020-11-19)

* Fix apply functions for CwlValue primitives - correctly handle Scala values 

## v0.2.3 (2020-11-16)

* Fix parsing of process requirements

## v0.2.2 (2020-11-12)

* Add `Parser.canParse` method

## v0.2.1 (2020-11-09)

* Parse identifiers
* Use file name as tool identifier if `id` not specified explicitly

## v0.2.0 (2020-11-02)

* Added documentation
* Fixed conversion of null-valued expression to string
* Renamed case class `CwlExpr` -> `ParameterValue`

## v0.1.0 (2020-11-01)

* Initial release