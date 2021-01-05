# Change log

## v0.2.9 (dev)

* Add optional name parameter to `Parser.parse*` functions, for tools/workflows that do not specify `id`
* Fixed bug with `Runtime.create`

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