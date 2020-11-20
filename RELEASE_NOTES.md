# Change log

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