## 0.5.11 ([commit log](https://github.com/japgolly/nyaya/compare/v0.5.10...v0.5.11))

* Added `.string{,1}` to `Char`, `List[Char]`, `NonEmptyList[Char]` generators.

## 0.5.10 ([commit log](https://github.com/japgolly/nyaya/compare/v0.5.9...v0.5.10))

* Upgraded Monocle to 1.1.0.

## 0.5.9 ([commit log](https://github.com/japgolly/nyaya/compare/v0.5.8...v0.5.9))

* `Gen.map{By,To}` now limit their sizes.

* `Domain` learned:
  * `pair`
  * `triple`

* `object Gen` learned:
  * `lazily`

* Upgraded Scala.JS to 0.6.2.
* Upgraded Scala 2.11 to 2.11.6.


## 0.5.8 ([commit log](https://github.com/japgolly/nyaya/compare/v0.5.7...v0.5.8))

* `object Eval` and `EvalOver` learned:
  * `allPresent`
  * `blacklist`
  * `distinct`
  * `distinctC`
  * `either`
  * `fail`
  * `forall`
  * `whitelist`

* Added to `object Prop`:
  * `either`
  * `fail`

* Added to `object Gen`:
  * `oneofO`

* More methods to `Multimap`.

* Upgrade Scala.JS to 0.6.1.


## 0.5.7 ([commit log](https://github.com/japgolly/nyaya/compare/v0.5.3...v0.5.7))

* Bugfix for `Multimap[?, Vector, ?]`

* Changed signature of `Prop.forall`.

* Added to `class Gen`:
  * `strengthL`
  * `strengthR`
  * `mapByEachKey`
  * `mapByKeySubset`

* Added to `object Gen`:
  * `byName`
  * `byNeed`
  * `newOrOld`
  * `oneofGL`
  * `traverse`
  * `traverseG`

* Added to `object GenS`:
  * `apply(GenSize => Gen[A])`
  * `choosesize`

* Added `Platform.choose(jvm,js)` and helper `jvm 'JVM|JS' js`
  to choose a value depending on which platform the code is running.

* Added `set...` convenience methods to Settings.

* Removed `Gen.oneofGC`. Import `Gen.Covariance._` and use `Gen.oneofG` instead.

## 0.5.3 ([commit log](https://github.com/japgolly/nyaya/compare/v0.5.2...v0.5.3))

* Fix μTest being exported from core module.

## 0.5.2 ([commit log](https://github.com/japgolly/nyaya/compare/v0.5.1...v0.5.2))

* Build for Scala.JS 0.6.0.
* Scala 2.11.{4 ⇒ 5}

## 0.5.1 ([commit log](https://github.com/japgolly/nyaya/compare/v0.5.0...v0.5.1))

* New generators:
  * `oneofSeq`
  * `subset`


# 0.5.0

First public release.
