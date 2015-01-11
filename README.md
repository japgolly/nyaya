Nyaya
=====

Nyaya is a library to validate propositions/properties/laws…
* …in tests using random data. (behaviour)
* …at (non-prod) runtime using real data. (state)

Nyaya is
* Built for Scala.JS and standard Scala.
* Addresses the overlap between testing state, and testing behaviour via state.
* Includes tools for uniqueness validation, and generating random data that complies with complex uniquess constraints.

It was initially created because ScalaCheck wasn't available on Scala.JS.
Rather than being a port or clone, it's evolved into its own unique solution building on good and bad experiences using ScalaCheck, and my own needs, and values.


Status
======
It's been used for months and months behind closed doors; I consider it useful and stable.

As for the API itself, I don't know how little or large future change will be.

For example, on a separate branch I have shrinking working that is way more effective than
my experience with ScalaCheck's, but haven't decided how or if I'll merge it. It could
be quite distruptive to the API. (?)


Features
========

### Composition with Detailed Reporting

* Propositions are composable using all operations available in first-order logic.
* When propositions fail, care is taken to provide enough information to comprehend why, quickly.
  This is a goal even when the proposition is the composite of many underlying propositions of which, only a few failed.
* Individual propositions can written to provide specialised info about why they fail, when they fail.

##### Example
This property checks that each number in a list is even, divisible by 3 and divisible by 5:
```scala
val even = Prop.test[Int]("even", _ % 2 == 0)
val div3 = Prop.test[Int]("div3", _ % 3 == 0)
val div5 = Prop.test[Int]("div5", _ % 5 == 0)
val prop = (even & mod3 & mod5).forallF[List] rename "Example"
```
This is a sample failure report:
```
Property [Example] failed.

Input: [List(15, 30, 60, 25)].

Root causes:
  2 failed axioms, 2 causes of failure.
  ├─ even
  │  ├─ Invalid input [15]
  │  └─ Invalid input [25]
  └─ div3
     └─ Invalid input [25]

Failure tree:
  Example
  └─ (even ∧ div3 ∧ div5)
     ├─ even
     └─ div3
```

### Runtime Assertion

In cases where we can't encode constraints using types, we generally write property-based tests.
Just as the compiler cannot prove these constraints, nor can it prove that we've written data
generators that will cover all cases. Thus as an additional safety check I like to validate
the assumed laws at runtime using real data. I (may) also want these checks omitted from
production code so as not to impact performance. Nyaya covers these scenarios.


* Use the same code to validate real data at runtime, and test with random data in unit tests.
* Runtime validation can be removed by specifying a scalac flag `-Xelide-below`.

##### Example
```scala
object MyProps {
  val foo: Prop[Foo] = ...
}

case class Foo(a: Int, b: String) {
  import shipreq.prop._
  this assertSatisfies MyProps.foo
}
```

### Generating Random Data

* When building random data generators, under the covers, this uses [NICTA/rng](https://github.com/NICTA/rng).
* I find NICTA/rng combinators ***immensely*** powerful and easy and powerful to work with, much better than typeclasses. As such, many useful combinators have been added as this project was used for real.
* I've never been a fan of `Gen` and `Arbitrary` in ScalaCheck. This has `Gen` but no `Arbitrary`. If you want to use this library and want implicits, using implicit `Gen`s by yourself will suffice.

##### Example
Say we have these data types
```scala
  case class Id(value: Int)

  case class Blah(ids: Set[Id], prevCoord: Option[(Double, Double, Double)])
```
Generators for each are:
```scala
  lazy val id: Gen[Id] =
    Gen.positiveint map Id.apply
  
  lazy val blah: Gen[Blah] =
    Gen.apply2(Blah.apply)(
      id.set,
      Gen.double.triple.option)
```

### Testing with Random Data (pt.1)

* Different settings for JVM/JS. By default…
  * JVM tests are large in quantity and data size, and they run in parallel.
  * JS tests are smaller in quantity and data size, and run on a single-thread.
* Settings can be configured per-test or per-module if required. (Here, implicits are used for the `Settings`.)
* A debug-mode is available in `Settings` to display verbose info about the testing and data-generation. It also has a nice little max line length so the output remains comprehensible. And it's in colour. Whoa!....

##### Example
```scala
val gen : Gen[A] = ...
val prop: Prop[A] = ...

import japgolly.nyaya.test.PropTest._

gen mustSatisfy prop        // Method 1
prop mustBeSatisfiedBy gen  // Method 2
```

### Testing with Random Data (pt.2)

This is where things may start to differ from what you're used to...
What happens when your proposition needs a little extra data just for testing? Wouldn't that require type gymnastics
for composition? Yes. But there's another way...

When writing a suite of propositions to test behaviour, there are often many props with similar inputs. The strategy
thus far has been to create a context (class) in which an iteration of random data has been generated (supplied via
class constructor), and then test within that context.
So instead of composing `Prop[Set[A]]` with `Prop[(Set[A],A,A)]` (which needs two As just for testing),
you instead create a context class like `class SetTest[A](s: Set[A], a1: A, a2: A)` and within that
use `Eval` instead of `Prop`, the difference being that `Eval` is evaluated immediately and doesn't need input later.

`Eval` has all the same logic operations that `Prop` has; implication, negation, forall, etc.

A nice benefit of this approach is that your data generators do less work. If you're testing `(s: String) => s.reverse.reverse == s` and `(a: String, b: String) => (a + b).length == a.length + b.length` then you gain nothing by ensuring that the reverse test gets a string different than `a` or `b`. It's more efficient to generate two strings and pick one to use for the reverse test.

##### Example
See [MultimapTest.scala](https://github.com/japgolly/nyaya/blob/master/nyaya-test/src/test/scala/japgolly/nyaya/util/MultimapTest.scala) for a real example.

### Proving

### Uniqueness
* Validation
* Generation
* 
- special features

* uniqueness: Validate uniqueness within data. Easily generate data with complex uniquess constraints.

### Other validation
* `CycleDetector` - Easily detect cycles in recursive data. Directed and undirected checks available.
* Set membership tests:
  * `Prop.whitelist` - Test that all members are on a whitelist.
  * `Prop.blacklist` - Test that no members are on a blacklist.
  * `Prop.allPresent` - Test that no required items are missing.


Quick Overview
==============

### `nyaya-core`

* Create propositions (≈ properties).
* Assert them in dev (elided in prod).
* Get a detailed report about what sub-propositions failed and why.
* Proposition composition.
* Easily validate uniqueness constraints.

### `nyaya-test`

* Generate random data.
  * No implicits.
  * Excellent combinators. _(thanks NICTA/rng)_
* Test propositions with random data.
* Prove propositions.
* Generate data with uniqueness constraints.


Licence
=======
```
Copyright (C) 2014-2015 David Barri

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
```
