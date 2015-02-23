Nyaya
=====

(Nya-[what?](http://en.wikipedia.org/wiki/Nyaya) Oh [as in...](http://en.wikipedia.org/wiki/History_of_logic) Cool ok.)

Nyaya is a library to validate propositions/properties/laws…
* …in tests using random data. (behaviour)
* …at (non-prod) runtime using real data. (state)

Nyaya is
* Built for Scala.JS and standard Scala.
* Addresses the overlap between testing state, and testing behaviour via state.
* Includes tools for uniqueness validation, and generating random data that complies with complex uniquess constraints.

It was initially created because ScalaCheck wasn't available on Scala.JS.
Rather than being a port or clone, it's evolved into its own unique solution building on good and bad experiences using ScalaCheck, and my own needs, and values.

SBT setup:
```scala
// Proposition creation & assertion
libraryDependencies += "com.github.japgolly.nyaya" %%% "nyaya-core" % "0.5.7"

// Proposition testing & random data generation
libraryDependencies += "com.github.japgolly.nyaya" %%% "nyaya-test" % "0.5.7" % "test"
```

Status
======
It's been used extensively behind closed doors since mid-2014; I consider it stable.

The API may experience minor changes but nothing too major.

There's one exception: shrinking. Nyaya doesn't come with shrinking functionality,
but it may. On a separate branch I have shrinking working that is more effective than
what I've seen with ScalaCheck or QuickCheck, but I haven't decided if I'll merge it.
If merged, it would likely require significant API changes.


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
val prop = (even & div3 & div5).forallF[List] rename "Example"
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
* No `suchThat` or filtering available. Don't generate data just to throw it away, no, write an accurate generator. If a number is too large then keep halving it until its not. There are better ways than throw-away-retry.

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

A nice benefit of this approach is that your data generators do less work. If you're testing 2 string props:

1. `(s: String) => s.reverse.reverse == s`
2. `(a: String, b: String) => (a + b).length == a.length + b.length`

then you gain nothing by ensuring that the reverse test gets a string different than `a` or `b`. It's more efficient to generate two strings and pick one to use for the reverse test.

##### Example
See [MultimapTest.scala](https://github.com/japgolly/nyaya/blob/master/nyaya-test/src/test/scala/japgolly/nyaya/util/MultimapTest.scala) for a real example.

### Proving

You can _prove_ a proposition by testing it with all possible, or legal values.
`Domain` exists for this purpose and should be used in place of `Gen`.

##### Example
```scala
// This is (the type of) proposition we want to prove
val prop: Prop[Option[Boolean]] = ...

// There are three possibilities: None, Some(false), Some(true)
// Establishing all possible values is easy
val domain: Prop[Option[Boolean]] =
  Domain.boolean.option

import japgolly.nyaya.test.PropTest._

domain mustProve prop       // Method 1
prop mustBeProvedBy domain  // Method 2
```

### Uniqueness
**Validation** is the easy part. Just use `Prop.distinct`.
```scala
case class Thing(id: Int, name: String)
case class AllThings(timestamp: Long, things: List[Thing])

val p: Prop[AllThings] = Prop.distinct("thing IDs", (_: AllThings).things.map(_.id))
```

**Generation** is normally much harder but with Nyaya, hopefully you'll find it easy. I sure do!

There is a class called `Distinct` that will ensure generated data is unique, usually with just a few combinators. I'll introduce it first by showing a few examples, with an explanation after.

```scala
// Let's start with something trivial: uniqueness in a List[Int]
val d = Distinct.int.lift[List]
val g = Gen.int.list.map(d.run)

// How about people with unique names
case class Person(id: Long, name: String)
val g = Gen.apply2(Person.apply)(Gen.long, Gen.string1)
val d = Distinct.str.contramap[Person](_.name, (person, newName) => person.copy(name = newName))
g map d.run

// The whole (a,b)=>a.copy(b=b) thing gets old after a while
// Let's use Monocle to create lenses!
object Person {
  private def l = Lenser[Person]
  val id   = l(_.id)
  val name = l(_.name)
}

// Now let's generate a List[Person] with unique ids and names
val personGen = Gen.apply2(Person.apply)(Gen.long, Gen.string1)

val distinctId   = Distinct.long.at(Person.id)
val distinctName = Distinct.str.at(Person.name)
val d            = (distinctId * distinctName).lift[List]

val g: Gen[List[Person]] = personGen.list map d.run
```

It works by knowing how to provide a unique value when a duplicate is found, the behaviour of which is provided by `Distinct.Fixer`. Once a `Fixer` exists, it is then passed to a `Distinct` which is used for type navigation and composition.

Uniqueness doesn't have to be restricted to a single field; it can extend across multiple fields in multiple types.
This is an example:
```scala
// Say we have these types...

case class Id(value: Long)

case class Donkey(id: Id, name: String)
case class Horse (id: Id, happy: Boolean)

case class Farm(ds: List[Donkey], hs: Vector[Horse])

// Now say we want to prevent duplicate IDs between the donkeys and horses in Farm

// First lets create some boring, boring lenses...
object Donkey {
  private def l = Lenser[Donkey]
  val id   = l(_.id)
  val name = l(_.name)
}
object Horse {
  private def l = Lenser[Horse]
  val id    = l(_.id)
  val happy = l(_.happy)
}
object Farm {
  private def l = Lenser[Farm]
  val ds = l(_.ds)
  val hs = l(_.hs)
}

// No duplicate IDs in Farm is acheived thus:

val distinctId     = Distinct.flong.xmap(Id.apply)(_.value).distinct
val distinctDonkey = distinctId.at(Donkey.id)
val distinctHorse  = distinctId.at(Horse.id)
val distinctFarm   = distinctDonkey.lift[List].at(Farm.ds) +
                     distinctHorse.lift[Vector].at(Farm.hs)

val farmGen : Gen[Farm] = ...
val farmGen2: Gen[Farm] = farmGen map distinctFarm.run
```

### Other validation
* Set membership tests. All give detailed info on failure.
  * `Prop.whitelist` - Test that all members are on a whitelist.
  * `Prop.blacklist` - Test that no members are on a blacklist.
  * `Prop.allPresent` - Test that no required items are missing.
* `CycleDetector` - Easily detect cycles in recursive data.
  * Directed graphs.
  * Undirected graphs.


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
