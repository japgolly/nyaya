Nyaya
=====

Nyaya is a Scala/Scala.JS library to:
* Test properties using random data.
* Prove properties with (reasonably-sized) finite domains.
* Assert properties in real data.
* Generate random data.
* Ensure uniqueness in random data.

It is:
* **Fast**. Probably the fastest Scala random data gen / prop tester. (Benchmarks coming soon…)
* Has a nice, fluent API for generating random data. [(examples)](doc/FEATURES.md#generating-random-data)
```
scala> import nyaya.gen._
import nyaya.gen._

scala> val g = Gen.int mapTo Gen.boolean.option
g: nyaya.gen.Gen[Map[Int,Option[Boolean]]] = Gen(<function1>)

scala> g.samples().take(1).foreach(println)
Map(609117252 -> None, -339 -> Some(true), 1684851879 -> Some(false), 78379 -> None)
```

<br>

#### SBT setup

```scala
ver nyayaVer = "1.1.0"

// Property expression, evaluation, assertion.
libraryDependencies += "com.github.japgolly.nyaya" %%% "nyaya-prop" % nyayaVer

// Random data generation.
libraryDependencies += "com.github.japgolly.nyaya" %%% "nyaya-gen" % nyayaVer

// Random JSON generation
libraryDependencies += "com.github.japgolly.nyaya" %%% "nyaya-gen-circe" % nyayaVer

// Property testing with random data.
// Property proving.
libraryDependencies += "com.github.japgolly.nyaya" %%% "nyaya-test" % nyayaVer % Test
```

<br>

#### Doc

* [Features in more detail](doc/FEATURES.md).
* [Changelogs](doc/changelog/).

<br>

#### Requires:
* Scala 2.13+
* Scala.JS 1.10+ *(optional)*

<br>

##### What does Nyaya mean?

See:
* https://en.wikipedia.org/wiki/Nyaya
* https://en.wikipedia.org/wiki/History_of_logic

<br>

#### Support
If you like what I do
—my OSS libraries, my contributions to other OSS libs, [my programming blog](https://japgolly.blogspot.com)—
and you'd like to support me, more content, more lib maintenance, [please become a patron](https://www.patreon.com/japgolly)!
I do all my OSS work unpaid so showing your support will make a big difference.

<br>

#### Licence
```
Copyright (C) 2014-2022 David Barri

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
```
