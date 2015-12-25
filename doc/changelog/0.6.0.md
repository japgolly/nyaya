# 0.6.0

### Performance.

This release is **FAST**. Very fast.

Random data generation is now 1200% ~ 35000% faster, and uses 1900% ~ 33000% less memory.

Obviously, unless you throw the random data you generate away without looking at it, you won't see a speedup in your
tests of that magnitude.
Antecdotally, in my work-project's tests where Nyaya is used alongside an equal amount of typical unit testing,
total test times dropped between 50% to 80%'ish. That's a massive saving!

### New Random Data Generator

Mostly for performance reasons, Nyaya no longer depends on [NICTA/rng](https://github.com/NICTA/rng) for random data generation.
<br>It instead comes with its own, new module. [(examples)](doc/FEATURES.md#generating-random-data)
<br>*(A number of methods on `Gen` won't line up with old NICTA/rng names so there'll be some breakage there.)*

You now have more control over the size of subsets of your random data.


Unspecified: (eg. `.list`, `.string`, etc)
```
scala> Gen.chooseInt(10).list.samples().take(5).foreach(println)
List(2, 7, 2, 6, 0, 0, 1, 3, 2, 2, 7, 0, 3, 3, 1, 1, 3, 2, 4, 8, 3, 0, 4, 6, 5, 3, 1)
List(2, 4, 3, 9, 9, 1, 4, 9, 0, 6, 2, 3, 2, 6, 8, 9, 2, 3, 8)
List(0, 5, 2, 7, 7, 3, 8, 6, 1, 5, 0, 3, 9, 2, 9, 8, 2, 5, 0, 2, 5, 8)
List(4)
List(0, 5, 0, 2, 2, 0, 0, 9, 1, 4, 4, 8, 4, 6, 9, 0)
```

Exact: (eg. `.list(3)`, `.string(10)`, etc)
```
scala> Gen.chooseInt(10).list(3).samples().take(5).foreach(println)
List(2, 2, 6)
List(5, 3, 1)
List(5, 5, 6)
List(1, 0, 4)
List(3, 9, 3)
```

Range: (eg. `.list(0 to 3)`, `.string(4 to 8)`, etc)
```
scala> Gen.chooseInt(10).list(0 to 3).samples().take(5).foreach(println)
List(4)
List()
List(3, 9, 7)
List(1, 4)
List(6)
```

It is also easier to use. Build up your generator, call `.samples()` and you have an infinite stream of data.
Example:

```
scala> import nyaya.gen._
import nyaya.gen._

scala> val gen = Gen.int mapTo Gen.boolean.option
gen: nyaya.gen.Gen[Map[Int,Option[Boolean]]] = Gen(<function1>)

scala> gen.samples().take(1).foreach(println)
Map(609117252 -> None, --1220912339 -> Some(true), 1684851879 -> Some(false), 783799927 -> None)
```

### Modularisation.

The previous two modules `core` and `test` have now been expanded into:

|Module|Package|Contents|
| ---- | ---- | ---- |
| nyaya-prop| `nyaya.prop` | Proposition expression and evaluation. |
| nyaya-gen | `nyaya.gen`  | Random data generation. |
| nyaya-test| `nyaya.test` | Prop testing and proving. |
| nyaya-util| `nyaya.util` | A few utilities used in the modules above. |

Additionally the root package has changed from `japgolly.nyaya` to just `nyaya`.

Automate the transition with:
```sh
find . -type f -name '*.scala' -exec perl -pi -e 's/(?<![\w.])japgolly\.(?=nyaya)//g' {} +
```

<br>
*[[commit log]](https://github.com/japgolly/nyaya/compare/v0.5.12...v0.6.0)*
