package nyaya.gen

import java.nio.charset.Charset
import scalaz.{BindRec, NonEmptyList, -\/, \/-}
import scalaz.std.AllInstances._
import utest._
import nyaya.prop._
import nyaya.test.PropTest._
import scala.collection.compat._

object GenTest extends TestSuite {

  type VI = Vector[Int]
  type SI = Set[Int]

  val utf8 = Charset.forName("UTF-8")
  val validUnicode =
    Prop.equalSelf[String](s"Unicode string ↔ $utf8", s => {
      val b = s.getBytes(utf8)
      new String(b, utf8)
    })

  val shuffleProp =
    Prop.equal[(VI, VI), VI]("shuffle", _._1.sorted, _._2.sorted)
  val shuffleGen =
    for {
      before <- Gen.int.vector
      after  <- Gen.shuffle(before)
    } yield (before, after)

  val mapByEachKeyProp =
    Prop.equal[(VI, Map[Int, Char]), SI]("", _._1.toSet, _._2.keySet)
  val mapByEachKeyGen =
    for {
      is <- Gen.int.vector
      m  <- Gen.char.mapByEachKey(is)
    } yield (is, m)

  val fillProp =
    Prop.equal[(Int, Vector[Unit]), Int]("fill.length", _._1, _._2.length)
  val fillGen =
    for {
      n <- Gen.chooseSize
      v <- Gen.unit.vector(n)
    } yield (n, v)

  val freqArgs: Gen[NonEmptyList[Gen.Freq[Char]]] = {
    val freq = Gen.chooseInt(1, 16)
    val gen = Gen.char.map(Gen.pure)
    (freq *** gen).scalazNEL
  }

  // For cases when parametricity means there's nothing useful to test without the ability to write a ∃-test
  def didntCrash[A] = Prop.test[A]("Didn't crash", _ => true)

  def assertType[T](f: => T): Unit = ()

  val abc = "abc".toCharArray.toList

  override def tests = Tests {

    "chooseInt" - {
      "bound" - {
        for (b <- 1 to 34) {
          val values = Gen.chooseInt(b).samples().take(b * 1000).toSet
          assert(values == 0.until(b).toSet)
        }
      }
      "range" - {
        val values = Gen.chooseInt(3, 6).samples().take(500).toSet
        assert(values == Set(3, 4, 5, 6))
      }
      "singleValue" - {
        Gen.chooseInt(5, 5).samples().take(20).toSet[Int].foreach(l => assert(l == 5))
      }
      "wholeRange" - {
        Gen.chooseInt(Int.MinValue, Int.MaxValue).sample() // Just test it doesn't crash
        ()
      }
      "positiveRange" - {
        Gen.chooseInt(0, Int.MaxValue).samples().take(1000).foreach(l => assert(l > 0L))
      }
      "hugeRange" - {
        // Bit hard to test this better
        def test(l: Int, h: Int): Unit =
          Gen.chooseInt(l, h).samples().take(1000).foreach(x => assert(x >= l && x <= h))
        test(Int.MinValue, Int.MaxValue)
        test(Int.MinValue, Int.MaxValue - 1)
        test(Int.MinValue + 1, Int.MaxValue)
        test(Int.MinValue + 1, Int.MaxValue - 1)
      }
    }

    "chooseLong" - {
      "bound" - {
        for (b <- 1 to 34) {
          val values = Gen.chooseLong(b).samples().take(b * 1000).toSet
          assert(values == 0.until(b).toSet)
        }
      }
      "bigBound" - {
        val big = 922337203685477580L
        val values = Gen.chooseLong(big).samples().take(100000).toSet
        val bad = values.filter(l => l < 0 || l >= big)
        assert(bad.isEmpty)
      }
      "range" - {
        val values = Gen.chooseLong(3, 6).samples().take(500).toSet
        assert(values == Set[Long](3, 4, 5, 6))
      }
      "singleValue" - {
        Gen.chooseLong(5, 5).samples().take(20).toSet[Long].foreach(l => assert(l == 5))
      }
      "wholeRange" - {
        Gen.chooseLong(Long.MinValue, Long.MaxValue).sample() // Just test it doesn't crash
        ()
      }
      "positiveRange" - {
        Gen.chooseLong(0, Long.MaxValue).samples().take(1000).foreach(l => assert(l > 0L))
      }
      "hugeRange" - {
        // Bit hard to test this better
        def test(l: Long, h: Long): Unit =
          Gen.chooseLong(l, h).samples().take(1000).foreach(x => assert(x >= l && x <= h))
        test(Long.MinValue, Long.MaxValue)
        test(Long.MinValue, Long.MaxValue - 1)
        test(Long.MinValue + 1, Long.MaxValue)
        test(Long.MinValue + 1, Long.MaxValue - 1)
      }

      "by2" - {
        def test(l: Long, h: Long, s: Int = 200): Unit = {
          val actual = Gen.chooseLongBy2(l, h).samples().take(s).toList.sorted.distinct
          val expect = (l to h).toList
          assert(actual == expect)
        }
        "ee" - test(2, 10)
        "eo" - test(2, 11)
        "oe" - test(3, 10)
        "oo" - test(3, 11)
      }
    }

    "chooseChar" - {
      // Ensure scalac actually allows these overloads (in some cases it compiles but fails at callsite)
      val s1 = Gen.chooseChar('a', 'b' to 'z')
      val s2 = Gen.chooseChar('@', "=!")
      val s3 = Gen.chooseChar('@', "=!", 'a' to 'z', 'A' to 'Z')
      val u1 = Gen.chooseChar_!('a' to 'z')
      val u2 = Gen.chooseChar_!("@=!")
      val u3 = Gen.chooseChar_!("@=!", 'a' to 'z', 'A' to 'Z')
      def test(gs: Gen[Char]*) = { val _ = gs; () }
      test(s1, s2, s3)
      test(u1, u2, u3)
    }

    "charToString" - {
      assertType[Gen[String]](Gen.char.string)
      assertType[Gen[String]](Gen.char.string1)
      assertType[Gen[String]](Gen.ascii.string)
    }

    "unicodeString" - Gen.string.mustSatisfy(validUnicode)// (DefaultSettings.propSettings.setSampleSize(500000))

    "fill"           - fillGen                                       .mustSatisfy(fillProp)
    "shuffle"        - shuffleGen                                    .mustSatisfy(shuffleProp)

    // TODO: https://github.com/lampepfl/dotty/issues/11681
    // "subset"         - Gen.int.list.subset                           .mustSatisfy(didntCrash)
    // "subset1"        - Gen.int.vector.subset1                        .mustSatisfy(didntCrash)
    // "take"           - Gen.int.list.take(0 to 210)                   .mustSatisfy(didntCrash)

    "mapBy"          - Gen.int.mapBy(Gen.char)                       .mustSatisfy(didntCrash)
    "mapByKeySubset" - Gen.int.list.flatMap(Gen.int.mapByKeySubset)  .mustSatisfy(didntCrash)
    "mapByEachKey"   - mapByEachKeyGen                               .mustSatisfy(mapByEachKeyProp)
    "newOrOld"       - Gen.int.list.flatMap(Gen.newOrOld(Gen.int, _)).mustSatisfy(didntCrash)
    "frequency"      - freqArgs.flatMap(Gen frequencyNE _)           .mustSatisfy(didntCrash)

    "sequence" - {
      val inp = "heheyay"
      val vgc = inp.toCharArray.toVector.map(Gen.pure)
      val gvc = Gen sequence vgc
      val res = gvc.map(_ mkString "") run GenCtx(GenSize(0), ThreadNumber(0))
      assert(res == inp)
    }

    "tailrec" - {
      val lim = 100000
      def test(g: Int => Gen[Int]): Unit = assert(g(0).samples().next() == lim)
      "plain" - test(Gen.tailrec[Int, Int](i => Gen.pure(if (i < lim) Left(i + 1) else Right(i))))
      "scalaz" - test(BindRec[Gen].tailrecM[Int, Int](i => Gen.pure(if (i < lim) -\/(i + 1) else \/-(i))))
    }

    "optionGet" - {
      "pass" - Gen.pure(666).option.optionGet.mustSatisfy(Prop.test("be 666", _ == 666))
      "fail" - {
        val s = Gen.pure(None: Option[Int]).optionGet.samples()
        try {
          s.next()
          sys error "Crash expected."
        } catch {
          case e: Throwable => assert(e.getMessage contains "Failed to generate")
        }
      }
    }

    "reseed" - {
      val g = for {
        a <- Gen.setConstSeed(0) >> Gen.int
        b <- Gen.setConstSeed(0) >> Gen.reseed >> Gen.int
        c <- Gen.setConstSeed(0) >> Gen.reseed >> Gen.int
      } yield Set(a, b, c)
      g.mustSatisfy(Prop.test("must have 3 elements", _.size == 3))
    }

    "orderedSeq" - {
      def assertAll(g: Gen[Vector[Char]])(expect: IterableOnce[String]): Unit = {
        val e = expect.iterator.toSet
        val a = g.samples().take(e.size * 50).map(_ mkString "").toSet
        assert(a == e)
      }

      def combos(r: Range): Seq[String] =
        for {
          a <- r
          b <- r
          c <- r
        } yield ("a" * a) + ("b" * b) + ("c" * c)

      "noDrop" - {
        "dups0" - assertAll(Gen.orderedSeq(abc, 0, dropElems = false))(List("abc"))
        "dups1" - assertAll(Gen.orderedSeq(abc, 1, dropElems = false))(combos(1 to 2))
        "dups2" - assertAll(Gen.orderedSeq(abc, 2, dropElems = false))(combos(1 to 3))
      }

      "dropEmpty" - {
        def test(dups: Int) =
          assertAll(Gen.orderedSeq(abc, dups, dropElems = true, emptyResult = true))(
            combos(0 to (dups + 1)))
        "dups0" - test(0)
        "dups1" - test(1)
      }

      "dropNonEmpty" - {
        def test(dups: Int) =
          assertAll(Gen.orderedSeq(abc, dups, dropElems = true, emptyResult = false))(
            combos(0 to (dups + 1)).filter(_.nonEmpty))
        "dups0" - test(0)
        "dups1" - test(1)
      }
    }

    "choose" - {
      for (n <- 1 to 17) {
        val src = (0 until n).iterator.map(i => (i + 'a').toChar).toSet
        def test(g: Gen[Char]): Unit = {
          val a = g.samples().take(n * n * n * 2).toSet
          assert(a == src)
        }
        test(Gen.choose_!(src.toVector))
        test(Gen.choose_!(src.toList))
        test(Gen.chooseArray_!(src.toArray))
      }
    }

    "sizedSet" - {
      for (set <- Gen.chooseInt(0, 50).sizedSet(40).samples().take(50))
        assert(set.size == 40)
    }

    "fairlyDistributedSeq" - {
      for {
        n <- 0 to 30
        s <- Gen.fairlyDistributedSeq(List(true, false))(n).samples().take(10)
      } {
        assert(s.length == n)
        val t = s.count(identity)
        val f = s.length - t
        assert(Math.abs(t - f) <= 1)
      }
    }

    "uuid" - {
      val (a, b) = Gen.uuid.pair.sample()
      assert(a != b)
    }

    "withSeed" - {
      val (a,b,c,d,e) = Gen.tuple5(
        Gen.long,
        Gen.long,
        Gen.long withSeed 0,
        Gen.long withSeed 1,
        Gen.long
      ).sample()
      // println((c, d, e))
      val (s0, s1a, s1b) = (-4962768465676381896L,-4964420948893066024L,7564655870752979346L)
      assert(c == s0, d == s1a, e == s1b)
      assert(Set(a,b,c,d,e).size == 5)
    }

    "withSeedAcrossSamples" - {
      val actual = Gen.long.withSeed(0).samples().take(3).toList
      val expect = List(-4962768465676381896L, -7346045213905167455L, 8717422115870565898L)
      assert(actual == expect)
    }

    "withConstSeed" - {
      val actual = Gen.long.withConstSeed(0).samples().take(3).toList
      val expect = List.fill(3)(-4962768465676381896L)
      assert(actual == expect)
    }

    "dateTime" - {
      "deterministic" - {
        implicit val genNow = Gen pure Gen.Now(1476661717639L)
        val g = Gen.dateTime.aroundNowDays(10).asEpochMs.withSeed(0).sample()
        assert(g == 1477367458999L)
      }
    }

  }
}
