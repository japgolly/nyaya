package nyaya.gen

import cats.FlatMap
import cats.data.NonEmptyList
import cats.instances.all._
import java.nio.charset.Charset
import java.util.SplittableRandom
import nyaya.prop._
import nyaya.test.PropTest._
import utest._

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
    (freq & gen).catsNEL
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

    "positiveAndNegative" - {
      "int" - {
        Gen.positiveInt.samples().take(1000).foreach(i => assert(i >= 0))
        Gen.negativeInt.samples().take(1000).foreach(i => assert(i <= 0))
      }
      "long" - {
        Gen.positiveLong.samples().take(1000).foreach(l => assert(l >= 0L))
        Gen.negativeLong.samples().take(1000).foreach(l => assert(l <= 0L))
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

    "subset1" - {
      var runs = 0
      val g = Gen[Vector[Int]] { _ => runs += 1; Vector(1, 2, 3) }
      val s = g.subset1.sample()
      assert(runs == 1)
      assert(s.nonEmpty && s.forall(Set(1, 2, 3).contains))
    }

    "subset"         - Gen.int.list.subset                           .mustSatisfy(didntCrash)
    "subset1"        - Gen.int.vector.subset1                        .mustSatisfy(didntCrash)
    "take"           - Gen.int.list.take(0 to 210)                   .mustSatisfy(didntCrash)

    "mapBy"          - Gen.int.mapBy(Gen.char)                       .mustSatisfy(didntCrash)
    "mapByKeySubset" - Gen.int.list.flatMap(Gen.int.mapByKeySubset)  .mustSatisfy(didntCrash)
    "mapByEachKey"   - mapByEachKeyGen                               .mustSatisfy(mapByEachKeyProp)
    "newOrOld"       - Gen.int.list.flatMap(Gen.newOrOld(Gen.int, _)).mustSatisfy(didntCrash)
    "frequency" - {
      "coverage" - {
        val g = Gen.frequency(1 -> Gen.pure("a"), 1 -> Gen.pure("b"))
        val actual = g.samples().take(100).toSet
        assert(actual == Set("a", "b"))
      }
      "random" - freqArgs.flatMap(Gen frequencyNE _).mustSatisfy(didntCrash)
    }

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
      "cats" - test(FlatMap[Gen].tailRecM[Int, Int](_)(i => Gen.pure(if (i < lim) Left(i + 1) else Right(i))))
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

    "tryChoose" - {
      "empty" - {
        assert(Gen.tryChoose(Nil).sample().isEmpty)
        assert(Gen.tryChoose(Iterator.empty).sample().isEmpty)
        assert(Gen.tryChoose(Vector.empty[Int]).sample().isEmpty)
      }
      "iterator" - {
        val it = Iterator(10, 20, 30)
        val g = Gen.tryChoose(it)
        val samples = g.samples().take(20).toList
        assert(samples.flatten.forall(Set(10, 20, 30).contains))
        assert(samples.flatten.nonEmpty)
        assert(samples.contains(None))
      }
    }

    "tryGenChoose" - {
      "empty" - {
        assert(Gen.tryGenChoose(Nil).isEmpty)
        assert(Gen.tryGenChoose(Iterator.empty).isEmpty)
      }
      "iterator" - {
        val it = Iterator(10, 20, 30)
        val optG = Gen.tryGenChoose(it)
        assert(optG.isDefined)
        val samples = optG.get.samples().take(20).toList
        assert(samples.forall(Set(10, 20, 30).contains))
      }
    }

    "tryGenChooseLazily" - {
      "empty" - {
        assert(Gen.tryGenChooseLazily(Nil).isEmpty)
        assert(Gen.tryGenChooseLazily(Iterator.empty).isEmpty)
      }
      "iterator" - {
        val it = Iterator(10, 20, 30)
        val optG = Gen.tryGenChooseLazily(it)
        assert(optG.isDefined)
        val samples = optG.get.samples().take(20).toList
        assert(samples.forall(Set(10, 20, 30).contains))
      }
    }

    "sizedSet" - {
      for (set <- Gen.chooseInt(0, 50).sizedSet(40).samples().take(50))
        assert(set.size == 40)

      "empty" - {
        assert(Gen.int.sizedSet(0).sample().isEmpty)
      }

      "exhaustion" - {
        intercept[NoSuchElementException] {
          Gen.boolean.sizedSet(3).sample(); ()
        }
      }

      "negative" - {
        intercept[IllegalArgumentException] {
          Gen.boolean.sizedSet(-1).sample(); ()
        }
      }
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

    "batches" - {
      val items = (1 to 20).toVector

      "normal" - {
        val b = Gen.batches(items, 3 to 5).sample()
        assert(b.flatten == items)
        assert(b.dropRight(1).forall(x => x.length >= 3 && x.length <= 5))
      }

      "dropRemainder" - {
        val b = Gen.batches(items, 6 to 6, keepRemainder = false).sample()
        assert(b.forall(_.length == 6))
        assert(b.length == 3)
        assert(b.flatten == items.take(18))
      }

      "emptyInput" - {
        val b = Gen.batches(Vector.empty[Int], 2 to 4).sample()
        assert(b.isEmpty)
      }

      "zeroAllowed" - {
        val b = Gen.batches(items, 0 to 3).samples().take(20).toList
        b.foreach { batchList =>
          assert(batchList.flatten == items)
          assert(batchList.forall(_.length <= 3))
        }
        b.map(_.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")).mkString("\n")
      }
    }

    "nextBit" - {
      val ctx = GenCtx(GenSize(0), ThreadNumber(0))
      ctx.setSeed(1L)
      val r = new SplittableRandom(1L)
      val int1 = r.nextInt()
      val int2 = r.nextInt()
      val expectedBits1 = (0 until 32).map(b => (int1 & (1 << b)) == 0)
      val actualBits1 = (0 until 32).map(_ => ctx.nextBit())
      assert(actualBits1 == expectedBits1)
      val actualBit33 = ctx.nextBit()
      val expectedBit33 = (int2 & 1) == 0
      assert(actualBit33 == expectedBit33)
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
      val (s0, s1a, s1b) = (-2152535657050944081L, -7995527694508729151L, -4689498862643123097L)
      assert(c == s0, d == s1a, e == s1b)
      assert(Set(a,b,c,d,e).size == 5)
    }

    "withSeedAcrossSamples" - {
      val actual = Gen.long.withSeed(0).samples().take(3).toList
      val expect = List(-2152535657050944081L, 696566373075308979L, 8604477608339801740L)
      assert(actual == expect)
    }

    "withConstSeed" - {
      val actual = Gen.long.withConstSeed(0).samples().take(3).toList
      val expect = List.fill(3)(-2152535657050944081L)
      assert(actual == expect)
    }

    "dateTime" - {
      "deterministic" - {
        implicit val genNow = Gen pure Gen.Now(1476661717639L)
        val g = Gen.dateTime.aroundNowDays(10).asEpochMs.withSeed(0).sample()
        assert(g == 1476208275317L)
      }
    }

    "arity" - {
      def counter(): Gen[Int] = {
        var count = 0
        Gen(_ => { count += 1; count })
      }

      "tuple" - {
        val c2 = counter()
        assert(Gen.tuple2(c2, c2).sample() == (1, 2))
        val c3 = counter()
        assert(Gen.tuple3(c3, c3, c3).sample() == (1, 2, 3))
        val c4 = counter()
        assert(Gen.tuple4(c4, c4, c4, c4).sample() == (1, 2, 3, 4))
        val c5 = counter()
        assert(Gen.tuple5(c5, c5, c5, c5, c5).sample() == (1, 2, 3, 4, 5))
        val c6 = counter()
        assert(Gen.tuple6(c6, c6, c6, c6, c6, c6).sample() == (1, 2, 3, 4, 5, 6))
        val c7 = counter()
        assert(Gen.tuple7(c7, c7, c7, c7, c7, c7, c7).sample() == (1, 2, 3, 4, 5, 6, 7))
        val c8 = counter()
        assert(Gen.tuple8(c8, c8, c8, c8, c8, c8, c8, c8).sample() == (1, 2, 3, 4, 5, 6, 7, 8))
        val c9 = counter()
        assert(Gen.tuple9(c9, c9, c9, c9, c9, c9, c9, c9, c9).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9))
        val c10 = counter()
        assert(Gen.tuple10(c10, c10, c10, c10, c10, c10, c10, c10, c10, c10).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
        val c11 = counter()
        assert(Gen.tuple11(c11, c11, c11, c11, c11, c11, c11, c11, c11, c11, c11).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
        val c12 = counter()
        assert(Gen.tuple12(c12, c12, c12, c12, c12, c12, c12, c12, c12, c12, c12, c12).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
        val c13 = counter()
        assert(Gen.tuple13(c13, c13, c13, c13, c13, c13, c13, c13, c13, c13, c13, c13, c13).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
        val c14 = counter()
        assert(Gen.tuple14(c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
        val c15 = counter()
        assert(Gen.tuple15(c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
        val c16 = counter()
        assert(Gen.tuple16(c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
        val c17 = counter()
        assert(Gen.tuple17(c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
        val c18 = counter()
        assert(Gen.tuple18(c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
        val c19 = counter()
        assert(Gen.tuple19(c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19))
        val c20 = counter()
        assert(Gen.tuple20(c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
        val c21 = counter()
        assert(Gen.tuple21(c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))
        val c22 = counter()
        assert(Gen.tuple22(c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
      }

      "apply" - {
        val c2 = counter()
        assert(Gen.apply2((a: Int, b: Int) => (a, b))(c2, c2).sample() == (1, 2))
        val c3 = counter()
        assert(Gen.apply3((a: Int, b: Int, c: Int) => (a, b, c))(c3, c3, c3).sample() == (1, 2, 3))
        val c4 = counter()
        assert(Gen.apply4((a: Int, b: Int, c: Int, d: Int) => (a, b, c, d))(c4, c4, c4, c4).sample() == (1, 2, 3, 4))
        val c5 = counter()
        assert(Gen.apply5((a: Int, b: Int, c: Int, d: Int, e: Int) => (a, b, c, d, e))(c5, c5, c5, c5, c5).sample() == (1, 2, 3, 4, 5))
        val c6 = counter()
        assert(Gen.apply6((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int) => (a, b, c, d, e, f))(c6, c6, c6, c6, c6, c6).sample() == (1, 2, 3, 4, 5, 6))
        val c7 = counter()
        assert(Gen.apply7((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int) => (a, b, c, d, e, f, g))(c7, c7, c7, c7, c7, c7, c7).sample() == (1, 2, 3, 4, 5, 6, 7))
        val c8 = counter()
        assert(Gen.apply8((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) => (a, b, c, d, e, f, g, h))(c8, c8, c8, c8, c8, c8, c8, c8).sample() == (1, 2, 3, 4, 5, 6, 7, 8))
        val c9 = counter()
        assert(Gen.apply9((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int) => (a, b, c, d, e, f, g, h, i))(c9, c9, c9, c9, c9, c9, c9, c9, c9).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9))
        val c10 = counter()
        assert(Gen.apply10((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int) => (a, b, c, d, e, f, g, h, i, j))(c10, c10, c10, c10, c10, c10, c10, c10, c10, c10).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
        val c11 = counter()
        assert(Gen.apply11((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int) => (a, b, c, d, e, f, g, h, i, j, k))(c11, c11, c11, c11, c11, c11, c11, c11, c11, c11, c11).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
        val c12 = counter()
        assert(Gen.apply12((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int) => (a, b, c, d, e, f, g, h, i, j, k, l))(c12, c12, c12, c12, c12, c12, c12, c12, c12, c12, c12, c12).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
        val c13 = counter()
        assert(Gen.apply13((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m))(c13, c13, c13, c13, c13, c13, c13, c13, c13, c13, c13, c13, c13).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
        val c14 = counter()
        assert(Gen.apply14((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n))(c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
        val c15 = counter()
        assert(Gen.apply15((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))(c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
        val c16 = counter()
        assert(Gen.apply16((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))(c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
        val c17 = counter()
        assert(Gen.apply17((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))(c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
        val c18 = counter()
        assert(Gen.apply18((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))(c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
        val c19 = counter()
        assert(Gen.apply19((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))(c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19))
        val c20 = counter()
        assert(Gen.apply20((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))(c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
        val c21 = counter()
        assert(Gen.apply21((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int, u: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))(c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))
        val c22 = counter()
        assert(Gen.apply22((a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int, u: Int, v: Int) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))(c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
      }

      "lift" - {
        val c2 = counter()
        assert(Gen.lift2(c2, c2)((a, b) => (a, b)).sample() == (1, 2))
        val c3 = counter()
        assert(Gen.lift3(c3, c3, c3)((a, b, c) => (a, b, c)).sample() == (1, 2, 3))
        val c4 = counter()
        assert(Gen.lift4(c4, c4, c4, c4)((a, b, c, d) => (a, b, c, d)).sample() == (1, 2, 3, 4))
        val c5 = counter()
        assert(Gen.lift5(c5, c5, c5, c5, c5)((a, b, c, d, e) => (a, b, c, d, e)).sample() == (1, 2, 3, 4, 5))
        val c6 = counter()
        assert(Gen.lift6(c6, c6, c6, c6, c6, c6)((a, b, c, d, e, f) => (a, b, c, d, e, f)).sample() == (1, 2, 3, 4, 5, 6))
        val c7 = counter()
        assert(Gen.lift7(c7, c7, c7, c7, c7, c7, c7)((a, b, c, d, e, f, g) => (a, b, c, d, e, f, g)).sample() == (1, 2, 3, 4, 5, 6, 7))
        val c8 = counter()
        assert(Gen.lift8(c8, c8, c8, c8, c8, c8, c8, c8)((a, b, c, d, e, f, g, h) => (a, b, c, d, e, f, g, h)).sample() == (1, 2, 3, 4, 5, 6, 7, 8))
        val c9 = counter()
        assert(Gen.lift9(c9, c9, c9, c9, c9, c9, c9, c9, c9)((a, b, c, d, e, f, g, h, i) => (a, b, c, d, e, f, g, h, i)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9))
        val c10 = counter()
        assert(Gen.lift10(c10, c10, c10, c10, c10, c10, c10, c10, c10, c10)((a, b, c, d, e, f, g, h, i, j) => (a, b, c, d, e, f, g, h, i, j)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
        val c11 = counter()
        assert(Gen.lift11(c11, c11, c11, c11, c11, c11, c11, c11, c11, c11, c11)((a, b, c, d, e, f, g, h, i, j, k) => (a, b, c, d, e, f, g, h, i, j, k)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
        val c12 = counter()
        assert(Gen.lift12(c12, c12, c12, c12, c12, c12, c12, c12, c12, c12, c12, c12)((a, b, c, d, e, f, g, h, i, j, k, l) => (a, b, c, d, e, f, g, h, i, j, k, l)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
        val c13 = counter()
        assert(Gen.lift13(c13, c13, c13, c13, c13, c13, c13, c13, c13, c13, c13, c13, c13)((a, b, c, d, e, f, g, h, i, j, k, l, m) => (a, b, c, d, e, f, g, h, i, j, k, l, m)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
        val c14 = counter()
        assert(Gen.lift14(c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14, c14)((a, b, c, d, e, f, g, h, i, j, k, l, m, n) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
        val c15 = counter()
        assert(Gen.lift15(c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15, c15)((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
        val c16 = counter()
        assert(Gen.lift16(c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16, c16)((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
        val c17 = counter()
        assert(Gen.lift17(c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17, c17)((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
        val c18 = counter()
        assert(Gen.lift18(c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18, c18)((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
        val c19 = counter()
        assert(Gen.lift19(c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19, c19)((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19))
        val c20 = counter()
        assert(Gen.lift20(c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20, c20)((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
        val c21 = counter()
        assert(Gen.lift21(c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21, c21)((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))
        val c22 = counter()
        assert(Gen.lift22(c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22, c22)((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)).sample() == (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
      }

      "product" - {
        val c = counter()
        assert((c & c).sample() == (1, 2))
      }
    }

    "filter" - {
      "match" - {
        assert(Gen.boolean.filter(identity).sample())
      }
      "fail" - {
        intercept[NoSuchElementException] {
          Gen.chooseInt(2).filter(_ > 2).sample(); ()
        }
      }
    }

  }
}
