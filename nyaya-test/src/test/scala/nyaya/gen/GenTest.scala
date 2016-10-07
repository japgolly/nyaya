package nyaya.gen

import java.nio.charset.Charset
import scalaz.{BindRec, NonEmptyList, -\/, \/-}
import scalaz.std.AllInstances._
import utest._
import nyaya.prop._
import nyaya.test.PropTest._

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

  override def tests = TestSuite {

    'chooseInt {
      'bound {
        val values = Gen.chooseInt(3).samples().take(200).toSet
        assert(values == Set(0, 1, 2))
      }
      'range {
        val values = Gen.chooseInt(3, 6).samples().take(500).toSet
        assert(values == Set(3, 4, 5, 6))
      }
    }

    'chooseLong {
      'bound {
        val values = Gen.chooseLong(3).samples().take(200).toSet
        assert(values == Set[Long](0, 1, 2))
      }
      'range {
        val values = Gen.chooseLong(3, 6).samples().take(500).toSet
        assert(values == Set[Long](3, 4, 5, 6))
      }
    }

    'chooseChar {
      // Ensure scalac actually allows these overloads (in some cases it compiles but fails at callsite)
      val s1 = Gen.chooseChar('a', 'b' to 'z')
      val s2 = Gen.chooseChar('@', "=!")
      val s3 = Gen.chooseChar('@', "=!", 'a' to 'z', 'A' to 'Z')
      val u1 = Gen.chooseChar_!('a' to 'z')
      val u2 = Gen.chooseChar_!("@=!")
      val u3 = Gen.chooseChar_!("@=!", 'a' to 'z', 'A' to 'Z')
      def test(gs: Gen[Char]*) = ()
      test(s1, s2, s3)
      test(u1, u2, u3)
    }

    'charToString {
      assertType[Gen[String]](Gen.char.string)
      assertType[Gen[String]](Gen.char.string1)
      assertType[Gen[String]](Gen.ascii.string)
    }

    'unicodeString - Gen.string.mustSatisfy(validUnicode)// (DefaultSettings.propSettings.setSampleSize(500000))

    'fill           - fillGen                                       .mustSatisfy(fillProp)
    'shuffle        - shuffleGen                                    .mustSatisfy(shuffleProp)
    'subset         - Gen.int.list.subset                           .mustSatisfy(didntCrash)
    'subset1        - Gen.int.vector.subset1                        .mustSatisfy(didntCrash)
    'take           - Gen.int.list.take(0 to 210)                   .mustSatisfy(didntCrash)
    'mapBy          - Gen.int.mapBy(Gen.char)                       .mustSatisfy(didntCrash)
    'mapByKeySubset - Gen.int.list.flatMap(Gen.int.mapByKeySubset)  .mustSatisfy(didntCrash)
    'mapByEachKey   - mapByEachKeyGen                               .mustSatisfy(mapByEachKeyProp)
    'newOrOld       - Gen.int.list.flatMap(Gen.newOrOld(Gen.int, _)).mustSatisfy(didntCrash)
    'frequency      - freqArgs.flatMap(Gen frequencyNE _)           .mustSatisfy(didntCrash)

    'sequence - {
      val inp = "heheyay"
      val vgc = inp.toCharArray.toVector.map(Gen.pure)
      val gvc = Gen sequence vgc
      val res = gvc.map(_ mkString "") run GenCtx(GenSize(0))
      assert(res == inp)
    }

    'tailrec {
      val lim = 100000
      def test(g: Int => Gen[Int]): Unit = assert(g(0).samples().next() == lim)
      'plain - test(Gen.tailrec[Int, Int](i => Gen.pure(if (i < lim) Left(i + 1) else Right(i))))
      'scalaz - test(BindRec[Gen].tailrecM[Int, Int](i => Gen.pure(if (i < lim) -\/(i + 1) else \/-(i))))
    }

    'optionGet {
      'pass - Gen.pure(666).option.optionGet.mustSatisfy(Prop.test("be 666", _ == 666))
      'fail - {
        val s = Gen.pure(None: Option[Int]).optionGet.samples()
        try {
          s.next()
          sys error "Crash expected."
        } catch {
          case e: Throwable => assert(e.getMessage contains "Failed to generate")
        }
      }
    }

    'reseed {
      val g = for {
        s <- Gen.reseed
        a <- Gen.int
        _ <- Gen.setSeed(s)
        b <- Gen.int
      } yield (a, b)
      g.mustSatisfy(Prop.test("be equal", {case (a,b) => a == b }))
    }

    'orderedSeq {
      def assertAll(g: Gen[Vector[Char]])(expect: TraversableOnce[String]): Unit = {
        val e = expect.toSet
        val a = g.samples().take(e.size * 50).map(_ mkString "").toSet
        assert(a == e)
      }

      def combos(r: Range): Seq[String] =
        for {
          a <- r
          b <- r
          c <- r
        } yield ("a" * a) + ("b" * b) + ("c" * c)

      'noDrop {
        'dups0 - assertAll(Gen.orderedSeq(abc, 0, dropElems = false))(List("abc"))
        'dups1 - assertAll(Gen.orderedSeq(abc, 1, dropElems = false))(combos(1 to 2))
        'dups2 - assertAll(Gen.orderedSeq(abc, 2, dropElems = false))(combos(1 to 3))
      }

      'dropEmpty {
        def test(dups: Int) =
          assertAll(Gen.orderedSeq(abc, dups, dropElems = true, emptyResult = true))(
            combos(0 to (dups + 1)))
        'dups0 - test(0)
        'dups1 - test(1)
      }

      'dropNonEmpty {
        def test(dups: Int) =
          assertAll(Gen.orderedSeq(abc, dups, dropElems = true, emptyResult = false))(
            combos(0 to (dups + 1)).filter(_.nonEmpty))
        'dups0 - test(0)
        'dups1 - test(1)
      }
    }

    'choose {
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

    'sizedSet {
      for (set <- Gen.chooseInt(0, 50).sizedSet(40).samples().take(50))
        assert(set.size == 40)
    }

    'fairlyDistributedSeq {
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

    'uuid {
      val a, b = Gen.uuid.sample()
      assert(a != b)
    }

    'withSeed {
      val (a,b,c,d,e) = Gen.tuple5(
        Gen.long,
        Gen.long,
        Gen.long withSeed 0,
        Gen.long withSeed 1,
        Gen.long).sample()
      val s0 = -4962768465676381896L
      val s1 = -4964420948893066024L
      val n1 = 7564655870752979346L
      assert(c == s0, d == s1, e == n1)
      assert(Set(a,b,c,d,e).size == 5)
    }

  }
}
