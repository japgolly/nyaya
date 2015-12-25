package nyaya.gen

import java.nio.charset.Charset
import scalaz.NonEmptyList
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
    (freq *** gen).nel
  }

  // For cases when parametricity means there's nothing useful to test without the ability to write a ∃-test
  def didntCrash[A] = Prop.test[A]("Didn't crash", _ => true)

  def assertType[T](f: => T): Unit = ()

  override def tests = TestSuite {

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
    'subset         - Gen.int.set.subset                            .mustSatisfy(didntCrash)
    'subset1        - Gen.int.vector.subset1                        .mustSatisfy(didntCrash)
    'take           - Gen.int.set.take(0 to 210)                    .mustSatisfy(didntCrash)
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
  }
}
