package japgolly.nyaya.test

import java.nio.charset.Charset
import scalaz.NonEmptyList
import scalaz.std.AllInstances._
import utest._
import japgolly.nyaya._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTest._

object GenTest extends TestSuite {

  type I = List[Int]

  val utf8 = Charset.forName("UTF-8")
  val validUnicode =
    Prop.equalSelf[String](s"Unicode string ↔ $utf8", s => {
      val b = s.getBytes(utf8)
      new String(b, utf8)
    })

  val shuffleProp =
    Prop.equal[(I, I), I]("shuffle.sorted = sorted", _._1.sorted, _._2.sorted)

  val shuffleGen =
    for {
      before <- Gen.int.list
      after  <- Gen.shuffle(before)
    } yield (before, after)

  def assertType[T](f: => T): Unit = ()

  override def tests = TestSuite {
    'shuffle - shuffleGen.mustSatisfy(shuffleProp)
    'charToString {
      assertType[GenS[String]](Gen.char.string)
      assertType[GenS[String]](Gen.char.string1)
      assertType[Gen[String]]((null: Gen[List[Char]]).string)
      assertType[Gen[String]]((null: Gen[NonEmptyList[Char]]).string1)
    }
    'unicodeString - Gen.unicodestring.mustSatisfy(validUnicode)// (DefaultSettings.propSettings.setSampleSize(500000))
  }
}
