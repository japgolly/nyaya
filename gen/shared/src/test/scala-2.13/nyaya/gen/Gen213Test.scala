package nyaya.gen

import scala.collection.immutable.ArraySeq
import utest._

object Gen213Test extends TestSuite {

  private def assertType[A](a: Any)(implicit ev: a.type <:< A) = ()

  def tests = Tests {

    "Gen#to"           - assertType[Gen[List[Int]    ]](Gen.int.to(List))
    "Gen#arraySeq"     - assertType[Gen[ArraySeq[Int]]](Gen.int.arraySeq)
    "Gen#arraySeq(ss)" - assertType[Gen[ArraySeq[Int]]](Gen.int.arraySeq(1 to 2))

  }
}
