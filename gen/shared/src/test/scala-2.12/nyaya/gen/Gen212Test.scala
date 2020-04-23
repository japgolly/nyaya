package nyaya.gen

import utest._

object Gen212Test extends TestSuite {

  private def assertType[A](a: AnyRef)(implicit ev: a.type <:< A) = ()

  def tests = Tests {

    // "Gen#to" - assertType[Gen[List[Int]]](Gen.int.to[List])

  }
}
