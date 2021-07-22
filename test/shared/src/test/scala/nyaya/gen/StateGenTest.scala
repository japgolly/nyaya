package nyaya.gen

import utest._

object StateGenTest extends TestSuite {

  val g: StateGen[Int, Int] =
    for {
      s <- StateGen.get[Int]
      i <- Gen.chooseInt(10).toStateGen
      _ <- StateGen.put(s + 10)
    } yield s + i

  override def tests = Tests {
    val (s, i) = g.run(40).samples().next()
    assert(s == 50, i >= 40 && i < 50)
  }
}
