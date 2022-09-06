package nyaya.gen.circe

import nyaya.gen.Gen
import utest._

object GenJsonTest extends TestSuite {

  private def test[A](gen: Gen[A], min: Int, size: Int = 100): Int = {
    val jsons = gen.list(size).sample()
    val unique = jsons.toSet.size
    assert(unique >= min)
    unique
  }

  override def tests = Tests {

    "genJson"       - test(GenJson.small, 30)
    "genJsonObject" - test(GenJson.smallObj, 30)

  }
}
