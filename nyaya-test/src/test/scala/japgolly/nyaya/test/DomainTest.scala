package japgolly.nyaya.test

import utest._
import scalaz._, Scalaz._

object DomainTest extends TestSuite {

  val b = Domain.boolean
  val z = Domain.ofValues(0, 1)
  val abc = Domain.ofRangeN('a' to 'c')

  def test[A](d: Domain[A], as: A*): Unit = {
    val (sa,se) = (d.size, as.length)
    val (ca,ce) = (d.toStream.toSet, as.toSet)
    assert(sa == se, ca == ce)
  }

  override def tests = TestSuite {
    'boolean - test(b, true, false)
    'option  - test(b.option, None, Some(true), Some(false))
    'either  - test(b +++ z, true.left, false.left, 0.right, 1.right)
    'pair    - test(b *** z, (true, 0), (true, 1), (false, 0), (false, 1))
    'abc     - test(abc, 'a', 'b', 'c')
    'map     - test(abc.map(c => (c - 32).toChar), 'A', 'B', 'C')
  }
}
