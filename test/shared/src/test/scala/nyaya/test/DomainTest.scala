package nyaya.test

import utest._
import scalaz._, Scalaz._

object DomainTest extends TestSuite {

  val b = Domain.boolean
  val z = Domain.ofValues(0, 1)
  val abc = Domain.ofRangeN('a' to 'c')

  def test[A, B](d: Domain[A])(as: A*): Unit = {
    val (sa,se) = (d.size, as.length)
    val (ca,ce) = (d.iterator.toSet, as.toSet)
    assert(sa == se, ca == ce)
  }

  override def tests = TestSuite {
    'boolean - test(b)(true, false)
    'option  - test(b.option)(None, Some(true), Some(false))
    'either  - test(b +++ z)(true.left, false.left, 0.right, 1.right)
    'pair    - test(b *** z)((true, 0), (true, 1), (false, 0), (false, 1))
    'abc     - test(abc)('a', 'b', 'c')
    'map     - test(abc.map(c => (c - 32).toChar))('A', 'B', 'C')

    'vector02 - test(z.vector(0 to 2))(
      Vector(),
      Vector(0), Vector(1),
      Vector(0, 0), Vector(0, 1), Vector(1, 0), Vector(1, 1))

    'vector01 - test(abc.vector(0 to 1))(Vector(), Vector('a'), Vector('b'), Vector('c'))

    'vector12 - test(z.vector(1 to 2))(
      Vector(0), Vector(1),
      Vector(0, 0), Vector(0, 1), Vector(1, 0), Vector(1, 1))

    'vector2 - test(abc.vector(2))(
      Vector('a', 'a'), Vector('a', 'b'), Vector('a', 'c'),
      Vector('b', 'a'), Vector('b', 'b'), Vector('b', 'c'),
      Vector('c', 'a'), Vector('c', 'b'), Vector('c', 'c'))

    'array01 - test(abc.array(0 to 1) map String.valueOf)("", "a", "b", "c")

    'array12 - test(abc.array(1 to 2) map String.valueOf)(
      "a", "b", "c",
      "aa", "ab", "ac",
      "ba", "bb", "bc",
      "ca", "cb", "cc")
  }
}
