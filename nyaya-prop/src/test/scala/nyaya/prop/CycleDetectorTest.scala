package nyaya.prop

import utest._

object CycleDetectorTest extends TestSuite {

  case class N(id: Int, vs: N*)

  val `24` = N(2, N(4))
  val `34` = N(3, N(4))
  val `245` = N(2, N(4, N(5)))
  val `1234` = N(1, N(2, `34`))
  val `5678` = N(5, N(6, N(7), N(8)))
  val `9→9` = N(9, N(9))
  val diamond = N(1, `24`, `34`)
  val `41234` = N(4, `1234`)
  def `35x`(x: N) = N(3, N(5, x))
  val `1(35)24` = N(1, `24`, `35x`(`24`))
  val `1(35)245` = N(1, `245`, `35x`(`245`))

  def testn(cd: CycleDetector[Stream[N], N], e: Option[(Int, Int)], sort: Boolean, ns: N*): Unit = {
    def s: ((Int, Int)) => ((Int, Int)) = p => if (sort && p._1 > p._2) (p._2, p._1) else p
    val actual = cd.findCycle(ns.toStream).map(p => (p._1.id, p._2.id)) map s
    val expect = e map s
    assert(actual == expect)
  }

  override def tests = TestSuite {
    'directed {
      val cd = CycleDetector.Directed.tree[N, Int](_.vs.toStream, _.id)
      def test(e: Option[(Int, Int)], ns: N*) = testn(cd, e, false, ns: _*)

      'lines     - test(None,      `1234`,`5678`)
      'diamond   - test(None,      diamond)
      "1(35)24"  - test(None,      `1(35)24`)
      "1(35)245" - test(Some(4,5), `1(35)245`)
      'circle    - test(Some(3,4), `41234`)
      'self      - test(Some(9,9), `9→9`)
    }

    'undirected {
      val cd = CycleDetector.Undirected.tree[N, Int](_.vs.toStream, _.id)
      def test(e: Option[(Int, Int)], ns: N*) = testn(cd, e, true, ns: _*)

      'lines     - test(None,    `1234`,`5678`)
      'diamond   - test(Some(3,4), diamond)
      "1(35)24"  - test(Some(5,2), `1(35)24`)
      "1(35)245" - test(Some(3,5), `1(35)245`)
      'circle    - test(Some(3,4), `41234`)
      'self      - test(Some(9,9), `9→9`)
    }
  }
}