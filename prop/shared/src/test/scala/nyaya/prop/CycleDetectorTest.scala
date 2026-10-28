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

  def testn(cd: CycleDetector[Iterator[N], N], e: Option[(Int, Int)], sort: Boolean, ns: N*): Unit = {
    def s: ((Int, Int)) => ((Int, Int)) = p => if (sort && p._1 > p._2) (p._2, p._1) else p
    val actual = cd.findCycle(ns.iterator).map(p => (p._1.id, p._2.id)) map s
    val expect = e map s
    assert(actual == expect)
  }

  override def tests = Tests {
    "directed" - {
      val cd = CycleDetector.Directed.tree[N, Int](_.vs.iterator, _.id)
      def test(e: Option[(Int, Int)], ns: N*) = testn(cd, e, false, ns: _*)

      "lines"    - test(None,      `1234`,`5678`)
      "diamond"  - test(None,      diamond)
      "1(35)24"  - test(None,      `1(35)24`)
      "1(35)245" - test(Some(4,5), `1(35)245`)
      "circle"   - test(Some(3,4), `41234`)
      "self"     - test(Some(9,9), `9→9`)

      "map" - {
        val cd = CycleDetector.Directed.map[Int, Int](identity)
        "empty"    - assert(cd.noCycle(Map.empty))
        "line"     - assert(cd.noCycle(Map(1 -> 2, 2 -> 3, 3 -> 4)))
        "cycle"    - assert(cd.hasCycle(Map(1 -> 2, 2 -> 3, 3 -> 1)))
        "self"     - assert(cd.findCycle(Map(1 -> 1)) == Some((1, 1)))
        "lasso"    - assert(cd.hasCycle(Map(1 -> 2, 2 -> 3, 3 -> 2)))
        "disjoint" - assert(cd.hasCycle(Map(1 -> 2, 2 -> 3, 4 -> 5, 5 -> 4)))
      }

      "multimap" - {
        val cd = CycleDetector.Directed.multimap[Set, Int, Int](identity, Set.empty)
        "empty"          - assert(cd.noCycle(Map.empty))
        "dagDiamond"     - assert(cd.noCycle(Map(1 -> Set(2, 3), 2 -> Set(4), 3 -> Set(4))))
        "dagCrossEdges"  - assert(cd.noCycle(Map(1 -> Set(2, 3), 2 -> Set(4, 5), 3 -> Set(4, 5), 4 -> Set(5))))
        "sharedSubgraph" - assert(cd.noCycle(Map(1 -> Set(3), 2 -> Set(3), 3 -> Set(4))))
        "cycle"          - assert(cd.hasCycle(Map(1 -> Set(2), 2 -> Set(3), 3 -> Set(1))))
        "disjointCycle"  - assert(cd.hasCycle(Map(1 -> Set(2, 3), 2 -> Set(3), 4 -> Set(5), 5 -> Set(6), 6 -> Set(4))))
        "self"           - assert(cd.hasCycle(Map(1 -> Set(1))))
        "nonRootSelf"    - assert(cd.hasCycle(Map(1 -> Set(2), 2 -> Set(2))))
        "innerCycle"     - assert(cd.hasCycle(Map(1 -> Set(2), 2 -> Set(3), 3 -> Set(2))))
        "multiEntry"     - assert(cd.hasCycle(Map(1 -> Set(3), 2 -> Set(3), 3 -> Set(4), 4 -> Set(3))))
        "denseDag"       - {
          val dag = Map(
            1 -> Set(2, 3),
            2 -> Set(4, 5), 3 -> Set(5, 6),
            4 -> Set(7, 8), 5 -> Set(8, 9), 6 -> Set(9, 10),
          )
          assert(cd.noCycle(dag))
        }
        "cycleFree" - {
          assert(cd.cycleFree(Map(1 -> Set(2))).isRight)
          assert(cd.cycleFree(Map(1 -> Set(1))).isLeft)
        }
      }
    }

    "undirected" - {
      val cd = CycleDetector.Undirected.tree[N, Int](_.vs.iterator, _.id)
      def test(e: Option[(Int, Int)], ns: N*) = testn(cd, e, true, ns: _*)

      "lines"    - test(None,    `1234`,`5678`)
      "diamond"  - test(Some(3,4), diamond)
      "1(35)24"  - test(Some(5,2), `1(35)24`)
      "1(35)245" - test(Some(3,5), `1(35)245`)
      "circle"   - test(Some(3,4), `41234`)
      "self"     - test(Some(9,9), `9→9`)

      "multimap" - {
        val cdSet = CycleDetector.Undirected.multimap[Set, Int, Int](identity, Set.empty)
        val cdList = CycleDetector.Undirected.multimap[List, Int, Int](identity, Nil)
        "path"          - assert(cdSet.noCycle(Map(1 -> Set(2), 2 -> Set(1, 3), 3 -> Set(2))))
        "triangle"      - assert(cdSet.hasCycle(Map(1 -> Set(2, 3), 2 -> Set(1, 3), 3 -> Set(1, 2))))
        "parallelEdges" - assert(cdList.hasCycle(Map(1 -> List(2, 2), 2 -> List(1, 1))))
      }
    }
  }
}
