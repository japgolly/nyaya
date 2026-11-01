package nyaya.gen

import japgolly.microlibs.testutil.TestUtil._
import utest._

object ShrinkableTest extends TestSuite {

  override def tests = Tests {

    "long" - {
      "toZero" - {
        val s = Shrinkable.longToZero(80)
        "children" - assertEq(s.children.map(_.value).toList, List(0L, 40, 60, 70, 75, 78, 79))
        "shrink" - assertEq(s.shrink(_ > 42), 43)
      }
      "posPos" - {
        val s = Shrinkable.longInRange(90)(10, 100)
        "children" - assertEq(s.children.map(_.value).toList, List(10L, 50, 70, 80, 85, 88, 89))
        "shrink" - assertEq(s.shrink(_ > 42), 43)
      }
      "negNeg" - {
        val s = Shrinkable.longInRange(-90)(-100, -10)
        "children" - assertEq(s.children.map(_.value).toList, List(-10L, -50, -70, -80, -85, -88, -89))
        "shrink" - assertEq(s.shrink(_ < -42), -43)
      }
    }

    "int" - {
      "toZero" - {
        val s = Shrinkable.intToZero(80)
        "children" - assertEq(s.children.map(_.value).toList, List(0, 40, 60, 70, 75, 78, 79))
        "shrink" - assertEq(s.shrink(_ > 42), 43)
      }
      "posPos" - {
        val s = Shrinkable.intInRange(90)(10, 100)
        "children" - assertEq(s.children.map(_.value).toList, List(10, 50, 70, 80, 85, 88, 89))
        "shrink" - assertEq(s.shrink(_ > 42), 43)
      }
      "negNeg" - {
        val s = Shrinkable.intInRange(-90)(-100, -10)
        "children" - assertEq(s.children.map(_.value).toList, List(-10, -50, -70, -80, -85, -88, -89))
        "shrink" - assertEq(s.shrink(_ < -42), -43)
      }
    }

    "pair" - {
      val s = for {
        a <- Shrinkable.intToZero(80)
        b <- Shrinkable.intToZero(40)
      } yield (a, b)
      assertEq(s.shrink(x => x._1 > 6 && x._2 > 10), (7, 11))
    }

    "vector" - {
      val a = Shrinkable.intToZero(1)
      val b = Shrinkable.intToZero(2)
      val c = Shrinkable.intToZero(3)
      val v = Shrinkable.vector(0, Vector(a, b, c))
      "children" - assertEq(v.children.map(_.value).toList, List(
        Vector(),
        Vector (2, 3),
        Vector (1, 3),
        Vector (1, 2),
        Vector (0, 2, 3),
        Vector (1, 0, 3),
        Vector (1, 1, 3),
        Vector (1, 2, 0),
        Vector (1, 2, 2),
      ))
      "shrink" - assertEq(v.shrink(_.contains(1)), Vector(1))
      // v.children.map(_.value).mkString("\n")
    }

    "vector1" - {
      val a = Shrinkable.intToZero(1)
      val b = Shrinkable.intToZero(2)
      val c = Shrinkable.intToZero(3)
      val v = Shrinkable.vector(1, Vector(a, b, c))
      "children" - assertEq(v.children.map(_.value).toList, List(
        Vector(1),
        Vector(2, 3),
        Vector(1, 3),
        Vector(1, 2),
        Vector(0, 2, 3),
        Vector(1, 0, 3),
        Vector(1, 1, 3),
        Vector(1, 2, 0),
        Vector(1, 2, 2),
      ))
      "shrink" - assertEq(v.shrink(_.contains(1)), Vector(1))
      // v.children.map(_.value).mkString("\n")
    }

    "vector2" - {
      val a = Shrinkable.intToZero(1)
      val b = Shrinkable.intToZero(2)
      val c = Shrinkable.intToZero(3)
      val v = Shrinkable.vector(2, Vector(a, b, c))
      "children" - assertEq(v.children.map(_.value).toList, List(
        Vector(1, 2),
        Vector(0, 2, 3),
        Vector(1, 0, 3),
        Vector(1, 1, 3),
        Vector(1, 2, 0),
        Vector(1, 2, 2),
      ))
      "shrink" - assertEq(v.shrink(_.contains(1)), Vector(1, 0))
      // v.children.map(_.value).mkString("\n")
    }
  }
}
