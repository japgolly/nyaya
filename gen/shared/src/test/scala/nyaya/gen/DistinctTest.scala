package nyaya.gen

import alleycats.std.set._
import cats.data.State
import cats.instances.list._
import monocle.Lens
import utest._

object DistinctTest extends TestSuite {

  case class Age(value: Int)
  case class Person(name: String, age: Age, set: Set[Age])
  case class Stuff(list: List[Age], ppl: List[Person])

  val personName = Lens((_: Person).name)(v => _.copy(name = v))
  val personAge  = Lens((_: Person).age)(v => _.copy(age = v))
  val personSet  = Lens((_: Person).set)(v => _.copy(set = v))
  val stuffList  = Lens((_: Stuff).list)(v => _.copy(list = v))
  val stuffPpl   = Lens((_: Stuff).ppl)(v => _.copy(ppl = v))

  val dAge        = Distinct.fint.xmap(Age.apply)(_.value).addh(Age(0)).distinct
  val dPersonAges = dAge.at(personAge) + dAge.lift[Set].at(personSet).addh(Age(500))
  val dStuffAges  = dPersonAges.lift[List].at(stuffPpl) + dAge.lift[List].at(stuffList)
  val dPplNames   = Distinct.str.at(personName).lift[List]
  val dStuff      = dPplNames.at(stuffPpl) * dStuffAges

  implicit def autoAge(i: Int): Age = Age(i)
  val p1 = Person("A", 10, Set(10, 20, 30))
  val p2 = Person("B", 10, Set(50, 20))
  val p3 = Person("B", 500, Set(5, 500))
  val as = List[Age](10, 100, 0, 200, 30)
  val s = Stuff(as, List(p1, p2, p3))

  def ages(s: Stuff): List[Age] = s.list ::: s.ppl.flatMap(p => p.age :: p.set.toList)
  def names(s: Stuff): List[String] = s.ppl.map(_.name)

  val t = dStuff.run(s)
  val List(sa, ta) = List(s, t) map ages
  val r = ta.toSet

  override def tests = Tests {

    "names" - assert(names(t).sorted == List("A", "B", "C"))

    "ages" - {
      "Same number of ages"     - assert(sa.size == ta.size)
      "All ages distinct"       - assert(r.size == ta.size)
      "Original ages preserved" - (sa.toSet[Age] - 0 - 500).foreach(o => assert(r contains o))
      "No blacklisted ages"     - assert(!r.contains(0), !r.contains(500))
    }

    "composition" - {
      def appendD(n: Int): Distinct[List[Int], Int, Set, Int, Int, List[Int]] =
        Distinct(Distinct.Fixer.lift[Set, Int](_ => 0), list => _ => State.pure(list :+ n))

      val d1 = appendD(1)
      val d2 = appendD(2)
      val d3 = appendD(3)
      val d4 = appendD(4)

      "order" - {
        val d = d1 * d2
        val result = d.run(List.empty)
        assert(result == List(1, 2))
      }

      "associativity" - {
        val l = ((d1 * d2) * d3).run(List.empty)
        val r = (d1 * (d2 * d3)).run(List.empty)
        val lr = ((d1 * d2) * (d3 * d4)).run(List.empty)
        assert(l == List(1, 2, 3))
        assert(r == List(1, 2, 3))
        assert(lr == List(1, 2, 3, 4))
      }
    }
  }
}
