package nyaya.test

import cats.instances.list._
import java.util.concurrent.atomic._
import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import utest._

object PropTestTest extends TestSuite {

//  implicit def defaultPropSettings = DefaultSettings.propSettings.copy(debug = true)

  val prop = Prop.test[List[Int]]("distinct ints", is => is.distinct == is)
  val intGen = Gen.chooseInt(0,5).list(0 to 10).map(Distinct.int.lift[List].run)

  override def tests = Tests {

    "distinct" - {
      prop mustBeSatisfiedBy intGen
    }

    "runs" - {
      def test(s: Int): Unit = {
        def t(sd: Settings.SizeDist): Unit = {
          val i = new AtomicInteger(0)
          def p = Prop.test[Int]("", _ => {i.incrementAndGet(); true})
          val S = defaultPropSettings.copy(sampleSize = SampleSize(s), sizeDist = sd)
          val r = PTest.test(p, Gen.int, S)
          assert(r.runs == s, r.result == Result.Satisfied, i.get() == s)
        }
        t(Seq.empty)
        t(Seq(1 -> Right(GenSize(4)), 1 -> Left(0.2), 8 -> Left(0.8)))
      }
        "1" - test(  1)
        "4" - test(  4)
        "7" - test(  7)
        "9" - test(  9)
       "71" - test( 71)
      "100" - test(100)
    }

    "proof" - {
      val lock = new Object
      var is = List.empty[Option[Boolean]]
      val p = Prop.test[Option[Boolean]]("proof", i => lock.synchronized{is ::= i; true})
      p mustBeProvedBy Domain.boolean.option
      lock.synchronized{is = is.sortBy(_.toString)}
      assert(is == List(None, Some(false), Some(true)))
    }
  }
}
