package nyaya.gen

import nyaya.prop._
import nyaya.test.ParallelExecutor
import nyaya.test.PropTest._
import utest._

object GenTestJvm extends TestSuite {

  override def tests = Tests {

    "parallel" - {
      def settings = defaultPropSettings.copy(executor = ParallelExecutor(2)).setSampleSize(4)
      val lock = new AnyRef
      var seen = Vector.empty[Int]
      val prop = Prop.atom[Int]("", i => {
        lock.synchronized(seen :+= i)
        None
      })
      def results() = lock.synchronized(seen)

      "noSeed" - {
        Gen.int.mustSatisfy(prop)(settings)
        val r = results()
        assert(r.toSet.size == r.size) // no duplicates
      }

      "withSeed" - {
        Gen.int.withSeed(0).mustSatisfy(prop)(settings)
        val r = results()
        assert(r.toSet.size == r.size) // no duplicates
      }

      "withConstSeed" - {
        Gen.int.withConstSeed(0).mustSatisfy(prop)(settings)
        assert(results().toSet.size == 1)
      }
    }

  }
}
