package nyaya.test

import java.util.concurrent.atomic.AtomicInteger
import nyaya.gen.Gen
import nyaya.prop.Prop
import utest._

object ParallelExecutorTest extends TestSuite {

  override def tests = Tests {

    "cancellation" - {
      val workers = 4

      "earlyFailure" - {
        val evaluated = new AtomicInteger(0)
        val prop = Prop.test[Int]("fail at 100", _ => {
          val n = evaluated.incrementAndGet()
          n < 100
        })
        val settings = Settings(
          executor = ParallelExecutor(workers),
          sampleSize = SampleSize(10000)
        )
        val rs = PTest.test(prop, Gen.int, settings)
        assert(!rs.success)
        assert(evaluated.get() < 2000)
      }

      "earlyError" - {
        val evaluated = new AtomicInteger(0)
        val prop = Prop.test[Int]("error at 100", _ => {
          val n = evaluated.incrementAndGet()
          if (n >= 100) sys.error("boom") else true
        })
        val settings = Settings(
          executor = ParallelExecutor(workers),
          sampleSize = SampleSize(10000)
        )
        val rs = PTest.test(prop, Gen.int, settings)
        assert(!rs.success)
        assert(rs.result.isInstanceOf[Result.Error[_]])
        assert(evaluated.get() < 2000)
      }

      "earlyProveFailure" - {
        val evaluated = new AtomicInteger(0)
        val prop = Prop.test[Int]("fail at 100", _ => {
          val n = evaluated.incrementAndGet()
          n < 100
        })
        val domain = Domain.ofRange(0 until 10000)
        val settings = Settings(
          executor = ParallelExecutor(workers)
        )
        val rs = PTest.prove(prop, domain, settings)
        assert(!rs.success)
        assert(evaluated.get() < 2000)
      }
    }

    "merge" - {
      val e1 = Result.Error[Int](Some(1), new Exception("e1"))
      val e2 = Result.Error[Int](Some(2), new Exception("e2"))
      val s1 = RunState[Int](5, Result.Satisfied)
      val s2 = RunState[Int](10, Result.Satisfied)
      val f1 = RunState[Int](5, e1)
      val f2 = RunState[Int](8, e2)

      "successAndSuccess" - {
        val r = ParallelExecutor.merge(s1, s2)
        assert(r.runs == 10, r.success)
      }

      "failureAndSuccess" - {
        val r = ParallelExecutor.merge(f1, s2)
        assert(r.runs == 10, !r.success, r.result == e1)
      }

      "successAndFailure" - {
        val r = ParallelExecutor.merge(s2, f1)
        assert(r.runs == 10, !r.success, r.result == e1)
      }

      "twoFailures" - {
        val r1 = ParallelExecutor.merge(f1, f2)
        assert(r1.runs == 8, !r1.success, r1.result == e1)

        val r2 = ParallelExecutor.merge(f2, f1)
        assert(r2.runs == 8, !r2.success, r2.result == e1)
      }
    }

  }
}
