package nyaya.test

import java.util.concurrent._, atomic.AtomicInteger
import nyaya.gen.ThreadNumber
import nyaya.prop.Prop
import ParallelExecutor._
import PTest._
import Executor.{DataCtx, Data}

// TODO data SampleSize = TotalSamples(n) | Fn(qty|%, gensize|%) | PerWorker(sampleSize)

object ParallelExecutor {
  val defaultThreadCount = 1.max(Runtime.getRuntime.availableProcessors - 1)

  def merge[A](a: RunState[A], b: RunState[A]): RunState[A] = {
    val runs = a.runs max b.runs
    (a.success, b.success) match {
      case (false, true) => RunState(runs, a.result)
      case _             => RunState(runs, b.result)
    }
  }
}

case class ParallelExecutor(workers: Int = defaultThreadCount) extends Executor {

  val debugPrefixes = (0 until workers).toVector.map(i => s"Worker #$i: ")

  override def run[A](p: Prop[A], g: Data[A], S: Settings): RunState[A] = {
    val sss = {
      var rem = S.sampleSize.value
      var i = workers
      var v = Vector.empty[SampleSize]
      while(i > 0) {
        val p = rem / i
        v :+= SampleSize(p)
        rem -= p
        i -= 1
      }
      v
    }

    if (S.debug) {
      val szs = sss.map(_.value)
      println(s"Samples/Worker: ${szs.mkString("{", ",", "}")} = Σ${szs.sum}")
    }

    val ai = new AtomicInteger(0)
    def task(worker: Int) = mkTask {
      val dp = debugPrefixes(worker)
      val data = g(DataCtx(sss(worker), ThreadNumber(worker), S.seed, dp))
      testN(p, data, ai.incrementAndGet, S)
    }
    runAsync2(workers, task)
  }

  override def prove[A](p: Prop[A], d: Domain[A], S: Settings): RunState[A] = {
    val threads = workers min d.size

    val ai = new AtomicInteger(0)
    def task(worker: Int) = mkTask {
      proveN(p, d, worker, threads, _ => ai.incrementAndGet, S)
    }
    runAsync2(threads, task)
  }

  private[this] def mkTask[A](f: => RunState[A]) = new Callable[RunState[A]] {
    override def call(): RunState[A] = f
  }

  private[this] def runAsync2[A](threads: Int, f: Int => Callable[RunState[A]]): RunState[A] =
    runAsync(es => (0 until threads).toList.map(es submit f(_)))

  private[this] def runAsync[A](start: ExecutorService => List[Future[RunState[A]]]): RunState[A] = {
    val es: ExecutorService = Executors.newFixedThreadPool(workers)
    val fs = start(es)
    es.shutdown()
    val rss = fs.map(_.get())
    es.awaitTermination(1, TimeUnit.MINUTES)
    rss.foldLeft(RunState.empty[A])(merge)
  }
}