package nyaya.test

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.{Callable, ExecutorService, Executors, TimeUnit}
import nyaya.gen.ThreadNumber
import nyaya.prop.Prop
import nyaya.test.Executor.{Data, DataCtx}
import nyaya.test.PTest._
import nyaya.test.ParallelExecutor._

// TODO data SampleSize = TotalSamples(n) | Fn(qty|%, gensize|%) | PerWorker(sampleSize)

object ParallelExecutor {
  val defaultThreadCount = 1.max(Runtime.getRuntime.availableProcessors - 1)

  def merge[A](a: RunState[A], b: RunState[A]): RunState[A] = {
    val runs = a.runs max b.runs
    (a.success, b.success) match {
      case (false, true)  => RunState(runs, a.result)
      case (false, false) => RunState(runs, if (a.runs <= b.runs) a.result else b.result)
      case _              => RunState(runs, b.result)
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
    runAsync(workers) { (worker, cont, cancel) =>
      mkTask {
        val dp = debugPrefixes(worker)
        val data = g(DataCtx(sss(worker), ThreadNumber(worker), S.seed, dp))
        testN(p, data, () => ai.incrementAndGet(), S, continue = cont, onFailure = cancel)
      }
    }
  }

  override def prove[A](p: Prop[A], d: Domain[A], S: Settings): RunState[A] = {
    val threads = workers min d.size

    val ai = new AtomicInteger(0)
    runAsync(threads) { (worker, cont, cancel) =>
      mkTask {
        proveN(p, d, worker, threads, _ => ai.incrementAndGet(), S, continue = cont, onFailure = cancel)
      }
    }
  }

  private[this] def mkTask[A](f: => RunState[A]) = new Callable[RunState[A]] {
    override def call(): RunState[A] = f
  }

  private[this] def runAsync[A](threads: Int)(task: (Int, () => Boolean, () => Unit) => Callable[RunState[A]]): RunState[A] = {
    val es: ExecutorService = Executors.newFixedThreadPool(workers)
    try {
      val cancelled = new AtomicBoolean(false)
      val cancel = () => cancelled.set(true)
      val cont = () => !cancelled.get()
      val fs = (0 until threads).toList.map(i => es.submit(task(i, cont, cancel)))
      val rss = fs.map(_.get())
      rss.foldLeft(RunState.empty[A])(merge)
    } finally {
      es.shutdown()
      es.awaitTermination(1, TimeUnit.MINUTES)
      ()
    }
  }
}
