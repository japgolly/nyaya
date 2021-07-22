package nyaya.test

import nyaya.gen._
import nyaya.prop._
import nyaya.test.Executor.Data
import scala.annotation.tailrec
import scala.collection.AbstractIterator

case class RunState[A](runs: Int, result: Result[A])
object RunState {
  implicit def RunStateToResult[A](r: RunState[A]): Result[A] = r.result

  def empty[A] = RunState[A](0, Result.Satisfied)
}

object PTest {
  case class BatchSize(samples: SampleSize, genSize: GenSize)

  private[this] def planBatchSizes(sampleSize: SampleSize, sizeDist: Settings.SizeDist, genSize: GenSize): Vector[BatchSize] = {
    val empty = Vector.empty[BatchSize]
    if (sizeDist.isEmpty)
      empty :+ BatchSize(sampleSize, genSize)
    else {
      var total = sizeDist.foldLeft(0)(_ + _._1)
      var rem = sampleSize.value
      sizeDist.foldLeft(empty) { (q, x) =>
        val si = x._1
        val gg = x._2
        val gs = gg.fold[GenSize](p => genSize.map(v => (v * p + 0.5).toInt max 0), identity)
        val ss = SampleSize((si.toDouble / total * rem + 0.5).toInt)
        total -= si
        rem -= ss.value
        q :+ BatchSize(ss, gs)
      }
    }
  }

  private[this] def iterateInBatches[A](gen: Gen[A], ctx: GenCtx, plan: Vector[BatchSize], logNewBatch: BatchSize => Unit): Iterator[A] = {
    var remainingPlan = plan
    var remainingInThisBatch = 0

    @tailrec
    def prepareNextBatch(): Boolean =
      if (remainingPlan.isEmpty)
        false
      else {
        val bs = remainingPlan.head
        remainingPlan = remainingPlan.tail
        if (bs.samples.value == 0)
          prepareNextBatch()
        else {
          logNewBatch(bs)
          remainingInThisBatch = bs.samples.value
          ctx.setGenSize(bs.genSize)
          true
        }
      }

    new AbstractIterator[A] {
      override def hasNext =
        (remainingInThisBatch > 0) || prepareNextBatch()

      override def next(): A = {
        remainingInThisBatch -= 1
        ctx.sample(gen)
      }
    }
  }

  private[this] val dontLogNewBatch = (_: Any) => ()

  private[this] def prepareData[A](gen: Gen[A], sizeDist: Settings.SizeDist, genSize: GenSize, debug: Boolean): Data[A] =
    dataCtx => {
      val logNewBatch: (BatchSize) => Unit =
        if (debug)
          bs => println(s"${dataCtx.debugPrefix}Generating ${bs.samples.value} samples @ sz ${bs.genSize.value}...")
        else
          dontLogNewBatch

      val plan = planBatchSizes(dataCtx.sampleSize, sizeDist, genSize)
      val ctx = GenCtx(genSize, dataCtx.threadNumber)
      dataCtx.seed.foreach(Gen.setSeed(_) run ctx)
      iterateInBatches(gen, ctx, plan, logNewBatch)
    }

  def test[A](p: Prop[A], gen: Gen[A], S: Settings): RunState[A] = {
    if (S.debug) println(s"\n$p")
    S.executor.run(p, prepareData(gen, S.sizeDist, S.genSize, S.debug), S)
  }

  private[test] def testN[A](p: Prop[A], it: Iterator[A], runInc: () => Int, S: Settings): RunState[A] = {
    var rs = RunState.empty[A]
    while (rs.success && it.hasNext) {
      val run = runInc()
      try {
        val a = it.next()

        try {
          rs = RunState(run, Result(a, p(a)))
          if (S.debug) debug1(a, rs, S)
        } catch {
          case e: Throwable =>
            rs = RunState(run, Result.Error(Some(a), e))
        }

      } catch {
        case e: Throwable =>
          rs = RunState(run, Result.Error(None, e))
      }
    }
    rs
  }

  private[test] def debug1[A](a: A, r: RunState[A], S: Settings): Unit = {
    def c(code: String, m: Any) = s"\u001b[${code}m${m}\u001b[0m"
    var aa = a.toString
    val maxLen = if (r.success) S.debugMaxLen else aa.length
    val al = aa.length
    if (al > maxLen)
      aa = aa.substring(0, maxLen)
    aa = c("37", aa)
    if (al > maxLen)
      aa = s"%s … %.0f%%".format(aa, maxLen.toDouble / al * 100.0)
    val pc = if (r.success) "32;1" else "31;1"
    println(s"${c(pc, S.sampleProgressFmt.format(r.runs))}$aa")
    //if (al > 200) println()
  }

  def prove[A](p: Prop[A], d: Domain[A], S1: Settings): RunState[A] = {
    val S = S1.copy(sampleSize = SampleSize(d.size))
    if (S.debug) println(s"\n$p\nAttempting to prove with ${d.size} values...")
    S.executor.prove(p, d, S) match {
      case RunState(n, Result.Satisfied) if n == d.size =>
        RunState(n, Result.Proved)
      case r =>
        if (S.debug && r.success) println(s"Test was successful but didn't prove proposition: $r")
        r
    }
  }

  private[test] def proveN[A](p: Prop[A], d: Domain[A], start: Int, step: Int, runInc: Int => Int, S: Settings): RunState[A] = {
    var rs = RunState.empty[A]
    var i = start
    while (rs.success && i < d.size) {
      val a = d(i)
      val run = runInc(i)

      try {
        rs = RunState(run, Result(a, p(a)))
        if (S.debug) debug1(a, rs, S)
      } catch {
        case e: Throwable =>
          rs = RunState(run, Result.Error(Some(a), e))
      }

      i += step
    }
    rs
  }
}
