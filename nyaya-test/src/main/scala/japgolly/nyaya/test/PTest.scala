package japgolly.nyaya.test

import japgolly.nyaya._
import Executor.Data
import Samples.BatchSize

case class RunState[A](runs: Int, result: Result[A])
object RunState {
  implicit def RunStateToResult[A](r: RunState[A]): Result[A] = r.result

  def empty[A] = RunState[A](0, Satisfied)
}

object PTest {
  private[this] val dontLogNewBatch = (_: Any) => ()

  private[this] def prepareData[A](gen: Gen[A], sizeDist: Settings.SizeDist, genSize: GenSize, debug: Boolean): Data[A] =
    dataCtx => {
      val logNewBatch: (BatchSize) => Unit =
        if (debug)
          bs => println(s"${dataCtx.debugPrefix}Generating ${bs.samples.value} samples @ sz ${bs.genSize.value}...")
        else
          dontLogNewBatch

      val plan = Samples.planBatchSizes(dataCtx.sampleSize, sizeDist, genSize)
      val ctx = GenCtx(genSize, dataCtx.seed)
      Samples.batches(gen, ctx, plan, logNewBatch)
    }

  def test[A](p: Prop[A], gen: Gen[A], S: Settings): RunState[A] = {
    if (S.debug) println(s"\n$p")
    S.executor.run(p, prepareData(gen, S.sizeDist, S.genSize, S.debug), S)
  }

  private[test] def testN[A](p: Prop[A], it: Samples[A], runInc: () => Int, S: Settings): RunState[A] = {
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
            rs = RunState(run, Error(Some(a), e))
        }

      } catch {
        case e: Throwable =>
          rs = RunState(run, Error(None, e))
      }
    }
    rs
  }

  private[test] def debug1[A](a: A, r: RunState[A], S: Settings): Unit = {
    def c(code: String, m: Any) = s"\033[${code}m$m\033[0m"
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
      case RunState(n, Satisfied) if n == d.size =>
        RunState(n, Proved)
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
          rs = RunState(run, Error(Some(a), e))
      }

      i += step
    }
    rs
  }
}
