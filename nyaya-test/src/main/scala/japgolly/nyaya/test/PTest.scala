package japgolly.nyaya.test

import scalaz.EphemeralStream
import japgolly.nyaya._
import Executor.Data

case class RunState[A](runs: Int, result: Result[A])
object RunState {
  implicit def RunStateToResult[A](r: RunState[A]): Result[A] = r.result

  def empty[A] = RunState[A](0, Satisfied)
}

object PTest {

  private[this] def prepareData[A](gen: Gen[A], sizeDist: Settings.SizeDist, genSize: GenSize, debug: Boolean): Data[A] =
    (sampleSize, seedOption, debugPrefix) => {

      val plan: Seq[(SampleSize, GenSize)] =
        if (sizeDist.isEmpty)
          ((sampleSize, genSize)) :: Nil
        else {
          var total = sizeDist.foldLeft(0)(_ + _._1)
          var rem = sampleSize.value
          sizeDist.map { x =>
            val si = x._1
            val gg = x._2
            val gs = gg.fold[GenSize](p => genSize.map(v => (v * p + 0.5).toInt max 0), identity)
            val ss = SampleSize((si.toDouble / total * rem + 0.5).toInt)
            total -= si
            rem -= ss.value
            (ss, gs)
          }
        }

      val dataGen =
        plan.foldLeft(Gen pure EphemeralStream.emptyEphemeralStream[A]) { (q, x) =>
          val ss = x._1
          val gs = x._2
          Gen { c =>
            val as = q.run(c)
            as ++ { // ++ is non-strict
              if (debug) println(s"${debugPrefix}Generating ${ss.value} samples @ sz ${gs.value}...")
              c.setGenSize(gs)
              gen.estream(ss.value).run(c)
            }
          }
        }

      val ctx = GenCtx(genSize, seedOption)
      dataGen run ctx
    }

  def test[A](p: Prop[A], gen: Gen[A], S: Settings): RunState[A] = {
    if (S.debug) println(s"\n$p")
    S.executor.run(p, prepareData(gen, S.sizeDist, S.genSize, S.debug), S)
  }

  private[test] def testN[A](p: Prop[A], data: EphemeralStream[A], runInc: () => Int, S: Settings): RunState[A] = {
    val it = EphemeralStream.toIterable(data).iterator
    var rs = RunState.empty[A]
    while (rs.success && it.hasNext) {
      val a = try it.next() catch {case t: Throwable => t.printStackTrace(); throw t} // TODO Do better!
      rs = RunState(runInc(), test1(p, a))
      if (S.debug) debug1(a, rs, S)
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

  private[test] def test1[A](p: Prop[A], a: A): Result[A] =
    try {
      Result(a, p(a))
    } catch {
      case e: Throwable => Error(a, e)
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
      rs = RunState(runInc(i), test1(p, a))
      if (S.debug) debug1(a, rs, S)
      i += step
    }
    rs
  }
}
