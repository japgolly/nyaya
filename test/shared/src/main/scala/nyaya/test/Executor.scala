package nyaya.test

import nyaya.gen.ThreadNumber
import nyaya.prop.Prop
import nyaya.test.PTest._

object Executor {
  type DebugPrefix = String
  case class DataCtx(sampleSize: SampleSize, threadNumber: ThreadNumber, seed: Option[Long], debugPrefix: DebugPrefix)
  type Data[A] = DataCtx => Iterator[A]
}

import Executor.{DataCtx, Data}

trait Executor {
  def run[A](p: Prop[A], g: Data[A], S: Settings): RunState[A]
  def prove[A](p: Prop[A], d: Domain[A], S: Settings): RunState[A]
}

object SingleThreadedExecutor extends Executor {
  override def run[A](p: Prop[A], g: Data[A], S: Settings): RunState[A] = {
    val data = g(DataCtx(S.sampleSize, ThreadNumber(0), S.seed, ""))
    var i = 0
    testN(p, data, () => {i+=1; i}, S)
  }

  override def prove[A](p: Prop[A], d: Domain[A], S: Settings): RunState[A] =
    proveN(p, d, 0, 1, _ + 1, S)
}