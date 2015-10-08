package japgolly.nyaya.test

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.generic.CanBuildFrom

/**
 * Iterator over generated data.
 */
abstract class Samples[+A] {
  def hasNext: Boolean
  def next(): A

  override def toString =
    s"Samples{hasNext = $hasNext}"

  final def nextOption(): Option[A] =
    if (hasNext)
      Some(next())
    else
      None

  final def foreach[U](f: A => U): Unit =
    while (hasNext)
      f(next())

  final def to[B](implicit cbf: CanBuildFrom[Nothing, A, B]): B = {
    val b = cbf()
    while (hasNext) b += next()
    b.result()
  }

  final def toList       : List  [A] = to[List  [A]]
  final def toVector     : Vector[A] = to[Vector[A]]
  final def toSet[B >: A]: Set   [B] = to[Set   [B]]

  final def toIterator: Iterator[A] = {
    val underlying = this
    new AbstractIterator[A] {
      override def hasNext = underlying.hasNext
      override def next() = underlying.next()
    }
  }

  final def toStream: Stream[A] =
    if (hasNext)
      Stream.cons(next(), toStream)
    else
      Stream.empty

  def take(n: Int): Samples[A] = {
    val underlying = this
    new Samples[A] {
      var i = n
      override def hasNext = i > 0 && underlying.hasNext
      override def next(): A = { i -= 1; underlying.next() }
    }
  }
}

object Samples {
  case class BatchSize(samples: SampleSize, genSize: GenSize)

  def planBatchSizes(sampleSize: SampleSize, sizeDist: Settings.SizeDist, genSize: GenSize): Vector[BatchSize] = {
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

  def batches[A](gen: Gen[A], ctx: GenCtx, plan: Vector[BatchSize], logNewBatch: BatchSize => Unit): Samples[A] = {
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

    new Samples[A] {
      override def hasNext =
        (remainingInThisBatch > 0) || prepareNextBatch()

      override def next(): A = {
        remainingInThisBatch -= 1
        gen run ctx
      }
    }
  }

  def continually[A](f: () => A): Samples[A] =
    new Samples[A] {
      override def hasNext = true
      override def next(): A = f()
    }
}
