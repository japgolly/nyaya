package nyaya.gen

import scala.collection.AbstractIterator
import scala.collection.Factory

final case class Gen2[+A](run: Gen2.Run[A]) extends AnyVal {

  /** Produce a sample datum. */
  def sample(): Shrinkable[A] =
    samples().next()

  /**
   * Produce an infinite stream of generated data.
   *
   * Use `.take(n)` for a finite number of samples.
   */
  def samples(genSize: GenSize = GenSize.Default): Iterator[Shrinkable[A]] =
    samplesUsing(GenCtx(genSize, ThreadNumber(0)))

  /**
   * Produce an infinite stream of generated data.
   *
   * Use `.take(n)` for a finite number of samples.
   */
  def samplesUsing(ctx: GenCtx): Iterator[Shrinkable[A]] =
    new AbstractIterator[Shrinkable[A]] {
      override def hasNext = true
      override def next(): Shrinkable[A] = ctx.sample2(Gen2.this)
    }

  def withSeed(seed: Long): Gen2[A] =
    Gen2.setSeed(seed) >> this

  def flatMap[B](f: A => Gen2[B]): Gen2[B] =
    Gen2(c => run(c).flatMap(f(_).run(c)))

  def >>[B](f: Gen2[B]): Gen2[B] =
    flatMap(_ => f)

  def vector(implicit ss: SizeSpec): Gen2[Vector[A]] = Gen2 { ctx =>
    val n = ss.gen.run(ctx)
    if (n >= 100000)
      println(s"WARNING: Gen.fill instructed to create very large data: n=$n")
    val b = Vector.newBuilder[Shrinkable[A]]
    var i = n
    while (i > 0) {
      val x = run(ctx)
      b += x
      i -= 1
    }
    val v = b.result()
    Shrinkable.vector(0, v)
  }
  //   fillSS(ss)

  // @inline def fillSS[B](ss: SizeSpec)(implicit cbf: Factory[A, B]): Gen2[B] =
  //   ss.gen flatMap fill[B]

  // def fill[B](n: Int)(implicit cbf: Factory[A, B]): Gen2[B] = {
  //   Gen2 { c =>
  //     ???
  //   }
  // }
}

object Gen2 {
  type Run[+A] = GenCtx => Shrinkable[A]

  val int: Gen2[Int] = Gen2 { ctx =>
    val i = ctx.rnd.nextInt()
    Shrinkable.intToZero(i)
  }

  def setSeedBy(f: SeedCtx => Long): Gen2[Unit] =
    Gen2(ctx => Shrinkable const ctx.setSeed(f(ctx.seedCtx())))

  def setSeed(seed: Long): Gen2[Unit] =
    setSeedBy(seed + _.offset)
}
