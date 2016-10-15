package nyaya.gen

case class ThreadNumber(value: Int) extends AnyVal

case class SampleNumber(value: Int) extends AnyVal

case class SeedCtx(thread: ThreadNumber, sample: SampleNumber) {
  def offset: Long =
    (sample.value.toLong << 16) | thread.value.toLong
}

/**
 * State and config passed to generators.
 *
 * This data is mutable.
 */
final class GenCtx(val rnd: java.util.Random, _genSize: GenSize, val thread: ThreadNumber) {
  private var lastBit = 32
  private var intBits = 0
  private var sampleNumber = SampleNumber(0)

  def seedCtx(): SeedCtx =
    SeedCtx(thread, sampleNumber)

  def sample[A](g: Gen[A]): A = {
    val a = g run this
    incSampleNumber()
    a
  }

  def incSampleNumber(): Unit =
    sampleNumber = SampleNumber(sampleNumber.value + 1)

  /** 32x less calls nextInt() compared to java.util.Random.nextBoolean() */
  def nextBit(): Boolean = {
    if (lastBit == 32) {
      lastBit = 0
      intBits = rnd.nextInt()
    } else
      lastBit += 1
    (intBits & (1 << lastBit)) == 0
  }

  def setSeed(seed: Long): Unit = {
    rnd setSeed seed
    lastBit = 32
  }

  def shiftLeft(i: Int): Int = {
    val j = i << 1
    if (nextBit()) j else (j | 1)
  }

  /** @return [0,2) */
  def nextInt2(): Int =
    if (nextBit()) 0 else 1

  /** @return [0,4) */
  def nextInt4(): Int =
    shiftLeft(nextInt2())

  /** @return [0,8) */
  def nextInt8(): Int =
    shiftLeft(nextInt4())

  /** @return [0,16) */
  def nextInt16(): Int =
    shiftLeft(nextInt8())

  private var genSize = _genSize
  var nextSize     = _nextSize
  var nextSizeMin1 = _nextSizeMin1

  def fixGenSize(n:  Int) = genSize.value min n
  def fixGenSize1(n: Int) = genSize.value min n max 1
  private[nyaya] def setGenSize(gs: GenSize): Unit = {
    genSize      = gs
    nextSize     = _nextSize
    nextSizeMin1 = _nextSizeMin1
  }

  def _nextSize: () => Int =
    if (genSize.value == 0)
      () => 0
    else {
      val n = genSize.value + 1
      () => rnd.nextInt(n)
    }

  def _nextSizeMin1: () => Int =
    if (genSize.value <= 1)
      () => 1
    else
      () => rnd.nextInt(genSize.value) + 1
}

// =====================================================================================================================

object GenCtx {
  def apply(gs: GenSize, thread: ThreadNumber): GenCtx =
    new GenCtx(new java.util.Random, gs, thread)
}