package nyaya.gen

/**
 * State and config passed to generators.
 *
 * This data is mutable.
 */
final class GenCtx(val rnd: java.util.Random, _genSize: GenSize) {
  private var lastBit = 32
  private var intBits = 0

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

  def apply(gs: GenSize): GenCtx =
    new GenCtx(new java.util.Random, gs)

  def apply(gs: GenSize, seed: Long): GenCtx = {
    val g = GenCtx(gs)
    g setSeed seed
    g
  }

  def apply(gs: GenSize, seed: Option[Long]): GenCtx = {
    val g = GenCtx(gs)
    seed foreach g.setSeed
    g
  }
}