package nyaya.test

import nyaya.gen.GenSize

/**
 * How many samples to generate.
 * How many times each prop is tested.
 */
final case class SampleSize(value: Int) {
  def map(f: Int => Int) = SampleSize(f(value))
}

case class Settings(
  executor   : Executor          = SingleThreadedExecutor,
  sizeDist   : Settings.SizeDist = Seq(1 -> Right(GenSize(4)), 1 -> Left(0.5), 8 -> Left(1)),
  sampleSize : SampleSize        = SampleSize(96),
  genSize    : GenSize           = GenSize.Default,
  seed       : Option[Long]      = None,
  debug      : Boolean           = false,
  debugMaxLen: Int               = 200) {

  lazy val sampleSizeLen = sampleSize.value.toString.length
  lazy val sampleProgressFmt = s"[%${sampleSizeLen}d/${sampleSize.value}] "

  // Convenience
  def setSingleThreaded     : Settings = copy(executor    = SingleThreadedExecutor)
  def setSampleSize(i: Int) : Settings = copy(sampleSize  = SampleSize(i))
  def setGenSize(i: Int)    : Settings = copy(genSize     = GenSize(i))
  def setSeed(s: Long)      : Settings = copy(seed        = Some(s))
  def setDebug              : Settings = copy(debug       = true)
  def setDebugMaxLen(i: Int): Settings = copy(debugMaxLen = i)
}

object Settings {
  type SizeDist = Seq[(Int, Either[Double, GenSize])]
}