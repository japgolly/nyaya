package japgolly.nyaya.test

// ==================================
// ==========              ==========
// ========== JVM settings ==========
// ==========              ==========
// ==================================

object DefaultSettings {

  implicit val propSettings =
    Settings(
      executor   = ParallelExecutor(),
      sampleSize = SampleSize(100),
      genSize    = GenSize(200))
}
