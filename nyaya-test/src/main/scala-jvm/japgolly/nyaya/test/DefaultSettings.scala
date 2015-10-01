package japgolly.nyaya.test

// ==================================
// ==========              ==========
// ========== JVM settings ==========
// ==========              ==========
// ==================================

object DefaultSettings {

  implicit val propSettings =
    Settings(executor = ParallelExecutor())
}
