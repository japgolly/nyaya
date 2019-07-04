package nyaya.test

// ==================================
// ==========              ==========
// ========== JVM settings ==========
// ==========              ==========
// ==================================

object DefaultSettings {

  implicit val propSettings: Settings =
    Settings(executor = ParallelExecutor())
}
