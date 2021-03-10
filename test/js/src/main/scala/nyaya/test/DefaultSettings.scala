package nyaya.test

import nyaya.gen.GenSize

// =========================================
// ==========                     ==========
// ========== JavaScript settings ==========
// ==========                     ==========
// =========================================

object DefaultSettings {

  implicit val propSettings: Settings =
    Settings(genSize = GenSize.Default.map(_ >>> 1)) // Halve the default GenSize. JS is slower.
}
