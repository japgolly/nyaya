package nyaya.test

import nyaya.gen.GenSize

// =========================================
// ==========                     ==========
// ========== JavaScript settings ==========
// ==========                     ==========
// =========================================

object DefaultSettings {

  implicit val propSettings =
    Settings(genSize = GenSize(16))
}
