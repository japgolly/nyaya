package japgolly.nyaya

import com.nicta.rng.Rng

package object test {

  implicit class RngExt[A](val rng: Rng[A]) extends AnyVal {
    def gen = Gen.unsized(rng)
  }
}