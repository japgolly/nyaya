package nyaya.gen

/**
 * The default maximum size of dynamically-sized data.
 *
 * Most significantly translates to collection and string length.
 */
final case class GenSize(value: Int) {
  assert(value >= 0, "GenSize must be ≥ 0.")

  def map(f: Int => Int): GenSize =
    GenSize(f(value))
}

object GenSize {
  val Default = GenSize(32)
}
