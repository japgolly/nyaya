package japgolly.nyaya

package object test {

  /**
   * Iterator over generated data.
   */
  type Samples[+A] = Iterator[A]
}
