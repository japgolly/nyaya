package japgolly.nyaya

import com.nicta.rng.Rng
import scalaz.NonEmptyList

package object test {

  implicit class RngExt[A](val rng: Rng[A]) extends AnyVal {
    def gen = Gen.unsized(rng)
  }

  implicit class GenSChars0Ext(val g: GenS[List[Char]]) extends AnyVal {
    def string: GenS[String] = g map (_.mkString)
  }

  implicit class GenSChars1Ext(val g: GenS[NonEmptyList[Char]]) extends AnyVal {
    def string1: GenS[String] = g map (_.list.mkString)
  }

  implicit class GenChars0Ext(val g: Gen[List[Char]]) extends AnyVal {
    def string: Gen[String] = g map (_.mkString)
  }

  implicit class GenChars1Ext(val g: Gen[NonEmptyList[Char]]) extends AnyVal {
    def string1: Gen[String] = g map (_.list.mkString)
  }

  implicit class GenCharExt(val g: Gen[Char]) extends AnyVal {
    def string: GenS[String] = g.list.string
    def string1: GenS[String] = g.list1.string1
  }
}