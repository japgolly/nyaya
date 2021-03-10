package nyaya.gen

import scala.collection.immutable.IndexedSeq

sealed trait SizeSpec {
  def gen : Gen[Int]
  def gen1: Gen[Int]
}

object SizeSpec {
  case object Default extends SizeSpec {
    override val gen  = Gen.chooseSize
    override val gen1 = Gen.chooseSizeMin1
    private[gen] val both = (gen, gen1)
  }

  implicit def default: SizeSpec =
    Default

  object DisableDefault {
    implicit def _disableDefaultSizeSpec1: SizeSpec = ???
    implicit def _disableDefaultSizeSpec2: SizeSpec = ???
  }

  // ===================================================================================================================

  case class Exactly(value: Int) extends SizeSpec {
    override val gen  = Gen(_ => value)
    override val gen1 = Gen(_ => value)
  }

  implicit def autoFromInt(i: Int): SizeSpec =
    Exactly(i)

  // ===================================================================================================================

  case class OneOf(possibilities: IndexedSeq[Int]) extends SizeSpec {

    override val (gen, gen1) = {
      val len = possibilities.length

      if (len == 1) {
        val e = Exactly(possibilities.head)
        (e.gen, e.gen1)

      } else if (len == 0) {
        Default.both

      } else {
        val g = Gen.chooseIndexed_!(possibilities)
        val a = g flatMap (n => Gen(_ fixGenSize n))
        val b = g flatMap (n => Gen(_ fixGenSize1 n))
        (a, b)
      }
    }
  }

  implicit def autoFromSeq(possibilities: IndexedSeq[Int]): SizeSpec =
    OneOf(possibilities)

  // ===================================================================================================================

  implicit def autoFromOption[A](o: Option[A])(implicit ev: A => SizeSpec): SizeSpec =
    o.fold[SizeSpec](Default)(ev)
}
