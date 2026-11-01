package nyaya.gen

import scala.annotation.tailrec

/*
trait Shrinker[+A] {
  // def apply[B >: A](initialValue: B)(accept: B => Boolean): B =
  //   ??? // loop

  def shrink[B >: A](value: B): Option[Shrunk[B]]
  def map[B](f: A => B): Shrinker[B]
}

trait Shrunk[+A] {
  val value: A
  def accept: Option[Shrunk[A]]
  def reject: Option[Shrunk[A]]
}

object OhNo {
  val s: Shrinker[Int] = ???
  // s.shrink(??? : Any)
  s.shrink(123)
}
*/

/*
trait Shrinkable[+A] { self =>
  val value: A
  def accept: Option[Shrinkable[A]]
  def reject: Option[Shrinkable[A]]

  final def shrink(accept: A => Boolean): A =
    ??? // loop

  final def map[B](f: A => B): Shrinkable[B] =
    new Shrinkable[B] {
      override val value = f(self.value)
      override def accept = self.accept.map(_.map(f))
      override def reject = self.reject.map(_.map(f))
    }

  // final def flatMap[B](f: A => Shrinkable[B]): Shrinkable[B] =
  //   new Shrinkable[B] {
  //     private[this] val sb = f(self.value)
  //     override val value = sb.value
  //     override def accept = Shrinkable.merge(self.accept.map(_.flatMap(f)), sb.accept)
  //     override def reject = Shrinkable.merge(self.reject.map(_.flatMap(f)), sb.reject)
  //   }
}

// object Shrinkable {
//   def merge[A](outer: Option[Shrinkable[A]], inner: Option[Shrinkable[A]]): Option[Shrinkable[A]] =
//     (outer, inner) match {
//       case (None, None) => None
//       case (s, None) => s
//       case (None, s) => s
//       case (Some(o), Some(i)) => Some(o.)
//     }
// }
*/

// final case class RoseTree[+A](value: A, children: LazyList[RoseTree[A]]) {

//   def map[B](f: A => B): RoseTree[B] =
//     RoseTree(f(value), children.map(_.map(f)))

//   def flatMap[B](f: A => RoseTree[B]): RoseTree[B] = {
//     val inner         = f(value)
//     val outerChildren = children.map(_.flatMap(f))
//     val allChildren   = outerChildren #::: inner.children
//     RoseTree(inner.value, allChildren)
//   }
// }

final case class Shrinkable[+A](value: A, children: LazyList[Shrinkable[A]]) {

  def shrink(accept: A => Boolean): A = {
    @tailrec def loop(s: Shrinkable[A]): A =
      s.children.find(c => accept(c.value)) match {
        case Some(child) => loop(child)
        case None        => s.value
      }
    loop(this)
  }

  def map[B](f: A => B): Shrinkable[B] =
    Shrinkable(f(value), children.map(_.map(f)))

  def flatMap[B](f: A => Shrinkable[B]): Shrinkable[B] = {
    val inner         = f(value)
    val outerChildren = children.map(_.flatMap(f))
    val allChildren   = outerChildren #::: inner.children
    Shrinkable(inner.value, allChildren)
  }
}

object Shrinkable {
  def const[A](value: A): Shrinkable[A] =
    apply(value, LazyList.empty)

  def longHalves(n: Long): LazyList[Long] = {
    val half = n / 2
    if (half == 0) LazyList.empty else half #:: longHalves(half)
  }

  def intHalves(n: Int): LazyList[Int] = {
    val half = n / 2
    if (half == 0) LazyList.empty else half #:: intHalves(half)
  }

  def longToZero(n: Long): Shrinkable[Long] = {
    val shrinks = if (n == 0) LazyList.empty else 0L #:: longHalves(n).map(n - _)
    apply(n, shrinks.map(longToZero))
  }

  /** Args are inclusive. [l,h] */
  def longInRange(n: Long)(l: Long, h: Long): Shrinkable[Long] =
    if (h >= 0) {
      if (l <= 0)
        longToZero(n) // [-,+]
      else
        longToZero(n - l).map(_ + l) // [+,+]
    } else
      longToZero(n - h).map(_ + h) // [-,-]

  def intToZero(n: Int): Shrinkable[Int] =
    longToZero(n).map(_.toInt)

  /** Args are inclusive. [l,h] */
  def intInRange(n: Int)(l: Int, h: Int): Shrinkable[Int] =
    longInRange(n)(l, h).map(_.toInt)

  def vector[A](minElements: Int, trees: Vector[Shrinkable[A]]): Shrinkable[Vector[A]] = {
    assert(trees.length >= minElements, s"Length (${trees.length}) cannot be smaller than minElements ($minElements)")

    val rootValues: Vector[A] =
      trees.map(_.value)

    val structural: LazyList[Shrinkable[Vector[A]]] =
      dropChunksBounded(minElements, trees).map(vector(minElements, _))

    val elementWise: LazyList[Shrinkable[Vector[A]]] =
      shrinkElements(trees).map(vector(minElements, _))

    Shrinkable(rootValues, structural #::: elementWise)
  }

  private def dropChunksBounded[A](minElements: Int, vec: Vector[Shrinkable[A]]): LazyList[Vector[Shrinkable[A]]] = {
    val len = vec.length
    val excess = len - minElements

    if (excess <= 0) {
      // Already at or below the minimum length; no structural shrinking possible
      LazyList.empty
    } else {
      // 1. Most aggressive shrink first: truncate straight to minElements
      val minimalBoundCandidate: Vector[Shrinkable[A]] =
        vec.take(minElements)

      // 2. Binary search halving steps over the excess budget [excess/2, excess/4, ..., 1]
      val chunkSizes = intHalves(excess)

      val chunkRemovals = chunkSizes.flatMap { chunkSize =>
        // Only drop chunks from valid offsets such that length - chunkSize >= minElements
        (0 until len by chunkSize).to(LazyList).flatMap { offset =>
          val candidate = vec.patch(offset, Nil, chunkSize)
          if (candidate.length >= minElements) Some(candidate)
          else None
        }
      }

      minimalBoundCandidate #:: chunkRemovals
    }
  }

  private def shrinkElements[A](trees: Vector[Shrinkable[A]]): LazyList[Vector[Shrinkable[A]]] =
    (0 until trees.length).to(LazyList).flatMap { idx =>
      trees(idx).children.map { shrunkChild =>
        trees.updated(idx, shrunkChild)
      }
    }
}
