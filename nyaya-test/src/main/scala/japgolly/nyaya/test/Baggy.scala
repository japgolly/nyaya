package japgolly.nyaya.test

import scalaz.EphemeralStream

trait Baggy[H[_]] {
  def empty   [A]                  : H[A]
  def contains[A](h: H[A], a: A)   : Boolean
  def add     [A](h: H[A], a: A)   : H[A]
  def append  [A](h: H[A], i: H[A]): H[A]
}

object Baggy {

  implicit object EphemeralStreamBaggy extends Baggy[EphemeralStream] {
    override def empty   [A]                                               = EphemeralStream[A]
    override def contains[A](h: EphemeralStream[A], a: A)                  = !h.filter(_ == a).isEmpty
    override def add     [A](h: EphemeralStream[A], a: A)                  = a ##:: h
    override def append  [A](h: EphemeralStream[A], i: EphemeralStream[A]) = h ++ i
  }

  implicit object ListBaggy extends Baggy[List] {
    override def empty   [A]                         = List.empty
    override def contains[A](h: List[A], a: A)       = h contains a
    override def add     [A](h: List[A], a: A)       = a :: h
    override def append  [A](h: List[A], i: List[A]) = i ::: h
  }

  implicit object OptionBaggy extends Baggy[Option] {
    override def empty   [A]                             = None
    override def contains[A](h: Option[A], a: A)         = h contains a
    override def add     [A](h: Option[A], a: A)         = Some(a)
    override def append  [A](h: Option[A], i: Option[A]) = i orElse h
  }

  implicit object SetBaggy extends Baggy[Set] {
    override def empty   [A]                       = Set.empty
    override def contains[A](h: Set[A], a: A)      = h contains a
    override def add     [A](h: Set[A], a: A)      = h + a
    override def append  [A](h: Set[A], i: Set[A]) = h ++ i
  }

  implicit object StreamBaggy extends Baggy[Stream] {
    override def empty   [A]                             = Stream.empty
    override def contains[A](h: Stream[A], a: A)         = h contains a
    override def add     [A](h: Stream[A], a: A)         = a #:: h
    override def append  [A](h: Stream[A], i: Stream[A]) = i #::: h
  }

  implicit object VectorBaggy extends Baggy[Vector] {
    override def empty   [A]                             = Vector.empty
    override def contains[A](h: Vector[A], a: A)         = h contains a
    override def add     [A](h: Vector[A], a: A)         = h :+ a
    override def append  [A](h: Vector[A], i: Vector[A]) = h ++ i
  }

  object Implicits {
    implicit final class BaggyExt[H[_], A](val h: H[A]) extends AnyVal {
      def contains(a: A)   (implicit H: Baggy[H]) = H.contains(h, a)
      def +       (a: A)   (implicit H: Baggy[H]) = H.add(h, a)
      def ++      (i: H[A])(implicit H: Baggy[H]) = H.append(h, i)
      //def addAll  (as: A*) (implicit H: Baggy[H]) = as.foldLeft(h)(_ + _)
    }
  }
}
