package nyaya.gen

import scala.annotation.nowarn
import scala.reflect.ClassTag
import scalaz.EphemeralStream

trait SetLike[F[_], A] {
  def empty                     : F[A]
  def contains(h: F[A], a: A)   : Boolean
  def add     (h: F[A], a: A)   : F[A]
  def addAll  (h: F[A], i: F[A]): F[A]

  @deprecated("Renamed to .addAll", "0.10.0")
  final def append(h: F[A], i: F[A]): F[A] =
    addAll(h, i)
}

trait SetLikeLowPriImplicits {
  @inline implicit def fromGeneric[F[_], A](implicit g: SetLike.Generic[F]): SetLike[F, A] =
    g.apply[A]
}

object SetLike extends ScalaVerSpecific.SetLikeImplicits with SetLikeLowPriImplicits {

  trait Generic[F[_]] {
    def apply[A]: SetLike[F, A]
  }

  class CastFromAny[F[_]](a: SetLike[F, Any]) extends Generic[F] {
    final override def apply[A] = a.asInstanceOf[SetLike[F, A]]
  }

  trait ByClassTag[F[_]] {
    def apply[A: ClassTag]: SetLike[F, A]
  }

  @inline implicit def byClassTag[F[_], A](implicit g: Generic[F], a: ClassTag[A]): SetLike[F, A] =
    g.apply[A]

  // ===================================================================================================================

  implicit object ForOption extends CastFromAny[Option]({
    type F[A] = Option[A]
    type A = Any
    new SetLike[F, A] {
      override def empty                      = None
      override def contains(h: F[A], a: A)    = h contains a
      override def add     (h: F[A], a: A)    = Some(a)
      override def addAll  (h: F[A], i: F[A]) = i orElse h
    }
  })

  implicit object ForList extends CastFromAny[List]({
    type F[A] = List[A]
    type A = Any
    new SetLike[F, A] {
      override def empty                      = Nil
      override def contains(h: F[A], a: A)    = h contains a
      override def add     (h: F[A], a: A)    = a :: h
      override def addAll  (h: F[A], i: F[A]) = i ::: h
    }
  })

  implicit object ForVector extends CastFromAny[Vector]({
    type F[A] = Vector[A]
    type A = Any
    new SetLike[F, A] {
      override def empty                      = Vector.empty
      override def contains(h: F[A], a: A)    = h contains a
      override def add     (h: F[A], a: A)    = h :+ a
      override def addAll  (h: F[A], i: F[A]) = h ++ i
    }
  })

  implicit object ForSet extends CastFromAny[Set]({
    type F[A] = Set[A]
    type A = Any
    new SetLike[F, A] {
      override def empty                      = Set.empty
      override def contains(h: F[A], a: A)    = h contains a
      override def add     (h: F[A], a: A)    = h + a
      override def addAll  (h: F[A], i: F[A]) = h ++ i
    }
  })

  @nowarn("cat=deprecation")
  implicit object ForStream extends CastFromAny[Stream]({
    type F[A] = Stream[A]
    type A = Any
    new SetLike[F, A] {
      override def empty                      = Stream.empty
      override def contains(h: F[A], a: A)    = h contains a
      override def add     (h: F[A], a: A)    = a #:: h
      override def addAll  (h: F[A], i: F[A]) = i #::: h
    }
  })

  // TODO deprecate and stop relying on Scalaz
  implicit object ForEphemeralStream extends CastFromAny[EphemeralStream]({
    type F[A] = EphemeralStream[A]
    type A = Any
    new SetLike[F, A] {
      override def empty                      = EphemeralStream[A]
      override def contains(h: F[A], a: A)    = !h.filter(_ == a).isEmpty
      override def add     (h: F[A], a: A)    = a ##:: h
      override def addAll  (h: F[A], i: F[A]) = h ++ i
    }
  })

  // ===================================================================================================================

  object Implicits {
    implicit final class NyayaSetLikeExt[F[_], A](private val fa: F[A]) extends AnyVal {
      def contains(a: A)   (implicit F: SetLike[F, A]) = F.contains(fa, a)
      def +       (a: A)   (implicit F: SetLike[F, A]) = F.add(fa, a)
      def ++      (i: F[A])(implicit F: SetLike[F, A]) = F.addAll(fa, i)
      //def addAll  (as: A*) (implicit F: SetLike[F, A]) = as.foldLeft(fa)(_ + _)
    }
  }
}
