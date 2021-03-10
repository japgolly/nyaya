package nyaya.gen

import scala.collection.immutable.ArraySeq
import scala.collection.Factory
import scala.reflect.ClassTag

object ScalaVerSpecific {

  implicit object SetLikeForLazyList extends SetLike.CastFromAny[LazyList]({
    type F[A] = LazyList[A]
    type A = Any
    new SetLike[F, A] {
      override def empty                      = LazyList.empty
      override def contains(h: F[A], a: A)    = h contains a
      override def add     (h: F[A], a: A)    = a #:: h
      override def addAll  (h: F[A], i: F[A]) = i #::: h
    }
  })

  implicit object SetLikeForArraySeq extends SetLike.ByClassTag[ArraySeq] {
    override def apply[A: ClassTag]: SetLike[ArraySeq, A] =
      new SetLike[ArraySeq, A] {
        override def empty                                    = ArraySeq.empty
        override def contains(h: ArraySeq[A], a: A)           = h contains a
        override def add     (h: ArraySeq[A], a: A)           = h :+ a
        override def addAll  (h: ArraySeq[A], i: ArraySeq[A]) = h ++ i
      }
  }

  trait SetLikeImplicits {

    implicit def NyayaSetLikeForLazyList: SetLike.CastFromAny[LazyList] =
      ScalaVerSpecific.SetLikeForLazyList

    implicit def NyayaSetLikeForArraySeq: SetLike.ByClassTag[ArraySeq] =
      ScalaVerSpecific.SetLikeForArraySeq
  }

  trait GenClassExt[+A] extends Any { self: Gen[A] =>

    final inline def to[B](f: Factory[A, B])(implicit ss: SizeSpec): Gen[B] =
      fillSS(ss)(f)

    final inline def arraySeq[B >: A](implicit ct: ClassTag[B], ss: SizeSpec): Gen[ArraySeq[B]] =
      to(ArraySeq)

    final inline def arraySeq[B >: A](ss: SizeSpec)(implicit ct: ClassTag[B]): Gen[ArraySeq[B]] =
      to[ArraySeq[B]](ArraySeq)(ss)

    // --------------------------------------------------------------------------------------------

    final inline def to1[B](f: Factory[A, B])(implicit ss: SizeSpec): Gen[B] =
      fillSS1(ss)(f)

    final inline def arraySeq1[B >: A](implicit ct: ClassTag[B], ss: SizeSpec): Gen[ArraySeq[B]] =
      to1(ArraySeq)

    final inline def arraySeq1[B >: A](ss: SizeSpec)(implicit ct: ClassTag[B]): Gen[ArraySeq[B]] =
      to1[ArraySeq[B]](ArraySeq)(ss)

  }
}
