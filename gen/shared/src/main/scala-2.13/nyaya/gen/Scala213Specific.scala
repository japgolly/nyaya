package nyaya.gen

import scala.collection.immutable.ArraySeq
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
    implicit def NyayaSetLikeForLazyList = ScalaVerSpecific.SetLikeForLazyList
    implicit def NyayaSetLikeForArraySeq = ScalaVerSpecific.SetLikeForArraySeq
  }
}
