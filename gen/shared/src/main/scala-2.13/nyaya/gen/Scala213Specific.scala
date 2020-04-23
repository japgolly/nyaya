package nyaya.gen

import scala.collection.immutable.ArraySeq
import scala.collection.Factory
import scala.reflect.ClassTag

object ScalaVerSpecific {

  trait BaggyImplicits {

    implicit object LazyListBaggy extends Baggy[LazyList] {
      override def empty   [A]                                 = LazyList.empty
      override def contains[A](h: LazyList[A], a: A)           = h contains a
      override def add     [A](h: LazyList[A], a: A)           = a #:: h
      override def append  [A](h: LazyList[A], i: LazyList[A]) = i #::: h
    }

    implicit object ArraySeqBaggy extends Baggy[ArraySeq] {
      override def empty   [A]                                 = ArraySeq.empty
      override def contains[A](h: ArraySeq[A], a: A)           = h contains a
      override def add     [A](h: ArraySeq[A], a: A)           = h :+ a
      override def append  [A](h: ArraySeq[A], i: ArraySeq[A]) = h ++ i
    }
  }

  trait GenClassExt[+A] extends Any { self: Gen[A] =>

    final def to[B](f: Factory[A, B])(implicit ss: SizeSpec): Gen[B] =
      fillSS(ss)(f)

    final def arraySeq[B >: A](implicit ct: ClassTag[B], ss: SizeSpec): Gen[ArraySeq[B]] =
      to(ArraySeq)

    final def arraySeq[B >: A](ss: SizeSpec)(implicit ct: ClassTag[B]): Gen[ArraySeq[B]] =
      to[ArraySeq[B]](ArraySeq)(ss)

    // --------------------------------------------------------------------------------------------

    final def to1[B](f: Factory[A, B])(implicit ss: SizeSpec): Gen[B] =
      fillSS1(ss)(f)

    final def arraySeq1[B >: A](implicit ct: ClassTag[B], ss: SizeSpec): Gen[ArraySeq[B]] =
      to1(ArraySeq)

    final def arraySeq1[B >: A](ss: SizeSpec)(implicit ct: ClassTag[B]): Gen[ArraySeq[B]] =
      to1[ArraySeq[B]](ArraySeq)(ss)

  }
}
