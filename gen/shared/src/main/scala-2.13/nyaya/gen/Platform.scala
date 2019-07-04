package nyaya.gen

object Platform {

  trait BaggyImplicits {

    implicit object LazyListBaggy extends Baggy[LazyList] {
      override def empty   [A]                                 = LazyList.empty
      override def contains[A](h: LazyList[A], a: A)           = h contains a
      override def add     [A](h: LazyList[A], a: A)           = a #:: h
      override def append  [A](h: LazyList[A], i: LazyList[A]) = i #::: h
    }

  }

}
