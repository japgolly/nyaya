package nyaya.util

case class NonEmptyList[+A](head: A, tail: List[A]) {

  def ::[B >: A](newHead: B): NonEmptyList[B] =
    NonEmptyList(newHead, head :: tail)

  def map[B](f: A => B): NonEmptyList[B] =
    NonEmptyList(f(head), tail map f)

  def foreach[U](f: A => U): Unit = {
    f(head)
    tail foreach f
  }

  def exists(p: A => Boolean): Boolean =
    p(head) || tail.exists(p)

  def forall(p: A => Boolean): Boolean =
    p(head) && tail.forall(p)

  def toList: List[A] =
    head :: tail

  def iterator: Iterator[A] =
    toList.iterator
}

