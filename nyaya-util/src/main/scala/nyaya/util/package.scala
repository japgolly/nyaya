package nyaya

import nyaya.util.MultiValues.Commutative

package object util {

  implicit object ListMultiValues extends MultiValues[List] {
    override def empty  [A]                                     = List.empty[A]
    override def add1   [A]  (a: List[A], b: A)                 = b :: a
    override def del1   [A]  (a: List[A], b: A)                 = a.filterNot(_ == b)
    override def addn   [A]  (a: List[A], b: List[A])           = b ::: a
    override def deln   [A]  (a: List[A], b: List[A])           = {val s = b.toSet; a filterNot s.contains}
    override def foldl  [A,B](a: A, b: List[B])(f: (A, B) => A) = b.foldLeft(a)(f)
    override def foldr  [A,B](a: A, b: List[B])(f: (A, B) => A) = b.foldRight(a)((x, y) => f(y, x))
    override def stream [A]  (a: List[A])                       = a.toStream
    override def isEmpty[A]  (a: List[A])                       = a.isEmpty
  }

  implicit object SetMultiValues extends MultiValues[Set] with Commutative[Set] {
    override def empty  [A]                                    = Set.empty[A]
    override def add1   [A]  (a: Set[A], b: A)                 = a + b
    override def del1   [A]  (a: Set[A], b: A)                 = a - b
    override def addn   [A]  (a: Set[A], b: Set[A])            = a ++ b
    override def deln   [A]  (a: Set[A], b: Set[A])            = a -- b
    override def foldl  [A,B](a: A, b: Set[B])(f: (A, B) => A) = b.foldLeft(a)(f)
    override def foldr  [A,B](a: A, b: Set[B])(f: (A, B) => A) = b.foldRight(a)((x, y) => f(y, x))
    override def stream [A]  (a: Set[A])                       = a.toStream
    override def isEmpty[A]  (a: Set[A])                       = a.isEmpty
  }

  implicit object VectorMultiValues extends MultiValues[Vector] {
    override def empty  [A]                                       = Vector.empty[A]
    override def add1   [A]  (a: Vector[A], b: A)                 = a :+ b
    override def del1   [A]  (a: Vector[A], b: A)                 = a.filterNot(_ == b)
    override def addn   [A]  (a: Vector[A], b: Vector[A])         = a ++ b
    override def deln   [A]  (a: Vector[A], b: Vector[A])         = {val s = b.toSet; a filterNot s.contains}
    override def foldl  [A,B](a: A, b: Vector[B])(f: (A, B) => A) = b.foldLeft(a)(f)
    override def foldr  [A,B](a: A, b: Vector[B])(f: (A, B) => A) = b.foldRight(a)((x, y) => f(y, x))
    override def stream [A]  (a: Vector[A])                       = a.toStream
    override def isEmpty[A]  (a: Vector[A])                       = a.isEmpty
  }

  @inline implicit final class NyayaUtilAnyExt[A](private val a: A) extends AnyVal {
    @inline def `JVM|JS`(js: => A): A = Platform.choose(a, js)
  }
}
