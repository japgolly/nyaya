package nyaya.prop

import scala.annotation.tailrec
import scala.collection.Iterable

final case class CycleFree[A](value: A)

case class CycleDetector[A, B](extract: A => Iterator[B], check: (A, Iterator[B]) => Option[(B, B)]) {

  @inline final def hasCycle(a: A): Boolean =
    findCycle(a).isDefined

  @inline final def noCycle(a: A): Boolean =
    findCycle(a).isEmpty

  final def findCycle(a: A): Option[(B, B)] =
    check(a, extract(a))

  def cycleFree(a: A): Either[(B, B), CycleFree[A]] =
    findCycle(a).fold[Either[(B, B), CycleFree[A]]](Right(CycleFree(a)))(Left(_))

  def contramap[Z](f: Z => A) =
    new CycleDetector[Z, B](extract compose f, (z, b) => check(f(z), b))

  def noCycleProp(name: => String): Prop[A] =
    Prop.atom[A](name, findCycle(_).map{
      case (b1,b2) => s"Cycle detected: [$b1] → [$b2]"
    })
}

object CycleDetector {

  object Undirected extends GraphType {
    override def check[A, B, I](vertices: (A, B) => Iterator[B], id: B => I): (A, Iterator[B]) => Option[(B, B)] =
      (a, input) => {

        def outer(prev: B, is0: Set[I]): Either[Set[I], (B, B)] = {
          val it = vertices(a, prev)

          @tailrec
          def inner(is: Set[I]): Either[Set[I], (B, B)] =
            if (it.hasNext) {
              val b = it.next()
              val i = id(b)
              if (is contains i)
                Right((prev, b))
              else
                outer(b, is + i) match {
                  case Left(is2)  => inner(is2)
                  case r@ Right(_) => r
                }
            } else
              Left(is)

          inner(is0)
        }

        input
          .map(b => outer(b, Set(id(b))))
          .collectFirst { case Right(v) => v }
      }
  }

  object Directed extends GraphType {
    override def check[A, B, I](vertices: (A, B) => Iterator[B], id: B => I): (A, Iterator[B]) => Option[(B, B)] =
      (a, input) => {

        def loop(prev: B, is: Set[I]): Option[(B, B)] =
          vertices(a, prev)
            .map { b =>
              val i = id(b)
              if (is contains i)
                Some((prev, b))
              else
                loop(b, is + i)
            }
            .collectFirst { case Some(v) => v }

        input
          .map(b => loop(b, Set(id(b))))
          .collectFirst { case Some(v) => v }
      }
  }

  sealed abstract class GraphType {
    def check[A, B, I](vertices: (A, B) => Iterator[B], id: B => I): (A, Iterator[B]) => Option[(B, B)]

    def tree[A, I](vertices: A => Iterator[A], id: A => I) = CycleDetector[Iterator[A], A](
      identity, check((_, a) => vertices(a), id))

    def map[A, I](id: A => I) = CycleDetector[Map[A, A], A](
      _.keys.iterator, check(_.get(_).iterator, id))

    def multimap[V[_], A, I](id: A => I, empty: V[A])(implicit ev: V[A] <:< Iterable[A]) =
      CycleDetector[Map[A, V[A]], A](
        _.keys.iterator,
        check(_.getOrElse(_, empty).iterator, id))
  }
}
