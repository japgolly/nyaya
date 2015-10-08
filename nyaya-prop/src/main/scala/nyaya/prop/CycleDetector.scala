package nyaya.prop

import scala.annotation.tailrec
import scala.collection.GenTraversable
import scalaz.{\/-, -\/, \/}

final case class CycleFree[A](value: A)

case class CycleDetector[A, B](extract: A => Stream[B], check: (A, Stream[B]) => Option[(B, B)]) {

  @inline final def hasCycle(a: A): Boolean =
    findCycle(a).isDefined

  @inline final def noCycle(a: A): Boolean =
    findCycle(a).isEmpty

  final def findCycle(a: A): Option[(B, B)] =
    check(a, extract(a))

  def cycleFree(a: A): (B, B) \/ CycleFree[A] =
    findCycle(a).fold[(B, B) \/ CycleFree[A]](\/-(CycleFree(a)))(-\/.apply)

  def contramap[Z](f: Z => A) =
    new CycleDetector[Z, B](extract compose f, (z, b) => check(f(z), b))

  def noCycleProp(name: => String): Prop[A] =
    Prop.atom[A](name, findCycle(_).map{
      case (b1,b2) => s"Cycle detected: [$b1] → [$b2]"
    })
}

object CycleDetector {

  object Undirected extends GraphType {
    override def check[A, B, I](vertices: (A, B) => Stream[B], id: B => I): (A, Stream[B]) => Option[(B, B)] =
      (a, input) => {
        def loopo = loop _
        @tailrec
        def loop(prev: B, bs: Stream[B], is: Set[I]): Set[I] \/ (B, B) = {
          bs match {
            case Stream.Empty => -\/(is)
            case b #:: t =>
              val i = id(b)
              if (is contains i)
                \/-((prev, b))
              else
                loopo(b, vertices(a, b), is + i) match {
                  case -\/(is2)  => loop(prev, t, is2)
                  case r@ \/-(_) => r
                }
          }
        }
        input.map(b => loop(b, vertices(a, b), Set(id(b)))).map(_.toOption).find(_.isDefined).flatten
      }
  }

  object Directed extends GraphType {
    override def check[A, B, I](vertices: (A, B) => Stream[B], id: B => I): (A, Stream[B]) => Option[(B, B)] =
      (a, input) => {
        def loop(prev: B, bs: Stream[B], is: Set[I]): Option[(B, B)] = {
          bs.map(b => {
            val i = id(b)
            if (is contains i)
              Some((prev, b))
            else
              loop(b, vertices(a, b), is + i)
          }).find(_.isDefined).flatten
        }
        input.map(b => loop(b, vertices(a, b), Set(id(b)))).find(_.isDefined).flatten
      }
  }

  sealed abstract class GraphType {
    def check[A, B, I](vertices: (A, B) => Stream[B], id: B => I): (A, Stream[B]) => Option[(B, B)]

    def tree[A, I](vertices: A => Stream[A], id: A => I) = CycleDetector[Stream[A], A](
      identity, check((_, a) => vertices(a), id))

    def map[A, I](id: A => I) = CycleDetector[Map[A, A], A](
      _.keys.toStream, check(_.get(_).toStream, id))

    def multimap[V[_], A, I](id: A => I, empty: V[A])(implicit ev: V[A] <:< GenTraversable[A]) =
      CycleDetector[Map[A, V[A]], A](
        _.keys.toStream,
        check(_.getOrElse(_, empty).toStream, id))
  }
}
