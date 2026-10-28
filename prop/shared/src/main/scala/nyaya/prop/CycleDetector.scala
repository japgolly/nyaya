package nyaya.prop

import scala.annotation.tailrec
import scala.collection.mutable
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

        def outer(prev: B, parent0: Option[I], is0: Set[I]): Either[Set[I], (B, B)] = {
          val it = vertices(a, prev)

          @tailrec
          def inner(parent: Option[I], is: Set[I]): Either[Set[I], (B, B)] =
            if (it.hasNext) {
              val b = it.next()
              val i = id(b)
              if (parent.contains(i))
                inner(None, is)
              else if (is contains i)
                Right((prev, b))
              else
                outer(b, Some(id(prev)), is + i) match {
                  case Left(is2)   => inner(parent, is2)
                  case r@ Right(_) => r
                }
            } else
              Left(is)

          inner(parent0, is0)
        }

        @tailrec
        def loop(is: Set[I]): Option[(B, B)] =
          if (input.hasNext) {
            val b = input.next()
            val i = id(b)
            if (is contains i)
              loop(is)
            else
              outer(b, None, is + i) match {
                case Left(is2)  => loop(is2)
                case Right(res) => Some(res)
              }
          } else
            None

        loop(Set.empty)
      }
  }

  object Directed extends GraphType {
    override def check[A, B, I](vertices: (A, B) => Iterator[B], id: B => I): (A, Iterator[B]) => Option[(B, B)] =
      (a, input) => {
        // Memoises explored subgraphs: maps node id -> set of all descendant ids reachable from it.
        // If a previously explored node's reachable set does not intersect the current DFS path,
        // it cannot reach any ancestor in `path` and re-exploration can be skipped in O(1).
        val done        = mutable.HashMap.empty[I, Set[I]]
        val hasChildren = mutable.HashSet.empty[I]

        def dfs(prev: B, path: Set[I]): Either[Set[I], (B, B)] = {
          val it = vertices(a, prev)
          val prevId = id(prev)

          @tailrec
          def inner(reachable: Set[I]): Either[Set[I], (B, B)] =
            if (it.hasNext) {
              val b = it.next()
              val i = id(b)
              if (path.contains(i))
                // Back-edge detected pointing to an ancestor on the current DFS path
                Right((prev, b))
              else if (done.contains(i) && !done(i).exists(path.contains) && (hasChildren.contains(i) || !vertices(a, b).hasNext))
                // Subgraph at `i` was already explored and verified to have no path back to `path`.
                // Prune traversal and reuse its memoised reachable set.
                inner(reachable ++ done(i))
              else
                dfs(b, path + i) match {
                  case r @ Right(_) => r
                  case Left(bReachable) =>
                    inner(reachable ++ bReachable)
                }
            } else {
              done.put(prevId, reachable)
              Left(reachable)
            }

          if (it.hasNext)
            hasChildren += prevId

          inner(Set(prevId))
        }

        @tailrec
        def loop(): Option[(B, B)] =
          if (input.hasNext) {
            val b = input.next()
            val i = id(b)
            // Skip roots whose subgraphs were already fully traversed
            if (done.contains(i) && (hasChildren.contains(i) || !vertices(a, b).hasNext))
              loop()
            else
              dfs(b, Set(i)) match {
                case Right(cycle) => Some(cycle)
                case Left(_)      => loop()
              }
          } else
            None

        loop()
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
