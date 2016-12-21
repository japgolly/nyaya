package nyaya.neo

import scalaz.{Name => _, _}
import Scalaz._
import japgolly.microlibs.recursion._
import japgolly.microlibs.name_fn._
import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._

object Neo2 {
/*
assertion
Eval.equal(a,3) && Eval.containsAll(a, Set(2,3,4))

shared props
Prop[A].equal(3) && Prop.containsAll[A](Set(2,3,4))

prop test (old)
class Test(a: A) {
  Eval.equal(a,3) && Eval.containsAll(a, Set(2,3,4))
}

prop test (new)
forAll[A](a =>
  a == 3 && Eval.containsAll(a, Set(2,3,4))
)


Eval has Name
Prop (unevaluated) has Name

Both have logic ops

Prop = A => Eval

Eval => trace tree
Eval => failure tree

Eval should be called...
* Evaluation
* Result
* Assertion
* Check
* Test
 */

  // TODO Maybe a good idea to make negative lazy later?
  // Or maybe both sides should just be scalaz.Need[A]
  case class Polar[+A](negative: A, positive: A) {
    def swap: Polar[A] = Polar(positive, negative)
    def map[B](f: A => B): Polar[B] = Polar(f(negative), f(positive))
  }
  object Polar {
    implicit val scalazInstance: Distributive[Polar] with Traverse[Polar] =
      new Distributive[Polar] with Traverse[Polar] {
        override def map[A, B](fa: Polar[A])(f: A => B): Polar[B] =
          fa map f
        override def traverseImpl[G[_], A, B](fa: Polar[A])(f: A => G[B])(implicit G: Applicative[G]): G[Polar[B]] =
          G.apply2(f(fa.negative), f(fa.positive))(Polar.apply)
        override def distributeImpl[G[_], A, B](ga: G[A])(f: A => Polar[B])(implicit G: Functor[G]): Polar[G[B]] =
          Polar[G[B]](ga.map(f(_).negative), ga.map(f(_).positive))
      }
    def cosequence[F[_]: Functor, A](fpa: F[Polar[A]]): Polar[F[A]] =
      fpa.distribute(x => x)

    def pass[A](a: A): Polar[Option[A]] = apply(None, Some(a))
    def fail[A](a: A): Polar[Option[A]] = apply(Some(a), None)
  }

  // TODO Maybe Eval should just have a Polar[() => Option[FailureTree]]

  /**
    * @tparam A Result
    * @tparam R Recursive case
    */
  sealed trait LogicF[+A, +R]
  object LogicF {
    case class Value[+A](value: A) extends LogicF[A, Nothing]
    sealed abstract class Recursive[+R] extends LogicF[Nothing, R]
    case class Conjunction[+R](a: R, b: R, c: List[R]) extends Recursive[R] {
      def conjuncts: List[R] = a :: b :: c
    }
    case class Disjunction[+R](a: R, b: R, c: List[R]) extends Recursive[R] {
      def disjuncts: List[R] = a :: b :: c
    }
    case class Negation[+R](a: R) extends Recursive[R]
    case class Implication[+R](a: R, b: R) extends Recursive[R]
    case class Biconditional[+R](a: R, b: R) extends Recursive[R]

    def traverseA[R]: Traverse[LogicF[?, R]] =
      new Traverse[LogicF[?, R]] {
        override def map[A, B](fa: LogicF[A, R])(f: A => B): LogicF[B, R] =
          ???
        override def traverseImpl[G[_], A, B](fa: LogicF[A, R])(f: A => G[B])(implicit G: Applicative[G]): G[LogicF[B, R]] =
          ???
      }

    def traverseR[A]: Traverse[LogicF[A, ?]] =
      new Traverse[LogicF[A, ?]] {
        override def map[X, Y](fa: LogicF[A, X])(f: X => Y): LogicF[A, Y] =
          ???
        override def traverseImpl[G[_], X, Y](fa: LogicF[A, X])(f: X => G[Y])(implicit G: Applicative[G]): G[LogicF[A, Y]] =
          ???
      }
  }

  @inline implicit class Xxxxxx[A, B](private val self: LogicF[A, Fix[LogicF[B, ?]]]) extends AnyVal {
    def flatMapLike(f: A => Fix[LogicF[B, ?]]): Fix[LogicF[B, ?]] =
      self match {
        case LogicF.Value(a) => f(a)
        case x: LogicF.Recursive[Fix[LogicF[B, ?]]] => Fix[LogicF[B, ?]](x)
      }
  }


//  case class Show[A](show: A => String) extends AnyVal
//  trait EvalInput { // TODO This seems wrong. Every Eval has an input?
//    type I
//    val input: I
//    val show: Show[I]
//  }
  // TODO In prop tests we need to show the random value used in failure
  case class Failure(reason: String)
  case class EvalN(name: Name, result: Polar[Option[Failure]])
  type EvalF[+R] = LogicF[EvalN, R]
  type Eval = Fix[EvalF]

  implicit val traverseEvalF: Traverse[EvalF] =
    LogicF.traverseR[EvalN]

  case class PropN[-A](nameFn: NameFn[A], prop: A => Eval)
  type PropF[A, +R] = LogicF[PropN[A], R]
  type Prop[A] = Fix[PropF[A, ?]]

  def runProp[A](p: Prop[A])(a: A): Eval = {
    implicit val traverse = LogicF.traverseR[PropN[A]]
    val alg: Algebra[PropF[A, ?], Eval] = _.flatMapLike(_.prop(a))
    Recursion.cata[PropF[A, ?], Eval](alg)(p)
  }

  // ===================================================================================================================

  type TraceTree = Eval

  sealed trait FailureTreeF[+R]
  object FailureTreeF {
    case class Leaf(failure: Failure) extends FailureTreeF[Nothing]
    case class Branch[+R](children: NonEmptyVector[R]) extends FailureTreeF[R]
  }
  type FailureTree = Fix[FailureTreeF]
  object FailureTree {
    def leaf(f: Failure): FailureTree = Fix[FailureTreeF](FailureTreeF.Leaf(f))
    def branch(f: NonEmptyVector[Fix[FailureTreeF]]): FailureTree = Fix[FailureTreeF](FailureTreeF.Branch(f))
    @inline def none: Option[FailureTree] = None
  }

  def traceToFailureTree(eval: Eval): Option[FailureTree] =
    Recursion.cata(traceToFailureTreeAlg)(eval).positive

  val traceToFailureTreeAlg: Algebra[EvalF, Polar[Option[FailureTree]]] = {

    case LogicF.Value(EvalN(_, r)) => r.map(_ map FailureTree.leaf)

    case LogicF.Negation(x) => x.swap

    case l: LogicF.Conjunction[Polar[Option[FailureTree]]] =>
      Polar.cosequence(l.conjuncts).map { ofailures =>
        val failures = ofailures.toIterator.filterDefined.toVector
        NonEmptyVector.maybe(failures, FailureTree.none)(ft => Some(FailureTree.branch(ft)))
      }
      // Maybe better to have:
      // Polar(exists(...), forall(...))

    case l: LogicF.Disjunction[Polar[Option[FailureTree]]] =>
      Polar.cosequence(l.disjuncts).map { ofailures =>
        val failures = ofailures.toIterator.filterDefined.toVector
        // TODO inefficient
        if (failures.length == ofailures.size)
          Some(FailureTree.branch(NonEmptyVector.force(failures)))
        else
          None
      }
    // Maybe better to have:
    // Polar(forall(...), exists(...))

    case LogicF.Implication(p, q) =>
      Polar[Option[FailureTree]](
        ???,
        (p.positive, q.positive) match {
          case (None, f@ Some(_)) => f
          case _ => None
        }
      )
  }

  val failureTreePrinter: Algebra[FailureTreeF, String] = {
    case FailureTreeF.Leaf(f)    => f.reason
    case FailureTreeF.Branch(fs) => fs.iterator.map(_.replace("\n", "\n  ")).mkString("\n")
  }

}
