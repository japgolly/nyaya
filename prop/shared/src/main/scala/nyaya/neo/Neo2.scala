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

  /**
    * @tparam A Result
    * @tparam R Recursive case
    */
  sealed trait LogicF[+A, +R]
  object LogicF {
    case class Value[+A](value: A) extends LogicF[A, Nothing]
    case class Conjunction[+R](a: R, b: R, c: List[R]) extends LogicF[Nothing, R] {
      def conjuncts: List[R] = a :: b :: c
    }
    case class Disjunction[+R](a: R, b: R, c: List[R]) extends LogicF[Nothing, R] {
      def disjuncts: List[R] = a :: b :: c
    }
    case class Negation[+R](a: R) extends LogicF[Nothing, R]
    case class Implication[+R](a: R, b: R) extends LogicF[Nothing, R]
    case class Biconditional[+R](a: R, b: R) extends LogicF[Nothing, R]
  }

//  case class Show[A](show: A => String) extends AnyVal
//  trait EvalInput { // TODO This seems wrong. Every Eval has an input?
//    type I
//    val input: I
//    val show: Show[I]
//  }
  // TODO In prop tests we need to show the random value used in failure
  case class Failure(reason: String)
  case class EvalN(name: Name, failure: Option[Failure])
  type EvalF[+R] = LogicF[EvalN, R]
  type Eval = Fix[EvalF]

  case class PropN[-A](nameFn: NameFn[A], prop: A => Eval)
  type PropF[A, +R] = LogicF[PropN[A], R]
  type Prop[A] = Fix[PropF[A, ?]]

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
  }

  // TODO metamorphism
  // prune branches

  def traceToFailureTree: Algebra[EvalF, Option[FailureTree]] = {
    case LogicF.Value(EvalN(_, None)) => None
    case LogicF.Value(EvalN(n, Some(f))) => Some(FailureTree.leaf(f))
    case LogicF.Negation(Some(ft)) => None
//    case LogicF.Negation(None) => Some(FailureTree.leaf( ?????? ))

    case l: LogicF.Conjunction[Option[FailureTree]] =>
      NonEmptyVector.option(l.conjuncts.toIterator.filterDefined.toVector)
        .map(FailureTree.branch)

    // TODO Biconditional(_, _), Disjunction(_, _, _), Implication(_, _)
  }

  // Algebra[EvalF, Option FT]

  // n, f           => S leaf(n, f)
  // n, (Sf1 & Sf2) => S branch(n, Sf1 + Sf2)
  // n, (Sf1 & Nf2) => S branch(n, Sf1)
  // n, (Nf1 & Sf2) => S branch(n, Sf2)
  // _, (Nf1 & Nf2) => N

}
