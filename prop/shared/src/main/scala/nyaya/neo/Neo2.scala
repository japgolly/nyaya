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

  sealed trait LogicF[+A, +R]
  object LogicF {
    case class Value[+A](value: A) extends LogicF[A, Nothing]
    case class Conjunction[+R](a: R, b: R, c: List[R]) extends LogicF[Nothing, R]
    case class Disjunction[+R](a: R, b: R, c: List[R]) extends LogicF[Nothing, R]
    case class Negation[+R](a: R) extends LogicF[Nothing, R]
    case class Implication[+R](a: R, b: R) extends LogicF[Nothing, R]
    case class Biconditional[+R](a: R, b: R) extends LogicF[Nothing, R]
  }

  case class Show[A](show: A => String) extends AnyVal
  trait Failure {
    type I
    val input: I
    val show: Show[I]
    val failure: String
  }
  case class EvalN(name: Name, failure: Option[Failure])
  type EvalF[+R] = LogicF[EvalN, R]
  type Eval = Fix[EvalF]

  case class PropN[-A](name: Name, prop: A => Eval)
  type PropF[A, +R] = LogicF[PropN[A], R]
  type Prop[A] = Fix[PropF[A, ?]]


  type TraceTree = Eval

//  type ConclusionB = Boolean
//  type ConclusionFirstFailure = Option[Failure]
//  type ConclusionFailures = List[Failure]

  sealed trait FailureTreeF[+R]
  object FailureTreeF {
    case class Leaf(failure: Failure) extends FailureTreeF[Nothing]
    case class Branch[+R](children: NonEmptyVector[R]) extends FailureTreeF[R]
  }
  type FailureTree = Fix[FailureTreeF]

  // TODO metamorphism
  // prune branches

  def traceToFailureTree: Algebra[EvalF, Option[FailureTree]] = {
    case LogicF.Value(EvalN(_, None)) => None
    case LogicF.Value(EvalN(n, Some(f))) => Some(Fix[FailureTreeF](FailureTreeF.Leaf(f)))
  }

  // Algebra[EvalF, Option FT]

  // n, f           => S leaf(n, f)
  // n, (Sf1 & Sf2) => S branch(n, Sf1 + Sf2)
  // n, (Sf1 & Nf2) => S branch(n, Sf1)
  // n, (Nf1 & Sf2) => S branch(n, Sf2)
  // _, (Nf1 & Nf2) => N

}
