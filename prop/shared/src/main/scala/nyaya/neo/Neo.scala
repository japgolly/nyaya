package nyaya.neo

import scalaz._
import Scalaz._

case class Fix[F[_]](unfix: F[Fix[F]])

object BLAH {

  type Algebra[F[_], A] = F[A] => A
  type CoAlgebra[F[_], A] = A => F[A]
  type AlgebraM[M[_], F[_], A] = F[A] => M[A]
  type CoAlgebraM[M[_], F[_], A] = A => M[F[A]]
  type CoFree[F[_], A] = (A, Fix[F])

  def cata[F[_] /*: Functor*/, A](alg: Algebra[F, A]): Fix[F] => A = ???
  def ana[F[_] /*: Functor*/, A](alg: CoAlgebra[F, A]): A => Fix[F] = ???
  def histo[F[_], A](a: F[Cofree[F, A]] => A): Fix[F] => A = ??? // Algebra[F[Cofree[F, ?]]] => A
  def prepro[F[_] /*: Functor*/, A](alg: Algebra[F, A], nt: F ~> F): Fix[F] => A = ???
  def zygo[F[_] /*: Functor*/, A, B](f: F[(A, B)] => A, alg: Algebra[F, B]): Fix[F] => A = ??? // Algebra[F[(?, B)], A]
  def para[F[_] /*: Functor*/, A](alg: F[(Fix[F], A)] => A): Fix[F] => A = ??? // Algebra[F[(Fix[F], ?)], A]

  def cataM_T[M[_], F[_], A](alg: AlgebraM[M, F, A])(implicit T: Traverse[F], M: Monad[M]): Fix[F] => M[A] =
    xf => {
      val fx: F[Fix[F]] = xf.unfix
      val mfa: M[F[A]] = T.traverse(fx)(cataM_T(alg)(T, M))
      val ma: M[A] = M.bind(mfa)(alg)
      ma
    }
  def cataM_D[M[_], F[_], A](alg: AlgebraM[M, F, A])(implicit D: Distributive[F], D2: Distributive[Fix], M: Monad[M]): Fix[F] => M[A] =
    xf => {
      val fx: F[Fix[F]] = xf.unfix
      // F Fix -> (Fix -> M A) -> M F A
      // F ← M   :: Dist -- rare
      // G ← F   :: Functor -- nope
      // A ← Fix
      // B ← A
      val mfa: M[F[A]] = D.distributeImpl(fx)(xf2 => cataM_D(alg)(D, D2, M)(xf2))
      val ma: M[A] = M.bind(mfa)(alg)
      ma
    }

  case class Name(value: String)
  case class Failure(value: String)
  // type Result = Option[Failure]

  trait LogicF[+N, +A, +R]
  case class Proposition[+N, +A](name: N, value: A) extends LogicF[N, A, Nothing]
  case class Conjunction[+N, +R](name: Option[N], lhs: R, rhs: R) extends LogicF[N, Nothing, R]


//  // Eval
//  case class EvalCtx(input: Any)
//  type EvalF[R] = Logic[OF, R]
//  type Eval = CoFree[EvalCtx, EvalF]
//
//  // Prop
//  type PropF[A, R] = Logic[A => OF, R]
//  type Prop[A] = Fix[PropF[A, ?]]


  // Prop A -> A -> Eval

  // Annotate leaves
  // input
  // time?
  // memoisation


  // EVAL

  // TREE OF {X,=,→,¬,∧,∨,↔}
  // TERMINALS (X) = OF

  // EVAL.RESULT ->
  // - TREE TRACED (OPTIONAL)
  // - OPTION FAILURE-TREE

  // FAILURE-TREE:
  // {NAME, FAILURE REASON} x CHILDREN.*

  // EVAL:
  // {LOGIC, FAILURE REASON}

  // TODO No input. Is that good?
  // case class EvalF[R](logic: LogicF[Option[Failure], R])
  type EvalF[R] = LogicF[Name, Option[Failure], R]
  type Eval = Fix[EvalF]

  val eval: Eval =
    Fix[EvalF](Conjunction(
      None,
      Fix[EvalF](Proposition(
        Name("a = b"), None
      )),
      Fix[EvalF](Proposition(
        Name("a ≠ b"), Some(Failure("3 = 3"))
      ))
    ))

  sealed trait FailureTreeF[+R] {
    val name: Name
  }

  object FailureTreeF {
    case class Terminal(name: Name, failure: Failure) extends FailureTreeF[Nothing]
    case class Branch[R](name: Name, head: R, tail: List[R]) extends FailureTreeF[R] {
      def nodes = head :: tail
    }
  }

  type FailureTree = Fix[FailureTreeF]
  val failureTree: FailureTree = {
    import FailureTreeF._
    Fix[FailureTreeF](Branch(
      Name("(a = b) & (a ≠ b)"),
      Fix[FailureTreeF](Terminal(
        Name("a ≠ b"),
        Failure("3 = 3")
      )),
      Nil
    ))
  }

  type EvalTrace = Cofree[EvalF, Option[Failure]]
  val evalTrace: EvalTrace = ???
  evalTrace.tail match {
    case Proposition(n, of) =>
    case Conjunction(n, a, b) => b: EvalTrace
  }

  def evalToFailureTreeCoalgrbra: CoAlgebra[FailureTreeF, EvalF[Eval]] = {
    case Proposition(n, Some(f)) => FailureTreeF.Terminal(n, f)
    case Proposition(n, None) => ???
  }

  // ===================================================================================================================

  type NameFn[A] = Option[A] => Name
  type PropF[A, R] = LogicF[NameFn[A], A => Option[Failure], R]
  type Prop[A] = Fix[({type L[R] = PropF[A, R]})#L]

  def fixP[A](p: PropF[A, Prop[A]]): Prop[A] = Fix[({type L[R] = PropF[A, R]})#L](p)

  val propI: Prop[Int] =
    fixP[Int](Conjunction(
      None,
      fixP[Int](Proposition[NameFn[Int], Int => Option[Failure]](
        _.fold(Name("a = 3"))(i => Name(s"$i = 3")),
        i => if (i == 3) None else Some(Failure(s"$i ≠ 3"))
      )),
      fixP[Int](Proposition[NameFn[Int], Int => Option[Failure]](
        _.fold(Name("a ≠ 3"))(i => Name(s"$i ≠ 3")),
        i => if (i != 3) None else Some(Failure(s"$i = 3"))
      ))
    ))

  def inspectPropAlg[A](oa: Option[A]): Algebra[({type L[R] = PropF[A, R]})#L, String] = {
    case Proposition(n, _) => n(oa).value
    case Conjunction(None, a, b) => s"($a & $b)"
    case Conjunction(Some(n), a, b) => n(oa).value
  }

  def propToEval[A](a: A): Prop[A] => Eval =
    p => ana(propToEval2(Some(a), a))(p.unfix)

  def propToEval2[A](sa: Some[A], a: A): CoAlgebra[EvalF, PropF[A, Prop[A]]] = {
    case Proposition(n, f) => Proposition(n(sa), f(a))
    case Conjunction(n, x, y) => Conjunction(n.map(_ apply sa), x.unfix, y.unfix)
  }

//  propToEval(1):
//    Fix[({type L[R] = LogicF[NameFn[Int], Int => Option[Failure], R] })#L] =>
//    Fix[({type L[R] = LogicF[Name, Option[Failure], R] })#L]

//  propToEval(1):
//    CoAlgebra[({type L[R] = LogicF[Name, Option[Failure], R] })#L, Prop[Int]]
//    Fix[({type L[R] = LogicF[NameFn[Int], Int => Option[Failure], R] })#L] =>
//    Fix[({type L[R] = LogicF[Name, Option[Failure], R] })#L]

}

