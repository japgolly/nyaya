package japgolly

import scala.annotation.elidable
import scalaz.{Need, Foldable}

package object nyaya {

  case class Input(a: Any) {
    def show: String = a.toString
  }

  type Name = Need[String]

  type FailureReason  = String
  type FailureReasonO = Option[FailureReason]

  type Eval_[x] = Eval
  type EvalL    = Logic[Eval_, Nothing]

  type Prop[A] = Logic[PropA, A]

  implicit class Prop_AnyExt[A](val _a: A) extends AnyVal {

    @elidable(elidable.ASSERTION)
    def assertSatisfies(p: Prop[A]): Unit = p.assert(_a)
  }

  implicit class LogicPropExt[A](val _l: Prop[A]) extends AnyVal {
    @inline def apply                     (a: A)        : Eval       = Prop.run(_l)(a)
    @inline def ∀      [B, F[_]: Foldable](f: B => F[A]): Prop[B]    = forall(f)
    @inline def forall [B, F[_]: Foldable](f: B => F[A]): Prop[B]    = Prop.forall(f, _l)
    @inline def forallF[F[_]: Foldable]                 : Prop[F[A]] = forall(j => j)

    @elidable(elidable.ASSERTION)
    @inline def assert(a: A): Unit = Prop.assert(_l)(a)
  }
}
