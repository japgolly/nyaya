package nyaya

import cats.Foldable
import scala.annotation.elidable

package object prop {

  case class Input(a: Any) {
    def show: String = a.toString
  }

  type Name = cats.Eval[String]

  type FailureReason  = String
  type FailureReasonO = Option[FailureReason]

  type Eval_[x] = Eval
  type EvalL    = Logic[Eval_, Nothing]

  type Prop[A] = Logic[PropA, A]

  implicit class Prop_AnyExt[A](private val a: A) extends AnyVal {

    @elidable(elidable.ASSERTION)
    def assertSatisfies(p: Prop[A]): Unit = p.assert(a)
  }

  implicit class LogicPropExt[A](private val prop: Prop[A]) extends AnyVal {
    @inline def apply(a: A): Eval =
      Prop.run(prop)(a)

    @inline def forall[B, F[_] : Foldable](f: B => F[A]): Prop[B] =
      Prop.forall(f)(_ => prop)

    @inline def forallS[B, F[_] : Foldable, C](f: B => F[C])(implicit ev: C <:< A): Prop[B] =
      Prop.forallS(f)(_ => prop)

    @inline def forallF[F[_] : Foldable]: Prop[F[A]] =
      forall(j => j)

    @elidable(elidable.ASSERTION)
    @inline def assert(a: A): Unit =
      Prop.assert(prop)(a)
  }
}
