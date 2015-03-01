package japgolly.nyaya

import scala.collection.GenTraversable
import scalaz.Equal

final case class EvalOver(input: Any) {

  def pass: EvalL =
    Eval.pass(input = this.input)

  def atom(name: => String, failure: FailureReasonO): EvalL =
    Eval.atom(name, input, failure)

  def test[A](name: => String, t: Boolean): EvalL =
    Eval.test(name, input, t)

  def equal[A: Equal](name: => String, actual: A, expect: A): EvalL =
    Eval.equal(name, input, actual, expect)

  def distinctC[A](name: => String, as: GenTraversable[A]): EvalL =
    Eval.distinctC(name, input, as)

  def distinct[A](name: => String, as: Stream[A]): EvalL =
    Eval.distinct(name, input, as)
}