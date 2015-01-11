package japgolly.nyaya

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
}