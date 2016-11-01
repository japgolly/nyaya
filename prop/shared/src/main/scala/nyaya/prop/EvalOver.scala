package nyaya.prop

import scala.collection.GenTraversable
import scalaz.{\/, Foldable, Equal}

final case class EvalOver(input: Any) {

  def pass: EvalL =
    Eval.pass(input = this.input)

  def fail(name: => String, reason: String): EvalL =
    Eval.fail(name, reason, input)

  def atom(name: => String, failure: FailureReasonO): EvalL =
    Eval.atom(name, input, failure)

  def test[A](name: => String, t: Boolean): EvalL =
    Eval.test(name, input, t)

  def equal[A: Equal](name: => String, actual: A, expect: A): EvalL =
    Eval.equal(name, input, actual, expect)

  def either[A](name: => String, data: String \/ A)(f: A => EvalL): EvalL =
    Eval.either(name, input, data)(f)

  def forall[F[_]: Foldable, B](fb: F[B])(each: B => EvalL): EvalL =
    Eval.forall(input, fb)(each)

  def distinct[A](name: => String, as: GenTraversable[A]): EvalL =
    Eval.distinct(name, input, as)

  def distinctI[A](name: => String, as: Iterator[A]): EvalL =
    distinct(name, as.toList)

  /** Test that all Cs are on a whitelist. */
  def whitelist[B, C](name: => String, whitelist: Set[B], testData: => TraversableOnce[C])(implicit ev: C <:< B): EvalL =
    Eval.whitelist(name, input, whitelist, testData)

  /** Test that no Cs are on a blacklist. */
  def blacklist[B, C](name: => String, blacklist: Set[B], testData: => TraversableOnce[C])(implicit ev: C <:< B): EvalL =
    Eval.blacklist(name, input, blacklist, testData)

  /** Test that all Bs are present in Cs. */
  def allPresent[B, C](name: => String, required: Set[B], testData: => TraversableOnce[C])(implicit ev: B <:< C): EvalL =
    Eval.allPresent(name, input, required, testData)
}