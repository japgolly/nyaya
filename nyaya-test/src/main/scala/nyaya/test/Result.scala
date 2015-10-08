package nyaya.test

import nyaya.prop.Eval

object Result {
  def apply[A](a: A, e: Eval): Result[A] =
    if (e.success)
      Satisfied
    else
      Falsified(a, e)
}

sealed abstract class Result[+A] {
  final def success: Boolean = this match {
    case Satisfied | Proved            => true
    case Falsified(_, _) | Error(_, _) => false
  }
}

case object      Satisfied                                extends Result[Nothing]
case object      Proved                                   extends Result[Nothing]
final case class Falsified[A](a: A, f: Eval)              extends Result[A]
final case class Error    [A](a: Option[A], e: Throwable) extends Result[A]
