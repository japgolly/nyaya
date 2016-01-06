package nyaya.test

import nyaya.prop.Eval

sealed abstract class Result[+A] {
  def success: Boolean
}

object Result {
  def apply[A](a: A, e: Eval): Result[A] =
    if (e.success)
      Satisfied
    else
      Falsified(a, e)

  sealed abstract class Success extends Result[Nothing] {
    final override def success = true
  }

  case object Satisfied extends Success

  case object Proved extends Success

  sealed abstract class Failure[+A] extends Result[A] {
    final override def success = false
  }

  final case class Falsified[+A](a: A, eval: Eval) extends Failure[A]

  final case class Error[+A](a: Option[A], error: Throwable) extends Failure[A]
}

