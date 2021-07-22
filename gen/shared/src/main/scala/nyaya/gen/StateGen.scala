package nyaya.gen

import cats.data.{State, StateT}

object StateGen {

  def apply[S, A](f: S => Gen[(S, A)]): StateGen[S, A] =
    StateT(f)

  def genS[S](f: S => Gen[S]): StateGen[S, Unit] =
    apply(f(_).strengthR(()))

  def genA[S, A](f: S => Gen[A]): StateGen[S, A] =
    apply(s => f(s).strengthL(s))

  def state[S, A](s: State[S, A]): StateGen[S, A] =
    s.mapK(Gen.fromEval)

  def get[S]: StateGen[S, S] =
    StateT(s => Gen.pure((s, s)))

  def put[S](s: => S): StateGen[S, Unit] =
    StateT(_ => Gen.pure((s, ())))

  def putStrict[S](s: S): StateGen[S, Unit] =
    StateT(Function const Gen.pure((s, ())))

  def mod[S](f: S => S): StateGen[S, Unit] =
    StateT(s => Gen.pure((f(s), ())))

  def gets[S, A](f: S => A): StateGen[S, A] =
    StateT(s => Gen.pure((s, f(s))))

  def ret[S, A](a: => A): StateGen[S, A] =
    StateT(s => Gen.pure((s, a)))

  def retStrict[S, A](a: A): StateGen[S, A] =
    StateT(s => Gen.pure((s, a)))

  def tailrec[S, A](f: S => Gen[Either[S, (S, A)]]): StateGen[S, A] =
    StateGen(Gen.catsInstance.tailRecM(_)(f))
}
