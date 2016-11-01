package nyaya

import scalaz.StateT

package object gen {

  type StateGen[S, A] = StateT[Gen, S, A]

  @inline implicit def GenOpsWithInvariantA[A](g: Gen[A]): GenOpsWithInvariantA[A] =
    new GenOpsWithInvariantA(g.run)

  final class GenOpsWithInvariantA[A](private val run: Gen.Run[A]) extends AnyVal {
    @inline private def gen = Gen(run)

    def toStateGen[S]: StateGen[S, A] =
      StateT(gen.strengthL)

    /**
      * Discard existing state and replace with the result of this generator.
      */
    def statePut: StateGen[A, Unit] =
      StateT(Function const gen.strengthR(()))
  }
}