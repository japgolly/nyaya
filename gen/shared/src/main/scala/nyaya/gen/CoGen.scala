package nyaya.gen

import java.util.UUID

/**
  * https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-Arbitrary.html#t:CoArbitrary
  *
  *
  */
final case class CoGen[-A](perturb: CoGen.Perturb[A]) extends AnyVal {

  @inline def apply(a: A): Gen[Unit] =
    perturb(a)

  def contramap[B](f: B => A): CoGen[B] =
    new CoGen(perturb compose f)

  def const(a: A): CoGen[Any] =
    contramap(_ => a)
}

object CoGen {

//  type Effect = VarySeed
//  case class VarySeed(value: Long) extends AnyVal {
//    def ++(b: VarySeed) = VarySeed((value - 23) * 31 + b.value.hashCode)
//  }

  type Perturb[-A] = A => Gen[Unit]

  def const(g: Gen[Unit]): CoGen[Any] =
    CoGen(_ => g)

  def variant[A](f: A => Long): CoGen[A] =
    CoGen(a => Gen.setSeedVariant(f(a)))

  implicit lazy val coGenUnit: CoGen[Unit] =
    const(Gen.unit)

  implicit lazy val coGenBoolean: CoGen[Boolean] =
    variant(b => if (b) 1 else 0)

  implicit lazy val coGenByte : CoGen[Byte ] = variant(_.toLong)
  implicit lazy val coGenChar : CoGen[Char ] = variant(_.toLong)
  implicit lazy val coGenShort: CoGen[Short] = variant(_.toLong)
  implicit      val coGenInt  : CoGen[Int  ] = variant(_.toLong)
  implicit lazy val coGenLong : CoGen[Long ] = variant(a => a)

  implicit lazy val coGenFloat: CoGen[Float] =
    variant(n => java.lang.Float.floatToIntBits(n).toLong)

  implicit lazy val coGenDouble: CoGen[Double] =
    variant(n => java.lang.Double.doubleToLongBits(n))

  implicit lazy val coGenString: CoGen[String] =
    variant(s => (s.length.toLong << 32) | s.hashCode.toLong)

  implicit lazy val coGenSymbol: CoGen[Symbol] =
    coGenString.contramap(_.name)

  implicit lazy val coGenUUID: CoGen[UUID] =
    variant(u => u.getLeastSignificantBits ^ u.getMostSignificantBits)

  implicit def coGenTuple2[A, B](implicit ca: CoGen[A], cb: CoGen[B]): CoGen[(A, B)] =
    CoGen(t => ca(t._1) >> cb(t._2))

  // TODO MORE

  // example
  implicit def genFn1[A, Z](implicit ca: CoGen[A], gz: Gen[Z]): Gen[A => Z] =
//    Gen(ctx => a => ca(a, gz).run(ctx))
    Gen(ctx => a => { ca(a).run(ctx); gz.run(ctx) })
  implicit def genFn2[A, B, Z](implicit ca: CoGen[A], cb: CoGen[B], gz: Gen[Z]): Gen[(A, B) => Z] =
//    Gen(ctx => (a, b) => ca(a, cb(b, gz)).run(ctx))
//    Gen(ctx => (a, b) => (ca(a) >> cb(b) >> gz).run(ctx))
    Gen(ctx => (a, b) => { ca(a).run(ctx); cb(b).run(ctx); gz.run(ctx) })

  implicit def coGenFn1[A, Z](implicit ga: Gen[A], cz: CoGen[Z]): CoGen[A => Z] =
    CoGen(f => Gen(ctx => {
      val a = ga.run(ctx)
      val z = f(z)
      cz(z).run(ctx)
    }))

  implicit def coGenOption[A](implicit ca: CoGen[A]): CoGen[Option[A]] =
    CoGen {
      case Some(a) => Gen.setSeedVariant(1) >> ca(a)
      case None    => Gen.setSeedVariant(0)
    }

  implicit def coGenEither[A, B](implicit ca: CoGen[A], cb: CoGen[B]): CoGen[Either[A, B]] =
    CoGen {
      case Left(a)  => Gen.setSeedVariant(0) >> ca(a)
      case Right(b) => Gen.setSeedVariant(1) >> cb(b)
    }

  implicit def coGenTraversable[C[x] <: Traversable[x], A](implicit ca: CoGen[A]): CoGen[C[A]] =
    CoGen(as =>
      Gen { ctx =>
//        var len = 0
        for (a <- as) {
          ca(a).run(ctx)
//          len += 1
        }
//        coGenInt(len).run(ctx)
      }
    )

}
