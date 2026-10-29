package nyaya.gen

import cats.{ContravariantMonoidal, MonoidK}
import java.time._
import java.util.{Date, UUID}
import java.util.concurrent.TimeUnit
import scala.concurrent.{duration => scd}
import scala.util.{Failure, Success, Try}

final case class CoGen[-A](perturb: (Long, A) => Long) extends AnyVal {

  def contramap[B](f: B => A): CoGen[B] =
    CoGen((seed, b) => perturb(seed, f(b)))
}

object CoGen {

  def conquer[A]: CoGen[A] =
    apply((s, _) => s)

  def seedMix(seed: Long, value: Long) = {
    val z0 = seed + value + 0x9e3779b97f4a7c15L
    var z = (z0 ^ (z0 >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)
  }

  implicit val long: CoGen[Long] =
    apply(seedMix)

  implicit val int: CoGen[Int] =
    long.contramap(_.toLong)

  implicit val short: CoGen[Short] =
    long.contramap(_.toLong)

  implicit val byte: CoGen[Byte] =
    long.contramap(_.toLong)

  implicit val char: CoGen[Char] =
    long.contramap(_.toLong)

  implicit val boolean: CoGen[Boolean] =
    long.contramap(b => if (b) 1L else 0L)

  implicit val unit: CoGen[Unit] =
    conquer

  implicit val double: CoGen[Double] =
    long.contramap(java.lang.Double.doubleToRawLongBits)

  implicit val float: CoGen[Float] =
    long.contramap(java.lang.Float.floatToRawIntBits(_).toLong)

  implicit def iteratorFn[A](implicit A: CoGen[A]): CoGen[() => Iterator[A]] =
    CoGen { (s0, f) =>
      val it = f()
      var s = s0
      var count = 0
      while (it.hasNext) {
        s = A.perturb(s, it.next())
        count += 1
      }
      s = seedMix(s, count.toLong)
      s
    }

  implicit def iterable[A](implicit A: CoGen[A]): CoGen[Iterable[A]] =
    iteratorFn[A].contramap(i => () => i.iterator)

  implicit def array[A](implicit A: CoGen[A]): CoGen[Array[A]] =
    iteratorFn[A].contramap(a => () => a.iterator)

  implicit val string: CoGen[String] =
    iteratorFn[Char].contramap(s => () => s.iterator)

  implicit val throwable: CoGen[Throwable] =
    string.contramap(_.toString)

  implicit def option[A](implicit A: CoGen[A]): CoGen[Option[A]] =
    CoGen { (s, o) =>
      o match {
        case Some(a) => A.perturb(seedMix(s, 2), a)
        case None    => seedMix(s, 1)
      }
    }

  implicit def either[A, B](implicit A: CoGen[A], B: CoGen[B]): CoGen[Either[A, B]] =
    CoGen { (s, e) =>
      e match {
        case Right(b) => B.perturb(seedMix(s, 2), b)
        case Left(a)  => A.perturb(seedMix(s, 1), a)
      }
    }

  implicit def scalaUtilTry[A](implicit A: CoGen[A]): CoGen[Try[A]] =
    CoGen { (s, t) =>
      t match {
        case Success(a) => A.perturb(seedMix(s, 2), a)
        case Failure(e) => throwable.perturb(seedMix(s, 1), e)
      }
    }

  implicit lazy val uuid: CoGen[UUID] =
    divide2((u: UUID) => (u.getLeastSignificantBits, u.getMostSignificantBits))

  // Java dates and times
  implicit lazy val date          : CoGen[Date]           = long.contramap(_.getTime)
  implicit lazy val dayOfWeek     : CoGen[DayOfWeek]      = int.contramap(_.getValue)
  implicit lazy val duration      : CoGen[Duration]       = divide2((d: Duration) => (d.getSeconds, d.getNano))
  implicit lazy val instant       : CoGen[Instant]        = divide2((i: Instant) => (i.getEpochSecond, i.getNano))
  implicit lazy val localDate     : CoGen[LocalDate]      = long.contramap(_.toEpochDay)
  implicit lazy val localDateTime : CoGen[LocalDateTime]  = divide2((d: LocalDateTime) => (d.toLocalDate, d.toLocalTime))
  implicit lazy val localTime     : CoGen[LocalTime]      = long.contramap(_.toNanoOfDay)
  implicit lazy val month         : CoGen[Month]          = int.contramap(_.getValue)
  implicit lazy val monthDay      : CoGen[MonthDay]       = divide2((m: MonthDay) => (m.getMonthValue, m.getDayOfMonth))
  implicit lazy val offsetDateTime: CoGen[OffsetDateTime] = divide2((d: OffsetDateTime) => (d.toLocalDateTime, d.getOffset))
  implicit lazy val offsetTime    : CoGen[OffsetTime]     = divide2((t: OffsetTime) => (t.toLocalTime, t.getOffset))
  implicit lazy val period        : CoGen[Period]         = divide3((p: Period) => (p.getYears, p.getMonths, p.getDays))
  implicit lazy val timeUnit      : CoGen[TimeUnit]       = int.contramap(_.ordinal)
  implicit lazy val year          : CoGen[Year]           = int.contramap(_.getValue)
  implicit lazy val yearMonth     : CoGen[YearMonth]      = divide2((m: YearMonth) => (m.getYear, m.getMonthValue))
  implicit lazy val zonedDateTime : CoGen[ZonedDateTime]  = divide2((d: ZonedDateTime) => (d.toInstant, d.getZone))
  implicit lazy val zoneId        : CoGen[ZoneId]         = string.contramap(_.getId)
  implicit lazy val zoneOffset    : CoGen[ZoneOffset]     = int.contramap(_.getTotalSeconds)

  // Scala durations
  implicit lazy val deadline      : CoGen[scd.Deadline]       = finiteDuration.contramap(_.time)
  implicit lazy val finiteDuration: CoGen[scd.FiniteDuration] = long.contramap(_.toNanos)
  implicit lazy val scalaDuration : CoGen[scd.Duration]       =
    CoGen { (s, d) =>
      d match {
        case fd: scd.FiniteDuration => finiteDuration.perturb(s, fd)
        case scd.Duration.Inf       => seedMix(s, 1)
        case scd.Duration.MinusInf  => seedMix(s, 2)
        case _                      => seedMix(s, 3)
      }
    }

  // ------------------------------------------------------
  // Arity boilerplate
  // ------------------------------------------------------

  implicit def tuple2[A, B](implicit A: CoGen[A], B: CoGen[B]): CoGen[(A, B)] = divide2(identity)
  implicit def tuple3[A, B, C](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C]): CoGen[(A, B, C)] = divide3(identity)
  implicit def tuple4[A, B, C, D](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D]): CoGen[(A, B, C, D)] = divide4(identity)
  implicit def tuple5[A, B, C, D, E](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E]): CoGen[(A, B, C, D, E)] = divide5(identity)
  implicit def tuple6[A, B, C, D, E, F](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F]): CoGen[(A, B, C, D, E, F)] = divide6(identity)
  implicit def tuple7[A, B, C, D, E, F, G](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G]): CoGen[(A, B, C, D, E, F, G)] = divide7(identity)
  implicit def tuple8[A, B, C, D, E, F, G, H](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H]): CoGen[(A, B, C, D, E, F, G, H)] = divide8(identity)
  implicit def tuple9[A, B, C, D, E, F, G, H, I](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I]): CoGen[(A, B, C, D, E, F, G, H, I)] = divide9(identity)
  implicit def tuple10[A, B, C, D, E, F, G, H, I, J](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J]): CoGen[(A, B, C, D, E, F, G, H, I, J)] = divide10(identity)
  implicit def tuple11[A, B, C, D, E, F, G, H, I, J, K](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K]): CoGen[(A, B, C, D, E, F, G, H, I, J, K)] = divide11(identity)
  implicit def tuple12[A, B, C, D, E, F, G, H, I, J, K, L](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L)] = divide12(identity)
  implicit def tuple13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = divide13(identity)
  implicit def tuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = divide14(identity)
  implicit def tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = divide15(identity)
  implicit def tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = divide16(identity)
  implicit def tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = divide17(identity)
  implicit def tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = divide18(identity)
  implicit def tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R], S: CoGen[S]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = divide19(identity)
  implicit def tuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R], S: CoGen[S], T: CoGen[T]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = divide20(identity)
  implicit def tuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R], S: CoGen[S], T: CoGen[T], U: CoGen[U]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = divide21(identity)
  implicit def tuple22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R], S: CoGen[S], T: CoGen[T], U: CoGen[U], V: CoGen[V]): CoGen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = divide22(identity)

  def divide2[A, B, Z](fz: Z => (A, B))(implicit A: CoGen[A], B: CoGen[B]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b) = fz(z)
      B.perturb(A.perturb(s0, a), b)
    }

  def divide3[A, B, C, Z](fz: Z => (A, B, C))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c) = fz(z)
      C.perturb(B.perturb(A.perturb(s0, a), b), c)
    }

  def divide4[A, B, C, D, Z](fz: Z => (A, B, C, D))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d) = fz(z)
      D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d)
    }

  def divide5[A, B, C, D, E, Z](fz: Z => (A, B, C, D, E))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e) = fz(z)
      E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e)
    }

  def divide6[A, B, C, D, E, F, Z](fz: Z => (A, B, C, D, E, F))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f) = fz(z)
      F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f)
    }

  def divide7[A, B, C, D, E, F, G, Z](fz: Z => (A, B, C, D, E, F, G))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g) = fz(z)
      G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g)
    }

  def divide8[A, B, C, D, E, F, G, H, Z](fz: Z => (A, B, C, D, E, F, G, H))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h) = fz(z)
      H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h)
    }

  def divide9[A, B, C, D, E, F, G, H, I, Z](fz: Z => (A, B, C, D, E, F, G, H, I))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i) = fz(z)
      I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i)
    }

  def divide10[A, B, C, D, E, F, G, H, I, J, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j) = fz(z)
      J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j)
    }

  def divide11[A, B, C, D, E, F, G, H, I, J, K, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k) = fz(z)
      K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k)
    }

  def divide12[A, B, C, D, E, F, G, H, I, J, K, L, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l) = fz(z)
      L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l)
    }

  def divide13[A, B, C, D, E, F, G, H, I, J, K, L, M, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m) = fz(z)
      M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m)
    }

  def divide14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = fz(z)
      N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n)
    }

  def divide15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = fz(z)
      O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o)
    }

  def divide16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = fz(z)
      P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
    }

  def divide17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = fz(z)
      Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q)
    }

  def divide18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = fz(z)
      R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r)
    }

  def divide19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R], S: CoGen[S]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = fz(z)
      S.perturb(R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s)
    }

  def divide20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R], S: CoGen[S], T: CoGen[T]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = fz(z)
      T.perturb(S.perturb(R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t)
    }

  def divide21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R], S: CoGen[S], T: CoGen[T], U: CoGen[U]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = fz(z)
      U.perturb(T.perturb(S.perturb(R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u)
    }

  def divide22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))(implicit A: CoGen[A], B: CoGen[B], C: CoGen[C], D: CoGen[D], E: CoGen[E], F: CoGen[F], G: CoGen[G], H: CoGen[H], I: CoGen[I], J: CoGen[J], K: CoGen[K], L: CoGen[L], M: CoGen[M], N: CoGen[N], O: CoGen[O], P: CoGen[P], Q: CoGen[Q], R: CoGen[R], S: CoGen[S], T: CoGen[T], U: CoGen[U], V: CoGen[V]): CoGen[Z] =
    CoGen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = fz(z)
      V.perturb(U.perturb(T.perturb(S.perturb(R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v)
    }

  // ------------------------------------------------------
  // Cats stuff
  // ------------------------------------------------------

  implicit object catsInstance extends ContravariantMonoidal[CoGen] with MonoidK[CoGen] {
    override def contramap[A, B](fa: CoGen[A])(f: B => A): CoGen[B] =
      fa.contramap(f)

    override def product[A, B](fa: CoGen[A], fb: CoGen[B]): CoGen[(A, B)] =
      CoGen.tuple2(fa, fb)

    override val unit: CoGen[Unit] =
      CoGen.unit

    override def empty[A]: CoGen[A] =
      conquer[A]

    override def combineK[A](x: CoGen[A], y: CoGen[A]): CoGen[A] =
      CoGen((s, a) => y.perturb(x.perturb(s, a), a))
  }
}
