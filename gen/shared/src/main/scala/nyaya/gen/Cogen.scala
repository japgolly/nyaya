package nyaya.gen

import cats.{ContravariantMonoidal, MonoidK}
import cats.data._
import java.math.{BigDecimal => JBigDecimal, BigInteger => JBigInteger}
import java.nio.charset.Charset
import java.time._
import java.util.{Currency, Date, Locale, Optional, OptionalDouble, OptionalInt, OptionalLong, UUID}
import java.util.concurrent.TimeUnit
import java.util.regex.Pattern
import scala.collection.immutable.BitSet
import scala.concurrent.{duration => scd}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

final case class Cogen[-A](perturb: (Long, A) => Long) extends AnyVal {

  def contramap[B](f: B => A): Cogen[B] =
    Cogen((seed, b) => perturb(seed, f(b)))
}

object Cogen {

  def conquer[A]: Cogen[A] =
    apply((s, _) => s)

  def seedMix(seed: Long, value: Long) = {
    val z0 = seed + value + 0x9e3779b97f4a7c15L
    var z = (z0 ^ (z0 >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)
  }

  implicit val long: Cogen[Long] =
    apply(seedMix)

  implicit val int: Cogen[Int] =
    long.contramap(_.toLong)

  implicit val short: Cogen[Short] =
    long.contramap(_.toLong)

  implicit val byte: Cogen[Byte] =
    long.contramap(_.toLong)

  implicit val char: Cogen[Char] =
    long.contramap(_.toLong)

  implicit val boolean: Cogen[Boolean] =
    long.contramap(b => if (b) 1L else 0L)

  implicit val unit: Cogen[Unit] =
    conquer

  implicit val double: Cogen[Double] =
    long.contramap(java.lang.Double.doubleToRawLongBits)

  implicit val float: Cogen[Float] =
    long.contramap(java.lang.Float.floatToRawIntBits(_).toLong)

  implicit def iteratorFn[A](implicit A: Cogen[A]): Cogen[() => Iterator[A]] =
    Cogen { (s0, f) =>
      val it = f()
      var s = s0
      var count = 0
      while (it.hasNext) {
        s = A.perturb(s, it.next())
        count += 1
      }
      seedMix(s, count.toLong)
    }

  implicit def iterable[A](implicit A: Cogen[A]): Cogen[Iterable[A]] =
    iteratorFn[A].contramap(i => () => i.iterator)

  implicit def array[A](implicit A: Cogen[A]): Cogen[Array[A]] =
    iteratorFn[A].contramap(a => () => a.iterator)

  implicit val string: Cogen[String] =
    iteratorFn[Char].contramap(s => () => s.iterator)

  implicit val throwable: Cogen[Throwable] =
    string.contramap(_.toString)

  implicit def option[A](implicit A: Cogen[A]): Cogen[Option[A]] =
    Cogen { (s, o) =>
      o match {
        case Some(a) => A.perturb(seedMix(s, 2), a)
        case None    => seedMix(s, 1)
      }
    }

  implicit def either[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[Either[A, B]] =
    Cogen { (s, e) =>
      e match {
        case Right(b) => B.perturb(seedMix(s, 2), b)
        case Left(a)  => A.perturb(seedMix(s, 1), a)
      }
    }

  implicit def scalaUtilTry[A](implicit A: Cogen[A]): Cogen[Try[A]] =
    Cogen { (s, t) =>
      t match {
        case Success(a) => A.perturb(seedMix(s, 2), a)
        case Failure(e) => throwable.perturb(seedMix(s, 1), e)
      }
    }

  implicit lazy val uuid: Cogen[UUID] =
    divide2((u: UUID) => (u.getLeastSignificantBits, u.getMostSignificantBits))

  // Java dates and times
  implicit lazy val date          : Cogen[Date]           = long.contramap(_.getTime)
  implicit lazy val dayOfWeek     : Cogen[DayOfWeek]      = int.contramap(_.getValue)
  implicit lazy val duration      : Cogen[Duration]       = divide2((d: Duration) => (d.getSeconds, d.getNano))
  implicit lazy val instant       : Cogen[Instant]        = divide2((i: Instant) => (i.getEpochSecond, i.getNano))
  implicit lazy val localDate     : Cogen[LocalDate]      = long.contramap(_.toEpochDay)
  implicit lazy val localDateTime : Cogen[LocalDateTime]  = divide2((d: LocalDateTime) => (d.toLocalDate, d.toLocalTime))
  implicit lazy val localTime     : Cogen[LocalTime]      = long.contramap(_.toNanoOfDay)
  implicit lazy val month         : Cogen[Month]          = int.contramap(_.getValue)
  implicit lazy val monthDay      : Cogen[MonthDay]       = divide2((m: MonthDay) => (m.getMonthValue, m.getDayOfMonth))
  implicit lazy val offsetDateTime: Cogen[OffsetDateTime] = divide2((d: OffsetDateTime) => (d.toLocalDateTime, d.getOffset))
  implicit lazy val offsetTime    : Cogen[OffsetTime]     = divide2((t: OffsetTime) => (t.toLocalTime, t.getOffset))
  implicit lazy val period        : Cogen[Period]         = divide3((p: Period) => (p.getYears, p.getMonths, p.getDays))
  implicit lazy val timeUnit      : Cogen[TimeUnit]       = int.contramap(_.ordinal)
  implicit lazy val year          : Cogen[Year]           = int.contramap(_.getValue)
  implicit lazy val yearMonth     : Cogen[YearMonth]      = divide2((m: YearMonth) => (m.getYear, m.getMonthValue))
  implicit lazy val zonedDateTime : Cogen[ZonedDateTime]  = divide2((d: ZonedDateTime) => (d.toInstant, d.getZone))
  implicit lazy val zoneId        : Cogen[ZoneId]         = string.contramap(_.getId)
  implicit lazy val zoneOffset    : Cogen[ZoneOffset]     = int.contramap(_.getTotalSeconds)

  // Scala durations
  implicit lazy val deadline      : Cogen[scd.Deadline]       = finiteDuration.contramap(_.time)
  implicit lazy val finiteDuration: Cogen[scd.FiniteDuration] = long.contramap(_.toNanos)
  implicit lazy val scalaDuration : Cogen[scd.Duration]       =
    Cogen { (s, d) =>
      d match {
        case fd: scd.FiniteDuration => finiteDuration.perturb(s, fd)
        case scd.Duration.Inf       => seedMix(s, 1)
        case scd.Duration.MinusInf  => seedMix(s, 2)
        case _                      => seedMix(s, 3)
      }
    }

  // Math & Numerics
  implicit lazy val javaBigInteger: Cogen[JBigInteger] = array[Byte].contramap(_.toByteArray)
  implicit lazy val bigInt        : Cogen[BigInt]      = javaBigInteger.contramap(_.bigInteger)
  implicit lazy val javaBigDecimal: Cogen[JBigDecimal] = divide2((d: JBigDecimal) => (d.unscaledValue, d.scale))
  implicit lazy val bigDecimal    : Cogen[BigDecimal]  = javaBigDecimal.contramap(_.bigDecimal)

  // Java Optionals
  implicit def javaOptional[A](implicit A: Cogen[A]): Cogen[Optional[A]] =
    Cogen { (s, o) => if (o.isPresent) A.perturb(seedMix(s, 2), o.get) else seedMix(s, 1) }

  implicit lazy val javaOptionalInt: Cogen[OptionalInt] =
    Cogen { (s, o) => if (o.isPresent) int.perturb(seedMix(s, 2), o.getAsInt) else seedMix(s, 1) }

  implicit lazy val javaOptionalLong: Cogen[OptionalLong] =
    Cogen { (s, o) => if (o.isPresent) long.perturb(seedMix(s, 2), o.getAsLong) else seedMix(s, 1) }

  implicit lazy val javaOptionalDouble: Cogen[OptionalDouble] =
    Cogen { (s, o) => if (o.isPresent) double.perturb(seedMix(s, 2), o.getAsDouble) else seedMix(s, 1) }

  // Misc
  implicit lazy val bitSet               : Cogen[BitSet]            = array[Long].contramap(_.toBitMask)
  implicit lazy val charset              : Cogen[Charset]           = string.contramap(_.name)
  implicit lazy val currency             : Cogen[Currency]          = string.contramap(_.getCurrencyCode)
  implicit lazy val javaEnum             : Cogen[java.lang.Enum[_]] = int.contramap(_.ordinal)
  implicit lazy val locale               : Cogen[Locale]            = string.contramap(_.toLanguageTag)
  implicit lazy val pattern              : Cogen[Pattern]           = divide2((p: Pattern) => (p.pattern, p.flags))
  implicit lazy val regex                : Cogen[Regex]             = pattern.contramap(_.pattern)
  implicit lazy val scalaEnumerationValue: Cogen[Enumeration#Value] = int.contramap(_.id)
  implicit lazy val symbol               : Cogen[Symbol]            = string.contramap(_.name)

  // ------------------------------------------------------
  // Arity boilerplate
  // ------------------------------------------------------

  implicit def tuple2[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[(A, B)] = divide2(identity)
  implicit def tuple3[A, B, C](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C]): Cogen[(A, B, C)] = divide3(identity)
  implicit def tuple4[A, B, C, D](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D]): Cogen[(A, B, C, D)] = divide4(identity)
  implicit def tuple5[A, B, C, D, E](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E]): Cogen[(A, B, C, D, E)] = divide5(identity)
  implicit def tuple6[A, B, C, D, E, F](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F]): Cogen[(A, B, C, D, E, F)] = divide6(identity)
  implicit def tuple7[A, B, C, D, E, F, G](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G]): Cogen[(A, B, C, D, E, F, G)] = divide7(identity)
  implicit def tuple8[A, B, C, D, E, F, G, H](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H]): Cogen[(A, B, C, D, E, F, G, H)] = divide8(identity)
  implicit def tuple9[A, B, C, D, E, F, G, H, I](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I]): Cogen[(A, B, C, D, E, F, G, H, I)] = divide9(identity)
  implicit def tuple10[A, B, C, D, E, F, G, H, I, J](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J]): Cogen[(A, B, C, D, E, F, G, H, I, J)] = divide10(identity)
  implicit def tuple11[A, B, C, D, E, F, G, H, I, J, K](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K]): Cogen[(A, B, C, D, E, F, G, H, I, J, K)] = divide11(identity)
  implicit def tuple12[A, B, C, D, E, F, G, H, I, J, K, L](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L)] = divide12(identity)
  implicit def tuple13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = divide13(identity)
  implicit def tuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = divide14(identity)
  implicit def tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = divide15(identity)
  implicit def tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = divide16(identity)
  implicit def tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = divide17(identity)
  implicit def tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = divide18(identity)
  implicit def tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R], S: Cogen[S]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = divide19(identity)
  implicit def tuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R], S: Cogen[S], T: Cogen[T]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = divide20(identity)
  implicit def tuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R], S: Cogen[S], T: Cogen[T], U: Cogen[U]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = divide21(identity)
  implicit def tuple22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R], S: Cogen[S], T: Cogen[T], U: Cogen[U], V: Cogen[V]): Cogen[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = divide22(identity)

  def divide2[A, B, Z](fz: Z => (A, B))(implicit A: Cogen[A], B: Cogen[B]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b) = fz(z)
      B.perturb(A.perturb(s0, a), b)
    }

  def divide3[A, B, C, Z](fz: Z => (A, B, C))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c) = fz(z)
      C.perturb(B.perturb(A.perturb(s0, a), b), c)
    }

  def divide4[A, B, C, D, Z](fz: Z => (A, B, C, D))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d) = fz(z)
      D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d)
    }

  def divide5[A, B, C, D, E, Z](fz: Z => (A, B, C, D, E))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e) = fz(z)
      E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e)
    }

  def divide6[A, B, C, D, E, F, Z](fz: Z => (A, B, C, D, E, F))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f) = fz(z)
      F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f)
    }

  def divide7[A, B, C, D, E, F, G, Z](fz: Z => (A, B, C, D, E, F, G))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g) = fz(z)
      G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g)
    }

  def divide8[A, B, C, D, E, F, G, H, Z](fz: Z => (A, B, C, D, E, F, G, H))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h) = fz(z)
      H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h)
    }

  def divide9[A, B, C, D, E, F, G, H, I, Z](fz: Z => (A, B, C, D, E, F, G, H, I))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i) = fz(z)
      I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i)
    }

  def divide10[A, B, C, D, E, F, G, H, I, J, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j) = fz(z)
      J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j)
    }

  def divide11[A, B, C, D, E, F, G, H, I, J, K, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k) = fz(z)
      K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k)
    }

  def divide12[A, B, C, D, E, F, G, H, I, J, K, L, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l) = fz(z)
      L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l)
    }

  def divide13[A, B, C, D, E, F, G, H, I, J, K, L, M, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m) = fz(z)
      M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m)
    }

  def divide14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = fz(z)
      N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n)
    }

  def divide15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = fz(z)
      O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o)
    }

  def divide16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = fz(z)
      P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
    }

  def divide17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = fz(z)
      Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q)
    }

  def divide18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = fz(z)
      R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r)
    }

  def divide19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R], S: Cogen[S]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = fz(z)
      S.perturb(R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s)
    }

  def divide20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R], S: Cogen[S], T: Cogen[T]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = fz(z)
      T.perturb(S.perturb(R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t)
    }

  def divide21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R], S: Cogen[S], T: Cogen[T], U: Cogen[U]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = fz(z)
      U.perturb(T.perturb(S.perturb(R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u)
    }

  def divide22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z](fz: Z => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))(implicit A: Cogen[A], B: Cogen[B], C: Cogen[C], D: Cogen[D], E: Cogen[E], F: Cogen[F], G: Cogen[G], H: Cogen[H], I: Cogen[I], J: Cogen[J], K: Cogen[K], L: Cogen[L], M: Cogen[M], N: Cogen[N], O: Cogen[O], P: Cogen[P], Q: Cogen[Q], R: Cogen[R], S: Cogen[S], T: Cogen[T], U: Cogen[U], V: Cogen[V]): Cogen[Z] =
    Cogen { (s0, z) =>
      val (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = fz(z)
      V.perturb(U.perturb(T.perturb(S.perturb(R.perturb(Q.perturb(P.perturb(O.perturb(N.perturb(M.perturb(L.perturb(K.perturb(J.perturb(I.perturb(H.perturb(G.perturb(F.perturb(E.perturb(D.perturb(C.perturb(B.perturb(A.perturb(s0, a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v)
    }

  // ------------------------------------------------------
  // Cats stuff
  // ------------------------------------------------------

  implicit def catsChain[A](implicit A: Cogen[A]): Cogen[Chain[A]] =
    iteratorFn[A].contramap(c => () => c.iterator)

  implicit def catsNonEmptyChain[A](implicit A: Cogen[A]): Cogen[NonEmptyChain[A]] =
    iteratorFn[A].contramap(c => () => c.iterator)

  implicit def catsNonEmptyList[A](implicit A: Cogen[A]): Cogen[NonEmptyList[A]] =
    iterable[A].contramap(_.toList)

  implicit def catsNonEmptyVector[A](implicit A: Cogen[A]): Cogen[NonEmptyVector[A]] =
    iterable[A].contramap(_.toVector)

  implicit def catsNonEmptySet[A](implicit A: Cogen[A]): Cogen[NonEmptySet[A]] =
    iterable[A].contramap(_.toSortedSet)

  implicit def catsValidated[E, A](implicit E: Cogen[E], A: Cogen[A]): Cogen[Validated[E, A]] =
    either[E, A].contramap(_.toEither)

  implicit def catsIor[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[Ior[A, B]] =
    Cogen { (s, i) =>
      i match {
        case Ior.Left(a)    => A.perturb(seedMix(s, 1), a)
        case Ior.Right(b)   => B.perturb(seedMix(s, 2), b)
        case Ior.Both(a, b) => B.perturb(A.perturb(seedMix(s, 3), a), b)
      }
    }

  implicit object catsInstance extends ContravariantMonoidal[Cogen] with MonoidK[Cogen] {
    override def contramap[A, B](fa: Cogen[A])(f: B => A): Cogen[B] =
      fa.contramap(f)

    override def product[A, B](fa: Cogen[A], fb: Cogen[B]): Cogen[(A, B)] =
      Cogen.tuple2(fa, fb)

    override val unit: Cogen[Unit] =
      Cogen.unit

    override def empty[A]: Cogen[A] =
      conquer[A]

    override def combineK[A](x: Cogen[A], y: Cogen[A]): Cogen[A] =
      Cogen((s, a) => y.perturb(x.perturb(s, a), a))
  }
}
