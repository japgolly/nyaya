package nyaya.gen

import nyaya.util.{NonEmptyList, MultiValues, Multimap}
import monocle._
import scalaz.{Monoid, Foldable, State, Traverse}
import scalaz.Leibniz.===
import scalaz.syntax.foldable._
import Baggy._
import Baggy.Implicits._
import Distinct.{Fixer, foldableList}

sealed trait DistinctFn[A, B] {
  def run: A => B
}

case class Distinct[A, X, H[_] : Baggy, Y, Z, B](
    fixer: Fixer[X, H, Y, Z], t: A => (X => State[H[Y], Z]) => State[H[Y], B]) extends DistinctFn[A, B]{

  final type S[λ] = State[H[Y], λ]

  def runs(a: A): S[B] =
    t(a)(fixer.apply)

  def run: A => B =
    runs(_).eval(fixer.inith)

  def addh(xs: X*): Distinct[A, X, H, Y, Z, B] =
    copy(fixer = this.fixer.addh(xs: _*))

  @inline final def at[M, N](l: PLens[M, N, A, B]): Distinct[M, X, H, Y, Z, N] =
    dimap(l.get, (m, b) => l.set(b)(m))

  @inline final def at[M, N](l: PIso[M, N, A, B]): Distinct[M, X, H, Y, Z, N] =
    at(l.asLens)

  @inline final def at[M, N](l: POptional[M, N, A, B]): Distinct[M, X, H, Y, Z, N] =
    dimaps(m => asb =>
      l.getOrModify(m).fold[S[N]](
        n => State state n, // No A = no dup
        a => asb(a).map(b => l.set(b)(m))))

  @inline final def at[M, N](l: PPrism[M, N, A, B]): Distinct[M, X, H, Y, Z, N] =
    at(l.asOptional)

  def at[M, N](l: PTraversal[M, N, A, B]): Distinct[M, X, H, Y, Z, N] =
    dimaps[M, N](m => a_sb => State { h0 =>
      var h = h0
      val n =
        l.modify(a => {
          val (h2, b) = a_sb(a).run(h)
          h = h2
          b
        })(m)
      (h, n)
    })

  @inline final def contramap[C](f: C => A, g: (C, B) => C): Distinct[C, X, H, Y, Z, C] =
    dimap(f, g)

  def dimap[M, N](f: M => A, g: (M, B) => N): Distinct[M, X, H, Y, Z, N] =
    Distinct[M, X, H, Y, Z, N](fixer, m => x_sz => t(f(m))(x_sz).map(b => g(m, b)))

  def dimaps[M, N](f: M => (A => S[B]) => S[N]): Distinct[M, X, H, Y, Z, N] =
    Distinct[M, X, H, Y, Z, N](fixer, m => x_sz => f(m)(a => t(a)(x_sz)))

  def traversal[S, T](traversal: PTraversal[S, T, A, B]): Distinct[S, X, H, Y, Z, T] =
    dimaps[S, T](t => d_sd => State { h0 =>
      var h = h0
      val t2 =
        traversal.modify({ data =>
          val (h2, d2) = d_sd(data).run(h)
          h = h2
          d2
        })(t)
      (h, t2)
    })

  def lift[F[_] : Foldable : Baggy]: Distinct[F[A], X, H, Y, Z, F[B]] =
    dimaps[F[A], F[B]](fa => ab => State { h0 =>
      var h = h0
      val fb =
        fa.foldl(implicitly[Baggy[F]].empty[B])(q => a => {
          val (h2, b) = ab(a).run(h)
          h = h2
          q + b
        })
      (h, fb)
    })

  def liftT[F[_] : Traverse]: Distinct[F[A], X, H, Y, Z, F[B]] =
    at(PTraversal.fromTraverse[F, A, B])

  def liftL[R]: Distinct[(A, R), X, H, Y, Z, (B, R)] =
    dimap[(A, R), (B, R)](_._1, (a, b) => (b, a._2))

  def liftR[L]: Distinct[(L, A), X, H, Y, Z, (L, B)] =
    dimap[(L, A), (L, B)](_._2, (a, b) => (a._1, b))

  def liftMapValues[K]: Distinct[Map[K, A], X, H, Y, Z, Map[K, B]] =
    liftR[K].lift[List].dimap[Map[K, A], Map[K, B]](_.toList, (_, l) => l.toMap)

  def liftMultimapValues[K, L[_], VA, VB]
        (implicit l: MultiValues[L], va: Map[K, L[VA]] =:= Map[K, A], vb: Map[K, B] =:= Map[K, L[VB]])
        : Distinct[Multimap[K, L, VA], X, H, Y, Z, Multimap[K, L, VB]] =
    liftMapValues[K].dimap[Multimap[K, L, VA], Multimap[K, L, VB]](x => va(x.m), (_, x) => Multimap(vb(x)))

  def compose[M](f: Distinct[M, X, H, Y, Z, A]): Distinct[M, X, H, Y, Z, B] =
    f + this

  def +[C](f: Distinct[B, X, H, Y, Z, C]): Distinct[A, X, H, Y, Z, C] =
    Distinct[A, X, H, Y, Z, C](fixer + f.fixer, a => _ => runs(a) flatMap f.runs)

  def *(f: DistinctFn[A, A])(implicit ev: B === A): DistinctEndo[A] =
    DistinctEndo(NonEmptyList(ev.subst[({type λ[α] = Distinct[A, X, H, Y, Z, α]})#λ](this), f :: Nil))

  // def ***[C, D](f: Distinct1[C, D]): Distinct1[(A, C), (B, D)] =
}

case class DistinctEndo[A](ds: NonEmptyList[DistinctFn[A, A]]) extends DistinctFn[A, A] {
  def run: A => A =
    ds.tail.foldLeft(ds.head.run)(_ compose _.run)

  def *(d: DistinctFn[A, A]): DistinctEndo[A] =
    DistinctEndo(d :: ds)

  def map[B](f: DistinctFn[A, A] => DistinctFn[B, B]): DistinctEndo[B] =
    DistinctEndo(ds map f)

  def contramap[B](f: B => A, g: (B, A) => B): DistinctEndo[B] = map {
    case d@DistinctEndo(_) => d.contramap(f, g)
    case d@Distinct(_, _)  => d.contramap(f, g)
  }

  def lift[F[_] : Foldable : Baggy]: DistinctEndo[F[A]] = map {
    case d@DistinctEndo(_) => d.lift[F]
    case d@Distinct(_, _)  => d.lift[F]
  }
}

// =====================================================================================================================

object Distinct {

  // scalaz.std.list.listInstance pulls in too much other unneeded crap
  private[gen] implicit val foldableList: Foldable[List] =
    new Foldable[List] {
      override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B =
        fa.foldLeft(z)(f)
      override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B =
        fa.foldRight(z)(f(_, _))
      override def foldMap[A, B](fa: List[A])(f: A => B)(implicit F: Monoid[B]): B =
        fa.foldLeft(F.zero)((b, a) => F.append(b, f(a)))
    }

  case class Fixer[X, H[_] : Baggy, Y, Z](f: X => Y, g: Y => Z, fix: H[Y] => Y, inith: H[Y]) {
    def apply(x: X): State[H[Y], Z] =
      State[H[Y], Z](h => {
        var y = f(x)
        if (h contains y)
          y = fix(h)
        (h + y, g(y))
      })

    @inline final def xmap[A](b: Z => A)(a: A => X): Fixer[A, H, Y, A] =
      dimap(a, b)

    def dimap[A, B](a: A => X, b: Z => B): Fixer[A, H, Y, B] =
      Fixer(f compose a, b compose g, fix, inith)

    @inline final def addh(xs: X*): Fixer[X, H, Y, Z] =
      addhs(xs)

    def addhs(xs: TraversableOnce[X]): Fixer[X, H, Y, Z] =
      copy(inith = xs.foldLeft(this.inith)(_ + f(_)))

    def +(φ: Fixer[X, H, Y, Z]): Fixer[X, H, Y, Z] =
      copy(inith = this.inith ++ φ.inith)

    def distinct: Distinct[X, X, H, Y, Z, Z] =
      Distinct(this, x => f => f(x))
  }

  object Fixer {
    def lift[H[_], A](f: H[A] => A)(implicit H: Baggy[H]): Fixer[A, H, A, A] =
      Fixer[A, H, A, A](identity, identity, f, H.empty)
  }

  // =====================================================================================================================

  def fixInt(is: Set[Int]): Int = {
    var i = is.max + 1
    if (i == Int.MinValue) while (is contains i) i += 1
    i
  }

  def fixLong(is: Set[Long]): Long = {
    var i = is.max + 1L
    if (i == Long.MinValue) while (is contains i) i += 1L
    i
  }

  def fixStr(ss: Set[String]): String = {
    val x = ss.max
    if (x.nonEmpty) {
      val c = x.head
      if (c < 0xffff) return (c + 1).toChar.toString
    }
    val y = ss.min
    if (y.nonEmpty) {
      val c = y.head
      if (c > 32) return (c - 1).toChar.toString
    }
    "\uffff" + x
  }

  lazy val fstr  = Fixer lift fixStr
  lazy val fint  = Fixer lift fixInt
  lazy val flong = Fixer lift fixLong

  lazy val str  = fstr.distinct
  lazy val int  = fint.distinct
  lazy val long = flong.distinct
}