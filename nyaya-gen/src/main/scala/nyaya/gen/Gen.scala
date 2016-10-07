package nyaya.gen

import java.util.UUID
import nyaya.util.{NonEmptyList => NonEmptyListN}
import scala.annotation.{switch, tailrec}
import scala.collection.AbstractIterator
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{IndexedSeq, NumericRange}
import scala.collection.mutable.ArrayBuffer
import scalaz.{NonEmptyList => NonEmptyListZ}
import scalaz.std.function._
import SizeSpec.DisableDefault._

final case class Gen[+A](run: Gen.Run[A]) extends AnyVal {

  /**
   * Produce a sample datum.
   */
  def sample(): A =
    samples().next()

  /**
   * Produce an infinite stream of generated data.
   *
   * Use `.take(n)` for a finite number of samples.
   */
  def samples(): Iterator[A] =
    samples(GenCtx(GenSize.Default))

  /**
   * Produce an infinite stream of generated data.
   *
   * Use `.take(n)` for a finite number of samples.
   */
  def samplesSized(genSize: Int): Iterator[A] =
    samples(GenCtx(GenSize(genSize)))

  /**
   * Produce an infinite stream of generated data.
   *
   * Use `.take(n)` for a finite number of samples.
   */
  def samples(ctx: GenCtx): Iterator[A] =
    new AbstractIterator[A] {
      override def hasNext = true
      override def next(): A = run(ctx)
    }

  def map[B](f: A => B): Gen[B] =
    Gen(f compose run)

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(c => f(run(c)).run(c))

  def flatten[B](implicit ev: A <:< Gen[B]): Gen[B] =
    flatMap(ev)

  def >>[B](f: Gen[B]): Gen[B] =
    flatMap(_ => f)

  def withFilter(p: A => Boolean): Gen[A] =
    map(a => if (p(a)) a else
    // This is what scala.Future does
      throw new NoSuchElementException("Gen.withFilter predicate is not satisfied"))

  def withSeed(seed: Long): Gen[A] =
    Gen.setSeed(seed) >> this

  def withOptionalSeed(s: Option[Long]): Gen[A] =
    Gen.setOptionalSeed(s) >> this

  def withRandomSeed: Gen[A] =
    Gen.randomSeed >> this

  def option: Gen[Option[A]] =
    Gen(c => if (c.nextBit()) None else Some(run(c)))

  def pair: Gen[(A, A)] =
    Gen(c => (run(c), run(c)))

  def triple: Gen[(A, A, A)] =
    Gen(c => (run(c), run(c), run(c)))

  def strengthL[B](b: B): Gen[(B, A)] = map((b, _))
  def strengthR[B](b: B): Gen[(A, B)] = map((_, b))

  def ***[B](g: Gen[B]): Gen[(A, B)] =
    for {a <- this; b <- g} yield (a, b)

  def either[B](g: Gen[B]): Gen[Either[A, B]] =
    Gen(c => if (c.nextBit()) Left(run(c)) else Right(g run c))

  def fillFold[B](n: Int, z: B)(f: (B, A) => B): Gen[B] =
    Gen { c =>
      var b = z
      var i = n
      while (i > 0) {
        b = f(b, run(c))
        i -= 1
      }
      b
    }

  @inline def fillFoldSS[B](ss: SizeSpec, z: B)(f: (B, A) => B): Gen[B] =
    ss.gen flatMap (fillFold[B](_, z)(f))

  @inline def fillFoldSS1[B](ss: SizeSpec, z: B)(f: (B, A) => B): Gen[B] =
    ss.gen1 flatMap (fillFold[B](_, z)(f))

  def fill[B](n: Int)(implicit cbf: CanBuildFrom[Nothing, A, B]): Gen[B] = {
    if (n >= 100000)
      println(s"WARNING: Gen.fill instructed to create very large data: n=$n")
    Gen { c =>
      val b = cbf()
      var i = n
      while (i > 0) {
        val x = run(c)
        b += x
        i -= 1
      }
      b.result()
    }
  }

  @inline def fillSS[B](ss: SizeSpec)(implicit cbf: CanBuildFrom[Nothing, A, B]): Gen[B] =
    ss.gen flatMap fill[B]

  @inline def fillSS1[B](ss: SizeSpec)(implicit cbf: CanBuildFrom[Nothing, A, B]): Gen[B] =
    ss.gen1 flatMap fill[B]

  def list       (implicit ss: SizeSpec): Gen[List  [A]] = fillSS(ss)
  def set[B >: A](implicit ss: SizeSpec): Gen[Set   [B]] = fillSS(ss)
  def stream     (implicit ss: SizeSpec): Gen[Stream[A]] = fillSS(ss)
  def vector     (implicit ss: SizeSpec): Gen[Vector[A]] = fillSS(ss)

  def list1       (implicit ss: SizeSpec): Gen[List  [A]] = fillSS1(ss)
  def set1[B >: A](implicit ss: SizeSpec): Gen[Set   [B]] = fillSS1(ss)
  def stream1     (implicit ss: SizeSpec): Gen[Stream[A]] = fillSS1(ss)
  def vector1     (implicit ss: SizeSpec): Gen[Vector[A]] = fillSS1(ss)

  /**
    * This will ensure that only unique random data is used and that the resulting set has the desired size.
    *
    * This is dangerous in that it will block until it generates enough unique elements.
    * For example, `Gen.bool.set(3)` will never return.
    */
  def sizedSet[B >: A](implicit ss: SizeSpec): Gen[Set[B]] =
    _sizedSet(ss.gen)

  /**
    * This will ensure that only unique random data is used and that the resulting set has the desired size.
    *
    * This is dangerous in that it will block until it generates enough unique elements.
    * For example, `Gen.bool.set1(3)` will never return.
    */
  def sizedSet1[B >: A](implicit ss: SizeSpec): Gen[Set[B]] =
  _sizedSet(ss.gen1)

  private def _sizedSet[B >: A](genSize: Gen[Int]): Gen[Set[B]] =
    Gen { ctx ⇒
      val size = genSize.run(ctx)
      var set = Set.empty[B]
      samples(ctx)
        .filter(a ⇒ if (set.contains(a)) false else { set += a; true })
        .take(size)
        .foreach(_ ⇒ ())
      set
    }

  def shuffle[C[X] <: TraversableOnce[X], B](implicit ev: A <:< C[B], cbf: CanBuildFrom[C[B], B, C[B]]): Gen[C[B]] =
    Gen { c =>
      val orig = run(c)
      val buf = new ArrayBuffer[B] ++= orig
      Gen.runShuffle(buf, c.rnd)
      (cbf(orig) ++= buf).result()
    }

  def subset[C[X] <: TraversableOnce[X], B](implicit ev: A <:< C[B], cbf: CanBuildFrom[Nothing, B, C[B]]): Gen[C[B]] =
    Gen(c => Gen.runSubset(run(c), c))

  /**
   * Generates a non-empty subset, unless the underlying seq is empty (in which case this returns an empty seq too).
   */
  def subset1[C[X] <: IndexedSeq[X], B](implicit ev: A <:< C[B], cbf: CanBuildFrom[Nothing, B, C[B]]): Gen[C[B]] =
    Gen { c =>
      val a = run(c)
      var r = Gen.runSubset[C, B](run(c), c)
      if (r.isEmpty && a.nonEmpty) {
        val b = cbf()
        b.sizeHint(1)
        val i = c.rnd.nextInt(a.length)
        b += a(i)
        r = b.result()
      }
      r
    }

  def take[C[X] <: TraversableOnce[X], B](n: SizeSpec)(implicit ev: A <:< C[B], cbf: CanBuildFrom[Nothing, B, C[B]]): Gen[C[B]] =
    Gen { c =>
      val takeSize = n.gen run c
      if (takeSize == 0)
        cbf().result()
      else {
        val orig = ev(run(c))

        // First shuffle
        val buf = new ArrayBuffer[B] ++= orig
        Gen.runShuffle(buf, c.rnd)

        // Now take
        var i = takeSize min buf.length
        val b = cbf()
        b.sizeHint(i)
        while (i > 0) {
          i -= 1
          b += buf(i)
        }
        b.result()
    }
  }

  def mapBy[K](gk: Gen[K])(implicit ss: SizeSpec): Gen[Map[K, A]] =
    // GenS(sz => Gen.pair(k, this).list.lim(sz.value).map(_.toMap)) <-- old impl, below is faster
    Gen { c =>
      var m = Map.empty[K, A]
      var i = ss.gen.run(c)
      while (i > 0) {
        val k = gk run c
        val a = run(c)
        m = m.updated(k, a)
        i -= 1
      }
      m
    }

  @inline def mapTo[K >: A, V](gv: Gen[V])(implicit ss: SizeSpec): Gen[Map[K, V]] =
    gv.mapBy(this: Gen[K])(ss)

  def mapByKeySubset[K](legalKeys: TraversableOnce[K]): Gen[Map[K, A]] =
    // Gen.subset(legalKeys).flatMap(mapByEachKey) <-- works fine, below is faster
    Gen { c =>
      var m = Map.empty[K, A]
      legalKeys.foreach(k =>
        if (c.nextBit())
          m = m.updated(k, run(c)))
      m
    }

  def mapByEachKey[K](keys: TraversableOnce[K]): Gen[Map[K, A]] =
    // Gen.traverse(keys)(strengthL).map(_.toMap) <-- works fine, below is faster
    Gen { c =>
      var m = Map.empty[K, A]
      keys.foreach(k =>
        m = m.updated(k, run(c)))
      m
    }

  /**
   * Will keep generating options until one is defined, in which case it is returned.
   *
   * If a non-empty option still isn't generated after 1000 attempts, an exception will be thrown.
   *
   * It is recommended that you use this very sparingly. In nearly all cases, the better alternative is to write your
   * generators such that a return value is guaranteed, rather than generating then discarding.
   */
  def optionGet[B](implicit ev: A <:< Option[B]): Gen[B] =
    optionGetLimit(1000)

  def optionGetLimit[B](maxAttempts: Int)(implicit ev: A <:< Option[B]): Gen[B] =
    Gen { c =>
      var attempts = 0
      @tailrec
      def go(): B =
        ev(run(c)) match {
          case None =>
            attempts += 1
            if (attempts == maxAttempts)
              sys error s"Failed to generate a non-empty Option after $maxAttempts attempts."
            go()
          case Some(b) => b
        }
      go()
    }

  // ------------------------------------------------------
  // Nyaya stuff
  // ------------------------------------------------------

  def nyayaNEL(implicit ss: SizeSpec): Gen[NonEmptyListN[A]] =
    for (l <- list1(ss)) yield NonEmptyListN(l.head, l.tail)

  // ------------------------------------------------------
  // Scalaz stuff
  // ------------------------------------------------------
  import scalaz._

  @deprecated("Replace with scalazNEL.", "0.7.0")
  def nel[B >: A](implicit ss: SizeSpec): Gen[NonEmptyListZ[B]] =
    scalazNEL(ss)

  def scalazNEL[B >: A](implicit ss: SizeSpec): Gen[NonEmptyListZ[B]] =
    for (l <- list1(ss)) yield NonEmptyListZ.nels[B](l.head, l.tail: _*)

  def \/[B](g: Gen[B]): Gen[A \/ B] =
    Gen(c => if (c.nextBit()) -\/(run(c)) else \/-(g run c))

  @inline def +++[B](g: Gen[B]): Gen[A \/ B] =
    \/(g)

  def \&/[B](g: Gen[B]): Gen[A \&/ B] =
    Gen { c =>
      import scalaz.\&/._
      c.rnd.nextInt(3) match {
        case 0 => Both(run(c), g run c)
        case 1 => This(run(c))
        case 2 => That(g run c)
      }
    }
}

object Gen {
  type Run[+A] = GenCtx => A

  final class GenCharExt(private val g: Run[Char]) extends AnyVal {
    @inline def string (implicit ss: SizeSpec): Gen[String] = Gen.stringOf (Gen(g))(ss)
    @inline def string1(implicit ss: SizeSpec): Gen[String] = Gen.stringOf1(Gen(g))(ss)
  }

  @inline implicit def _GenCharExt(g: Gen[Char]) = new GenCharExt(g.run)

  final case class ToNonEmptySeq[S, A](toSeq: S => Seq[A]) extends AnyVal

  object ToNonEmptySeq {
    def merge[A](a: A, s: Traversable[A]): Seq[A] =
      a +: s.toIndexedSeq // toIndexedSeq because this is really for choose_!

    implicit def headAndTraversable[S[x] <: Traversable[x], A]: ToNonEmptySeq[(A, S[A]), A] =
      apply(o => merge(o._1, o._2))

    import scalaz.OneAnd

    implicit def scalazNEL[A]: ToNonEmptySeq[NonEmptyListZ[A], A] =
      apply(_.list.toList)

    implicit def scalazOneAndTraversable[S[x] <: Traversable[x], A]: ToNonEmptySeq[OneAnd[S, A], A] =
      apply(o => merge(o.head, o.tail))
  }

  // ===================================================================================================================

  import scalaz.{BindRec, Distributive, Kleisli, Monad, Name, Need, Traverse}

  implicit val scalazInstance: Monad[Gen] with BindRec[Gen] =
    new Monad[Gen] with BindRec[Gen] {
      override def point[A](a: => A)                         : Gen[A] = Gen pure a
      override def ap[A, B](fa: => Gen[A])(g: => Gen[A => B]): Gen[B] = g flatMap fa.map
      override def bind[A, B](fa: Gen[A])(f: A => Gen[B])    : Gen[B] = fa flatMap f
      override def map[A,B](fa: Gen[A])(f: A => B)           : Gen[B] = fa map f

      import scalaz.{\/, -\/, \/-}
      override def tailrecM[A, B](f: A => Gen[A \/ B])(a: A): Gen[B] =
        Gen { c =>
          @tailrec
          def go(a1: A): B =
            f(a1).run(c) match {
              case -\/(a2) => go(a2)
              case \/-(b)  => b
            }
          go(a)
        }
    }

  def tailrec[A, B](f: A => Gen[Either[A, B]])(a: A): Gen[B] =
    Gen { c =>
      @tailrec
      def go(a1: A): B =
        f(a1).run(c) match {
          case Left(a2) => go(a2)
          case Right(b) => b
        }
      go(a)
    }

  private[Gen] def runSubset[C[X] <: TraversableOnce[X], A](as: C[A], c: GenCtx)(implicit cbf: CanBuildFrom[Nothing, A, C[A]]): C[A] = {
    val r = cbf()
    as foreach (b => if (c.nextBit()) r += b)
    r.result()
  }

  private[Gen] def runShuffle[A](buf: ArrayBuffer[A], r: java.util.Random): Unit = {
    var n = buf.length
    while (n > 1) {
      val k = r.nextInt(n)
      n -= 1
      val tmp = buf(n)
      buf(n) = buf(k)
      buf(k) = tmp
    }
  }

  def setSeed(seed: Long): Gen[Unit] =
    Gen(_ setSeed seed)

  def setOptionalSeed(s: Option[Long]): Gen[Unit] =
    s.fold(unit)(setSeed)

  /**
   * Apply a new deterministic seed to the RNG.
   *
   * @return The seed used.
   */
  def reseed: Gen[Long] =
    for {
      seed <- long
      _    <- setSeed(seed)
    } yield seed

  /**
   * Apply a new, non-deterministic seed to the RNG.
   *
   * @return The seed used.
   */
  def randomSeed: Gen[Long] =
    for {
      seed <- uuid.map(i => java.lang.Long.valueOf(i.toString.replace("-","") take 15, 16))
      _    <- setSeed(seed)
    } yield seed

  /** Returns a number in [0,GenSize) */
  val chooseSize: Gen[Int] =
    Gen(_.nextSize())

  /** Returns a number in [1,GenSize) */
  val chooseSizeMin1: Gen[Int] =
    Gen(_.nextSizeMin1())

  def pure[A](a: A): Gen[A] =
    Gen(_ => a)

  def point[A](a: => A): Gen[A] =
    Gen(_ => a)

  def byName[A](ga: => Gen[A]): Gen[A] =
    pure(Name(ga)) flatMap (_.value)

  def byNeed[A](ga: => Gen[A]): Gen[A] =
    pure(Need(ga)) flatMap (_.value)

  @inline def lazily[A](ga: => Gen[A]): Gen[A] = byNeed(ga)

  val int    : Gen[Int]     = Gen(_.rnd.nextInt())
  val long   : Gen[Long]    = Gen(_.rnd.nextLong())
  val double : Gen[Double]  = Gen(_.rnd.nextDouble())
  def float  : Gen[Float]   = Gen(_.rnd.nextFloat())
  def short  : Gen[Short]   = Gen(_.rnd.nextInt().toShort)
  def byte   : Gen[Byte]    = Gen(_.rnd.nextInt().toByte)
  val boolean: Gen[Boolean] = Gen(_.nextBit())
  def unit   : Gen[Unit]    = pure(())
  def uuid   : Gen[UUID]    = point(UUID.randomUUID())

  val positiveInt   : Gen[Int]     = int    map (Math abs _)
  val positiveLong  : Gen[Long]    = long   map (Math abs _)
  val positiveDouble: Gen[Double]  = double map (Math abs _)
  def positiveFloat : Gen[Float]   = float  map (Math abs _)
  val negativeInt   : Gen[Int]     = positiveInt    map (-_)
  val negativeLong  : Gen[Long]    = positiveLong   map (-_)
  val negativeDouble: Gen[Double]  = positiveDouble map (-_)
  def negativeFloat : Gen[Float]   = positiveFloat  map (-_)

  /*
  import java.nio.charset.Charset
  val utf8 = Charset.forName("UTF-8")
  val valid = (0 to Character.MAX_VALUE).filter{i => val s = i.toChar.toString; new String(s getBytes utf8, utf8) == s}
  > 0-55295,57344-65535
  val mimic = (1 to 65535-2048).map(i => if (i>55295) i+2048 else i)
   */
  val unicode: Gen[Char] = Gen { c =>
    var i = c.rnd.nextInt(63487) + 1 // 1 to 65535-2048
    if (i > 55295) i += 2048
    i.toChar
  }

  private[this] val charsNumeric      = ('0' to '9').toArray
  private[this] val charsUpper        = ('A' to 'Z').toArray
  private[this] val charsLower        = ('a' to 'z').toArray
  private[this] val charsAlpha        = charsUpper ++ charsLower
  private[this] val charsAlphaNumeric = charsAlpha ++ charsNumeric
  private[this] val charsAscii        = (' ' to '~').toArray

  val numeric     : Gen[Char] = chooseArray_!(charsNumeric)
  val upper       : Gen[Char] = chooseArray_!(charsUpper)
  val lower       : Gen[Char] = chooseArray_!(charsLower)
  val alpha       : Gen[Char] = chooseArray_!(charsAlpha)
  val alphaNumeric: Gen[Char] = chooseArray_!(charsAlphaNumeric)
  val ascii       : Gen[Char] = chooseArray_!(charsAscii)

  private def mkString(cs: Gen[Char], size: Gen[Int]): Gen[String] = Gen {c =>
    var i = size run c
    if (i == 0)
      ""
    else {
      val array = new Array[Char](i)
      while (i > 0) {
        i -= 1
        array(i) = cs run c
      }
      new String(array)
    }
  }
  def stringOf (cs: Gen[Char])(implicit ss: SizeSpec): Gen[String] = mkString(cs, ss.gen)
  def stringOf1(cs: Gen[Char])(implicit ss: SizeSpec): Gen[String] = mkString(cs, ss.gen1)

  /** An alias for [[unicode]], as unicode is the default. */
  @inline def char: Gen[Char] = unicode
  def string (implicit ss: SizeSpec): Gen[String] = stringOf (char)(ss)
  def string1(implicit ss: SizeSpec): Gen[String] = stringOf1(char)(ss)

  def chooseChar(nonEmptinessProof: Char, rs: NumericRange[Char]*): Gen[Char] =
    chooseChar(nonEmptinessProof, "", rs: _*)

  def chooseChar(nonEmptinessProof: Char, s: String, rs: NumericRange[Char]*): Gen[Char] = {
    val b = Array.newBuilder[Char]
    b += nonEmptinessProof
    b ++= s
    rs foreach (b ++= _)
    chooseArray_!(b.result())
  }

  def chooseChar_!(s: String, rs: NumericRange[Char]*): Gen[Char] = {
    val b = Array.newBuilder[Char]
    b ++= s
    rs foreach (b ++= _)
    chooseArray_!(b.result())
  }

  def chooseChar_!(rs: NumericRange[Char]*): Gen[Char] =
    chooseChar_!("", rs: _*)

  /**
   * Generate an int ∈ [0,bound).
   *
   * @param bound Upper-bound (exclusive).
   */
  def chooseInt(bound: Int): Gen[Int] =
    Gen(_.rnd nextInt bound)

  /** Args are inclusive. [l,h] */
  def chooseInt(l: Int, h: Int): Gen[Int] =
    chooseIndexed_!(l to h)

  /**
    * Generate a long ∈ [0,bound).
    *
    * @param bound Upper-bound (exclusive).
    */
  def chooseLong(bound: Long): Gen[Long] = {
    if (bound < 0)
      throw new IllegalArgumentException(s"Bound ($bound) must be ≥ 0.")
    if (bound == 0)
      Gen pure 0L
    else {
      // Copied from https://github.com/apache/commons-rng/blob/master/src/main/java/org/apache/commons/rng/internal/BaseProvider.java
      val bound_1 = bound - 1
      Gen { ctx ⇒
        @tailrec def go(): Long = {
          val bits = ctx.rnd.nextLong()
          val v = bits % bound
          if (bits - v + bound_1 < 0) go() else v
        }
        go()
      }
    }
  }

  /** Args are inclusive. [l,h] */
  def chooseLong(l: Long, h: Long): Gen[Long] = {
    val range = h - l + 1
    chooseLong(range).map(_ + l)
  }

  /** Args are inclusive. [l,h] */
  def chooseDouble(l: Double, h: Double): Gen[Double] = {
    var ll = l
    var hh = h
    if (h < l) {
      ll = h
      hh = l
    }
    val diff = hh - ll
    double map (_ * diff + ll)
  }

  /** Args are inclusive. [l,h] */
  def chooseFloat(l: Float, h: Float): Gen[Float] = {
    var ll = l
    var hh = h
    if (h < l) {
      ll = h
      hh = l
    }
    if ((ll <= 0 && hh <= 0) || (ll >= 0 && hh >= 0)) {
      val diff = hh - ll
      float map (ll + diff * _)
    } else
      float map (x => ll * (1 - x) + hh * x)
  }

  private def __choose_![A](length: Int, a: Int => A): Gen[A] =
    (length: @switch) match {
      case  1 => pure(a(0))
      case  2 => Gen(c => a(c.nextInt2()))
      case  4 => Gen(c => a(c.nextInt4()))
      case  8 => Gen(c => a(c.nextInt8()))
      case 16 => Gen(c => a(c.nextInt16()))
      case  n => Gen(c => a(c.rnd nextInt n))
    }

  /**
   * Randomly selects one of the given elements.
   *
   * @param as Possible elements. MUST NOT BE EMPTY.
   */
  def chooseIndexed_![A](as: IndexedSeq[A]): Gen[A] =
    __choose_!(as.length, as.apply)

  /**
   * Randomly selects one of the given elements.
   *
   * @param as Possible elements. MUST NOT BE EMPTY.
   */
  def choose_![A](as: TraversableOnce[A]): Gen[A] =
    as match {
      case is: IndexedSeq[A] => chooseIndexed_!(is)
      case _                 => chooseIndexed_!((Vector.newBuilder[A] ++= as).result())
    }

  def choose[A](a: A, as: A*): Gen[A] =
    chooseIndexed_!((Vector.newBuilder[A] += a ++= as).result())

  /**
   * Randomly selects one of the given elements.
   *
   * @param as Possible elements. MUST NOT BE EMPTY.
   */
  def chooseArray_![A](as: Array[A]): Gen[A] =
    __choose_!(as.length, as.apply)

  def chooseGen[A](a: Gen[A], as: Gen[A]*): Gen[A] =
    choose(a, as: _*).flatten

  def chooseNE[S, A](s: S)(implicit ne: ToNonEmptySeq[S, A]): Gen[A] =
    choose_!(ne toSeq s)

  def chooseGenNE[S, G, A](s: S)(implicit ne: ToNonEmptySeq[S, G], g: G <:< Gen[A]): Gen[A] =
    chooseNE(s).flatten

  def tryChoose[A](as: TraversableOnce[A]): Gen[Option[A]] =
    if (as.isEmpty)
      pure(None)
    else
      choose_!(as).option

  def tryGenChoose[A](as: TraversableOnce[A]): Option[Gen[A]] =
    if (as.isEmpty)
      None
    else
      Some(choose_!(as))

  def tryGenChooseLazily[A](as: TraversableOnce[A]): Option[Gen[A]] =
    if (as.isEmpty)
      None
    else
      Some(lazily(choose_!(as)))

  @inline def shuffle[A, C[X] <: TraversableOnce[X]](as: C[A])(implicit bf: CanBuildFrom[C[A], A, C[A]]): Gen[C[A]] =
    pure(as).shuffle

  @inline def subset[A, C[X] <: TraversableOnce[X]](as: C[A])(implicit bf: CanBuildFrom[Nothing, A, C[A]]): Gen[C[A]] =
    pure(as).subset

  /**
   * Generates a non-empty subset, unless the underlying seq is empty (in which case this returns an empty seq too).
   */
  @inline def subset1[A, C[X] <: IndexedSeq[X]](as: C[A])(implicit bf: CanBuildFrom[Nothing, A, C[A]]): Gen[C[A]] =
    pure(as).subset1

  /** Randomly either generates a new value, or chooses one from a known set. */
  def newOrOld[A](newGen: => Gen[A], old: => TraversableOnce[A]): Gen[A] = {
    lazy val n: Gen[A] = newGen
    lazy val o: Gen[A] = tryGenChoose(old) getOrElse n
    Gen(c => (if (c.nextBit()) n else o) run c)
  }

  /** Int = Probability of being chosen. ≥ 0 */
  type Freq[A] = (Int, Gen[A])

  def frequency[A](x: Freq[A], xs: Freq[A]*): Gen[A] =
    frequencyL_!(x :: xs.toList)

  def frequencyNE[S, F, A](s: S)(implicit ne: ToNonEmptySeq[S, F], f: Seq[F] <:< Seq[Freq[A]]): Gen[A] =
    frequencyL_!(f(ne toSeq s).toList)

  def frequencyL_![A](xs: List[Freq[A]]): Gen[A] =
    if (xs.lengthCompare(1) == 0)
      xs.head._2
    else {
      def reportFreqs = xs.map(_._1).mkString("{", ", ", "}")
      val total = xs.foldLeft(0)((q, x) => {
        val n = x._1
        assert(n > 0, s"Gen.frequency: n must be > 0, found $n in $reportFreqs}.")
        val q2 = q + n
        assert(q2 > q, s"Gen.frequency: Overflow detected adding $reportFreqs.")
        q2
      })
      @tailrec def pick(n: Int, head: Freq[A], tail: List[Freq[A]]): Gen[A] = {
        val q = head._1
        val r = head._2
        if (n < q)
          r
        else tail match {
          case Nil     => r
          case e :: es => pick(n - q, e, es)
        }
      }
      val h = xs.head
      val t = xs.tail
      chooseInt(total - 1) flatMap (pick(_, h, t))
    }

  /**
   * Generates a sequence of elements in a fixed order.
   *
   * Example: [a,b,c] can generate [a,b,c], [a,b,b,b,c,c], etc. but never [b,a,c].
   *
   * @param orderedElems Legal elements in a relevant order.
   * @param maxDups      The maximum number of consecutive, duplicate elements (can be 0).
   * @param dropElems    Whether or not the generator can drop elements. (eg. drop b and return [a,c])
   * @param emptyResult  Whether or not the generator can return an empty vector as a result.
   */
  def orderedSeq[A](orderedElems: Traversable[A],
                    maxDups     : Int,
                    dropElems   : Boolean = true,
                    emptyResult : Boolean = true): Gen[Vector[A]] =
    if (orderedElems.isEmpty)
      Gen pure Vector.empty
    else {
      val ss: SizeSpec = (if (dropElems) 0 else 1) to (maxDups + 1)
      lazy val genOne = Gen.choose_!(orderedElems).map(Vector.empty[A] :+ _)
      Gen { ctx =>
        val v = Vector.newBuilder[A]
        for (a <- orderedElems) {
          var count = ss.gen.run(ctx)
          while (count > 0) {
            v += a
            count -= 1
          }
        }
        var r = v.result()
        if (!emptyResult && r.isEmpty)
          r = genOne run ctx
        r
      }
    }

  /**
    * Ensures that an element is never chosen more than once per n elements.
    *
    * fairlyDistributedSeq(1, 2, 3)(6)
    * may return [1,3,2,2,1,3] or [3,2,1,3,2,1] but never [1,1,1,1,2,3].
    */
  def fairlyDistributedSeq[A](as: Traversable[A])(implicit ss: SizeSpec): Gen[Vector[A]] =
    if (as.isEmpty)
      Gen pure Vector.empty
    else
      for {
        sz ← ss.gen
        it ← fairlyDistributed(as)
      } yield it.take(sz).toVector

  /**
    * Ensures that an element is never chosen more than once per n elements.
    *
    * fairlyDistributedSeq(1, 2, 3)(6)
    * may return [1,3,2,2,1,3] or [3,2,1,3,2,1] but never [1,1,1,1,2,3].
    */
  def fairlyDistributed[A](as: Traversable[A]): Gen[Iterator[A]] =
    if (as.isEmpty)
      Gen pure Iterator.empty
    else
      Gen(Gen.shuffle(as).samples(_).flatten)

  // --------------------------------------------------------------
  // Traverse using plain Scala collections and CanBuildFrom (fast)
  // --------------------------------------------------------------

  def traverse[T[X] <: TraversableOnce[X], A, B](as: T[A])(f: A => Gen[B])(implicit cbf: CanBuildFrom[T[A], B, T[B]]): Gen[T[B]] =
    Gen { c =>
      val r = cbf(as)
      as.foreach(a => r += f(a).run(c))
      r.result()
    }

  def traverseG[T[X] <: TraversableOnce[X], A, B](gs: T[Gen[A]])(f: A => Gen[B])(implicit cbf: CanBuildFrom[T[Gen[A]], B, T[B]]): Gen[T[B]] =
    Gen { c =>
      val r = cbf(gs)
      gs.foreach(g => r += f(g run c).run(c))
      r.result()
    }

  @inline def sequence[T[X] <: TraversableOnce[X], A](gs: T[Gen[A]])(implicit cbf: CanBuildFrom[T[Gen[A]], A, T[A]]): Gen[T[A]] =
    traverse(gs)(identity)

  // ---------------------
  // Traverse using Scalaz
  // ---------------------

  def traverseZ [T[_], A, B](as: T[A]     )(f: A => Gen[B])(implicit T: Traverse[T]): Gen[T[B]] = T.traverse(as)(f)
  def traverseZG[T[_], A, B](gs: T[Gen[A]])(f: A => Gen[B])(implicit T: Traverse[T]): Gen[T[B]] = T.traverse(gs)(_ flatMap f)
  def sequenceZ [T[_], A   ](gs: T[Gen[A]])                (implicit T: Traverse[T]): Gen[T[A]] = T.sequence(gs)

  def distribute  [F[_], B]   (a: Gen[F[B]])(implicit D: Distributive[F])            : F[Gen[B]]             = D.cosequence(a)
  def distributeR [A, B]      (a: Gen[A => B])                                       : A => Gen[B]           = distribute[({type f[x] = A => x})#f, B](a)
  def distributeRK[A, B]      (a: Gen[A => B])                                       : Kleisli[Gen, A, B]    = Kleisli(distributeR(a))
  def distributeK [F[_], A, B](a: Gen[Kleisli[F, A, B]])(implicit D: Distributive[F]): Kleisli[F, A, Gen[B]] = distribute[({type f[x] = Kleisli[F, A, x]})#f, B](a)

  def tuple2[A,B](A:Gen[A], B:Gen[B]): Gen[(A,B)] = for {a←A;b←B} yield (a,b)
  def tuple3[A,B,C](A:Gen[A], B:Gen[B], C:Gen[C]): Gen[(A,B,C)] = for {a←A;b←B;c←C} yield (a,b,c)
  def tuple4[A,B,C,D](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D]): Gen[(A,B,C,D)] = for {a←A;b←B;c←C;d←D} yield (a,b,c,d)
  def tuple5[A,B,C,D,E](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E]): Gen[(A,B,C,D,E)] = for {a←A;b←B;c←C;d←D;e←E} yield (a,b,c,d,e)
  def tuple6[A,B,C,D,E,F](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F]): Gen[(A,B,C,D,E,F)] = for {a←A;b←B;c←C;d←D;e←E;f←F} yield (a,b,c,d,e,f)
  def tuple7[A,B,C,D,E,F,G](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F], G:Gen[G]): Gen[(A,B,C,D,E,F,G)] = for {a←A;b←B;c←C;d←D;e←E;f←F;g←G} yield (a,b,c,d,e,f,g)
  def tuple8[A,B,C,D,E,F,G,H](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F], G:Gen[G], H:Gen[H]): Gen[(A,B,C,D,E,F,G,H)] = for {a←A;b←B;c←C;d←D;e←E;f←F;g←G;h←H} yield (a,b,c,d,e,f,g,h)
  def tuple9[A,B,C,D,E,F,G,H,I](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F], G:Gen[G], H:Gen[H], I:Gen[I]): Gen[(A,B,C,D,E,F,G,H,I)] = for {a←A;b←B;c←C;d←D;e←E;f←F;g←G;h←H;i←I} yield (a,b,c,d,e,f,g,h,i)

  def apply2[A,B,Z](z: (A,B)⇒Z)(A:Gen[A], B:Gen[B]): Gen[Z] = for {a←A;b←B} yield z(a,b)
  def apply3[A,B,C,Z](z: (A,B,C)⇒Z)(A:Gen[A], B:Gen[B], C:Gen[C]): Gen[Z] = for {a←A;b←B;c←C} yield z(a,b,c)
  def apply4[A,B,C,D,Z](z: (A,B,C,D)⇒Z)(A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D]): Gen[Z] = for {a←A;b←B;c←C;d←D} yield z(a,b,c,d)
  def apply5[A,B,C,D,E,Z](z: (A,B,C,D,E)⇒Z)(A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E]): Gen[Z] = for {a←A;b←B;c←C;d←D;e←E} yield z(a,b,c,d,e)
  def apply6[A,B,C,D,E,F,Z](z: (A,B,C,D,E,F)⇒Z)(A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F]): Gen[Z] = for {a←A;b←B;c←C;d←D;e←E;f←F} yield z(a,b,c,d,e,f)
  def apply7[A,B,C,D,E,F,G,Z](z: (A,B,C,D,E,F,G)⇒Z)(A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F], G:Gen[G]): Gen[Z] = for {a←A;b←B;c←C;d←D;e←E;f←F;g←G} yield z(a,b,c,d,e,f,g)
  def apply8[A,B,C,D,E,F,G,H,Z](z: (A,B,C,D,E,F,G,H)⇒Z)(A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F], G:Gen[G], H:Gen[H]): Gen[Z] = for {a←A;b←B;c←C;d←D;e←E;f←F;g←G;h←H} yield z(a,b,c,d,e,f,g,h)
  def apply9[A,B,C,D,E,F,G,H,I,Z](z: (A,B,C,D,E,F,G,H,I)⇒Z)(A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F], G:Gen[G], H:Gen[H], I:Gen[I]): Gen[Z] = for {a←A;b←B;c←C;d←D;e←E;f←F;g←G;h←H;i←I} yield z(a,b,c,d,e,f,g,h,i)

  @inline def lift2[A,B,Z](A:Gen[A], B:Gen[B])(z: (A,B)⇒Z): Gen[Z] = apply2(z)(A,B)
  @inline def lift3[A,B,C,Z](A:Gen[A], B:Gen[B], C:Gen[C])(z: (A,B,C)⇒Z): Gen[Z] = apply3(z)(A,B,C)
  @inline def lift4[A,B,C,D,Z](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D])(z: (A,B,C,D)⇒Z): Gen[Z] = apply4(z)(A,B,C,D)
  @inline def lift5[A,B,C,D,E,Z](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E])(z: (A,B,C,D,E)⇒Z): Gen[Z] = apply5(z)(A,B,C,D,E)
  @inline def lift6[A,B,C,D,E,F,Z](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F])(z: (A,B,C,D,E,F)⇒Z): Gen[Z] = apply6(z)(A,B,C,D,E,F)
  @inline def lift7[A,B,C,D,E,F,G,Z](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F], G:Gen[G])(z: (A,B,C,D,E,F,G)⇒Z): Gen[Z] = apply7(z)(A,B,C,D,E,F,G)
  @inline def lift8[A,B,C,D,E,F,G,H,Z](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F], G:Gen[G], H:Gen[H])(z: (A,B,C,D,E,F,G,H)⇒Z): Gen[Z] = apply8(z)(A,B,C,D,E,F,G,H)
  @inline def lift9[A,B,C,D,E,F,G,H,I,Z](A:Gen[A], B:Gen[B], C:Gen[C], D:Gen[D], E:Gen[E], F:Gen[F], G:Gen[G], H:Gen[H], I:Gen[I])(z: (A,B,C,D,E,F,G,H,I)⇒Z): Gen[Z] = apply9(z)(A,B,C,D,E,F,G,H,I)

  // ===================================================================================================================
  // Deprecated

  import scalaz.OneAnd

  @deprecated("Replace with Gen.upper.string", "0.6.0")
  def upperString(implicit ss: SizeSpec): Gen[String] = stringOf(upper)(ss)

  @deprecated("Replace with Gen.upper.string1", "0.6.0")
  def upperString1(implicit ss: SizeSpec): Gen[String] = stringOf1(upper)(ss)

  @deprecated("Replace with Gen.lower.string", "0.6.0")
  def lowerString(implicit ss: SizeSpec): Gen[String] = stringOf(lower)(ss)

  @deprecated("Replace with Gen.lower.string1", "0.6.0")
  def lowerString1(implicit ss: SizeSpec): Gen[String] = stringOf1(lower)(ss)

  @deprecated("Replace with Gen.alpha.string", "0.6.0")
  def alphaString(implicit ss: SizeSpec): Gen[String] = stringOf(alpha)(ss)

  @deprecated("Replace with Gen.alpha.string1", "0.6.0")
  def alphaString1(implicit ss: SizeSpec): Gen[String] = stringOf1(alpha)(ss)

  @deprecated("Replace with Gen.numeric.string", "0.6.0")
  def numericString(implicit ss: SizeSpec): Gen[String] = stringOf(numeric)(ss)

  @deprecated("Replace with Gen.numeric.string1", "0.6.0")
  def numericString1(implicit ss: SizeSpec): Gen[String] = stringOf1(numeric)(ss)

  @deprecated("Replace with Gen.alphaNumeric.string", "0.6.0")
  def alphaNumericString(implicit ss: SizeSpec): Gen[String] = stringOf(alphaNumeric)(ss)

  @deprecated("Replace with Gen.alphaNumeric.string1", "0.6.0")
  def alphaNumericString1(implicit ss: SizeSpec): Gen[String] = stringOf1(alphaNumeric)(ss)

  @deprecated("Replace with Gen.ascii.string", "0.6.0")
  def asciiString(implicit ss: SizeSpec): Gen[String] = stringOf(ascii)(ss)

  @deprecated("Replace with Gen.ascii.string1", "0.6.0")
  def asciiString1(implicit ss: SizeSpec): Gen[String] = stringOf1(ascii)(ss)

  @deprecated("Replace with Gen.unicode.string", "0.6.0")
  def unicodeString(implicit ss: SizeSpec): Gen[String] = stringOf(unicode)(ss)

  @deprecated("Replace with Gen.unicode.string1", "0.6.0")
  def unicodeString1(implicit ss: SizeSpec): Gen[String] = stringOf1(unicode)(ss)

  // lazy val digit: Gen[Digit] = chooseArray_!(Digit.digits.toArray)
  // @deprecated("Replace with Gen.digit.list.", "0.6.0")
  // def digits(implicit ss: SizeSpec): Gen[List[Digit]] = digit.list(ss)
  // @deprecated("Replace with Gen.digit.nel or Gen.digit.list1.", "0.6.0")
  // def digits1(implicit ss: SizeSpec): Gen[NonEmptyListZ[Digit]] = digit.nel(ss)

  //  def identifier          : Gen[NonEmptyListZ[Char]]  = identifier)
  //  def identifierString    : Gen[String]              = identifierstring)
  //  def properNoun          : Gen[NonEmptyListZ[Char]]  = propernoun)
  //  def properNounString    : Gen[String]              = propernounstring)
  //  def mkUnicodeString(bs: List[Byte]): String = new String(bs.toArray, "UTF-16")

  @deprecated("Replace with Gen.numeric.list.", "0.6.0")
  def numerics(implicit ss: SizeSpec): Gen[List[Char]] = numeric.list(ss)

  @deprecated("Replace with Gen.char.list.", "0.6.0")
  def chars(implicit ss: SizeSpec): Gen[List[Char]] = char.list(ss)

  @deprecated("Replace with Gen.upper.list.", "0.6.0")
  def uppers(implicit ss: SizeSpec): Gen[List[Char]] = upper.list(ss)

  @deprecated("Replace with Gen.lower.list.", "0.6.0")
  def lowers(implicit ss: SizeSpec): Gen[List[Char]] = lower.list(ss)

  @deprecated("Replace with Gen.alpha.list.", "0.6.0")
  def alphas(implicit ss: SizeSpec): Gen[List[Char]] = alpha.list(ss)

  @deprecated("Replace with Gen.alphaNumeric.list.", "0.6.0")
  def alphaNumerics(implicit ss: SizeSpec): Gen[List[Char]] = alphaNumeric.list(ss)

  @deprecated("Replace with Gen.numeric.nel or Gen.numeric.list1.", "0.6.0")
  def numerics1(implicit ss: SizeSpec): Gen[NonEmptyListZ[Char]] = numeric.nel(ss)

  @deprecated("Replace with Gen.char.nel or Gen.char.list1.", "0.6.0")
  def chars1(implicit ss: SizeSpec): Gen[NonEmptyListZ[Char]] = char.nel(ss)

  @deprecated("Replace with Gen.upper.nel or Gen.upper.list1.", "0.6.0")
  def uppers1(implicit ss: SizeSpec): Gen[NonEmptyListZ[Char]] = upper.nel(ss)

  @deprecated("Replace with Gen.lower.nel or Gen.lower.list1.", "0.6.0")
  def lowers1(implicit ss: SizeSpec): Gen[NonEmptyListZ[Char]] = lower.nel(ss)

  @deprecated("Replace with Gen.alpha.nel or Gen.alpha.list1.", "0.6.0")
  def alphas1(implicit ss: SizeSpec): Gen[NonEmptyListZ[Char]] = alpha.nel(ss)

  @deprecated("Replace with Gen.alphaNumeric.nel or Gen.alphaNumeric.list1.", "0.6.0")
  def alphaNumerics1(implicit ss: SizeSpec): Gen[NonEmptyListZ[Char]] = alphaNumeric.nel(ss)

  @deprecated("Replace with Gen.chooseChar.", "0.6.0")
  def charOf(ev: Char, s: String, rs: NumericRange[Char]*): Gen[Char] = chooseChar(ev, s, rs: _*)

  @deprecated("Replace with Gen.pure.", "0.6.0")
  def insert[A](a: A): Gen[A] = pure(a)

  @deprecated("Replace with Gen.chooseGen.", "0.6.0")
  def oneOfG[A](a: Gen[A], as: Gen[A]*): Gen[A] = chooseGen(a, as: _*)

  @deprecated("Replace with Gen.chooseGenNE.", "0.6.0")
  def oneOfGL[A](gs: NonEmptyListZ[Gen[A]]): Gen[A] = chooseGenNE(gs)

  @deprecated("Replace with Gen.tryChoose.", "0.6.0")
  def oneOfSeq[A](as: Seq[A]): Gen[Option[A]] = tryChoose(as)

  @deprecated("Replace with Gen.tryGenChoose.", "0.6.0")
  def oneOfO[A](as: Seq[A]): Option[Gen[A]] = tryGenChoose(as)

  @deprecated("Replace with Gen.choose.", "0.6.0")
  def oneOf[A](a: A, as: A*): Gen[A] = choose(a, as: _*)

  @deprecated("Replace with Gen.chooseNE(nel).", "0.6.0")
  def oneOfL[A](x: NonEmptyListZ[A]): Gen[A] = chooseNE(x)

  @deprecated("Replace with Gen.chooseNE.", "0.6.0")
  def oneOfV[A](x: OneAnd[Vector, A]): Gen[A] = chooseNE(x)

  @deprecated("Replace with `a pair b` or `Gen.tuple2`.", "0.6.0")
  def pair[A, B](A: Gen[A], B: Gen[B]): Gen[(A, B)] = tuple2(A, B)

  @deprecated("Replace with Gen.tuple3.", "0.6.0")
  def triple[A, B, C](A: Gen[A], B: Gen[B], C: Gen[C]): Gen[(A, B, C)] = tuple3(A, B, C)

  @deprecated("Replace with `g strengthL l`.", "0.6.0")
  def sequencePair[X, A](x: X, r: Gen[A]): Gen[(X, A)] = r strengthL x

  @deprecated("Replace with Gen.frequencyNE.", "0.6.1")
  def frequencyL[A](xs: NonEmptyListZ[Freq[A]]): Gen[A] = frequencyNE(xs)
}
