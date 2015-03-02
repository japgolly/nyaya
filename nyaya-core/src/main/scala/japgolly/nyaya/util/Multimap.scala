package japgolly.nyaya.util

import scalaz.Equal
import MultiValues.Commutative
import Multimap._, Internal._

object MultiValues {
  trait Commutative[L[_]]
  @inline def apply[L[_]](implicit F: MultiValues[L]): MultiValues[L] = F
}

trait MultiValues[L[_]] {
  def empty[A]: L[A]
  def add1[A](a: L[A], b: A): L[A]
  def del1[A](a: L[A], b: A): L[A]
  def addn[A](a: L[A], b: L[A]): L[A]
  def deln[A](a: L[A], b: L[A]): L[A]
  def foldl[A, B](a: A, b: L[B])(f: (A, B) => A): A
  def foldr[A, B](a: A, b: L[B])(f: (A, B) => A): A
  def stream[A](a: L[A]): Stream[A]
  def isEmpty[A](a: L[A]): Boolean
}

final class Multimap[K, L[_], V](val m: Map[K, L[V]])(implicit L: MultiValues[L]) {

  override def toString = m.toString()

  override def hashCode = m.##

  override def equals(o: Any) = o match {
    case t: Multimap[_, _, _] => m equals t.m
    case t: Map[_, _]         => m equals t
    case _                    => false
  }

  @inline def keys   = m.keys
  @inline def keySet = m.keySet
  @inline def values = m.values

  @inline private[this] def copy(m: Map[K, L[V]]) = new Multimap[K, L, V](m)

  def apply   (k: K): L[V]            = m getOrEmpty k
  def mod     (k: K, f: L[V] => L[V]) = copy(m.mod(k, f))
  def add     (k: K, v: V)            = mod(k, _ add1 v)
  def addPair (kv: (K, V))            = copy(m add kv)
  def addPairs(kvs: (K, V)*)          = copy(kvs.foldLeft(m)(_ add _))
  def addvs   (k: K, vs: L[V])        = mod(k, _ addn vs)
  def addks   (ks: L[K], v: V)        = copy(m.addks(ks, v))
  def del     (k: K, v: V)            = mod(k, _ del1 v)
  def delk    (k: K)                  = copy(m - k)
  def delv    (v: V)                  = copy(m delv v)
  def delks   (ks: L[K])              = copy(ks.foldl(m)(_ - _))
  def delvs   (vs: L[V])              = copy(m.mapValues(_ deln vs))
  def setks   (ks: L[K], v: V)        = copy(m.delv(v).addks(ks, v))
  def setvs   (k: K, vs: L[V])        = mod(k, _ => vs)

  def reverse(implicit ev: Commutative[L]): Multimap[V, L, K] = Multimap.reverse(m)

  def reverseM[M[_]: MultiValues](implicit ev: Commutative[M]): Multimap[V, M, K] = Multimap.reverseM(m)

  def ++(n: Map[K, L[V]]) =
    copy(n.foldLeft(m)((q, x) => q.addn(x._1, x._2)))

  /** Removes x entirely. Same as delk(x).delv(x) */
  def delkv(x: K)(implicit ev: K =:= V) =
    copy((m - x) delv x)

  /** Removes a↔b and b↔a. */
  def unlink(a: K, b: V)(implicit e: K =:= V, f: V =:= K) =
    copy(m.mod(a, _ del1 b).mod(b, _ del1 a))

  def allValues                        : Stream[V]     = vstreamf(L.stream)
  def streamKV                         : Stream[(K,V)] = m.toStream.flatMap(kv => kv._2.stream.map(v => (kv._1, v)))
  def vstream [A](f: L[V] => A)        : Stream[A]     = values.toStream.map(f)
  def vstreamf[A](f: L[V] => Stream[A]): Stream[A]     = values.toStream.flatMap(f)


  def isEmpty     = m.isEmpty
  def nonEmpty    = m.nonEmpty
  def keyCount    = m.size
  def valueCount  = m.valuesIterator.foldLeft(0)(_ + _.count)
  def sizeSummary = SizeSummary(m.valuesIterator.foldLeft(Vector.empty[Int])(_ :+ _.count))
}

object Multimap {
  implicit def multimapEqual[K, L[_], V](implicit e: Equal[Map[K, L[V]]]): Equal[Multimap[K, L, V]] =
    Equal.equalBy((_: Multimap[K, L, V]).m)

  private[util] object Internal {
    implicit final class MultiMapExt[K, L[_], V](val m: Map[K, L[V]]) extends AnyVal {
      @inline def delv(v: V)                (implicit L: MultiValues[L]): Map[K, L[V]] = m.mapValues(_ del1 v)
      @inline def getOrEmpty(k: K)          (implicit L: MultiValues[L]): L[V]         = m.getOrElse(k, L.empty)
      @inline def add(kv: (K, V))           (implicit L: MultiValues[L]): Map[K, L[V]] = mod(kv._1, _ add1 kv._2)
      @inline def add(k: K, v: V)           (implicit L: MultiValues[L]): Map[K, L[V]] = mod(k, _ add1 v)
      @inline def addn(k: K, vs: L[V])      (implicit L: MultiValues[L]): Map[K, L[V]] = mod(k, _ addn vs)
      @inline def addks(ks: L[K], v: V)     (implicit L: MultiValues[L]): Map[K, L[V]] = ks.foldl(m)(_.add(_, v))
      @inline def mod(k: K, f: L[V] => L[V])(implicit L: MultiValues[L]): Map[K, L[V]] = put(k, f(getOrEmpty(k)))
      @inline def put(k: K, v: L[V])        (implicit L: MultiValues[L]): Map[K, L[V]] =
        if (v.isEmpty) m - k else m.updated(k, v)
    }

    implicit final class MultiValueOps[L[_], A](val as: L[A]) extends AnyVal {
      def add1(b: A)                    (implicit L: MultiValues[L]) = L.add1(as, b)
      def del1(b: A)                    (implicit L: MultiValues[L]) = L.del1(as, b)
      def addn(b: L[A])                 (implicit L: MultiValues[L]) = L.addn(as, b)
      def deln(b: L[A])                 (implicit L: MultiValues[L]) = L.deln(as, b)
      def foldl[Z](z: Z)(f: (Z, A) => Z)(implicit L: MultiValues[L]) = L.foldl(z, as)(f)
      def foldr[Z](z: Z)(f: (Z, A) => Z)(implicit L: MultiValues[L]) = L.foldr(z, as)(f)
      def stream                        (implicit L: MultiValues[L]) = L.stream(as)
      def set                           (implicit L: MultiValues[L]) = stream.toSet
      def count                         (implicit L: MultiValues[L]) = foldl(0)((q, _) => q + 1)
      def isEmpty                       (implicit L: MultiValues[L]) = L.isEmpty(as)
    }
  }

  case class SizeSummary(vs: Vector[Int]) {
    val keys = vs.size
    val values = vs.sum
    val vmin = if (vs.isEmpty) 0 else vs.min
    val vmax = if (vs.isEmpty) 0 else vs.max
    val avg = values.toDouble / keys.toDouble
    val avgi = avg.toInt
    val stddev = {
      val s = (0.0 /: vs)((q,x) => q + Math.pow(x - avg, 2))
      Math.sqrt(s / keys.toDouble)
    }
    override val toString =
      if (vs.isEmpty)
        "empty"
      else {
        def fmt(d: Double) = "%.03f" format d
        def rng(z: Double) = {
          val s = stddev * z
          val List(a, b) = List(-s, s).map(x => (x + avg).toInt)
          s"[$a…$b]"
        }
        s"$keys → Σ$values [$vmin … $avgi … $vmax] σ=${fmt(stddev)} 68%:${rng(1)} 95%:${rng(2)}"
      }
  }

  def apply[K, L[_], V](m: Map[K, L[V]])(implicit L: MultiValues[L]) =
    new Multimap[K, L, V](m.filterNot(_._2.isEmpty))

  def empty[K, L[_]: MultiValues, V] =
    new Multimap[K, L, V](Map.empty)

  def reverse[A, L[_]: MultiValues, B](ab: Map[A, L[B]])(implicit ev: Commutative[L]): Multimap[B, L, A] =
    (empty[B, L, A] /: ab){ case (q, (a, bs)) => bs.foldl(q)(_.add(_, a)) }

  def reverseM[A, L[_]: MultiValues, M[_]: MultiValues, B](ab: Map[A, L[B]])(implicit ev: Commutative[M]): Multimap[B, M, A] =
    (empty[B, M, A] /: ab){ case (q, (a, bs)) => bs.foldl(q)(_.add(_, a)) }
}